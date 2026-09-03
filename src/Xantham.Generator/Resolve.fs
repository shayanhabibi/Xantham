/// Tier 2 - Resolve: the type table. Breadth-first from the harvested exports' types, batched
/// per generation frontier through the mailbox, memoized on `TypeResponse.Id`. The tier's
/// invariant is closure: every type id a `TypeFacts` refers to is in the table or recorded in
/// `NotFollowed` with its reason.
module Xantham.Generator.Resolve

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Generations the breadth-first walk follows before recording the rest as deliberately not
/// followed. The frontier doubles as the cycle boundary - ids already derived are never
/// re-entered - so this bounds runaway utility-type expansion, not recursion.
[<Literal>]
let private FollowDepth = 12

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The server's complaint without the Go stack it arrives with: the first line, minus the
/// `panic: ` its recovery handler prefixes. The one refusal seen live is restated in fixed
/// words rather than quoted: Go's encoder says "cannot marshal" on one run and "unable to
/// marshal" on the next for the same value, and a finding is golden text.
let private complaint (message: string) =
    let line = message.Split('\n').[0].Trim()
    let line = if line.StartsWith "panic: " then line.Substring 7 else line
    let marker = "unsupported value: "

    match line.IndexOf marker with
    | -1 -> line
    | at -> $"its answer holds a value JSON cannot carry: {line.Substring(at + marker.Length)}"

/// Runs one request, turning the server's refusal into a reason rather than a raised
/// exception. Only `TsGoError` is caught: that is the server declining to answer *this*
/// request while the channel lives on. The one case seen live is a result it cannot encode -
/// a number literal type whose value is `1e999` is `+Inf` to Go's JSON encoder, and the
/// refusal is the whole answer (`type-fest`'s `PositiveInfinity`). Anything else still
/// raises: a dead channel has nothing trustworthy left to say.
let private attempt (work: Async<'T>) : Async<Result<'T, string>> =
    async {
        try
            let! value = work
            return Ok value
        with TsGoError(method, message) ->
            return Error $"the compiler could not answer {method} ({complaint message})"
    }

/// The type ids each export resolves to: the declared type for type-like symbols, the value
/// type for value-like ones, both for symbols that are both (a class). The responses land in
/// the table shallow; `resolveTypeTable` derives them.
let resolveExportTypes: Pass<ResolveModel> =
    {
        Name = "resolve-export-types"
        Run =
            fun ctx model ->
                async {
                    let! resolved =
                        model.Harvest.Exports
                        |> List.map (fun export ->
                            async {
                                let ask relevant (request: int -> Async<TypeResponse>) =
                                    if relevant then
                                        async {
                                            let! ty = attempt (request export.Symbol.Id)
                                            return Some ty
                                        }
                                    else
                                        async.Return None

                                let! declared =
                                    ask
                                        (hasAny SymbolFlags.Type export.Symbol.Flags)
                                        ctx.Session.getDeclaredTypeOfSymbol

                                let! value =
                                    ask (hasAny SymbolFlags.Value export.Symbol.Flags) ctx.Session.getTypeOfSymbol

                                return export, declared, value
                            })
                        // Sequential, unlike every other fan-out in this tier: asking for a
                        // declared type is what *creates* it in the checker, and a type alias
                        // stamps its name on the type it creates. Two aliases with the same right
                        // side (`type A = X & Y; type B = X & Y`) therefore race - whichever is
                        // asked for first owns the intersection, and the other either aliases it
                        // or widens. Under Async.Parallel that ordering came out of the thread
                        // pool, so the same package generated two different files.
                        |> Async.Sequential

                    let answered (response: Result<TypeResponse, string> option) =
                        response |> Option.bind Result.toOption

                    let exportTypes =
                        resolved
                        |> Array.fold
                            (fun map (export, declared, value) ->
                                Map.add
                                    export.Symbol.Id
                                    {
                                        Declared = answered declared |> Option.map _.Id
                                        Value = answered value |> Option.map _.Id
                                    }
                                    map)
                            model.ExportTypes

                    let types =
                        resolved
                        |> Array.collect (fun (_, declared, value) ->
                            [|
                                yield! Option.toArray (answered declared)
                                yield! Option.toArray (answered value)
                            |])
                        |> Array.fold (fun map ty -> Map.add ty.Id (TypeFacts.shallow ty) map) model.Types

                    // An export whose type the compiler would not hand over has nothing to shape.
                    // It drops here, loudly: the finding is what tells `audit-coverage` that the
                    // absence is owned.
                    let findings =
                        [
                            for export, declared, value in resolved do
                                for facet, response in [ "declared type", declared; "value type", value ] do
                                    match response with
                                    | Some(Error reason) ->
                                        Finding.make
                                            export.ExportName
                                            (ResolveExportTypes.FacetNotResolved(facet, reason))
                                    | _ -> ()
                        ]

                    let model =
                        { model with
                            ExportTypes = exportTypes
                            Types = types
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

/// The structure of a type that has members: properties, call and construct signatures, index
/// signatures, each with the responses it discovered. Object types and the intersections of
/// them share it, because the checker answers the same questions about both: the properties of
/// `A & B` are both sets, a property both declare typed as the intersection of its two types.
let private deriveStructure (ctx: Context) (ty: TypeResponse) =
    async {
        let! properties = ctx.Session.getPropertiesOfType ty.Id
        let properties = properties |> ValueOption.defaultValue [||]

        let resolveMember readOnlyRelevant (property: SymbolResponse) =
            async {
                let! propertyType = ctx.Session.getTypeOfSymbol property.Id
                let! docs = ctx.Session.getDocumentationComment property.Id
                let! tags = ctx.Session.getJsDocTags property.Id

                // `CheckFlags.Readonly` only marks transient symbols; a declared
                // `readonly` modifier is the checker's to see, so ask it.
                let! readOnly =
                    if readOnlyRelevant then
                        ctx.Session.isReadonlySymbol property.Id
                    else
                        async.Return false

                return
                    {
                        Symbol = property
                        Docs = docs
                        Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                        Optional =
                            property.Flags.HasFlag SymbolFlags.Optional
                            || property.CheckFlags.HasFlag CheckFlags.OptionalParameter
                        ReadOnly = readOnly
                        TypeId = propertyType.Id
                    },
                    propertyType
            }

        let! members = properties |> Array.map (resolveMember true) |> Async.Parallel

        let resolveSignatures kind =
            async {
                let! signatures = ctx.Session.getSignaturesOfType (ty.Id, kind)

                return!
                    signatures
                    |> Array.map (fun signature ->
                        async {
                            let! parameters = ctx.Session.getParametersOfSignature signature.Id
                            let parameters = parameters |> ValueOption.defaultValue [||]
                            let! parameterFacts = parameters |> Array.map (resolveMember false) |> Async.Parallel
                            let! returnType = ctx.Session.getReturnTypeOfSignature signature.Id

                            // A generic callback alias spells its parameters on the
                            // signature, not the type, so `Mapper<T> = (t: T) => T` has
                            // nothing to bind without this call.
                            let! typeParameters = ctx.Session.getTypeParametersOfSignature signature.Id
                            let typeParameters = typeParameters |> ValueOption.defaultValue [||]

                            return
                                {
                                    Parameters = parameterFacts |> Array.map fst |> Array.toList
                                    HasRest = signature.Flags.HasFlag SignatureFlags.HasRestParameter
                                    TypeParameters = typeParameters |> Array.map _.Id |> Array.toList
                                    ReturnTypeId = returnType.Id
                                },
                                [ yield! parameterFacts |> Array.map snd; yield! typeParameters; returnType ]
                        })
                    |> Async.Parallel
            }

        let! callSignatures = resolveSignatures SignatureKind.Call
        let! constructSignatures = resolveSignatures SignatureKind.Construct

        // An index signature is not a property: `getPropertiesOfType` returns nothing at
        // all for `interface Bag { [key: string]: number }`, so without this the type
        // reaches the shape tier looking empty and is never declared. Key and value are
        // followed into the table like any other referenced type.
        let! indexInfos = ctx.Session.getIndexInfosOfType ty.Id

        let indexInfos = indexInfos |> ValueOption.defaultValue [||] |> Array.toList

        let discovered =
            [
                yield! members |> Array.map snd
                yield! callSignatures |> Array.collect (snd >> List.toArray)
                yield! constructSignatures |> Array.collect (snd >> List.toArray)
                for info in indexInfos do
                    info.KeyType
                    info.ValueType
            ]

        return
            {|
                Members = members |> Array.map fst |> Array.toList
                IndexInfos =
                    indexInfos
                    |> List.map (fun info ->
                        {
                            KeyTypeId = info.KeyType.Id
                            ValueTypeId = info.ValueType.Id
                            IsReadonly = info.IsReadonly = ValueSome true
                        })
                CallSignatures = callSignatures |> Array.map fst |> Array.toList
                ConstructSignatures = constructSignatures |> Array.map fst |> Array.toList
                Discovered = discovered
            |}
    }

/// Derives one type's facts and reports the responses it discovered, for the next frontier.
let private deriveFacts (ctx: Context) (ty: TypeResponse) : Async<TypeFacts * TypeResponse list> =
    async {
        let has flag = ty.Flags.HasFlag(flag: TypeFlags)

        // `boolean` is a union of `true | false` wearing the Boolean flag; it maps as a
        // primitive, so its members are not worth a round trip.
        if has TypeFlags.Union && not (has TypeFlags.Boolean) then
            let! members = ctx.Session.getTypesOfType ty.Id
            let members = members |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            // A generic union alias binds its parameter on the alias, exactly as an object or
            // conditional alias does: `type Ref<T> = T | ((value: T) => void)` has nowhere
            // else to put `T`, and without it the shape tier reads the arms under no scope
            // and widens every one to obj.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            return
                { TypeFacts.shallow ty with
                    UnionMembers = members |> List.map _.Id
                    AliasTypeArguments = aliasTypeArguments |> List.map _.Id
                },
                members @ aliasTypeArguments
        elif has TypeFlags.Intersection then
            // The constituents, followed into the table. A branding intersection (§4.6) is
            // decided by what its object operands *contain* - a marker property or a real
            // one - so the operands are resolved in full rather than identified.
            let! members = ctx.Session.getTypesOfType ty.Id
            let members = members |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            // The alias's arguments, for the same reason an object alias needs them: a
            // flattened intersection is declared over them (§4.6), and a phantom is worth
            // nothing without the arity.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // An intersection of object types is a shape (§4.6): the checker's `getPropertiesOfType`
            // hands over both member sets flattened, a property both operands declare typed as the
            // intersection of its two types, so the shape tier declares it as one interface. Only
            // when every operand is an object: a primitive operand makes it a brand or nothing, and
            // a type-parameter operand (`T & { id: number }`) has no members to read until it is
            // instantiated. Asking would also drag the primitive's apparent members (`String`'s
            // whole prototype) into the table for nothing.
            let! structure =
                if
                    not members.IsEmpty
                    && members |> List.forall (fun m -> m.Flags.HasFlag TypeFlags.Object)
                then
                    async {
                        let! structure = deriveStructure ctx ty
                        return Some structure
                    }
                else
                    async.Return None

            return
                { TypeFacts.shallow ty with
                    Members = structure |> Option.map _.Members |> Option.defaultValue []
                    IndexInfos = structure |> Option.map _.IndexInfos |> Option.defaultValue []
                    CallSignatures = structure |> Option.map _.CallSignatures |> Option.defaultValue []
                    ConstructSignatures = structure |> Option.map _.ConstructSignatures |> Option.defaultValue []
                    IntersectionMembers = members |> List.map _.Id
                    AliasTypeArguments = aliasTypeArguments |> List.map _.Id
                },
                members
                @ aliasTypeArguments
                @ (structure |> Option.map _.Discovered |> Option.defaultValue [])
        elif has TypeFlags.EnumLiteral then
            // An enum member: its symbol names the F# enum case (§4.7); the value is already
            // on the response.
            let! symbol = ctx.Session.getSymbolOfType ty.Id

            return
                { TypeFacts.shallow ty with
                    Origin = Grouping.classify ctx.PackageDir symbol
                    SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                },
                []
        elif has TypeFlags.Object then
            let! symbol = ctx.Session.getSymbolOfType ty.Id
            let origin = Grouping.classify ctx.PackageDir symbol

            // Type arguments resolve for every group (O7 note): an external `Array<T>` or
            // `Promise<T>` carries entry-package types the walk must still reach.
            let isReference =
                ty.ObjectFlags
                |> ValueOption.map (fun flags -> flags.HasFlag ObjectFlags.Reference)
                |> ValueOption.defaultValue false

            let! typeArguments =
                if isReference then
                    ctx.Session.getTypeArguments ty.Id
                else
                    async.Return ValueNone

            let typeArguments =
                typeArguments |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            // A tuple's per-element flags live on its *target*, not on the reference the
            // checker hands back (verified live: the reference reports `elementFlags: null`).
            // The target is read for its flags and then dropped rather than followed - it is
            // the generic tuple type, so deriving it re-derives all of `Array.prototype` for
            // every distinct tuple shape.
            let! tupleElements =
                if ty.IsTupleType = ValueSome true then
                    async {
                        let! target = ctx.Session.getTargetOfType ty.Id

                        return
                            target.ElementFlags
                            |> ValueOption.map Array.toList
                            |> ValueOption.defaultValue []
                    }
                else
                    async.Return []

            // A generic alias hangs its parameters off the alias, not the type: the function
            // type behind `type Mapper<T> = (t: T) => T` reports none of its own. Only the
            // arguments that *are* type parameters bind anything - an instantiated alias
            // reports concrete arguments here, and those are already substituted in.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // The O7 shortcut below rests on the group's types having names to be referenced
            // by. A mapped type has none: `Partial<Options>` is written in lib.es5.d.ts, so it
            // groups as the compiler lib, but it is a type-level *function* with no runtime
            // identity - what it stands for is the entry package's own operand, transformed.
            // Deferring to a name that does not exist widens the whole expansion to obj and
            // loses the operand with it (D6), so an anonymous shape is resolved by content
            // whatever group it was written in.
            let objectFlags = ty.ObjectFlags |> ValueOption.defaultValue ObjectFlags.None

            // A member's type is named for the member: the type of `Promise.then` carries
            // the symbol `then`, a member name rather than a declaration head. Such a type
            // resolves by content whatever group it was written in; a symbol that declares
            // a type keeps the shortcut.
            let isMemberType =
                match symbol with
                | ValueSome s ->
                    hasAny (SymbolFlags.Method ||| SymbolFlags.Property ||| SymbolFlags.Signature) s.Flags
                    && not (
                        hasAny
                            (SymbolFlags.Interface
                             ||| SymbolFlags.Class
                             ||| SymbolFlags.TypeAlias
                             ||| SymbolFlags.RegularEnum
                             ||| SymbolFlags.ConstEnum)
                            s.Flags
                    )
                | ValueNone -> false

            // The name a symbol supplies a declaration with. The checker's placeholders -
            // `__type`, `__object`, `__@iterator@194` - all carry the prefix.
            let named (candidate: SymbolResponse voption) =
                match candidate with
                | ValueSome s when not (s.Name.StartsWith "__") -> Some s.Name
                | _ -> None

            // The name a `ship` run of the group declares the shape under. An alias body carries
            // `__type` on the type's own symbol and its declared name on the alias symbol -
            // `type Pair = { left: Widget }` is the whole family - so a shape the type's own
            // symbol leaves unnamed takes the name on its alias. A shape both symbols leave
            // unnamed resolves by content, into a declaration of its own in the entry package.
            //
            // The round trip is spent only where the answer can be used: the entry group and the
            // two kinds above resolve by content whatever either symbol says.
            let! shapeName =
                if
                    GeneratorConfig.disposition ctx.Config origin = Ship
                    || objectFlags.HasFlag ObjectFlags.Mapped
                    || isMemberType
                then
                    async.Return None
                else
                    match named symbol with
                    | Some name -> async.Return(Some name)
                    | None ->
                        async {
                            let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                            return named alias
                        }

            if shapeName.IsSome then
                // Identity only (O7): the shape tier renders references to this group by
                // templated name or widens them, and either way nothing reads its members.
                return
                    { TypeFacts.shallow ty with
                        Origin = origin
                        SymbolName = shapeName
                        TypeArguments = typeArguments |> List.map _.Id
                        TupleElements = tupleElements
                        AliasTypeArguments = aliasTypeArguments |> List.map _.Id
                    },
                    typeArguments @ aliasTypeArguments
            else

                // The generic declaration behind an instantiation (§4.9). `Ready<T>` reached only
                // through `Resource<T> = Ready<T> | ...` is a reference whose target is the
                // declaration itself; with only the reference in the table the shape tier has
                // nothing to name but the instantiation, and its members read a parameter that
                // is not theirs. Entry group only: a lib or dependency target is identity at
                // most, and the reference already carries that. A tuple's target is its generic
                // carrier and is deliberately read for its flags and dropped, above.
                let! target =
                    match ty.Target with
                    | ValueSome target when isReference && target <> ty.Id && ty.IsTupleType <> ValueSome true ->
                        async {
                            let! target = ctx.Session.getTargetOfType ty.Id
                            return [ target ]
                        }
                    | _ -> async.Return []

                let! structure = deriveStructure ctx ty
                let! baseTypes = ctx.Session.getBaseTypes ty.Id

                let baseTypes =
                    baseTypes |> ValueOption.map Array.toList |> ValueOption.defaultValue []

                let discovered =
                    [
                        yield! structure.Discovered
                        yield! baseTypes
                        yield! typeArguments
                        yield! aliasTypeArguments
                        yield! target
                    ]

                return
                    {
                        Response = ty
                        Origin = origin
                        SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                        Members = structure.Members
                        IndexInfos = structure.IndexInfos
                        CallSignatures = structure.CallSignatures
                        ConstructSignatures = structure.ConstructSignatures
                        BaseTypes = baseTypes |> List.map _.Id
                        TypeArguments = typeArguments |> List.map _.Id
                        TupleElements = tupleElements
                        AliasTypeArguments = aliasTypeArguments |> List.map _.Id
                        IntersectionMembers = []
                        Constraint = None
                        Default = None
                        Conditional = None
                        UnionMembers = []
                    },
                    discovered
        elif has TypeFlags.TypeParameter && ty.IsThisType <> ValueSome true then
            // A type parameter is named by its own symbol - `T`, not the declaration's - and
            // the response carries only the symbol's id, so the name costs a round trip. The
            // bound and the default are followed into the table: an unresolved constraint
            // cannot be rendered, and dropping one silently is the bug the manifest exists to
            // catch.
            let! symbol = ctx.Session.getSymbolOfType ty.Id
            let! bound = ctx.Session.getConstraintOfTypeParameter ty.Id
            let! fallback = ctx.Session.getDefaultFromTypeParameter ty.Id

            return
                { TypeFacts.shallow ty with
                    SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                    Constraint = bound |> ValueOption.map _.Id |> ValueOption.toOption
                    Default = fallback |> ValueOption.map _.Id |> ValueOption.toOption
                },
                [ yield! ValueOption.toList bound; yield! ValueOption.toList fallback ]
        elif has TypeFlags.Index then
            // `keyof T` at an operand the checker could not finish (§4.10). The operand is the
            // index type's target, and it is what the whole idiom is about - `keyof<'T>` needs
            // a `'T` - so it is followed rather than left dangling.
            let! operand = ctx.Session.getTargetOfType ty.Id
            return TypeFacts.shallow ty, [ operand ]
        elif has TypeFlags.IndexedAccess then
            // `T[K]`. Both halves are followed: the index is usually a key variable whose
            // constraint names the object, and reading one without the other cannot tell
            // `T[K]` apart from `T[keyof T]`.
            let! objectType = ctx.Session.getObjectTypeOfType ty.Id
            let! indexType = ctx.Session.getIndexTypeOfType ty.Id
            return TypeFacts.shallow ty, [ objectType; indexType ]
        else
            // A conditional, a template literal or an intrinsic string mapping. None of these
            // has a structure to read - each is a type-level computation over an argument the
            // checker could not supply - but the *arguments* are what the declaration binds,
            // and without them `type Unwrap<T> = ...` reaches the shape tier looking like a
            // plain alias with nothing generic about it.
            let! aliasTypeArguments = ctx.Session.getAliasTypeArgumentsOfType ty.Id

            let aliasTypeArguments =
                aliasTypeArguments
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun argument -> argument.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            // A template literal type is interned by its texts and operands, so it carries no
            // alias of its own: `` type Prefixed<T extends string> = `x-${T}` `` reports no
            // alias arguments at all. Its operands are the parameters it binds, and those it
            // does report.
            let! operands =
                if has TypeFlags.TemplateLiteral && List.isEmpty aliasTypeArguments then
                    ctx.Session.getTypesOfType ty.Id
                else
                    async.Return ValueNone

            let operands =
                operands
                |> ValueOption.defaultValue [||]
                |> Array.filter (fun operand -> operand.Flags.HasFlag TypeFlags.TypeParameter)
                |> Array.toList

            let bound = aliasTypeArguments @ operands

            // A conditional's two branches (§4.11). Both responses are read - choosing between
            // them is what they are for - and only the branch the mapping takes is followed
            // into the table, so a pair that stays deferred leaves the frontier alone.
            let! conditional =
                if has TypeFlags.Conditional then
                    async {
                        let! alias = ctx.Session.getAliasSymbolOfType ty.Id
                        let! whenTrue = ctx.Session.getTrueTypeOfConditionalType ty.Id
                        let! whenFalse = ctx.Session.getFalseTypeOfConditionalType ty.Id

                        // The condition's two sides. They are requested rather than read off
                        // `ty.CheckType`/`ty.ExtendsType`: the ids on a response are handles the
                        // compiler registers when it hands the type over, and an id nothing has
                        // asked for is rejected by the next request that names it.
                        let! check = ctx.Session.getCheckTypeOfType ty.Id
                        let! against = ctx.Session.getExtendsTypeOfType ty.Id

                        // `isTypeAssignableTo` reads a type parameter through its bound, so this
                        // asks whether every argument the head admits satisfies the condition.
                        // The checker defers a conditional it cannot answer for the parameter
                        // *without* its bound, so one it deferred can still be decided here.
                        // The reading holds while the parameter is the source. As the target a
                        // bound answers for a type an argument need only be assignable to, so
                        // `undefined extends T ? [] : [value: T]` and its kind stay deferred.
                        let! decided =
                            if against.Flags.HasFlag TypeFlags.TypeParameter then
                                async.Return false
                            else
                                ctx.Session.isTypeAssignableTo (check.Id, against.Id)

                        let uninhabited (branch: TypeResponse) = branch.Flags.HasFlag TypeFlags.Never

                        let branch =
                            if decided then
                                // A proven condition over an uninhabited true branch names an
                                // uninhabited type, which F# writes in no reference position.
                                if uninhabited whenTrue then
                                    None
                                else
                                    Some("true", whenTrue)
                            elif uninhabited whenTrue && not (uninhabited whenFalse) then
                                Some("sole inhabited", whenFalse)
                            elif uninhabited whenFalse && not (uninhabited whenTrue) then
                                Some("sole inhabited", whenTrue)
                            else
                                None

                        // The true branch reaches back into the condition: a `T` on that side
                        // arrives as a substitution type, `T` refined by what the condition
                        // proved of it. F# writes the parameter, so the refinement is peeled
                        // off - it is the same knowledge the head already carries.
                        let rec peeled (branch: TypeResponse) =
                            async {
                                if branch.Flags.HasFlag TypeFlags.Substitution then
                                    let! bare = ctx.Session.getBaseTypeOfType branch.Id
                                    return! peeled bare
                                else
                                    return branch
                            }

                        let! taken =
                            match branch with
                            | Some(side, whichever) ->
                                async {
                                    let! bare = peeled whichever
                                    return Some(side, bare)
                                }
                            | None -> async.Return None

                        return
                            Some
                                {
                                    Name = alias |> ValueOption.map _.Name |> ValueOption.toOption
                                    Branch = taken |> Option.map (fun (side, bare) -> side, bare.Id)
                                },
                            taken |> Option.map snd |> Option.toList
                    }
                else
                    async.Return(None, [])

            return
                { TypeFacts.shallow ty with
                    AliasTypeArguments = bound |> List.map _.Id
                    Conditional = fst conditional
                },
                bound @ snd conditional
    }

/// Builds the closed type table: derive the current frontier (sorted by id, so the fold is
/// deterministic whatever order answers arrive in), collect what derivation discovered, and
/// recurse until the frontier is exhausted or the depth cutoff records the remainder.
let resolveTypeTable: Pass<ResolveModel> =
    {
        Name = "resolve-type-table"
        Run =
            fun ctx model ->
                async {
                    let rec walk table derived notFollowed findings frontier depth =
                        async {
                            let fresh =
                                frontier
                                |> List.distinctBy (fun (ty: TypeResponse) -> ty.Id)
                                |> List.filter (fun ty -> not (Set.contains ty.Id derived))
                                |> List.sortBy _.Id

                            match fresh with
                            | [] -> return table, notFollowed, findings
                            | fresh when depth > FollowDepth ->
                                let notFollowed =
                                    fresh
                                    |> List.fold
                                        (fun map ty -> Map.add ty.Id $"beyond the depth cutoff ({FollowDepth})" map)
                                        notFollowed

                                // One finding for the frontier, not one per type: a type here has
                                // no name to report under, and its checker id is assigned in the
                                // order answers arrived, so a finding keyed by it differs run to
                                // run. The count is stable; the reference that reads one of these
                                // widens under its own owner's name (`typeRefOnPath`).
                                let findings =
                                    findings
                                    @ [
                                        Finding.make
                                            "<type-table>"
                                            (ResolveTypeTable.FrontierNotResolved(fresh.Length, FollowDepth))
                                    ]

                                return table, notFollowed, findings
                            | fresh ->
                                let! results =
                                    fresh
                                    |> List.map (fun ty ->
                                        async {
                                            let! result = attempt (deriveFacts ctx ty)
                                            return ty, result
                                        })
                                    |> Async.Parallel

                                // A type the compiler would not describe is recorded beside the
                                // depth cutoff's: not in the table, but with its reason, so the
                                // shape tier widens a reference to it and says why rather than
                                // reporting a hole in the closure.
                                let table =
                                    results
                                    |> Array.fold
                                        (fun map (_, result) ->
                                            match result with
                                            | Ok(facts, _) -> Map.add facts.Response.Id facts map
                                            | Error _ -> map)
                                        table

                                let notFollowed =
                                    results
                                    |> Array.fold
                                        (fun map (ty, result) ->
                                            match result with
                                            | Error reason -> Map.add ty.Id reason map
                                            | Ok _ -> map)
                                        notFollowed

                                let findings =
                                    findings
                                    @ [
                                        for ty, result in results do
                                            match result with
                                            | Error reason ->
                                                Finding.make $"type#{ty.Id}" (ResolveTypeTable.TypeNotResolved reason)
                                            | Ok _ -> ()
                                    ]

                                let derived = fresh |> List.fold (fun set ty -> Set.add ty.Id set) derived

                                let discovered =
                                    results
                                    |> Array.toList
                                    |> List.collect (fun (_, result) ->
                                        match result with
                                        | Ok(_, discovered) -> discovered
                                        | Error _ -> [])

                                return! walk table derived notFollowed findings discovered (depth + 1)
                        }

                    let seeds = model.Types |> Map.toList |> List.map (fun (_, facts) -> facts.Response)

                    let! table, notFollowed, findings =
                        walk model.Types Set.empty model.NotFollowed [] seeds 0

                    let model =
                        { model with
                            Types = table
                            NotFollowed = notFollowed
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }

/// The tier's pass list, in execution order.
let passes: Pass<ResolveModel> list = [ resolveExportTypes; resolveTypeTable ]
