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

/// The type ids each export resolves to: the declared type for type-like symbols, the value
/// type for value-like ones, both for symbols that are both (a class). The responses land in
/// the table shallow; `resolveTypeTable` derives them.
let resolveExportTypes: Pass<ResolveModel> =
    { Name = "resolve-export-types"
      Run =
        fun ctx model ->
            async {
                let! resolved =
                    model.Harvest.Exports
                    |> List.map (fun export ->
                        async {
                            let! declared =
                                if hasAny SymbolFlags.Type export.Symbol.Flags then
                                    async {
                                        let! ty = ctx.Session.getDeclaredTypeOfSymbol export.Symbol.Id
                                        return Some ty
                                    }
                                else
                                    async.Return None

                            let! value =
                                if hasAny SymbolFlags.Value export.Symbol.Flags then
                                    async {
                                        let! ty = ctx.Session.getTypeOfSymbol export.Symbol.Id
                                        return Some ty
                                    }
                                else
                                    async.Return None

                            return export.Symbol.Id, declared, value
                        })
                    // Sequential, unlike every other fan-out in this tier: asking for a
                    // declared type is what *creates* it in the checker, and a type alias
                    // stamps its name on the type it creates. Two aliases with the same right
                    // side (`type A = X & Y; type B = X & Y`) therefore race - whichever is
                    // asked for first owns the intersection, and the other either aliases it
                    // or widens. Under Async.Parallel that ordering came out of the thread
                    // pool, so the same package generated two different files.
                    |> Async.Sequential

                let exportTypes =
                    resolved
                    |> Array.fold
                        (fun map (symbolId, declared, value) ->
                            Map.add
                                symbolId
                                { Declared = declared |> Option.map _.Id
                                  Value = value |> Option.map _.Id }
                                map)
                        model.ExportTypes

                let types =
                    resolved
                    |> Array.collect (fun (_, declared, value) ->
                        [| yield! Option.toArray declared; yield! Option.toArray value |])
                    |> Array.fold (fun map ty -> Map.add ty.Id (TypeFacts.shallow ty) map) model.Types

                return
                    Advanced
                        { model with
                            ExportTypes = exportTypes
                            Types = types }
            } }

/// Derives one type's facts and reports the responses it discovered, for the next frontier.
let private deriveFacts (ctx: Context) (ty: TypeResponse) : Async<TypeFacts * TypeResponse list> =
    async {
        let has flag = ty.Flags.HasFlag(flag: TypeFlags)

        // `boolean` is a union of `true | false` wearing the Boolean flag; it maps as a
        // primitive, so its members are not worth a round trip.
        if has TypeFlags.Union && not (has TypeFlags.Boolean) then
            let! members = ctx.Session.getTypesOfType ty.Id
            let members = members |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            return
                { TypeFacts.shallow ty with
                    UnionMembers = members |> List.map _.Id },
                members
        elif has TypeFlags.EnumLiteral then
            // An enum member: its symbol names the F# enum case (§4.7); the value is already
            // on the response.
            let! symbol = ctx.Session.getSymbolOfType ty.Id

            return
                { TypeFacts.shallow ty with
                    Origin = Grouping.classify ctx.PackageDir symbol
                    SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption },
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

                        return target.ElementFlags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
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

            if GeneratorConfig.disposition ctx.Config origin <> Ship then
                // Identity only (O7): the shape tier renders references to this group by
                // templated name or widens them, and either way nothing reads its members.
                return
                    { TypeFacts.shallow ty with
                        Origin = origin
                        SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                        TypeArguments = typeArguments |> List.map _.Id
                        TupleElements = tupleElements
                        AliasTypeArguments = aliasTypeArguments |> List.map _.Id },
                    typeArguments @ aliasTypeArguments
            else

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
                        { Symbol = property
                          Docs = docs
                          Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                          Optional =
                            property.Flags.HasFlag SymbolFlags.Optional
                            || property.CheckFlags.HasFlag CheckFlags.OptionalParameter
                          ReadOnly = readOnly
                          TypeId = propertyType.Id },
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
                                    { Parameters = parameterFacts |> Array.map fst |> Array.toList
                                      HasRest = signature.Flags.HasFlag SignatureFlags.HasRestParameter
                                      TypeParameters = typeParameters |> Array.map _.Id |> Array.toList
                                      ReturnTypeId = returnType.Id },
                                    [ yield! parameterFacts |> Array.map snd
                                      yield! typeParameters
                                      returnType ]
                            })
                        |> Async.Parallel
                }

            let! callSignatures = resolveSignatures SignatureKind.Call
            let! constructSignatures = resolveSignatures SignatureKind.Construct
            let! baseTypes = ctx.Session.getBaseTypes ty.Id
            let baseTypes = baseTypes |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            let discovered =
                [ yield! members |> Array.map snd
                  yield! callSignatures |> Array.collect (snd >> List.toArray)
                  yield! constructSignatures |> Array.collect (snd >> List.toArray)
                  yield! baseTypes
                  yield! typeArguments
                  yield! aliasTypeArguments ]

            return
                { Response = ty
                  Origin = origin
                  SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                  Members = members |> Array.map fst |> Array.toList
                  CallSignatures = callSignatures |> Array.map fst |> Array.toList
                  ConstructSignatures = constructSignatures |> Array.map fst |> Array.toList
                  BaseTypes = baseTypes |> List.map _.Id
                  TypeArguments = typeArguments |> List.map _.Id
                  TupleElements = tupleElements
                  AliasTypeArguments = aliasTypeArguments |> List.map _.Id
                  Constraint = None
                  Default = None
                  UnionMembers = [] },
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
                    Default = fallback |> ValueOption.map _.Id |> ValueOption.toOption },
                [ yield! ValueOption.toList bound; yield! ValueOption.toList fallback ]
        else
            return TypeFacts.shallow ty, []
    }

/// Builds the closed type table: derive the current frontier (sorted by id, so the fold is
/// deterministic whatever order answers arrive in), collect what derivation discovered, and
/// recurse until the frontier is exhausted or the depth cutoff records the remainder.
let resolveTypeTable: Pass<ResolveModel> =
    { Name = "resolve-type-table"
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

                            let findings =
                                findings
                                @ [ for ty in fresh ->
                                        Finding.make
                                            Widened
                                            $"type#{ty.Id}"
                                            $"not resolved: beyond the depth cutoff ({FollowDepth})" ]

                            return table, notFollowed, findings
                        | fresh ->
                            let! results = fresh |> List.map (deriveFacts ctx) |> Async.Parallel

                            let table =
                                results
                                |> Array.fold (fun map (facts, _) -> Map.add facts.Response.Id facts map) table

                            let derived = fresh |> List.fold (fun set ty -> Set.add ty.Id set) derived
                            let discovered = results |> Array.toList |> List.collect snd
                            return! walk table derived notFollowed findings discovered (depth + 1)
                    }

                let seeds = model.Types |> Map.toList |> List.map (fun (_, facts) -> facts.Response)
                let! table, notFollowed, findings = walk model.Types Set.empty model.NotFollowed [] seeds 0

                let model =
                    { model with
                        Types = table
                        NotFollowed = notFollowed }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// The tier's pass list, in execution order.
let passes: Pass<ResolveModel> list = [ resolveExportTypes; resolveTypeTable ]
