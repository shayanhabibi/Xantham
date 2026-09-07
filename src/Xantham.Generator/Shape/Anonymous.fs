module Xantham.Generator.Shape.Anonymous

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// An operand whose resolution waits on an argument: a conditional, an indexed access, a
/// `keyof`, a template literal, a string mapping or a substitution. Under the alias's own
/// parameters it stays in this deferred form, and every application carries whatever it
/// resolved to in its place.
let private isDeferredOperand (facts: TypeFacts) =
    flag TypeFlags.Conditional facts
    || flag TypeFlags.IndexedAccess facts
    || flag TypeFlags.Index facts
    || flag TypeFlags.TemplateLiteral facts
    || flag TypeFlags.StringMapping facts
    || flag TypeFlags.Substitution facts

/// Whether an intersection branches on one of its own arguments. This is narrower than "no
/// members", which a type-parameter operand also produces: `solid-js`'s `FlowProps<P, C> = P &
/// { children: C }` reads false and each of its applications is hoisted on its own, while
/// `three`'s `Node<TNodeType>` reads true and its applications go back to one declaration.
let private hasConditionalOperand (model: ShapeModel) (facts: TypeFacts) =
    facts.IntersectionMembers
    |> List.exists (fun id ->
        match Map.tryFind id model.Types with
        | Some operand -> flag TypeFlags.Conditional operand
        | None -> false)

/// A generic alias over an intersection, in the form the checker declared it. `isFlattenable`
/// is the usual reading, and a conditional operand is the exception it misses: an intersection
/// surrenders its members only once every operand resolves, so `three`'s `Node<TNodeType> =
/// NodeInterface<…> & (unknown extends TNodeType ? {} : NodeExtensions<TNodeType>)` is
/// memberless at its declaration and four members wide at `Node<number>`. Both are the same
/// alias (`docs/plans/generator-three-rung.md` §11.4).
let private isAliasIntersectionForm (model: ShapeModel) (facts: TypeFacts) =
    isFlattenable model facts
    || (flag TypeFlags.Intersection facts
        && (brandedPrimitive model facts).IsNone
        && (arrayElement model facts).IsNone
        && hasConditionalOperand model facts)

/// The declaration form of every generic *alias* over an intersection: alias symbol -> the
/// smallest type id carrying it that binds parameters of its own. The checker creates an
/// alias's declared type before it can instantiate it, so the smallest such id is the declared
/// form and every larger one carrying the same alias symbol is an application of it.
let private aliasDeclarationForms (model: ShapeModel) : Map<int, int> =
    model.Types
    |> Map.toList
    |> List.sortBy fst
    |> List.fold
        (fun forms (typeId, facts) ->
            match facts.Response.AliasSymbol with
            | ValueSome alias when
                not (Map.containsKey alias forms)
                && isAliasIntersectionForm model facts
                && not (List.isEmpty (declParamIds facts))
                ->
                Map.add alias typeId forms
            | _ -> forms)
        Map.empty

/// The consistent parameter bindings recoverable from two compiler-identified forms of one
/// alias. Transformed fragments contribute no bindings; conflicting bindings reject the result.
let private unifyAlias (model: ShapeModel) (parameters: Set<int>) (declared: int) (instance: int) =
    let mutable subst = Map.empty
    let mutable ok = true
    let mutable seen = Set.empty

    /// The operand pairs two intersections share. A deferred operand resolves to `{}` under
    /// some arguments and the checker keeps no `{}` in an intersection, so an application
    /// carries between `declared - deferred` and `declared` operands; the pairing that fits
    /// the application's own count is the one the checker produced.
    let alignOperands (declaredFacts: TypeFacts) (instanceFacts: TypeFacts) =
        let declaredOperands = declaredFacts.IntersectionMembers
        let instanceOperands = instanceFacts.IntersectionMembers

        if declaredOperands.Length = instanceOperands.Length then
            Some(List.zip declaredOperands instanceOperands)
        else
            let surviving =
                declaredOperands
                |> List.filter (fun id ->
                    match Map.tryFind id model.Types with
                    | Some operand -> not (isDeferredOperand operand)
                    | None -> true)

            if surviving.Length = instanceOperands.Length then
                Some(List.zip surviving instanceOperands)
            else
                None

    let rec go (left: int) (right: int) =
        if ok && not (Set.contains (left, right) seen) then
            seen <- Set.add (left, right) seen

            if Set.contains left parameters then
                match Map.tryFind left subst with
                | Some bound when bound <> right -> ok <- false
                | _ -> subst <- Map.add left right subst
            else
                // An operand the instantiation did not move is still descended into, because a
                // parameter under it stands for itself and the application has to write it:
                // `VarNode<T, VarNode<T, U>>` shares its whole first operand with the
                // declaration, and `T` is only bound by walking it. `seen` ends the descent.
                match Map.tryFind left model.Types, Map.tryFind right model.Types with
                | Some declaredFacts, Some instanceFacts ->
                    match
                        declaredFacts.Response.AliasSymbol,
                        instanceFacts.Response.AliasSymbol,
                        declaredFacts.Response.AliasTypeArguments,
                        instanceFacts.Response.AliasTypeArguments
                    with
                    | ValueSome declaredAlias, ValueSome instanceAlias, ValueSome declaredArgs, ValueSome instanceArgs when
                        declaredAlias = instanceAlias
                        && declaredArgs.Length = instanceArgs.Length
                        && declaredArgs.Length > 0
                        ->
                        Array.iter2 go declaredArgs instanceArgs
                    | _ ->
                        match declaredFacts.Response.Target, instanceFacts.Response.Target with
                        // A deferred operand and its resolution, which stand in the same place and
                        // share no structure: `(unknown extends TNodeType ? {} : NodeExtensions<
                        // TNodeType>)` arrives at `Node<number>` as `NodeExtensions<number>`, and
                        // the checker keeps neither branch nor argument on the deferred form. The
                        // pair binds nothing and the walk continues on the remaining operands.
                        | _ when isDeferredOperand declaredFacts -> ()
                        // Two references to the same generic: the arguments are what differ.
                        | ValueSome declaredTarget, ValueSome instanceTarget when
                            declaredTarget = instanceTarget
                            && declaredFacts.TypeArguments.Length = instanceFacts.TypeArguments.Length
                            ->
                            List.iter2 go declaredFacts.TypeArguments instanceFacts.TypeArguments
                        // A nested alias: the checker keeps operand order under instantiation.
                        | _ when
                            flag TypeFlags.Intersection declaredFacts
                            && flag TypeFlags.Intersection instanceFacts
                            ->
                            match alignOperands declaredFacts instanceFacts with
                            | Some pairs ->
                                pairs
                                |> List.iter (fun (declaredOperand, instanceOperand) ->
                                    go declaredOperand instanceOperand)
                            | None -> ()
                        // An anonymous operand written inline in the alias body. `D1Response & {
                        // results: T[] }` instantiates its second operand in place, and the checker
                        // gives an instantiated anonymous object no `Target` to compare it by, so
                        // corresponding members provide parameter bindings at matching source paths.
                        | _ when
                            flag TypeFlags.Object declaredFacts
                            && flag TypeFlags.Object instanceFacts
                            && not declaredFacts.Members.IsEmpty
                            && (declaredFacts.Members |> List.map _.Symbol.Name) =
                                (instanceFacts.Members |> List.map _.Symbol.Name)
                            ->
                            List.iter2
                                (fun (declaredMember: ResolvedMember) (instanceMember: ResolvedMember) ->
                                    go declaredMember.TypeId instanceMember.TypeId)
                                declaredFacts.Members
                                instanceFacts.Members
                        | _ -> ()
                | _ -> ()

    go declared instance
    if ok then Some subst else None

/// What a flattened intersection is an application *of*, where this run already declares the
/// alias it came from: the declaration's name, and the arguments to write it with when they
/// can be recovered (§4.9, and `docs/plans/generator-three-rung.md` §9 blocker 1).
///
/// A generic alias over an intersection is erased by the checker. `VarNode<T, this>` arrives as
/// a bare intersection with no `Target`, so `instantiationOf` - which reads a reference's
/// target - cannot see it, and the shape is hoisted under a made-up name. Where the alias
/// contains `this`, every application is a strictly larger type, so that mints one declaration
/// per application until the depth cutoff stops the walk. The alias *symbol* survives on the
/// response and is the tie back. Compiler arguments are retained directly; otherwise matching
/// operand paths recover a complete, consistent substitution for the declaration parameters.
let internal aliasInstantiationOf
    (model: ShapeModel)
    (forms: Map<int, int>)
    (facts: TypeFacts)
    : (string * int list option) option =
    match facts.Response.AliasSymbol with
    | ValueSome alias when isFlattenable model facts ->
        match Map.tryFind alias forms with
        | Some declared when declared <> facts.Response.Id ->
            match Map.tryFind declared model.DeclNames, Map.tryFind declared model.Types with
            | Some name, Some declaredFacts ->
                let parameters = declParamIds declaredFacts

                let arguments =
                    facts.Response.AliasTypeArguments
                    |> ValueOption.toOption
                    |> Option.map Array.toList
                    |> Option.filter (fun arguments ->
                        arguments.Length = parameters.Length
                        && arguments |> List.forall (fun argument -> Map.containsKey argument model.Types))
                    |> Option.orElseWith (fun () ->
                        unifyAlias model (Set.ofList parameters) declared facts.Response.Id
                        |> Option.bind (fun subst ->
                            // A parameter the body never mentions cannot be recovered, and an
                            // application short of an argument is not writable F#.
                            if parameters |> List.forall (fun p -> Map.containsKey p subst) then
                                Some(parameters |> List.map (fun p -> Map.find p subst))
                            else
                                None))

                Some(name, arguments)
            | _ -> None
        | _ -> None
    | _ -> None

/// Walks the type graph reachable from the exports in deterministic order and names what needs
/// a declaration but has none (§4.4, hash-consed by type id). Non-exported named entry types
/// keep their name; the anonymous rest take path-derived ones.
///
/// A path-derived name is dotted, and `render-source` writes each dot as a module: the shape a
/// `Widget`'s `options` member reads is `Widget.Options`, declared inside `module Widget`. Two
/// owners of one member name are two declarations, each under its own owner.
///
/// A numeric suffix remains where several shapes reach one path: the arms of an anonymous
/// union, which share their owner's path, and the parameters of two overloads of one member.
let private nameAnonymous (ctx: Context) (model: ShapeModel) : ShapeModel * Finding list =
    let mutable names = model.DeclNames
    let mutable orders = model.DeclOrders
    let mutable declParams = model.DeclParams
    let mutable aliasApplications = model.AliasApplications
    let mutable findings = []
    let mutable taken = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList
    let mutable visited = Set.empty

    let aliasForms = aliasDeclarationForms model

    /// The module name a type nests under, where its own symbol is written inside a namespace
    /// this run names.
    let namespaceOf (facts: TypeFacts) =
        facts.SymbolParent
        |> Option.bind (fun parent -> Map.tryFind parent model.Harvest.Namespaces)
        |> Option.map Naming.pascalSegment

    let claim (owner: string option) (preferred: string) typeId order =
        // A member key reaches here verbatim, and a declaration name admits less than a member
        // name does: `Registry@cf/meta` is FS0883 with or without backticks.
        let admitted =
            preferred.Split '.' |> Array.map Naming.identifierName |> String.concat "."

        // A name TypeScript separates by the namespace it is written in separates the same way
        // here, rather than by number: `TailStream`'s `TailEvent` reads `TailStream.TailEvent`
        // beside the global class of that name.
        let wanted =
            match owner with
            | Some ns when Set.contains admitted taken -> nestUnder ns admitted
            | _ -> admitted

        let unique =
            if not (Set.contains wanted taken) then
                wanted
            else
                Seq.initInfinite (fun i -> $"{wanted}{i + 2}")
                |> Seq.find (fun candidate -> not (Set.contains candidate taken))

        names <- Map.add typeId unique names
        orders <- Map.add typeId order orders
        taken <- Set.add unique taken

        if admitted <> preferred then
            findings <-
                findings
                @ [
                    Finding.make unique (SynthesizeAnonymous.NameSanitisedForIdentifier(preferred, unique))
                ]

        if unique.Contains "." then
            findings <-
                findings
                @ [ Finding.make unique (SynthesizeAnonymous.NameNestedUnderOwner unique) ]

        unique

    /// A literal union worth a declaration: at least two non-nullish members, all literal,
    /// not just `true | false`, and no already-named union with the same member set.
    let isLiteralUnion (facts: TypeFacts) =
        let _, remaining = splitNullish model facts

        remaining.Length > 1
        && remaining
           |> List.forall (fun id ->
               match Map.tryFind id model.Types with
               | Some m -> (literalOf m).IsSome
               | None -> false)
        && not (isBooleanPair model remaining)
        && (namedUnionByMembers { model with DeclNames = names } remaining).IsNone

    /// A union `detect-tagged-unions` will declare (D4, §4.5(2)): every arm an object type
    /// carrying the same string-literal discriminant, and data a DU case can bind. That pass
    /// reads `DeclNames`, so a name minted here is what offers an inline union to it, and one
    /// spelling of a union maps as the other does.
    ///
    /// The erased union standing in otherwise is written rather than read: Fable type-tests a
    /// primitive alone, so `match` over `U3<Close, Error, Message>` reaches one arm for every
    /// value (the run gate's `workarounds`, item 1), and above the cap the site reads `obj`.
    ///
    /// A union another declaration answers for by member set is left to that name -
    /// `ResponseInputContent` is declared in two files of one package, and the second reads as
    /// the first.
    ///
    /// A union `detect-tagged-unions` declines stays a reference: a name minted for it would be
    /// written at every site and declared at none.
    let becomesTaggedUnion (facts: TypeFacts) =
        let nullish, remaining = splitNullish model facts

        List.isEmpty nullish
        && (namedUnionByMembers { model with DeclNames = names } remaining).IsNone
        && (match taggedUnionShape { model with DeclNames = names } facts with
            | Discriminated(tag, tagged, _) -> tagged |> List.forall (fst >> isTaggedCaseData tag)
            | TagCollides _
            | Untagged -> false)

    /// The type ids a declaration reads through call signatures rather than through a reference
    /// position: an export's own function type, and a method member's. A delegate declared for
    /// one of these is written nowhere, so the name is spent for nothing.
    let signatureShaped =
        let exported =
            model.ExportTypes
            |> Map.toList
            |> List.collect (fun (_, ids) -> Option.toList ids.Declared @ Option.toList ids.Value)

        let methods =
            model.Types
            |> Map.toList
            |> List.collect (fun (_, facts) ->
                facts.Members
                |> List.filter (fun m -> hasAny SymbolFlags.Method m.Symbol.Flags)
                |> List.map _.TypeId)

        Set.ofList (exported @ methods)

    let needsName (facts: TypeFacts) =
        if Map.containsKey facts.Response.Id names then
            false
        elif flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
            isLiteralUnion facts || becomesTaggedUnion facts
        elif flag TypeFlags.Object facts && isPureCallback facts then
            // A callback the arity rule retains as a delegate is declared under a name of its
            // own (D5), so the consumer reads `x: float * y: float` where `Func<float, float,
            // string>` said only how many arguments there are.
            (GeneratorConfig.disposition ctx.Config facts.Origin = Ship
             || facts.SymbolName |> Option.forall isSyntheticName)
            && not (Set.contains facts.Response.Id signatureShaped)
            // F# has no rank-2 form, so a generic signature can only be approximated by
            // hoisting its variables onto the declaration - and a reference has nothing to
            // apply them back with. Written inline, they stay in the scope that bound them.
            && facts.CallSignatures
               |> List.forall (fun signature -> signature.TypeParameters.IsEmpty)
            // An application of a generic callback alias - `ExportedHandlerFetchHandler<Env,
            // Cf, Props>` at a member of `ExportedHandler` - binds the alias's parameters on
            // its head and gives a reference nothing to apply them with. It expands in place,
            // under the name the alias itself already carries.
            && declParamIds facts = ownArguments facts
            && callbackRetainedAsDelegate model names facts
            && (instantiationOf { model with DeclNames = names } facts).IsNone
        elif flag TypeFlags.Object facts then
            // Entry-group object shapes with members become interfaces; arrays stay arrays and
            // tuples F# tuples (D7). An anonymous shape belongs to the entry package whatever
            // file its node sits in (D6).
            (GeneratorConfig.disposition ctx.Config facts.Origin = Ship
             || facts.SymbolName |> Option.forall isSyntheticName)
            && (arrayElement model facts).IsNone
            && not (isTuple facts)
            && facts.ConstructSignatures.IsEmpty
            // An index signature is shape too: `Record<string, boolean>` has no members
            // and one index signature, and is an interface of one `Item`.
            && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
            // A pure index signature with no symbol of its own resolves through the
            // support package's `Record`/`ReadonlyRecord` instead of a minted name
            // (TR059); an interface's own name, reached this way rather than through a
            // top-level export, still claims one below.
            && not (isAnonymousIndexSignature model facts)
            // An instantiation of a generic this run declares is written as an
            // application (§4.9). Naming it would declare the expansion a second time
            // under a made-up name and lose the tie to the generic it came from.
            && (instantiationOf { model with DeclNames = names } facts).IsNone
        elif flag TypeFlags.Intersection facts then
            // An intersection of object types is one flattened interface (§4.6): the
            // resolve tier read its members off the intersection itself, so it names and
            // declares like any anonymous shape. An operand-only intersection widens.
            isFlattenable model facts
            // Unless it is an application of an alias this run already declares, which is
            // written as that application rather than named a second time.
            && (aliasInstantiationOf { model with DeclNames = names } aliasForms facts).IsNone
        else
            false

    let rec walk (path: string) (order: DeclOrder option) (typeId: int) =
        if not (Set.contains typeId visited) then
            visited <- Set.add typeId visited

            match Map.tryFind typeId model.Types with
            | None -> ()
            | Some facts ->
                // The generic declaration behind an instantiation is named ahead of it, so
                // `Ready<T>` reached only through `Resource<T> = Ready<T> | ...` declares
                // `Ready<'T>` once and instantiations are applications of it (§4.9).
                match facts.Response.Target with
                | ValueSome target when target <> typeId && Map.containsKey target model.Types -> walk path order target
                | _ -> ()

                // The same rule for the declaration form of an alias whose body defers on a
                // conditional, which reaches this point nameless: it surrenders no members, so
                // `needsName` passes over it, and `three` exports neither `Node` nor `VarNode`
                // from the entry that would have named it (§11.4). The first application names
                // the family and every application after it is a reference; without the name
                // each one hoists a strictly larger shape until the depth cutoff stops it.
                match facts.Response.AliasSymbol with
                | ValueSome alias when isFlattenable model facts ->
                    match Map.tryFind alias aliasForms with
                    | Some declared when
                        declared <> typeId
                        && not (Map.containsKey declared names)
                        && (match Map.tryFind declared model.Types with
                            | Some declaredFacts -> hasConditionalOperand model declaredFacts
                            | None -> false)
                        ->
                        claim None path declared order |> ignore
                    | _ -> ()
                | _ -> ()

                if needsName facts then
                    // A path-derived name already carries its owner, so only a name taken from
                    // the type's own symbol has a namespace left to fall back on.
                    let preferred, owner =
                        match facts.SymbolName with
                        | Some name when not (isSyntheticName name) -> Naming.pascalSegment name, namespaceOf facts
                        | _ -> path, None

                    let claimed = claim owner preferred typeId order

                    // The delegate declaration `shape-callbacks` writes for it (D5): the arity
                    // guarantee `System.Func` gives, under a name whose parameters read as
                    // TypeScript spelled them.
                    if isPureCallback facts then
                        findings <-
                            findings
                            @ [ Finding.make claimed (SynthesizeAnonymous.CallbackDelegateNamed claimed) ]

                // An erased alias application: hash-consed onto the declaration it applies,
                // with the recovered arguments standing in for the parameters a hoisted
                // shape would have read free. The application map separates reference sites
                // from the declaration candidates in later shaping passes.
                let aliasApplication =
                    aliasInstantiationOf { model with DeclNames = names } aliasForms facts

                match aliasApplication with
                | Some(name, Some arguments) ->
                    names <- Map.add typeId name names
                    declParams <- Map.add typeId arguments declParams
                    let declared = aliasForms[facts.Response.AliasSymbol.Value]
                    aliasApplications <- Map.add typeId declared aliasApplications

                    findings <-
                        findings
                        @ [ Finding.make path (SynthesizeAnonymous.InstantiationNamedOnce name) ]
                | Some(name, None) ->
                    // The alias is recognised but its arguments do not come back out of the
                    // operands, so there is no application to write. Widening here is the
                    // whole point: hoisting is what runs away.
                    findings <-
                        findings
                        @ [ Finding.make path (SynthesizeAnonymous.HoistArgumentsNotRecovered name) ]
                | None -> ()

                // Recurse in the shape the declaration will read: members, signatures,
                // union members, then structural identity (element, arguments, bases).
                let named = Map.tryFind typeId names

                let into segment =
                    nestUnder (named |> Option.defaultValue path) segment

                // An instantiation of a named declaration reads only its arguments: its
                // members are the declaration's, substituted, and shaping happens there.
                if (instantiationOf { model with DeclNames = names } facts).IsSome then
                    for argument in facts.TypeArguments do
                        walk (into "Item") order argument
                elif aliasApplication.IsSome then
                    // Same rule for an erased alias application, and the reason the runaway
                    // stops: the members are the declaration's, so descending into them is
                    // what mints the next strictly larger type.
                    match aliasApplication with
                    | Some(_, Some arguments) ->
                        for argument in arguments do
                            walk (into "Item") order argument
                    | _ -> ()
                else

                    // A tuple reads only its components and an array only its element, so
                    // `length`, the numeric indices and `Array<T>`'s lib members claim no
                    // names. A symbol-keyed member is dropped at render, so nor does its type.
                    if not (isTuple facts) && (arrayElement model facts).IsNone then
                        for m in facts.Members do
                            if not (isSymbolKeyed m.Symbol.Name) then
                                walk (into (Naming.pascalSegment m.Symbol.Name)) order m.TypeId

                    // An index signature's value is shape the declaration reads too:
                    // `Record<string, A & B>` reaches its intersection nowhere else.
                    for info in facts.IndexInfos do
                        walk (into "Item") order info.ValueTypeId

                    let signatures = facts.CallSignatures @ facts.ConstructSignatures

                    for signature in signatures do
                        for p in signature.Parameters do
                            walk (into (Naming.pascalSegment p.Symbol.Name)) order p.TypeId

                        walk (into "Result") order signature.ReturnTypeId

                    for memberId in facts.UnionMembers do
                        walk (named |> Option.defaultValue path) order memberId

                    for argument in facts.TypeArguments do
                        walk (into "Item") order argument

                    for baseId in facts.BaseTypes do
                        walk (into "Base") order baseId

    let fallback = defaultExportName ctx

    for export in model.Harvest.Exports do
        let root = Naming.pascalSegment (fsName fallback export)

        match Map.tryFind export.Symbol.Id model.ExportTypes with
        | Some ids ->
            for typeId in [ yield! Option.toList ids.Declared; yield! Option.toList ids.Value ] do
                walk root export.Order typeId
        | None -> ()

    { model with
        DeclNames = names
        DeclOrders = orders
        DeclParams = declParams
        AliasApplications = aliasApplications
    },
    findings

let synthesizeAnonymous: Pass<ShapeModel> =
    {
        Name = "synthesize-anonymous"
        Run =
            fun ctx model ->
                async {
                    match nameAnonymous ctx model with
                    | model, [] -> return Advanced model
                    | model, findings -> return Degraded(model, findings)
                }
    }
