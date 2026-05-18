[<AutoOpen>]
module Xantham.Generator.Generator.SyntheticPathAssignment

open System.Collections.Generic
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

/// Nanopass-adjacent: assign stable concrete `TypePath` values to interned
/// synthetic literal types (Union of literals, Intersection, multi-member
/// TypeLiteral, TemplateLiteral) that appear in multiple positions.
///
/// Background: Shayan's documented Transient/Anchor system handles
/// synthetic literals via per-call-site anchor resolution. The encoder's
/// `[<ReferenceEquality>]`-based interning means structurally-identical
/// literals share one ResolvedType across multiple positions. When the
/// shared cached `TransientPath` atom is re-anchored at each call site,
/// different call sites with different anchor depths produce mismatched
/// concrete paths, so references don't navigate to the actual emission.
///
/// The pre-pass below walks the graph after `prerenderTypeAliases` and
/// before `prerenderFromGraph`, identifies multi-position interned
/// synthetics, and assigns each a single concrete `TypePath`. Prerender's
/// synthetic branches consult `ctx.SyntheticPaths` at first encounter and,
/// when present, produce a `ConcretePath` ref + `Anchored` Root rather
/// than a Transient pair.
///
/// Single-position synthetics (no interning across positions) continue
/// through the documented Transient/Anchor pipeline unchanged.
module SyntheticPathAssignment =

    /// Determine whether a ResolvedType is a synthetic literal type that
    /// would otherwise produce a Transient renderScope. Returns false for
    /// named types (Interface, Class, Enum, TypeAlias) which already get
    /// concrete paths via the interceptors.
    let private isSyntheticLiteral (resolvedType: ResolvedType) =
        match resolvedType with
        | ResolvedType.Union _
        | ResolvedType.Intersection _
        | ResolvedType.TypeLiteral _
        | ResolvedType.TemplateLiteral _ -> true
        | _ -> false

    /// Try to pull a concrete `TypePath` out of a ResolvedType that's
    /// already an export-shaped node. Used to find a "home" export for a
    /// multi-position synthetic.
    let private exportTypePath (ctx: GeneratorContext) (resolvedType: ResolvedType) : TypePath voption =
        match resolvedType with
        | ResolvedType.Interface iface ->
            ValueSome (Path.Interceptors.pipeInterface ctx iface)
        | ResolvedType.Class cls ->
            ValueSome (Path.Interceptors.pipeClass ctx cls)
        | ResolvedType.Enum enumType ->
            ValueSome (Path.Interceptors.pipeEnum ctx enumType)
        | _ -> ValueNone

    /// Recursively walk a ResolvedType graph, yielding any synthetic
    /// literal ResolvedTypes encountered. The `visited` HashSet prevents
    /// infinite loops on cyclic references — `[<ReferenceEquality>]` on
    /// ResolvedType records makes identity comparison cheap and correct.
    /// Yields only the synthetic types (Union, Intersection, TypeLiteral,
    /// TemplateLiteral); their bodies are walked through to discover
    /// further nested synthetics.
    let rec private walk (visited: HashSet<ResolvedType>) (rt: ResolvedType) : ResolvedType seq =
        seq {
            if visited.Add rt then
                if isSyntheticLiteral rt then yield rt
                match rt with
                | ResolvedType.Union union ->
                    for t in union.Types do
                        yield! walk visited t.Value
                | ResolvedType.Intersection intersection ->
                    for t in intersection.Types do
                        yield! walk visited t.Value
                | ResolvedType.TypeLiteral typeLiteral ->
                    for memberRef in typeLiteral.Members do
                        yield! walkMember visited memberRef
                | ResolvedType.TemplateLiteral template ->
                    for t in template.Types do
                        yield! walk visited t.Value
                | ResolvedType.Interface iface ->
                    for memberRef in iface.Members do
                        yield! walkMember visited memberRef
                | ResolvedType.Class cls ->
                    for memberRef in cls.Members do
                        yield! walkMember visited memberRef
                | ResolvedType.TypeReference typeRef ->
                    yield! walk visited typeRef.Type.Value
                    for arg in typeRef.TypeArguments do
                        yield! walk visited arg.Value
                | ResolvedType.Array inner ->
                    yield! walk visited inner
                | ResolvedType.ReadOnly inner ->
                    yield! walk visited inner
                | ResolvedType.Tuple tuple ->
                    for t in tuple.Types do
                        yield! walk visited t.Type.Value
                | ResolvedType.IndexedAccess indexed ->
                    yield! walk visited indexed.Object.Value
                    yield! walk visited indexed.Index.Value
                | ResolvedType.Index index ->
                    yield! walk visited index.Type.Value
                | ResolvedType.Optional opt ->
                    yield! walk visited opt.Type.Value
                | ResolvedType.TypeQuery query ->
                    yield! walk visited query.Type.Value
                | ResolvedType.Conditional cond ->
                    yield! walk visited cond.Check.Value
                    yield! walk visited cond.Extends.Value
                    yield! walk visited cond.True.Value
                    yield! walk visited cond.False.Value
                | ResolvedType.Substitution sub ->
                    yield! walk visited sub.Base.Value
                | _ -> ()
        }

    and private walkMember (visited: HashSet<ResolvedType>) (memberRef: Member) : ResolvedType seq =
        seq {
            match memberRef with
            | Member.Property prop ->
                yield! walk visited prop.Type.Value
            | Member.Method overloads ->
                for m in overloads do
                    for param in m.Parameters do
                        yield! walk visited param.Type.Value
                    yield! walk visited m.Type.Value
            | Member.GetAccessor get ->
                yield! walk visited get.Type.Value
            | Member.SetAccessor set ->
                yield! walk visited set.ArgumentType.Value
            | Member.IndexSignature idx ->
                for param in idx.Parameters do
                    yield! walk visited param.Type.Value
                yield! walk visited idx.Type.Value
            | Member.CallSignature signatures ->
                for sig' in signatures do
                    for param in sig'.Parameters do
                        yield! walk visited param.Type.Value
                    yield! walk visited sig'.Type.Value
            | Member.ConstructSignature signatures ->
                for sig' in signatures do
                    for param in sig'.Parameters do
                        yield! walk visited param.Type.Value
                    yield! walk visited sig'.Type.Value
        }

    /// Synthetic literal ResolvedTypes reachable from an Interface/Class
    /// type, walking through nested members, function returns, type
    /// arguments, union/intersection elements, etc.
    let private outgoingSyntheticRefs (resolvedType: ResolvedType) : ResolvedType seq =
        let visited = HashSet<ResolvedType>(HashIdentity.Reference)
        walk visited resolvedType

    /// Synthetic literal ResolvedTypes reachable from a Function export's
    /// overload list (parameters and return types), recursively.
    let private functionSyntheticRefs (functions: Function list) : ResolvedType seq =
        let visited = HashSet<ResolvedType>(HashIdentity.Reference)
        seq {
            for fn in functions do
                for param in fn.Parameters do
                    yield! walk visited param.Type.Value
                yield! walk visited fn.Type.Value
        }

    /// Synthetic literal ResolvedTypes reachable from a TypeAlias export's
    /// body, recursively.
    let private typeAliasSyntheticRefs (typeAlias: TypeAlias) : ResolvedType seq =
        let visited = HashSet<ResolvedType>(HashIdentity.Reference)
        walk visited typeAlias.Type.Value

    /// Synthetic literal ResolvedTypes reachable from a Variable export's
    /// type, recursively.
    let private variableSyntheticRefs (variable: Variable) : ResolvedType seq =
        let visited = HashSet<ResolvedType>(HashIdentity.Reference)
        walk visited variable.Type.Value

    /// Build a synthetic TypePath under the given home export's parent
    /// module + export name (as a sub-module). Counter ensures uniqueness
    /// within the home; identical synthetics with the same home would
    /// collide otherwise.
    let private buildSyntheticPath (homeTypePath: TypePath) (counter: int) : TypePath =
        let parentModule =
            ModulePath.createWithName homeTypePath.Name homeTypePath.Parent
        let name = sprintf "_Lit%d" counter |> Name.Pascal.create
        TypePath.createWithName name parentModule

    /// Walk a synthetic's body collecting any `ResolvedType.TypeParameter`
    /// references in first-appearance order. The `seen` set is keyed by the
    /// underlying `TypeParameter` record's reference identity
    /// (`[<ReferenceEquality>]` on the decoder record makes this cheap and
    /// correct).
    ///
    /// Walks transparently through every container including
    /// Interface/Class/Enum and through TypeReferences' declaration bodies.
    /// Counterintuitively this is the *correct* behavior: TS interns the
    /// synthetic at one point in the type graph, and the only typars that
    /// will be in scope at every reference site are typars from the
    /// surrounding declaration scopes the encoder threaded through. By
    /// collecting them all we capture the union of typars any reference
    /// site might need. A narrower walk (stopping at named-type boundaries)
    /// captures fewer typars and ends up emitting refs whose typars aren't
    /// in scope at any of the reference sites — strictly worse.
    ///
    /// Typars are added at first encounter to preserve declaration order:
    /// `(value: 'T, index: 'U) => 'R` captures `['T; 'U; 'R]` so the emitted
    /// synthetic declares typars in the same order references at use sites
    /// will apply them.
    let private collectFreeTypars (rootRt: ResolvedType) : TypeParameter list =
        let seen = HashSet<TypeParameter>(HashIdentity.Reference)
        let visited = HashSet<ResolvedType>(HashIdentity.Reference)
        let typars = ResizeArray<TypeParameter>()
        let rec walkT (rt: ResolvedType) =
            if visited.Add rt then
                match rt with
                | ResolvedType.TypeParameter tp ->
                    if seen.Add tp then typars.Add tp
                | ResolvedType.Union union ->
                    for t in union.Types do walkT t.Value
                | ResolvedType.Intersection intersection ->
                    for t in intersection.Types do walkT t.Value
                | ResolvedType.TypeLiteral typeLiteral ->
                    for m in typeLiteral.Members do walkM m
                | ResolvedType.TemplateLiteral template ->
                    for t in template.Types do walkT t.Value
                | ResolvedType.Interface iface ->
                    for m in iface.Members do walkM m
                | ResolvedType.Class cls ->
                    for m in cls.Members do walkM m
                | ResolvedType.TypeReference typeRef ->
                    walkT typeRef.Type.Value
                    for arg in typeRef.TypeArguments do walkT arg.Value
                | ResolvedType.Array inner -> walkT inner
                | ResolvedType.ReadOnly inner -> walkT inner
                | ResolvedType.Tuple tuple ->
                    for t in tuple.Types do walkT t.Type.Value
                | ResolvedType.IndexedAccess indexed ->
                    walkT indexed.Object.Value
                    walkT indexed.Index.Value
                | ResolvedType.Index index -> walkT index.Type.Value
                | ResolvedType.Optional opt -> walkT opt.Type.Value
                | ResolvedType.TypeQuery query -> walkT query.Type.Value
                | ResolvedType.Conditional cond ->
                    walkT cond.Check.Value
                    walkT cond.Extends.Value
                    walkT cond.True.Value
                    walkT cond.False.Value
                | ResolvedType.Substitution sub ->
                    walkT sub.Base.Value
                | _ -> ()
        and walkM (memberRef: Member) =
            match memberRef with
            | Member.Property prop ->
                walkT prop.Type.Value
            | Member.Method overloads ->
                for m in overloads do
                    for param in m.Parameters do walkT param.Type.Value
                    walkT m.Type.Value
            | Member.GetAccessor get ->
                walkT get.Type.Value
            | Member.SetAccessor set ->
                walkT set.ArgumentType.Value
            | Member.IndexSignature idx ->
                for param in idx.Parameters do walkT param.Type.Value
                walkT idx.Type.Value
            | Member.CallSignature signatures ->
                for sig' in signatures do
                    for param in sig'.Parameters do walkT param.Type.Value
                    walkT sig'.Type.Value
            | Member.ConstructSignature signatures ->
                for sig' in signatures do
                    for param in sig'.Parameters do walkT param.Type.Value
                    walkT sig'.Type.Value
        walkT rootRt
        List.ofSeq typars

    let private assignSynthetics
        (ctx: GeneratorContext)
        (counters: Dictionary<TypePath, int>)
        (homePath: TypePath)
        (synthetics: ResolvedType seq) =
        for synthetic in synthetics do
            if not (ctx.SyntheticPaths.ContainsKey synthetic) then
                let counter =
                    match counters.TryGetValue homePath with
                    | true, n -> n + 1
                    | _ -> 1
                counters[homePath] <- counter
                let path = buildSyntheticPath homePath counter
                ctx.SyntheticPaths[synthetic] <- path
                let capturedTypars = collectFreeTypars synthetic
                if not (List.isEmpty capturedTypars) then
                    ctx.SyntheticTypars[synthetic] <- capturedTypars

    /// A type is "infrastructure" if its source is a TS standard library
    /// file (`Source.LibEs _`). Such types shouldn't be hosts for synthetic
    /// assignments — their paths get pruned by the default Typescript-
    /// pruning interceptor, which would produce a homePath that doesn't
    /// match what eventual reference sites resolve to.
    let private isInfrastructureSource (source: ArenaInterner.Source) =
        match source with
        | ArenaInterner.Source.LibEs _ -> true
        | _ -> false

    /// Run the path-assignment pass against a fully-resolved arena. After
    /// this returns, `ctx.SyntheticPaths` contains entries for every
    /// multi-position interned synthetic literal reachable from
    /// Interface/Class/Function exports.
    let run (ctx: GeneratorContext) (arena: ArenaInterner) : unit =
        let counters = Dictionary<TypePath, int>()

        // Walk exports in their map order; assign synthetics encountered
        // along the way to the export's home. First export to reach a
        // synthetic wins; subsequent exports' references to the same
        // ResolvedType see the existing assignment.
        for KeyValue(_, export) in arena.ResolvedExports do
            match export with
            | ResolvedExport.Interface iface
                when not (isInfrastructureSource iface.Source) ->
                let homePath = Path.Interceptors.pipeInterface ctx iface
                assignSynthetics ctx counters homePath
                    (outgoingSyntheticRefs (ResolvedType.Interface iface))
            | ResolvedExport.Class cls
                when not (isInfrastructureSource cls.Source) ->
                let homePath = Path.Interceptors.pipeClass ctx cls
                assignSynthetics ctx counters homePath
                    (outgoingSyntheticRefs (ResolvedType.Class cls))
            | ResolvedExport.TypeAlias typeAlias
                when not (isInfrastructureSource typeAlias.Source) ->
                let homePath = Path.Interceptors.pipeTypeAlias ctx typeAlias
                assignSynthetics ctx counters homePath
                    (typeAliasSyntheticRefs typeAlias)
            | ResolvedExport.Variable variable ->
                let memberPath = Path.Interceptors.pipeVariable ctx variable
                let homePath =
                    TypePath.createWithName
                        (Name.Pascal.fromCase memberPath.Name)
                        (MemberPath.findParentModule memberPath)
                assignSynthetics ctx counters homePath
                    (variableSyntheticRefs variable)
            | ResolvedExport.Function functions ->
                // Free functions render as static members on a host type.
                // Use the function's own MemberPath's parent module + the
                // pascal-cased name as the home; synthetics from the
                // function's parameter types live alongside the function.
                // Skip lib.es-sourced functions whose parameter/return
                // shapes are part of the standard library surface and
                // should not be re-anchored here.
                match functions with
                | [] -> ()
                | first :: _ when isInfrastructureSource first.Source -> ()
                | first :: _ ->
                    let memberPath = Path.Interceptors.pipeFunction ctx first
                    let homePath =
                        TypePath.createWithName
                            (Name.Pascal.fromCase memberPath.Name)
                            (MemberPath.findParentModule memberPath)
                    assignSynthetics ctx counters homePath
                        (functionSyntheticRefs functions)
            | _ -> ()
