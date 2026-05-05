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

    let private yieldIfSynthetic (lazyRt: LazyResolvedType) : ResolvedType seq =
        seq {
            let rt = lazyRt.Value
            if isSyntheticLiteral rt then yield rt
        }

    let private membersSyntheticRefs (members: Member list) : ResolvedType seq =
        seq {
            for memberRef in members do
                match memberRef with
                | Member.Property prop ->
                    yield! yieldIfSynthetic prop.Type
                | Member.Method overloads ->
                    for m in overloads do
                        for param in m.Parameters do
                            yield! yieldIfSynthetic param.Type
                        yield! yieldIfSynthetic m.Type
                | Member.GetAccessor get ->
                    yield! yieldIfSynthetic get.Type
                | Member.SetAccessor set ->
                    yield! yieldIfSynthetic set.ArgumentType
                | Member.IndexSignature idx ->
                    for param in idx.Parameters do
                        yield! yieldIfSynthetic param.Type
                    yield! yieldIfSynthetic idx.Type
                | Member.CallSignature signatures ->
                    for sig' in signatures do
                        for param in sig'.Parameters do
                            yield! yieldIfSynthetic param.Type
                        yield! yieldIfSynthetic sig'.Type
                | Member.ConstructSignature signatures ->
                    for sig' in signatures do
                        for param in sig'.Parameters do
                            yield! yieldIfSynthetic param.Type
                        yield! yieldIfSynthetic sig'.Type
        }

    /// Synthetic literal ResolvedTypes reachable from an Interface/Class
    /// type (one hop through its members).
    let private outgoingSyntheticRefs (resolvedType: ResolvedType) : ResolvedType seq =
        match resolvedType with
        | ResolvedType.Interface iface -> membersSyntheticRefs iface.Members
        | ResolvedType.Class cls -> membersSyntheticRefs cls.Members
        | _ -> Seq.empty

    /// Synthetic literal ResolvedTypes reachable from a Function export's
    /// overload list (parameters and return types).
    let private functionSyntheticRefs (functions: Function list) : ResolvedType seq =
        seq {
            for fn in functions do
                for param in fn.Parameters do
                    yield! yieldIfSynthetic param.Type
                yield! yieldIfSynthetic fn.Type
        }

    /// Build a synthetic TypePath under the given home export's parent
    /// module + export name (as a sub-module). Counter ensures uniqueness
    /// within the home; identical synthetics with the same home would
    /// collide otherwise.
    let private buildSyntheticPath (homeTypePath: TypePath) (counter: int) : TypePath =
        let parentModule =
            ModulePath.createWithName homeTypePath.Name homeTypePath.Parent
        let name = sprintf "_Lit%d" counter |> Name.Pascal.create
        TypePath.createWithName name parentModule

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
            | ResolvedExport.Interface iface when not iface.IsLibEs ->
                let homePath = Path.Interceptors.pipeInterface ctx iface
                assignSynthetics ctx counters homePath
                    (outgoingSyntheticRefs (ResolvedType.Interface iface))
            | ResolvedExport.Class cls when not cls.IsLibEs ->
                let homePath = Path.Interceptors.pipeClass ctx cls
                assignSynthetics ctx counters homePath
                    (outgoingSyntheticRefs (ResolvedType.Class cls))
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
                | first :: _ when first.IsLibEs -> ()
                | first :: _ ->
                    let memberPath = Path.Interceptors.pipeFunction ctx first
                    let homePath =
                        TypePath.createWithName
                            (Name.Pascal.fromCase memberPath.Name)
                            (MemberPath.findParentModule memberPath)
                    assignSynthetics ctx counters homePath
                        (functionSyntheticRefs functions)
            | _ -> ()
