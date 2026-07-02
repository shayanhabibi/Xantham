[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Anchored

open System.Collections.Generic
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Anchored

module Render =
    let anchorMetadataPath (_ctx: GeneratorContext) (anchorPath: AnchorPath) (path: Path) =
        match path with
        | Path.Transient transientPath ->
            transientPath
            |> TransientPath.anchor anchorPath
        | Path.Anchor anchorPath -> anchorPath
    let anchorMetadata (ctx: GeneratorContext) (anchorPath: AnchorPath) (metadata: RenderMetadata) =
        if metadata.Path.IsAnchor then metadata else
        { Path = anchorMetadataPath ctx anchorPath metadata.Path |> Path.create
          Original = metadata.Original
          Source = ValueNone; FullyQualifiedName = ValueNone }

    // The render-time in-scope typar set for a list of typars, in the quoted form the atoms carry
    // (`'Env`, ...). Union'd cumulatively as we descend type -> method -> signature. Generic over the
    // typar-name measure so BOTH the Transient and Concrete anchoring paths can share it.
    let typarNameSet (typars: TypeParameterRender<'r, Name<'c>> list) : Set<string> =
        typars |> List.map (fun tp -> Name.Case.valueOrModified tp.Name) |> Set.ofList
    // Anchor, then SCRUB orphan typars, then localise — in that order. `localise` rewrites every atom
    // to an opaque Widget (erasing the Intrinsic-typar distinction), so the orphan-typar guard MUST run
    // on the anchored-but-not-yet-localised render, exactly as the Inheritance path does. A member/nested
    // render that references a typar its enclosing `type X =` does not declare (a free typar leaking from
    // a collapsed intermediate alias body, e.g. `S["_output"]`) is rewritten to `obj` — the only faithful
    // rendering, since that typar is unreachable at the emission site.
    /// The three ANCHORED-REF SCRUBS as one stage — forward-DAG degradation,
    /// home-child def/ref closure, opaque/erased prefix collapse — shared by the
    /// member path (`anchorScrubLocalise`) and the HERITAGE path (which interleaves
    /// its inheritability filters and must therefore compose the chain itself).
    /// Every degradation is ledgered by its own scrub.
    let scrubAnchored (ctx: GeneratorContext) anchorPath (render: TypeRefRender) =
        // Host unit root for the FORWARD-REFERENCE scrub: the def's anchor root
        // (its top module). Roots outside the placement order (pool/synthetic
        // modules) index as unit 1 inside scrubForwardRefs.
        let hostRoot =
            AnchorPath.flatten anchorPath
            |> List.tryHead
            |> Option.map Name.Case.valueOrModified
        render
        |> TypeRefRender.scrubForwardRefs
            ctx.SyntheticPlacementOrder
            (fun root -> GeneratorContext.Advisory.increment ctx $"forward-ref-scrub:{root}")
            hostRoot
        |> TypeRefRender.scrubUndefinedHomeChildren
            ctx.SyntheticHomePaths
            ctx.SyntheticHomeChildDefs
            (fun home -> GeneratorContext.Advisory.increment ctx $"home-child-scrub:{home}")
        |> TypeRefRender.collapseOpaquePrefixes ctx.ErasedRoots

    let anchorScrubLocalise (ctx: GeneratorContext) (inScope: Set<string>) anchorPath (render: Prelude.TypeRefRender) =
        TypeRefRender.anchor anchorPath render
        |> TypeRefRender.substituteForHeritage inScope
        |> scrubAnchored ctx anchorPath
        |> TypeRefRender.localise anchorPath

    // ---- Shared anchoring core (single source of truth for both the Transient and Concrete paths) ----
    // The two paths differ ONLY in (a) their input `TypeName` (`Name<pascal> voption` for Transient,
    // already-concrete `Name<pascal>` for Concrete) and (b) whether an enum's case paths are re-anchored.
    // Everything below is typed to the UNDERLYING record types (whose Member/Typar name measures are
    // identical across both paths), so a single definition serves both. The name difference is isolated
    // to a `resolveName` thunk supplied by each path's thin entry point.
    module private Shared =
        let resolvePathName (anchorPath: AnchorPath) : Name<Case.pascal> =
            match anchorPath with
            | AnchorPath.Type typePath -> typePath.Name
            | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
            | _ -> failwith "Unreachable branch"

        let anchorTypeParameters (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeParameter: TypeParameterRender<Prelude.TypeRefRender, Name<Case.typar>>) =
            let anchorPath =
                typeParameter.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typeParameter.Metadata.Original
                    Source = typeParameter.Metadata.Source
                    FullyQualifiedName = typeParameter.Metadata.FullyQualifiedName
                }
                TypeParameterRender.Name = typeParameter.Name
                Constraint =
                    typeParameter.Constraint
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Default =
                    typeParameter.Default
                    |> ValueOption.map (TypeRefRender.anchorAndLocalise anchorPath)
                Documentation = typeParameter.Documentation
            }
        // Members carry no TypeName, so a single definition serves both paths. `inScope` is the set of
        // typars legitimately in scope; orphan typars in the member's type are scrubbed to `obj`.
        let rec anchorTypedNameRender (ctx: GeneratorContext) (inScope: Set<string>) (anchorPath: AnchorPath) (typedName: TypedNameRender<Prelude.TypeRefRender, Name<Case.camel>, Name<Case.typar>>) =
            let anchorPath =
                typedName.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = typedName.Metadata.Original
                    Source = typedName.Metadata.Source
                    FullyQualifiedName = typedName.Metadata.FullyQualifiedName
                }
                TypedNameRender.Name = typedName.Name
                Type = typedName.Type |> anchorScrubLocalise ctx inScope anchorPath
                Traits = typedName.Traits
                TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = typedName.Documentation
            }
        let anchorFunctionSignature (ctx: GeneratorContext) (inScope: Set<string>) (anchorPath: AnchorPath) (functionSignature: FunctionLikeSignature<Prelude.TypeRefRender, Name<Case.camel>, Name<Case.typar>>) =
            // UNION this signature's own declared typars, so a generic method's `'P` is preserved.
            let inScope = Set.union inScope (typarNameSet functionSignature.TypeParameters)
            {
                FunctionLikeSignature.Metadata = {
                    Path = Path.create anchorPath
                    Original = functionSignature.Metadata.Original
                    Source = functionSignature.Metadata.Source
                    FullyQualifiedName = functionSignature.Metadata.FullyQualifiedName
                }
                Parameters =
                    functionSignature.Parameters
                    |> List.map (anchorTypedNameRender ctx inScope anchorPath)
                ReturnType =
                    functionSignature.ReturnType
                    |> anchorScrubLocalise ctx inScope anchorPath
                Traits = functionSignature.Traits
                Documentation = functionSignature.Documentation
                TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
            }
        let anchorFunction (ctx: GeneratorContext) (inScope: Set<string>) (anchorPath: AnchorPath) (functionLike: FunctionLikeRender<Prelude.TypeRefRender, Name<Case.camel>, Name<Case.typar>>) =
            let anchorPath =
                functionLike.Metadata.Path
                |> anchorMetadataPath ctx anchorPath
            // UNION the method-level typars before descending into its signatures.
            let inScope = Set.union inScope (typarNameSet functionLike.TypeParameters)
            {
                Metadata = {
                    Path = Path.create anchorPath
                    Original = functionLike.Metadata.Original
                    Source = functionLike.Metadata.Source
                    FullyQualifiedName = functionLike.Metadata.FullyQualifiedName
                }
                FunctionLikeRender.Name = functionLike.Name
                Signatures = functionLike.Signatures |> List.map (anchorFunctionSignature ctx inScope anchorPath)
                Traits = functionLike.Traits
                TypeParameters = functionLike.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
                Documentation = functionLike.Documentation
            }
        // `anchorTypeDefn`, given the already-resolved concrete `name` (each path resolves its own
        // voption-or-not name before calling). Members/functions/constructors are scrubbed of orphan
        // typars via the enclosing type's declared typars.
        let anchorTypeDefn (ctx: GeneratorContext) (anchorPath: AnchorPath) (name: Name<Case.pascal>) (typeDefn: TypeLikeRender<Prelude.TypeRefRender, _, Name<Case.camel>, Name<Case.typar>>) : TypeLikeRender =
            let inScope = typarNameSet typeDefn.TypeParameters
            {
                TypeLikeRender.Metadata = {
                    Path = Path.create anchorPath
                    Original = typeDefn.Metadata.Original
                    Source = typeDefn.Metadata.Source
                    FullyQualifiedName = typeDefn.Metadata.FullyQualifiedName
                }
                Name = name
                TypeParameters =
                    typeDefn.TypeParameters
                    |> List.map (anchorTypeParameters ctx anchorPath)
                Inheritance =
                    // An F# interface can only inherit other interfaces. Decide which bases
                    // are interface-shaped (Path atom / generic Prefix) versus scalars
                    // (Widget/Intrinsic from the lib.es substitution, e.g. Error->exn) BEFORE
                    // localising — localise rewrites every atom to a Widget, erasing the
                    // distinction (which silently dropped legitimate NON-generic interface
                    // bases). Keep interface bases, drop scalars, then localise the survivors.
                    // The substituteForHeritage step also scrubs orphan typars from the base.
                    typeDefn.Inheritance
                    |> List.map (TypeRefRender.anchor anchorPath)
                    // The SAME scrub stage the member path runs — heritage was the one
                    // ref channel without it, so forward-unit ARGUMENTS survived into
                    // bases (`inherit EventTarget<Agents.X>`: FS0039 + its FS0887
                    // cascade at the first real Workers typecheck). A scrubbed ARG
                    // degrades to obj and the base stays inheritable; a scrubbed HEAD
                    // degrades the whole base to a scalar, which the filters below
                    // then drop — every path ledgered by its scrub.
                    |> List.map (scrubAnchored ctx anchorPath)
                    |> List.filter (fun base' ->
                        // A base rewritten to the Erased.* advisory aliases is `obj` —
                        // uninheritable. Dropped HERE (anchor time; localise erases the
                        // distinction) and LEDGERED, never silent.
                        if TypeRefRender.isErasedBase base' then
                            GeneratorContext.Advisory.increment ctx $"erased-heritage:{Name.Case.valueOrModified name}"
                            false
                        else TypeRefRender.isInterfaceBase base')
                    |> List.map (TypeRefRender.substituteForHeritage inScope)
                    |> List.map (TypeRefRender.localise anchorPath)
                Members =
                    typeDefn.Members
                    |> List.map (anchorTypedNameRender ctx inScope anchorPath)
                Functions =
                    typeDefn.Functions
                    |> List.map (anchorFunction ctx inScope anchorPath)
                Constructors =
                    typeDefn.Constructors
                    |> List.map (List.map (anchorTypedNameRender ctx inScope anchorPath))
                Documentation = typeDefn.Documentation
            }

    module Transient =
        let anchorUnionCase (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralCaseRender<'T>) =
            let anchoredPath =
                enumUnion.Metadata.Path
                |> anchorMetadataPath ctx parentPath
            {
                LiteralCaseRender.Name =
                    enumUnion.Name
                    |> ValueOption.defaultValue (Shared.resolvePathName anchoredPath)
                Metadata = {
                    Path = Path.create anchoredPath
                    Original = enumUnion.Metadata.Original
                    Source = enumUnion.Metadata.Source
                    FullyQualifiedName = enumUnion.Metadata.FullyQualifiedName
                }
                Value = enumUnion.Value
                Documentation = enumUnion.Documentation
            }
        let anchorUnion (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralUnionRender<'T>) =
            let anchoredPath =
                enumUnion.Metadata.Path
                |> anchorMetadataPath ctx parentPath
            {
                Metadata = {
                    Path = Path.create anchoredPath
                    Original = enumUnion.Metadata.Original
                    Source = enumUnion.Metadata.Source
                    FullyQualifiedName = enumUnion.Metadata.FullyQualifiedName
                }
                LiteralUnionRender.Name =
                    enumUnion.Name
                    |> ValueOption.defaultWith (fun () -> Shared.resolvePathName anchoredPath)
                Cases =
                    enumUnion.Cases
                    |> List.map (anchorUnionCase ctx anchoredPath)
                Documentation = enumUnion.Documentation
            }
        let anchorTypeParameters ctx anchorPath (tp: Transient.TypeParameterRender) = Shared.anchorTypeParameters ctx anchorPath tp
        let anchorTypedNameRender ctx inScope anchorPath (tn: Transient.TypedNameRender) = Shared.anchorTypedNameRender ctx inScope anchorPath tn
        let anchorFunction ctx inScope anchorPath (f: Transient.FunctionLikeRender) = Shared.anchorFunction ctx inScope anchorPath f
        let anchorTypeDefn (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeDefn: Transient.TypeLikeRender) =
            let anchorPath = typeDefn.Metadata.Path |> anchorMetadataPath ctx anchorPath
            let name = typeDefn.Name |> ValueOption.defaultWith (fun () -> Shared.resolvePathName anchorPath)
            Shared.anchorTypeDefn ctx anchorPath name typeDefn
        let anchorTypeAlias (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeAlias: Transient.TypeAliasRender) =
            match typeAlias with
            | Transient.TypeAliasRender.Alias alias ->
                let anchorPath =
                    alias.Metadata.Path
                    |> anchorMetadataPath ctx anchorPath
                {
                    TypeAliasRenderRef.Metadata = {
                        Path = Path.create anchorPath
                        Original = alias.Metadata.Original
                        Source = alias.Metadata.Source
                        FullyQualifiedName = alias.Metadata.FullyQualifiedName
                    }
                    Name = alias.Name |> ValueOption.defaultWith (fun () -> Shared.resolvePathName anchorPath)
                    TypeParameters = alias.TypeParameters |> List.map (Shared.anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    // Scrub orphan typars from the abbreviation body too (same mechanism as members):
                    // `type UnionToIntersectionFn<'T> = option<'Intersection>` leaks the undeclared
                    // `'Intersection`. In-scope = the alias's OWN declared typars.
                    Type = alias.Type |> anchorScrubLocalise ctx (typarNameSet alias.TypeParameters) anchorPath
                }
                |> TypeAliasRender.Alias
            | Transient.TypeAliasRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx anchorPath typeLikeRender
                |> TypeAliasRender.TypeDefn
            | Transient.TypeAliasRender.StringUnion literalUnionRender ->
                anchorUnion ctx anchorPath literalUnionRender
                |> TypeAliasRender.StringUnion
            | Transient.TypeAliasRender.EnumUnion literalUnionRender ->
                anchorUnion ctx anchorPath literalUnionRender
                |> TypeAliasRender.EnumUnion
            | Transient.TypeAliasRender.Function functionLikeRender ->
                // Top-level function alias: only its own typars are in scope (unioned inside anchorFunction).
                Shared.anchorFunction ctx Set.empty anchorPath functionLikeRender
                |> TypeAliasRender.Function

        let anchor (ctx: GeneratorContext) (anchorPath: AnchorPath) (render: Transient.Render) =
            let ref, render = render
            match render.Value with
            | Transient.TypeRender.EnumUnion enumUnion ->
                anchorUnion ctx anchorPath enumUnion
                |> TypeRender.EnumUnion
            | Transient.TypeRender.StringUnion enumUnion ->
                anchorUnion ctx anchorPath enumUnion
                |> TypeRender.StringUnion
            | Transient.TypeRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx anchorPath typeLikeRender
                |> TypeRender.TypeDefn
            | Transient.TypeRender.TypeAlias typeAliasRender ->
                anchorTypeAlias ctx anchorPath typeAliasRender
                |> TypeRender.TypeAlias
            | Transient.TypeRender.Function functionLikeRender ->
                Shared.anchorFunction ctx Set.empty anchorPath functionLikeRender
                |> TypeRender.Function
            | Transient.TypeRender.Variable typedNameRender ->
                Shared.anchorTypedNameRender ctx Set.empty anchorPath typedNameRender
                |> TypeRender.Variable
            |> fun typeRender ->
                Anchored.Render( TypeRefRender.anchorAndLocalise anchorPath ref, lazy typeRender )

    module Concrete =
        // Concrete inputs are already anchored (their paths are `Path.Anchor`), so `anchorMetadataPath`
        // is an identity on the path; enum names/cases are already concrete and copy through unchanged.
        let inline anchorEnumCase (enumUnion: Concrete.LiteralCaseRender<'T>) =
            {
                Metadata = enumUnion.Metadata
                LiteralCaseRender.Name = enumUnion.Name
                Value = enumUnion.Value
                Documentation = enumUnion.Documentation
            }
        let inline anchorEnum (enumUnion: Concrete.LiteralUnionRender<'T>) =
            {
                Metadata = enumUnion.Metadata
                LiteralUnionRender.Name = enumUnion.Name
                Cases = enumUnion.Cases |> List.map anchorEnumCase
                Documentation = enumUnion.Documentation
            }
        let anchorTypeParameters ctx anchorPath (tp: Concrete.TypeParameterRender) = Shared.anchorTypeParameters ctx anchorPath tp
        let anchorTypedNameRender ctx anchorPath (tn: Concrete.TypedNameRender) = Shared.anchorTypedNameRender ctx Set.empty anchorPath tn
        let anchorFunction ctx anchorPath (f: Concrete.FunctionLikeRender) = Shared.anchorFunction ctx Set.empty anchorPath f
        let anchorTypeDefn (ctx: GeneratorContext) (typeDefn: Concrete.TypeLikeRender) =
            let anchorPath =
                match typeDefn.Metadata.Path with
                | Path.Anchor anchorPath -> anchorPath
                | _ -> failwith "UNREACHABLE"
            Shared.anchorTypeDefn ctx anchorPath typeDefn.Name typeDefn
        let anchorTypeAlias (ctx: GeneratorContext) (typeAlias: Concrete.TypeAliasRender) =
            match typeAlias with
            | Concrete.TypeAliasRender.Alias alias ->
                let anchorPath =
                    match alias.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                {
                    TypeAliasRenderRef.Metadata = {
                        Path = Path.create anchorPath
                        Original = alias.Metadata.Original
                        Source = alias.Metadata.Source
                        FullyQualifiedName = alias.Metadata.FullyQualifiedName
                    }
                    Name = alias.Name
                    TypeParameters = alias.TypeParameters |> List.map (Shared.anchorTypeParameters ctx anchorPath)
                    Documentation = alias.Documentation
                    Type = alias.Type |> anchorScrubLocalise ctx (typarNameSet alias.TypeParameters) anchorPath
                }
                |> TypeAliasRender.Alias
            | Concrete.TypeAliasRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx typeLikeRender
                |> TypeAliasRender.TypeDefn
            | Concrete.TypeAliasRender.StringUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeAliasRender.StringUnion
            | Concrete.TypeAliasRender.EnumUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeAliasRender.EnumUnion
            | Concrete.TypeAliasRender.Function functionLikeRender ->
                let anchorPath =
                    match functionLikeRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                Shared.anchorFunction ctx Set.empty anchorPath functionLikeRender
                |> TypeAliasRender.Function

        let anchor (ctx: GeneratorContext) (render: Concrete.Render) =
            let ref, render = render
            match render.Value with
            | Concrete.TypeRender.EnumUnion enumUnion ->
                anchorEnum enumUnion
                |> TypeRender.EnumUnion
            | Concrete.TypeRender.StringUnion literalUnionRender ->
                anchorEnum literalUnionRender
                |> TypeRender.StringUnion
            | Concrete.TypeRender.TypeDefn typeLikeRender ->
                anchorTypeDefn ctx typeLikeRender
                |> TypeRender.TypeDefn
            | Concrete.TypeRender.TypeAlias typeAliasRender ->
                anchorTypeAlias ctx typeAliasRender
                |> TypeRender.TypeAlias
            | Concrete.TypeRender.Function functionLikeRender ->
                let anchorPath =
                    match functionLikeRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                Shared.anchorFunction ctx Set.empty anchorPath functionLikeRender
                |> TypeRender.Function
            | Concrete.TypeRender.Variable typedNameRender ->
                let anchorPath =
                    match typedNameRender.Metadata.Path with
                    | Path.Anchor anchorPath -> anchorPath
                    | _ -> failwith "UNREACHABLE"
                Shared.anchorTypedNameRender ctx Set.empty anchorPath typedNameRender
                |> TypeRender.Variable
            |> fun typeRender ->
                let ref =
                    ModulePath.init ""
                    |> AnchorPath.create
                    |> TypeRefRender.anchorAndLocalise
                    |> funApply ref
                Anchored.Render(ref, lazy typeRender)

let inline addOrReplace (ctx: GeneratorContext) (key: ResolvedType) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
    GeneratorContext.Anchored.addResolvedType ctx key value
    

let rec anchor (ctx: GeneratorContext) anchors anchorPath resolvedType =
    GeneratorContext.Prelude.tryGet ctx resolvedType
    |> ValueOption.iter (anchorPreludeAnchorScope ctx (Some anchors) anchorPath)
        
and anchorPreludeAnchorScope (ctx: GeneratorContext) anchors anchorPath renderScope =
    let anchors = defaultArg anchors (Dictionary<ResolvedType, TypePath * Render>())
    match renderScope with
    | { Root = ValueSome (TypeLikePath.Anchored path); Render = Render.Concrete(renderTuple); TransientChildren = ValueSome transientChildren } ->
        let anchorPath = AnchorPath.create path
        let anchors = Dictionary<ResolvedType, TypePath * Render>()
        let render = Render.Concrete.anchor ctx renderTuple
        {
            RenderScope.Type = renderScope.Type
            Root = Choice1Of2 path
            TypeRef =
                renderScope.TypeRef
                |> TypeRefRender.anchor anchorPath
            Render = render
            Anchors = anchors
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
        transientChildren.TypeStore.Keys
        |> Seq.filter (anchors.ContainsKey >> not)
        |> Seq.iter (anchor ctx anchors anchorPath)
    | { Root = ValueSome (TypeLikePath.Anchored path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
        // Canonical hoisted enum (LiteralUnions.<name>) or SHARED-LITERAL canonical home
        // (SharedLiterals.<name>): anchored at its OWN absolute path, NOT re-anchored against any
        // referencing owner. Registered directly so collectModules emits it once; addResolvedType
        // is idempotent by ResolvedType so repeated drives are harmless.
        let selfAnchor = AnchorPath.create path
        // SHARED-HOME LAMBDA-LIFT override (stage 4) — the same TypeParameters override as the
        // Transient-root arm below: a lifted shared home's def must DECLARE its free typars so the
        // orphan-scrub inScope preserves the member references (every cached reference to this home
        // applies the same uniform typar args; out-of-scope sites scrub those args per-site).
        let renderTuple =
            match GeneratorContext.Prelude.tryGetHoistedHomeTypars ctx renderScope.Type with
            | ValueSome lifted ->
                let refRender, lazyRender = renderTuple
                refRender,
                lazy (match lazyRender.Value with
                      | Transient.TypeRender.TypeDefn td ->
                          Transient.TypeRender.TypeDefn { td with TypeParameters = lifted }
                      | other -> other)
            | ValueNone -> renderTuple
        let render = Render.Transient.anchor ctx selfAnchor renderTuple
        // NESTED-CHILD MATERIALIZATION (fixed 2026-07-04; both defects were unique to
        // this arm — the Concrete arm above and the export path were already correct):
        //  (1) children must anchor into THIS scope's registered Anchors dict — the old
        //      code registered a permanently-empty dict and recursed into the caller's
        //      throwaway one, so child renders were computed and dropped (collectModules
        //      reads registered scopes' Anchors only);
        //  (2) the child's anchor path must carry its LEAF from the TypeStore VALUE
        //      (`<home>.<Leaf>`, exactly as anchorPreludeExportScope and the grandchild
        //      recursion derive it) — a child's own root is nameless, so anchoring the
        //      leafless selfAnchor collapsed the def onto the home's own path.
        // The CALLER's anchors dict must NOT gate the recursion: a home re-anchored
        // through a second referencing context REPLACES its registered scope, so a
        // caller-dict filter (children recorded on the FIRST pass) would leave the
        // replacement's Anchors empty and silently drop the already-emitted child defs.
        // Only the local dict gates — each registration materializes its own children.
        let childAnchors = Dictionary<ResolvedType, TypePath * Render>()
        {
            RenderScope.Type = renderScope.Type
            Root = Choice1Of2 path
            TypeRef = renderScope.TypeRef |> TypeRefRender.anchor selfAnchor
            Render = render
            Anchors = childAnchors
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
        // ANCHOR-TIME DEF/REF COMPLETION: the home's lazy render forces under whichever
        // caller reaches it FIRST (often a main-pass export, whose scope then absorbs the
        // cache-hit re-registrations) — so by the time the driver anchors the home, its
        // TransientChildren store can be missing children whose molecule refs were cached.
        // Re-walk the home's own member types against ITS store: cached transient children
        // re-register here idempotently, so the recursion below materializes their defs
        // under the home — where the anchored member refs point.
        (match renderScope.Type with
         | ResolvedType.TypeLiteral _ ->
             reRegisterStructuralLiterals ctx transientChildren renderScope.Type
         | _ -> ())
        transientChildren.TypeStore
        |> Seq.filter (fun (KeyValue(key, _)) -> not (childAnchors.ContainsKey key))
        |> Seq.toList
        |> List.iter (fun (KeyValue(key, storedTransient)) ->
            let childPath = TransientPath.anchor selfAnchor (TransientPath.create storedTransient)
            anchor ctx childAnchors childPath key)
    | { Root = ValueSome (TypeLikePath.Transient path); Render = Render.Transient(renderTuple); TransientChildren = ValueSome transientChildren } ->
        let path = TransientTypePath.anchor anchorPath path
        // LAMBDA-LIFT override (stage 3b): a hoisted literal grafted under its owning alias whose
        // members reference the alias's typars carries a HoistedHomeTypars entry (written by
        // caseLiteralRef, keyed by this scope's ResolvedType). Override the def's TypeParameters
        // BEFORE Shared.anchorTypeDefn so the emitted nested def DECLARES the lifted typars and
        // the orphan-scrub inScope grows to preserve the member references. The lazy wrapper is
        // transparent — Transient.anchor forces the render immediately. Applies only to this
        // Transient-root arm (single-owner grafted defs); SharedLiterals canonical homes
        // (Anchored-root arm above) stay un-lifted until the stage-4 per-site-args extension.
        let renderTuple =
            match GeneratorContext.Prelude.tryGetHoistedHomeTypars ctx renderScope.Type with
            | ValueSome lifted ->
                let refRender, lazyRender = renderTuple
                refRender,
                lazy (match lazyRender.Value with
                      | Transient.TypeRender.TypeDefn td ->
                          Transient.TypeRender.TypeDefn { td with TypeParameters = lifted }
                      | other -> other)
            | ValueNone -> renderTuple
        let render = Render.Transient.anchor ctx anchorPath renderTuple
        // SHARED-LITERAL COUNTING (phase 1 only): record EVERY def-home VISIT for a hoisted
        // object-LITERAL, BEFORE the first-write-wins `tryAdd` below. A literal visited under
        // >1 distinct anchored path is reached through multiple owner contexts — its single def
        // lands under only the first, dangling the rest (the shared deep-nested FS0039 class).
        // `markSharedLiterals` consumes this to assign canonical `SharedLiterals.X` homes.
        ctx.SharedLiteralVisits
        |> ValueOption.iter (fun visits ->
            match renderScope.Type with
            | ResolvedType.TypeLiteral _ ->
                match visits.TryGetValue renderScope.Type with
                | true, homes -> homes.Add path |> ignore
                | _ ->
                    let homes = HashSet<TypePath>()
                    homes.Add path |> ignore
                    visits[renderScope.Type] <- homes
            | _ -> ())
        anchors
        |> Dictionary.tryAdd renderScope.Type (path, render)
        // Recurse into this hoisted literal's OWN nested literals (the two-deep case).
        // Each grandchild's NAMED transient (carrying its leaf, e.g. `Moored(_,"TimeRange")`)
        // is held here as the TypeStore VALUE — the same value that rendered its reference.
        // Anchor each grandchild against THAT stored transient (mirroring
        // `anchorPreludeExportScope`), NOT against the grandchild's own NAMELESS `Anchored`
        // root: re-deriving from the nameless root collapses the grandchild onto THIS node's
        // path (dropping the leaf), so its definition is misplaced/merged into the parent and
        // its `Parent.Leaf` reference dangles. Threading the stored transient lands the def at
        // `<thisPath>.<Leaf>`, restoring emission/reference symmetry.
        let childAnchorPath = AnchorPath.create path
        transientChildren.TypeStore
        |> Seq.filter (fun (KeyValue(key, _)) -> not (anchors.ContainsKey key))
        |> Seq.iter (fun (KeyValue(key, storedTransient)) ->
            let grandchildPath = TransientPath.anchor childAnchorPath (TransientPath.create storedTransient)
            anchor ctx anchors grandchildPath key)
    | { Root = ValueNone; Render = Render.RefOnly typeRef } ->
        typeRef
        |> TypeRefRender.anchor anchorPath
        |> Choice1Of2
        |> GeneratorContext.Anchored.addResolvedType ctx renderScope.Type
    | badScope ->
        eprintfn $"Bad scope: %A{badScope}"
and anchorPreludeExportScope (ctx: GeneratorContext) export (renderScopeStore: RenderScopeStore) =
    let anchors = Dictionary<ResolvedType, TypePath * Render>()
    let anchorPath = Interceptors.pipeExport ctx export
    renderScopeStore.TypeStore
    |> Seq.iter (fun (KeyValue(key, value)) ->
        let renderScope =
            GeneratorContext.Prelude.tryGet ctx key
            |> ValueOption.orElseWith (fun () ->
                prerender ctx renderScopeStore (LazyContainer.CreateTypeKeyDummy<ResolvedType> key)
                |> ignore
                GeneratorContext.Prelude.tryGet ctx key
                )
            |> ValueOption.defaultWith (fun () ->
                failwith "Could not find render scope for key")
        let path = TransientPath.anchor anchorPath (TransientPath.create value)
        anchorPreludeAnchorScope ctx (Some anchors) path renderScope)
    anchors

let rec registerAnchorFromExport (ctx: GeneratorContext) (export: ResolvedExport): unit =
    let scope = RenderScopeStore.create()
    // COUNTING PRE-PASS ONLY: attribute every union-wrapped hoistable object-literal reachable from
    // THIS top-level export to it as a referencing owner (a STRUCTURAL ResolvedType walk that, unlike
    // prerender, is not defeated by the interned/cached union subtree shared across owners). A
    // `Module` is a container with no body of its own — its children are walked when recursed below.
    // No-op in the real pass (`SharedLiteralRefOwners` unset).
    if ctx.SharedLiteralRefOwners.IsSome then
        match export with
        | ResolvedExport.Module _ -> ()
        | _ ->
            ctx.CurrentRefOwner.Value <- ValueSome (Interceptors.pipeExport ctx export)
            let bodyRts =
                match export with
                | ResolvedExport.Interface i -> i.Members |> List.choose (function Member.Property p -> Some p.Type.Value | Member.GetAccessor g -> Some g.Type.Value | _ -> None)
                | ResolvedExport.Class c -> c.Members |> List.choose (function Member.Property p -> Some p.Type.Value | Member.GetAccessor g -> Some g.Type.Value | _ -> None)
                | ResolvedExport.TypeAlias a -> [ a.Type.Value ]
                | ResolvedExport.Variable v -> [ v.Type.Value ]
                | ResolvedExport.Function fns -> fns |> List.map (fun f -> f.Type.Value)
                | ResolvedExport.Enum _ | ResolvedExport.Module _ -> []
            bodyRts |> List.iter (RenderScope_Prelude.recordUnionMemberRefOwners ctx)
    match export with
    | ResolvedExport.Class value ->
        let path = Interceptors.pipeClass ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            Class.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Class value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Variable value ->
        let path = Interceptors.pipeVariable ctx value
        let anchorPath = AnchorPath.create path
        let typeRef =
            value.Type
            |> prerender ctx scope
            |> TypeRefRender.anchorAndLocalise anchorPath
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            typeRef |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            {
                Metadata = {
                    Path = Path.create path
                    Original = Path.create path
                    Source = value.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome value.FullyQualifiedName
                }
                TypedNameRender.Name = value.Name
                Type = typeRef
                Traits = Set []
                TypeParameters = []
                Documentation = value.Documentation
            }
            |> TypeRender.Variable
        {
            Type = value.Type.Value
            Root = Choice2Of2 path 
            TypeRef = typeRef
            Render = Anchored.Render( typeRef, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Interface value ->
        let path = Interceptors.pipeInterface ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            Interface.render ctx scope value
            |> Render.Concrete.anchorTypeDefn ctx
            |> TypeRender.TypeDefn
        {
            Type = ResolvedType.Interface value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.TypeAlias value ->
        let path = Interceptors.pipeTypeAlias ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            TypeAlias.render ctx scope value
            |> Render.Concrete.anchorTypeAlias ctx
            |> TypeRender.TypeAlias
        {
            Type = value.Type.Value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Enum value ->
        let path = Interceptors.pipeEnum ctx value
        let ref = TypeRefRender.create false path
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors value && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            match Enum.render ctx value with
            | Concrete.TypeRender.EnumUnion enumUnion ->
                enumUnion
                |> Render.Concrete.anchorEnum
                |> TypeRender.EnumUnion
            | Concrete.TypeRender.StringUnion literalUnionRender ->
                literalUnionRender
                |> Render.Concrete.anchorEnum
                |> TypeRender.StringUnion
            | _ -> failwith "Unreachable branch"
        {
            Type = ResolvedType.Enum value
            Root = Choice1Of2 path
            TypeRef = ref
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Function [] -> failwith "Empty function list"
    | ResolvedExport.Function (headFunc :: rest) ->
        let path = Interceptors.pipeFunction ctx headFunc
        let anchorPath = AnchorPath.create path
        let ref =
            headFunc.SignatureKey.Value
            |> ResolvedType.TypeLiteral
            |> LazyContainer.CreateTypeKeyDummy<ResolvedType>
            |> prerender ctx scope
            |> TypeRefRender.anchor anchorPath
        if Interceptors.shouldIgnoreRender ctx.Customisation.Interceptors headFunc && not (ctx.TopLevelExports.Contains export) then
            ref |> Choice1Of2 |> GeneratorContext.Anchored.addResolvedExport ctx export
        else
        let render =
            {
                FunctionLikeRender.Name = headFunc.Name
                Metadata = {
                    Path = Path.create path
                    Original = Path.create path
                    Source = headFunc.Source |> Option.toValueOption
                    FullyQualifiedName = ValueSome headFunc.FullyQualifiedName
                }
                Signatures =
                    (headFunc :: rest)
                    |> List.map (fun func ->
                        {
                            FunctionLikeSignature.Metadata = {
                                Path = Path.create path
                                Original = Path.create path
                                Source = func.Source |> Option.toValueOption
                                FullyQualifiedName = ValueSome func.FullyQualifiedName
                            }
                            Parameters =
                                func.Parameters
                                |> List.map (
                                    Parameter.render ctx scope
                                    >> Render.Concrete.anchorTypedNameRender ctx anchorPath
                                    )
                            ReturnType =
                                func.Type
                                |> prerender ctx scope
                                |> TypeRefRender.anchorAndLocalise anchorPath
                            Traits = Set [ RenderTraits.Static ]
                            Documentation = func.Documentation
                            TypeParameters =
                                func.TypeParameters
                                |> List.map (
                                    _.Value
                                    >> TypeParameter.render ctx scope
                                    >> Render.Concrete.anchorTypeParameters ctx anchorPath
                                    )
                        }
                        )
                    |> List.distinct
                Traits = Set []
                TypeParameters =
                    headFunc.TypeParameters
                    |> List.map (fun typeParameter ->
                        let typeParameter = typeParameter.Value
                        let path =
                            path
                            |> TypeParamPath.createOnMember typeParameter.Name
                        {
                            TypeParameterRender.Metadata = {
                                Path = Path.create path
                                Original = Path.create path
                                Source = ValueNone
                                FullyQualifiedName = ValueNone
                            }
                            Name = typeParameter.Name
                            Constraint =
                                typeParameter.Constraint
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchorAndLocalise anchorPath)
                            Default = 
                                typeParameter.Default
                                |> Option.toValueOption
                                |> ValueOption.map (prerender ctx scope >> TypeRefRender.anchorAndLocalise anchorPath)
                            Documentation = typeParameter.Documentation
                        }
                        )
                Documentation = headFunc.Documentation
            }
            |> TypeRender.Function
        {
            Type = headFunc.SignatureKey.Value |> ResolvedType.TypeLiteral
            Root = Choice2Of2 path
            TypeRef = ref 
            Render = Anchored.Render( ref, lazy render )
            Anchors = anchorPreludeExportScope ctx export scope
        }
        |> Choice2Of2
        |> GeneratorContext.Anchored.addResolvedExport ctx export
    | ResolvedExport.Module value ->
        value.Exports
        |> List.iter (registerAnchorFromExport ctx)

let private registerExportsForAnchoring (ctx: GeneratorContext) = List.iter (registerAnchorFromExport ctx)

/// Assign canonical `SharedLiterals.<name>` homes to genuinely-shared hoisted object-literals.
///
/// `countingCtx` is a throwaway context that has already run `processExports` with
/// `SharedLiteralVisits = ValueSome _`, so it holds, per literal ResolvedType, the set of distinct
/// anchored def-home paths the literal was visited under. A literal with >1 distinct home is
/// reached through multiple owner contexts (its single def lands under only the first, dangling the
/// rest — the shared deep-nested FS0039 class). Each such literal is given a canonical absolute
/// home in `realCtx.SharedLiteralHomes`, which `prerender` then roots it at (mirroring LiteralUnions).
///
/// Naming is DETERMINISTIC and COLLISION-FREE: the human-readable stem comes from the literal's
/// member names, but distinct ResolvedTypes that share a stem (same property names, different
/// property types) are disambiguated by a stable suffix derived from sorting on the literal's
/// smallest def-home path (proven run-to-run stable). Keyed by ResolvedType identity so the SAME
/// shared literal always maps to ONE name and DISTINCT shared literals never collapse together.
let markSharedLiterals (countingCtx: GeneratorContext) (realCtx: GeneratorContext) =
    countingCtx.SharedLiteralVisits
    |> ValueOption.iter (fun visits ->
        let pathKey (p: TypePath) =
            TypePath.flatten p |> List.map Name.Case.valueOrModified |> String.concat "."
        let anchorKey (a: AnchorPath) =
            match a with
            | AnchorPath.Type tp -> pathKey tp
            // Member/Parameter/Module owners are rare for hoistable literals; a stable string form is
            // sufficient — `anchorKey` only feeds the deterministic representative tie-break.
            | other -> string other
        // Reference-owner counts for UNION-WRAPPED literals (empty dict when the ref-owner collector
        // was not enabled). A literal referenced through >1 distinct owner is shared even though the
        // def-VISIT gate (`visits`) never saw it as a def-home (it lives only in a cached union
        // molecule). Combined gate below: shared iff >1 def-home OR >1 distinct referencing owner.
        let refOwners =
            countingCtx.SharedLiteralRefOwners
            |> ValueOption.defaultValue (Dictionary<ResolvedType, HashSet<AnchorPath>>())
        // Names already assigned (idempotent across calls) — never reassign an rt (keeps the
        // canonical name STABLE) and never reuse a name (keeps DISTINCT literals distinct).
        let usedNames =
            realCtx.SharedLiteralHomes.Values
            |> Seq.map (fun p -> Name.Case.valueOrModified p.Name)
            |> System.Collections.Generic.HashSet
        // Candidate RTs = the union of def-VISIT keys and reference-owner keys. A literal reached
        // ONLY inside a union (never as a def-home) appears solely in `refOwners`.
        let candidateRts =
            seq {
                for KeyValue(rt, _) in visits do rt
                for KeyValue(rt, _) in refOwners do rt
            }
            |> Seq.distinct
        // Genuinely-shared literals NOT YET assigned: visited under >1 distinct def-home path OR
        // referenced through >1 distinct owner (the union-wrapped case). A literal reached through
        // exactly ONE owner (1 def-home and ≤1 ref-owner) stays nested under it (control preserved).
        let shared =
            candidateRts
            |> Seq.choose (fun rt ->
                let defHomes =
                    match visits.TryGetValue rt with
                    | true, h -> h
                    | _ -> HashSet<TypePath>()
                let owners =
                    match refOwners.TryGetValue rt with
                    | true, o -> o
                    | _ -> HashSet<AnchorPath>()
                let isShared = defHomes.Count > 1 || owners.Count > 1
                if isShared && (GeneratorContext.SharedLiterals.tryGetHome realCtx rt |> ValueOption.isNone) then
                    let stem =
                        match rt with
                        | ResolvedType.TypeLiteral tl -> sharedLiteralNameStem tl
                        | _ -> "Lit"
                    // Deterministic representative: lexicographically-smallest of all the owner/home
                    // discriminators (def-home paths AND referencing-owner anchors), so the tie-break
                    // ordering is stable run-to-run regardless of which collector saw the literal.
                    let rep =
                        Seq.append (defHomes |> Seq.map pathKey) (owners |> Seq.map anchorKey)
                        |> Seq.sort
                        |> Seq.head
                    // The ROOT module segments of every def-home and referencing owner — the
                    // placement inputs (which units use this literal).
                    let roots =
                        Seq.append
                            (defHomes |> Seq.choose (TypePath.flatten >> List.tryHead))
                            (owners |> Seq.choose (fun o -> AnchorPath.flatten o |> List.tryHead))
                        |> Seq.map Name.Case.valueOrModified
                        |> Set.ofSeq
                    Some (stem, rep, rt, roots)
                else None)
            // Stable order independent of dictionary enumeration: by stem, then representative path.
            |> Seq.sortBy (fun (stem, rep, _, _) -> stem, rep)
            |> Seq.toList
        // Host module for a literal's canonical home. With a placement order (emission):
        // the EARLIEST unit among the literal's owner roots hosts `<Host>.SharedLiterals.<name>`
        // — visible to every referencer through the unit DAG (each unit references all earlier
        // ones), and placed into the host's compilation unit by the ordinary top-module split.
        // Roots outside the order (pooled/erased-dep modules) map to the first unit.
        // Without an order (isolation tests): the root-level `SharedLiterals` module, unchanged.
        let hostModuleFor (roots: Set<string>) =
            match realCtx.SyntheticPlacementOrder with
            | [] -> sharedLiteralModule
            | _ when
                not (Set.isEmpty roots)
                && roots |> Set.forall (fun r -> realCtx.ErasedRoots |> List.contains r) ->
                // Owned ONLY by erased content: the home is erased content too. Minting
                // under an erased root lets the path substitution collapse every
                // reference to the Erased.* alias; the definition drops with the module.
                GeneratorContext.Advisory.increment realCtx $"erased-shared-home:{Set.minElement roots}"
                ModulePath.create "SharedLiterals" (ModulePath.init (Set.minElement roots))
            | order ->
                // Owners outside the order (pooled/erased-dep contexts) mean unit 1; and
                // unit 1 hosting uses the ROOT-LEVEL module (pooled into the first unit's
                // slice), never `<order[0]>.SharedLiterals` — the first unit's top module
                // may be overlay-replaced (zod) and would drop the homes with it.
                let indexOf root = order |> List.tryFindIndex ((=) root) |> Option.defaultValue 0
                let minIdx =
                    if Set.isEmpty roots then 0
                    else roots |> Seq.map indexOf |> Seq.min
                if minIdx = 0 then sharedLiteralModule
                else ModulePath.create "SharedLiterals" (ModulePath.init order[minIdx])
        // Assign names, disambiguating against names already in use (this round and prior rounds)
        // by appending the smallest free numeric suffix — deterministic given the stable order.
        for (stem, _rep, rt, roots) in shared do
            let name =
                if usedNames.Add stem then stem
                else
                    let mutable n = 2
                    while not (usedNames.Add $"{stem}_{n}") do n <- n + 1
                    $"{stem}_{n}"
            let typePath = hostModuleFor roots |> TypePath.createWithName (Name.Pascal.create name)
            GeneratorContext.SharedLiterals.addHome realCtx rt typePath)

/// Run the shared-literal counting pass over an explicit export LIST (no interner) and mark the
/// genuinely-shared literals on `ctx`. Drives a throwaway counting context whose
/// `SharedLiteralVisits` collector records every literal def-home VISIT, then assigns canonical
/// `SharedLiterals.<name>` homes to literals visited under >1 distinct home. Used by the
/// interner-driven entry point AND directly by tests that build exports via mocks.
let markSharedLiteralsFromExportList (ctx: GeneratorContext) (exports: ResolvedExport list) =
    let countingCtx =
        { GeneratorContext.Create(ctx.PreludeGetTypeRef, ctx.Customisation) with
            SharedLiteralVisits = ValueSome (Dictionary<ResolvedType, HashSet<TypePath>>())
            SharedLiteralRefOwners = ValueSome (Dictionary<ResolvedType, HashSet<AnchorPath>>()) }
    exports |> List.iter (registerAnchorFromExport countingCtx)
    markSharedLiterals countingCtx ctx

/// Emit the canonical OWNER-INDEPENDENT hoisted types once: literal-union enums at
/// `LiteralUnions.<name>` and shared object-literals at `SharedLiterals.<name>`. They are anchored
/// at their own absolute path, so the owner-driven export pass never registers them. Drive them
/// directly from the prelude cache — each anchored-root + Transient-render prelude scope is
/// registered into AnchorRenders by its self-anchor, so `collectModules` emits it. Interface/Class
/// scopes also have anchored roots but Concrete renders and are emitted through the export pass —
/// they must NOT be re-driven here. Shared by the interner-driven pipeline and the test harness.
let emitCanonicalPreludeScopes (ctx: GeneratorContext) =
    // DEF/REF COMPLETENESS for the home table: every SharedLiteralHomes entry is a
    // NAME siblings may already reference (home paths flow into member renders), so
    // every entry MUST materialize a definition scope. A literal whose only real-pass
    // render path was short-circuited away (e.g. an alias remap — including remaps the
    // erasure rewrite collapses) is assigned a home but never prerendered: its name is
    // referenced, its def never renders (measured: 8 sibling refs to a home whose def
    // was absent even with module drops disabled). Force-prerender the missing ones.
    let store = RenderScopeStore.create ()
    for KeyValue(rt, _home) in ctx.SharedLiteralHomes do
        if not (ctx.PreludeRenders.ContainsKey rt) then
            GeneratorContext.Advisory.increment ctx "shared-home-forced-def"
            prerender ctx store (LazyContainer.CreateTypeKeyDummy rt) |> ignore
    // FIXPOINT over rounds: driving a scope forces its lazy render, which can
    // REGISTER further canonical scopes (children of forced defs above all) —
    // those land after this round's snapshot and are picked up by the next.
    // Each round is path-sorted, so emission order stays deterministic.
    let driven = HashSet<TypePath>()
    let mutable pending = true
    while pending do
        let round =
            ctx.PreludeRenders.Values
            |> Seq.choose (fun renderScope ->
                match renderScope.Root, renderScope.Render with
                | ValueSome (TypeLikePath.Anchored path), Render.Transient _ when not (driven.Contains path) ->
                    Some (path, renderScope)
                | _ -> None)
    // Sort by the canonical absolute path so emission order is DETERMINISTIC: `PreludeRenders` is a
    // ResolvedType-keyed dictionary whose iteration order is hash-dependent (and the shared-literal
    // counting pre-pass forces interner lazies that perturb it run-to-run). Anchoring these
    // canonical scopes in path order makes `collectModules` lay out the LiteralUnions/SharedLiterals
    // members identically every run.
            |> Seq.sortBy (fun (path, _) -> TypePath.flatten path |> List.map Name.Case.valueOrModified)
            |> Seq.toList
        pending <- not round.IsEmpty
        for path, renderScope in round do
            driven.Add path |> ignore
            anchorPreludeAnchorScope ctx None (AnchorPath.create path) renderScope
    // ── SECOND ROUND: HOME-CHILD DEF/REF CLOSURE (the ledgered "around"; history in
    // partition-gate.baseline 2026-07-04 (6)/(7)) ────────────────────────────────────
    // With the fixpoint complete, the materialized def-set is FINAL: every canonical
    // scope's root and every child anchor it holds. Arm the home-child scrub (read by
    // `anchorScrubLocalise` on every subsequent anchoring) and re-anchor every
    // canonical scope: registration is last-wins by design and the transient renders
    // are already-forced pure values, so the overwrite is deterministic — identical
    // output except that refs under a canonical home with NO materialized def (the
    // multi-member shared-rt class: per-site leaf stamping vs the one-slot rt-keyed
    // store) degrade to `obj`, counted per home in the advisory ledger.
    let flattenTypePath (p: TypePath) =
        TypePath.flatten p |> List.map Name.Case.valueOrModified |> String.concat "."
    for home in ctx.SharedLiteralHomes.Values do
        ctx.SyntheticHomePaths.Add(flattenTypePath home) |> ignore
    for KeyValue(_, registered) in ctx.AnchorRenders do
        match registered with
        | Choice2Of2 scope ->
            (match scope.Root with
             | Choice1Of2 typePath -> ctx.SyntheticHomeChildDefs.Add(flattenTypePath typePath) |> ignore
             | Choice2Of2 _ -> ())
            for typePath, _ in scope.Anchors.Values do
                ctx.SyntheticHomeChildDefs.Add(flattenTypePath typePath) |> ignore
        | Choice1Of2 _ -> ()
    // GENERALIZED PARENTS: every DEFINED TYPE path is a potential type-as-module
    // parent — the nested-under-type dangle class is not specific to canonical
    // shared-literal homes (measured at the first real Workers typecheck:
    // `Fetcher.Fetch` / `Get.Fetch.Input` / `Cache.*` refs under CONCRETE types
    // whose children never materialized — the same per-owner nested-def convention).
    // A ref nested under a defined type either matches a materialized def (the
    // def-set below) or can never resolve.
    ctx.SyntheticHomePaths.UnionWith ctx.SyntheticHomeChildDefs
    if ctx.SyntheticHomePaths.Count > 0 then
        ctx.PreludeRenders.Values
        |> Seq.choose (fun renderScope ->
            match renderScope.Root, renderScope.Render with
            | ValueSome (TypeLikePath.Anchored path), Render.Transient _ -> Some (path, renderScope)
            | _ -> None)
        |> Seq.sortBy (fun (path, _) -> TypePath.flatten path |> List.map Name.Case.valueOrModified)
        |> Seq.toList
        |> List.iter (fun (path, renderScope) ->
            anchorPreludeAnchorScope ctx None (AnchorPath.create path) renderScope)

module ArenaInterner =
    let processExports (ctx: GeneratorContext) (interner: ArenaInterner) =
        // Make the surface's own top-level globals visible to the source-ignore gate
        // (registerAnchorFromExport) so a `typescript`-sourced top-level export emits its
        // full definition at the global root rather than being dropped to a bare ref.
        ctx.TopLevelExports.UnionWith interner.TopLevelExports
        interner.ExportMap
        |> Map.iter (fun _ -> registerExportsForAnchoring ctx)
        emitCanonicalPreludeScopes ctx
        // SECOND EXPORT PASS, scrub-armed: `emitCanonicalPreludeScopes` armed the
        // def/ref-closure scrub (SyntheticHomePaths/SyntheticHomeChildDefs) and
        // re-drove the CANONICAL scopes — but CONCRETE export scopes were anchored
        // in the first pass, before the def-set existed. Re-anchor them so their
        // member/heritage refs see it too (nested-under-type dangles degrade to
        // obj, ledgered). Registration is last-wins and the transient renders are
        // already-forced pure values, so this is a deterministic overwrite; on the
        // COUNTING context the collectors are sets, so double-visiting is a no-op.
        interner.ExportMap
        |> Map.iter (fun _ -> registerExportsForAnchoring ctx)

    /// Phase 1: drive the full anchoring once on a throwaway context whose `SharedLiteralVisits`
    /// collector records every literal def-home VISIT, then mark the genuinely-shared literals on
    /// the real context. Must run BEFORE the real `processExports`/prerender so `prerender` roots
    /// shared literals at their canonical home on the real pass. The throwaway context is fully
    /// independent (its own prelude/anchor caches) so the count never pollutes the real output.
    let markSharedLiteralsFromExports (ctx: GeneratorContext) (interner: ArenaInterner) =
        let countingCtx =
            { GeneratorContext.Create(ctx.PreludeGetTypeRef, ctx.Customisation) with
                SharedLiteralVisits = ValueSome (Dictionary<ResolvedType, HashSet<TypePath>>())
                SharedLiteralRefOwners = ValueSome (Dictionary<ResolvedType, HashSet<AnchorPath>>()) }
        RenderScope_Prelude.ArenaInterner.prerenderTypeAliases countingCtx interner
        processExports countingCtx interner
        markSharedLiterals countingCtx ctx