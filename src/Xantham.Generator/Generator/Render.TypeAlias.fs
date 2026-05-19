[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeAlias

open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.NamePath

module TypeAlias =
    let render (ctx: GeneratorContext) scopeStore (typ: TypeAlias) =
        let innerType = typ.Type
        let typeParameters =
            typ.TypeParameters
            |> List.map (_.Value >> TypeParameter.render ctx scopeStore)
        let documentation = typ.Documentation
        let name = typ.Name
        let path = Path.Interceptors.pipeTypeAlias ctx typ
        let metadata =
            (Path.create path, typ)
            ||> RenderMetadata.createWithPathFromExport
        // Walk the resolved-type graph (NOT the rendered TypeRefRender —
        // typar refs there are `Widget` atoms built via `Ast.LongIdent`
        // that can't be cheaply inspected) collecting every
        // `ResolvedType.TypeParameter` reachable from the body. Same
        // shape as Phase B's `collectFreeTypars` for synthetics. Walk
        // transparently through Interface/Class/TypeReference targets
        // because in alias bodies the typar references can be threaded
        // through any nested generic application.
        let collectBodyTypars (innerLazyResolved: LazyResolvedType) =
            let seenTp = System.Collections.Generic.HashSet<TypeParameter>(HashIdentity.Reference)
            let seenRt = System.Collections.Generic.HashSet<ResolvedType>(HashIdentity.Reference)
            let acc = System.Collections.Generic.List<TypeParameter>()
            let rec walkT (rt: ResolvedType) =
                if seenRt.Add rt then
                    match rt with
                    | ResolvedType.TypeParameter tp ->
                        if seenTp.Add tp then acc.Add tp
                    | ResolvedType.Union u ->
                        for t in u.Types do walkT t.Value
                    | ResolvedType.Intersection i ->
                        for t in i.Types do walkT t.Value
                    | ResolvedType.TypeLiteral tl ->
                        for m in tl.Members do walkM m
                    | ResolvedType.TemplateLiteral t ->
                        for t in t.Types do walkT t.Value
                    | ResolvedType.Interface iface ->
                        for m in iface.Members do walkM m
                    | ResolvedType.Class cls ->
                        for m in cls.Members do walkM m
                    | ResolvedType.TypeReference tr ->
                        // Walk the target body too — synthetic literals
                        // referenced via TypeReference with empty args at
                        // the decoder level carry their typar references
                        // inside their bodies. Must match the walker in
                        // `prerenderTypeAliases` so the declaration's
                        // hoisted typar list matches `ctx.TypeAliasArity`.
                        walkT tr.Type.Value
                        for arg in tr.TypeArguments do walkT arg.Value
                        match tr.ResolvedType with
                        | Some r -> walkT r.Value
                        | None -> ()
                    | ResolvedType.Array inner -> walkT inner
                    | ResolvedType.ReadOnly inner -> walkT inner
                    | ResolvedType.Tuple t ->
                        for e in t.Types do walkT e.Type.Value
                    | ResolvedType.IndexedAccess ia ->
                        walkT ia.Object.Value
                        walkT ia.Index.Value
                    | ResolvedType.Index ix -> walkT ix.Type.Value
                    | ResolvedType.Optional tr ->
                        walkT (ResolvedType.TypeReference tr)
                    | ResolvedType.TypeQuery tq -> walkT tq.Type.Value
                    | ResolvedType.Conditional c ->
                        walkT c.Check.Value
                        walkT c.Extends.Value
                        walkT c.True.Value
                        walkT c.False.Value
                    | ResolvedType.Substitution s ->
                        walkT s.Base.Value
                    | _ -> ()
            and walkM (m: Member) =
                match m with
                | Member.Property p -> walkT p.Type.Value
                | Member.Method overloads ->
                    for mt in overloads do
                        for param in mt.Parameters do walkT param.Type.Value
                        walkT mt.Type.Value
                | Member.GetAccessor g -> walkT g.Type.Value
                | Member.SetAccessor s -> walkT s.ArgumentType.Value
                | Member.IndexSignature ix ->
                    for param in ix.Parameters do walkT param.Type.Value
                    walkT ix.Type.Value
                | Member.CallSignature sigs ->
                    for sig' in sigs do
                        for param in sig'.Parameters do walkT param.Type.Value
                        walkT sig'.Type.Value
                | Member.ConstructSignature sigs ->
                    for sig' in sigs do
                        for param in sig'.Parameters do walkT param.Type.Value
                        walkT sig'.Type.Value
            walkT innerLazyResolved.Value
            List.ofSeq acc
        // Reconcile the declaration's typar list against what the body
        // actually uses. Drops declared-but-unused (FS0035 prevention,
        // original `pruneUnusedTypars` behavior) AND adds used-but-not-declared
        // typars as synthetic entries (FS0039 prevention for aliases
        // whose body references free typars from another scope, e.g.
        // `type MCPTransportOptions = U3<_Lit3<'T, 'S, ...>, ...>`).
        //
        // When the body collapsed to bare `obj`/`exn` (cycle-broken), drop
        // ALL typars — `type X = obj` with no declared typars matches the
        // use-site arity (Phase C `CycleBrokenPaths` truncates use-site
        // args to 0 for cycle-broken aliases). Keeping typars on the decl
        // with `type X<'T,'U> = obj` triggers FS0035 (unused typars) at
        // best and FS0033 at use sites at worst.
        let reconcileTyparList (resolvedRef: TypeRefRender) =
            let isErased =
                match resolvedRef.Kind with
                | TypeRefKind.Atom (TypeRefAtom.Intrinsic s) ->
                    s = Intrinsic.obj || s = Intrinsic.exn
                | _ -> false
            if isErased then [] else
            let bodyTypars = collectBodyTypars innerType
            let bodyNames =
                bodyTypars
                |> List.map (fun tp -> Name.Case.valueOrModified tp.Name)
                |> Set.ofList
            let kept =
                typeParameters
                |> List.filter (fun tp ->
                    Set.contains (Name.Case.valueOrModified tp.Name) bodyNames)
            let declaredNames =
                typeParameters
                |> List.map (fun tp -> Name.Case.valueOrModified tp.Name)
                |> Set.ofList
            let extra =
                bodyTypars
                |> List.filter (fun tp ->
                    not (Set.contains (Name.Case.valueOrModified tp.Name) declaredNames))
                |> List.distinctBy (fun tp -> Name.Case.valueOrModified tp.Name)
                |> List.map (fun tp ->
                    {
                        Prelude.TypeParameterRender.Name = tp.Name
                        Metadata = {
                            Path = Path.create TransientTypePath.Anchored
                            Original = Path.create TransientTypePath.Anchored
                            Source = ValueNone
                            FullyQualifiedName = ValueNone
                        }
                        Constraint = ValueNone
                        Default = ValueNone
                        Documentation = []
                    })
            kept @ extra
        // Backwards-compat alias.
        let pruneUnusedTypars = reconcileTyparList
        // Replace any self-reference to the alias's own target ref inside
        // the resolved molecule with `obj`. The CreateFromValue wrapping
        // used for Union elements zeroes the LazyContainer's Raw, which
        // can collide with a real alias TypeKey and produce
        // `type X = U15<X, X, ..., X>` (FS0953 cyclic abbreviation).
        // The documented cycle-break via RenderingAliasTargetRefs only
        // fires through prerender's `remap`; this catches cases that
        // survive past remap and reach the resolved molecule.
        let breakSelfReference (ref: TypeRefRender) =
            // The alias's own body is being resolved. Any atom that
            // re-references the alias being rendered would produce
            // `type X = ...X...` which F# rejects as a cyclic abbreviation
            // (FS0953). Rewrite those atoms to `obj` to break the cycle.
            // Self-reference can take three atom shapes depending on how
            // the encoder/prerender pipeline resolved the inner reference:
            //   1. `TransientPath Anchored` — anchors to the alias's
            //      call-site (the alias itself) at render time.
            //   2. `TransientPath (AnchoredAndMoored n)` where `n` is the
            //      alias's own name — resolves to the alias's name within
            //      its own parent module (e.g. `option<LiteralPart>` in
            //      a `LiteralPart` body, where `option` is just the
            //      Nullable flag and the underlying atom is the
            //      self-reference).
            //   3. `ConcretePath p` where `p = path` — direct
            //      self-reference via the alias's own assigned TypePath
            //      (e.g. `option<U5<..., JSONValue, ...>>` in a
            //      `JSONValue` body).
            // Complements the documented `RenderingAliasTargetRefs`
            // cycle-break by handling cases where the cycle survives past
            // prerender's `remap` and reaches the resolved render.
            //
            // Both top-level atoms and atoms nested inside molecules
            // need rewriting: a body resolved as `Optional(Self)` is a
            // top-level Atom with Nullable=true; a body resolved as
            // `option<U5<...,Self,...>>` is a Molecule whose Prefix args
            // contain the self-reference atom.
            // Self-application like `Self<TParA, TParB>` resolves as
            // `Prefix(SelfRef, args)`. Replacing only the prefix atom
            // produces invalid `obj<TParA, TParB>` (FS0033). When the
            // rewrite turns an atom into the obj placeholder, also drop
            // the surrounding generic-application args so the entire
            // self-application collapses to plain `obj`.
            let objAtom =
                RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic Intrinsic.obj
            let rewriteAtom (atom: TypeRefAtom) =
                match atom with
                | TypeRefAtom.TransientPath TransientTypePath.Anchored -> objAtom
                | TypeRefAtom.TransientPath (TransientTypePath.AnchoredAndMoored n)
                    when n = path.Name -> objAtom
                | TypeRefAtom.ConcretePath p when p = path -> objAtom
                | _ -> atom
            let isObjAtom (atom: TypeRefAtom) =
                match atom with
                | TypeRefAtom.Intrinsic s -> s = Intrinsic.obj
                | _ -> false
            ref |> TypeRefRender.mapAtomsWithPrefixCollapse rewriteAtom isObjAtom
        let resolveInnerRef () =
            let oldRef = ctx.PreludeGetTypeRef ctx scopeStore innerType
            match ctx.PreludeRenders.TryGetValue(innerType.Value) with
            | true, newRef ->
                match ctx.TypeAliasRemap.TryGetValue(innerType.Raw) with
                | true, value ->
                    let stripped = { oldRef with Nullable = false }
                    TypeRefRender.replace value newRef.TypeRef stripped
                    |> TypeRefRender.orNullable oldRef.Nullable
                    |> breakSelfReference
                | _ -> newRef.TypeRef |> breakSelfReference
            | false, _ -> oldRef |> breakSelfReference
        // Mark the alias as cycle-broken if its body resolved to a bare
        // `obj`/`exn` intrinsic (either via `breakSelfReference` rewriting
        // self-references, or via a lib substitution that collapsed an
        // alias body to `obj`). Reference sites that would otherwise apply
        // type-arguments, declare constraints, or take this alias as
        // heritage consult `ctx.CycleBrokenPaths` and drop the
        // application/constraint/heritage. Without this metadata, F#
        // rejects the rendered `Alias<X>`, `'T :> Alias`, or
        // `inherit Alias()` because the alias erased to a non-generic /
        // sealed intrinsic.
        let markCycleBrokenIfErased (body: TypeRefRender) =
            match body.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic s)
                when s = Intrinsic.obj || s = Intrinsic.exn ->
                ctx.CycleBrokenPaths.Add path |> ignore
            | _ -> ()
        let rec matchImpl = function
            | ResolvedType.TypeQuery { Type = Resolve typ } ->
                matchImpl typ
            | ResolvedType.Interface _
            | ResolvedType.Class _
            | ResolvedType.Primitive _
            | ResolvedType.IndexedAccess _
            | ResolvedType.Index _
            | ResolvedType.Tuple _
            | ResolvedType.TypeParameter _
            | ResolvedType.TypeReference _
            | ResolvedType.Predicate _
            | ResolvedType.Substitution _
            | ResolvedType.Optional _
            | ResolvedType.GlobalThis
            | ResolvedType.Conditional _
            | ResolvedType.Enum _
            | ResolvedType.Array _ ->
                let body = resolveInnerRef ()
                markCycleBrokenIfErased body
                {
                    TypeAliasRenderRef.Documentation = documentation
                    Metadata = metadata
                    Name = name
                    TypeParameters = pruneUnusedTypars body
                    Type = body
                }
                |> TypeAliasRender.Alias
            | ResolvedType.Intersection _
            | ResolvedType.TypeLiteral _ ->
                let members, functions =
                    Member.collectAllRecursively innerType.Value
                    |> Member.partitionRender ctx scopeStore
                // Hoist free typars from the body onto the declaration —
                // alias bodies that are TypeLiterals/Intersections can
                // reference typars from elsewhere (synthetic captures,
                // referenced Promise<T>, etc.) just like Union/Alias-shaped
                // bodies. Without hoisting, the declaration is non-generic
                // but use sites apply the hoisted-count args from
                // `ctx.TypeAliasArity` (FS0033 arity mismatch).
                //
                // Skip hoisting when the alias is in `CycleBrokenPaths`
                // (its rendered body collapsed to obj/exn) — use sites
                // drop args via Phase C, so the decl shouldn't keep typars
                // either.
                let isCycleBroken = ctx.CycleBrokenPaths.Contains path
                let bodyTypars =
                    if isCycleBroken then [] else collectBodyTypars innerType
                let declaredNames =
                    typeParameters
                    |> List.map (fun tp -> Name.Case.valueOrModified tp.Name)
                    |> Set.ofList
                let extraTypars =
                    bodyTypars
                    |> List.filter (fun tp ->
                        not (Set.contains (Name.Case.valueOrModified tp.Name) declaredNames))
                    |> List.distinctBy (fun tp -> Name.Case.valueOrModified tp.Name)
                    |> List.map (fun tp ->
                        {
                            Prelude.TypeParameterRender.Name = tp.Name
                            Metadata = {
                                Path = Path.create TransientTypePath.Anchored
                                Original = Path.create TransientTypePath.Anchored
                                Source = ValueNone
                                FullyQualifiedName = ValueNone
                            }
                            Constraint = ValueNone
                            Default = ValueNone
                            Documentation = []
                        })
                {
                    TypeLikeRender.Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters @ extraTypars
                    Members = members
                    Functions = functions
                    Inheritance = []
                    Implements = []
                    Constructors = []
                    Documentation = documentation
                    IsClass = false
                }
                |> TypeAliasRender.TypeDefn
            | ResolvedType.Union _ ->
                let body = resolveInnerRef ()
                markCycleBrokenIfErased body
                let typeRefRender =
                    {
                        TypeAliasRenderRef.Documentation = documentation
                        Metadata = metadata
                        Name = name
                        TypeParameters = pruneUnusedTypars body
                        Type = body
                    }
                    |> TypeAliasRender.Alias
                match ResolvedTypeCategories.create innerType.Value with
                // no literals, and no 'others' that require a transient type
                | { LiteralLike = literals; Others = []; Nullable = nullable; Primitives = []; EnumLike = [] } ->
                    match Union.renderLiterals ctx scopeStore literals with
                    | TypeRender.EnumUnion enumRender ->
                        {
                            LiteralUnionRender.Metadata = metadata
                            Name = name
                            Cases =
                                enumRender.Cases
                                |> List.map (fun case ->
                                    {
                                        LiteralCaseRender.Metadata = case.Metadata
                                        Name = case.Name.Value
                                        Value = case.Value
                                        Documentation = case.Documentation
                                    })
                            Documentation = documentation
                        }
                        |> TypeAliasRender.EnumUnion
                    | TypeRender.StringUnion literalRender ->
                        {
                            LiteralUnionRender.Metadata = metadata
                            Name = name
                            Cases =
                                literalRender.Cases
                                |> List.map (fun case ->
                                    {
                                        LiteralCaseRender.Metadata = case.Metadata
                                        Name = case.Name.Value
                                        Value = case.Value
                                        Documentation = case.Documentation
                                    })
                            Documentation = documentation
                        }
                        |> TypeAliasRender.StringUnion
                    | _ -> typeRefRender
                | _ -> typeRefRender
            | ResolvedType.Literal tsLiteral ->
                {
                    LiteralUnionRender.Metadata = metadata
                    Name = name
                    Cases = [
                        {
                            LiteralCaseRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                           Source = ValueNone; FullyQualifiedName = ValueNone }
                            Name = name
                            Value = tsLiteral
                            Documentation = documentation
                        }
                    ]
                    Documentation = documentation
                }
                |> TypeAliasRender.StringUnion
                
            | ResolvedType.ReadOnly resolvedType ->
                let members,functions =
                    Member.collectAllRecursively resolvedType
                    |> List.map Member.setReadOnly
                    |> Member.partitionRender ctx scopeStore
                {
                    TypeLikeRender.Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Members = members
                    Functions = functions
                    Inheritance = []
                    Implements = []
                    Constructors = []
                    Documentation = documentation
                    IsClass = false
                }
                |> TypeAliasRender.TypeDefn
            | ResolvedType.EnumCase enumCase ->
                {
                    LiteralUnionRender.Metadata = metadata
                    Name = name
                    Cases = [
                        {
                            LiteralCaseRender.Metadata = {
                                Path = Path.create TransientMemberPath.Anchored
                                Original = Path.create TransientMemberPath.Anchored
                                Source = ValueSome enumCase.Parent.Value.Source
                                FullyQualifiedName = ValueSome enumCase.FullyQualifiedName
                            }
                            Name = enumCase.Name
                            Value = enumCase.Value
                            Documentation = enumCase.Documentation
                        }
                    ]
                    Documentation = documentation
                }
                |> TypeAliasRender.StringUnion
            | ResolvedType.TemplateLiteral templateLiteral ->
                {
                    TypeLikeRender.Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Members = [
                        {
                            TypedNameRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                         Source = ValueNone; FullyQualifiedName = ValueNone }
                            Name = Name.create "Value" |> Case.addCamelMeasure
                            Type =
                                RenderScopeStore.TypeRefRender.create
                                    scopeStore
                                    (ResolvedType.Primitive TypeKindPrimitive.String)
                                    false
                                    Intrinsic.string 
                            Traits = Set [ RenderTraits.EmitSelf ]
                            TypeParameters = []
                            Documentation = []
                        }
                    ]
                    Functions = [
                        {
                            FunctionLikeRender.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                            Source = ValueNone; FullyQualifiedName = ValueNone }
                            Name = Name.create "Create" |> Case.addCamelMeasure
                            Signatures = [
                                {
                                    FunctionLikeSignature.Metadata = { Path = Path.create TransientMemberPath.Anchored; Original = Path.create TransientMemberPath.Anchored
                                                                       Source = ValueNone; FullyQualifiedName = ValueNone }
                                    Parameters =
                                        templateLiteral.Types
                                        |> List.mapi (fun i typeRef ->
                                            {
                                                TypedNameRender.Metadata = { Path = Path.create TransientParameterPath.Anchored; Original = Path.create TransientParameterPath.Anchored
                                                                             Source = ValueNone; FullyQualifiedName = ValueNone }
                                                Name = Name.Camel.create $"v{i}"
                                                Type = ctx.PreludeGetTypeRef ctx scopeStore typeRef
                                                Traits = Set.empty
                                                TypeParameters = []
                                                Documentation = []
                                            }
                                            )
                                    ReturnType =
                                        Ast.Anon(Name.Case.valueOrModified name)
                                        |> RenderScopeStore.TypeRefAtom.Unsafe.createWidget
                                        |> RenderScopeStore.TypeRef.Unsafe.createAtom
                                        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                                    Traits = Set [
                                        RenderTraits.Inline
                                        RenderTraits.StringBuilder
                                    ]
                                    TypeParameters = []
                                    Documentation = []
                                }
                            ]
                            Traits = Set [
                                RenderTraits.Inline
                                RenderTraits.StringBuilder
                            ]
                            TypeParameters = []
                            Documentation = []
                        }
                    ]
                    Inheritance = []
                    Implements = []
                    Constructors = []
                    Documentation = documentation
                    IsClass = false
                }
                |> TypeAliasRender.TypeDefn
        // Mark this alias's TypeAliasRemap target ref as in-flight so any
        // self-reference encountered during prerender below resolves to
        // `obj` instead of re-emitting the alias's own ConcretePath ref.
        // F# rejects recursive type aliases (FS0953); the only safe
        // rendering is to break the cycle. Matches Glutinum's TS-
        // preprocessing behaviour (cyclic refs replaced with `any`/`obj`).
        //
        // We track by *target ref* rather than body TypeKey because the
        // encoder produces multiple TypeKeys per conceptual alias (decl
        // and body), and intermediate wrappers like
        // `ResolvedType.Optional`'s `LazyContainer.CreateFromValue` lose
        // the original Raw. The remap value (the alias's ConcretePath
        // ref) is the stable identity all references converge on.
        let aliasTarget =
            match ctx.TypeAliasRemap.TryGetValue(innerType.Raw) with
            | true, target -> ValueSome target
            | _ -> ValueNone
        let added =
            match aliasTarget with
            | ValueSome target -> ctx.RenderingAliasTargetRefs.Add(target)
            | ValueNone -> false
        try
            matchImpl innerType.Value
        finally
            if added then
                match aliasTarget with
                | ValueSome target -> ctx.RenderingAliasTargetRefs.Remove(target) |> ignore
                | ValueNone -> ()
