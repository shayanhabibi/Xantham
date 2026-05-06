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
                {
                    TypeAliasRenderRef.Documentation = documentation
                    Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Type = resolveInnerRef ()
                }
                |> TypeAliasRender.Alias
            | ResolvedType.Intersection _ 
            | ResolvedType.TypeLiteral _ ->
                let members, functions =
                    Member.collectAllRecursively innerType.Value
                    |> Member.partitionRender ctx scopeStore
                // if members |> List.isEmpty && functions |> List.forall (_.Name >> Name.Case.valueOrSource >> (=) "Invoke") then
                    // ()
                // else
                {
                    TypeLikeRender.Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Members = members
                    Functions = functions
                    Inheritance = []
                    Constructors = []
                    Documentation = documentation
                    IsClass = false
                }
                |> TypeAliasRender.TypeDefn
            | ResolvedType.Union _ ->
                let typeRefRender =
                    {
                        TypeAliasRenderRef.Documentation = documentation
                        Metadata = metadata
                        Name = name
                        TypeParameters = typeParameters
                        Type = resolveInnerRef ()
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
                                Source = enumCase.Source |> Option.toValueOption
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
