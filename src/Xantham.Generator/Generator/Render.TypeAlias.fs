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
        let resolveInnerRef () =
            let oldRef = ctx.PreludeGetTypeRef ctx scopeStore innerType
            match ctx.PreludeRenders.TryGetValue(innerType.Value) with
            | true, newRef ->
                match ctx.TypeAliasRemap.TryGetValue(innerType.Value) with
                | true, value ->
                    let stripped = { oldRef with Nullable = false }
                    TypeRefRender.replace value newRef.TypeRef stripped
                    |> TypeRefRender.orNullable oldRef.Nullable
                | _ -> newRef.TypeRef
            | false, _ -> oldRef
        // ── Single-call-signature function-abbreviation guard ──────────────────────────────
        // A function-type abbreviation has NO companion module. A nested anonymous structural type in
        // the body (an Intersection / real object TypeLiteral / TemplateLiteral) prerenders to a
        // NAMELESS `TransientTypePath.Anchored` transient; with no companion module that nameless root
        // anchors onto — and collapses to — the alias's OWN name, emitting a self-cyclic abbreviation
        // (`type X = Request<C, X>`, X used at 0 of its declared args → FS0033). Detect this by
        // inspecting the ACTUAL prerendered molecule for a nameless `Anchored` transient atom: it is
        // remap-aware (a nested type aliased to a NAME via TypeAliasRemap renders as a ConcretePath,
        // never a nameless transient), so it neither misses a real collapse nor over-excludes a clean
        // handler whose args resolve through named aliases (`Without<..>`, `EventContext<..>`).
        let moleculeCollapsesOntoAlias (typeRef: Prelude.TypeRefRender) =
            let rec walkAtom (atom: Prelude.TypeRefAtom) =
                match atom with
                | Prelude.TypeRefAtom.TransientPath TransientTypePath.Anchored -> true
                | _ -> false
            and walk (tr: Prelude.TypeRefRender) =
                match tr.Kind with
                | Prelude.TypeRefKind.Atom atom -> walkAtom atom
                | Prelude.TypeRefKind.Molecule molecule ->
                    match molecule with
                    | Prelude.TypeRefMolecule.Tuple parts
                    | Prelude.TypeRefMolecule.Union parts -> parts |> List.exists walk
                    | Prelude.TypeRefMolecule.Function (parameters, returnType) ->
                        (parameters |> List.exists walk) || walk returnType
                    | Prelude.TypeRefMolecule.Prefix (prefix, args) ->
                        walk prefix || (args |> List.exists walk)
            walk typeRef
        // ── Anonymous-literal alias-body placement (FS0953 fix) ────────────────────────────
        // An anonymous object literal (`{ ... }`) that is the body — or a union/array member of
        // the body — of a type alias prerenders to a NAMELESS `TransientTypePath.Anchored` root.
        // When that ref is anchored against the ALIAS's OWN path (the alias is the owner), the
        // nameless root collapses onto the alias name, so e.g. `type X = { ... } | { ... }` emits
        // the ILLEGAL self-cyclic abbreviation `type X = U2<X, X>` and `type X = Array<{...}>`
        // emits `type X = ResizeArray<X>` (FS0953) even though the IR is NOT cyclic. Give each
        // such literal a distinct positional `<label>` identity rooted DIRECTLY under the alias's
        // companion module: register it in the alias's OWN scope TypeStore (so
        // `anchorPreludeExportScope` emits a nested `X.<label>` def at depth-1) AND reference it
        // by that same `<label>` path (emission/reference symmetry).
        let memberHasTransientDef (memberType: ResolvedType) =
            // A literal that ACTUALLY prerenders to a hoisted transient def (Root = Transient) is
            // both collapse-prone AND emittable as a nested def. A `TypeLiteral` that is purely a
            // call signature renders INLINE as a function molecule (Root = ValueNone) — it has no
            // def to emit, so naming it would dangle.
            match GeneratorContext.Prelude.tryGet ctx memberType with
            | ValueSome { Root = ValueSome (TypeLikePath.Transient _) } -> true
            | _ -> false
        // Returns (ref, isCaseNamed). For a TypeLiteral member with a real transient def, force the
        // alias scope's TypeStore entry to `<alias>.<label>` (a plain `TryAdd` would not overwrite a
        // nameless `Anchored` entry left by an earlier visit/cache-hit, leaving the def to collapse)
        // and build the molecule atom from that SAME path. Anything else keeps its normal ref so it
        // never dangles. ONLY a plain object literal qualifies: a template literal's def is
        // hard-named `TemplateLiteral`, an intersection nets new member-merge errors when relocated,
        // and a tuple/array/named-ref carries its own ref shape (never an `Anchored` root).
        let caseLiteralRef (label: string) (memberOther: ResolvedTypeOther) =
            let memberType = memberOther.AsResolvedType
            let lazyMember = LazyContainer.CreateFromValue memberType
            match memberOther with
            // A `TemplateLiteral` member collapses onto the alias name in exactly the same way as
            // an object literal: its prerender roots at a nameless `TransientTypePath.Anchored`, so
            // anchoring it against the alias yields the cyclic self-ref (`U15<X, X, ...>`, FS0953).
            // Give it the same positional `Case{i}` identity. Its def now renders as a path-derived
            // abbreviation `type Case{i} = string` (see TemplateLiteral.render in Render.Transient),
            // so the grafted `Case{i}` reference and def name match — no dangle.
            | ResolvedTypeOther.TypeLiteral _
            | ResolvedTypeOther.TemplateLiteral _ ->
                let caseScope = RenderScopeStore.appendStringToPathContext scopeStore label
                let prerendered = ctx.PreludeGetTypeRef ctx caseScope lazyMember
                if memberHasTransientDef memberType then
                    let casePath =
                        Name.Pascal.create label
                        |> fun n ->
                            TransientPath.toTransientModulePath scopeStore.PathContext
                            |> TransientTypePath.createOnTransientModuleWithName n
                    scopeStore.TypeStore[memberType] <- casePath
                    let caseRef =
                        casePath
                        |> RenderScopeStore.TypeRefAtom.Unsafe.createTransientPath
                        |> RenderScopeStore.TypeRef.Unsafe.createAtom
                        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                    caseRef, true
                else
                    prerendered, false
            | _ ->
                ctx.PreludeGetTypeRef ctx scopeStore lazyMember, false
        let rec matchImpl = function
            | ResolvedType.TypeQuery { Type = Resolve typ } ->
                matchImpl typ
            // `type X = Array<{ ... }>` whose element is an anonymous object literal collapses to
            // `ResizeArray<X>` (FS0953) for the same reason a union member does. Give the element a
            // single `Element` nested identity so it renders `ResizeArray<X.Element>` with a real
            // `X.Element` def. Falls through to the plain alias-ref when the element is not a
            // collapse-prone object literal (caseLiteralRef returns isCaseNamed = false).
            | ResolvedType.Array elementType when
                (match ResolvedTypeCategories.create elementType with
                 | { Others = [ ResolvedTypeOther.TypeLiteral _ as other ] } -> snd (caseLiteralRef "Element" other)
                 | _ -> false) ->
                let elementRef =
                    match ResolvedTypeCategories.create elementType with
                    | { Others = [ other ] } -> caseLiteralRef "Element" other |> fst
                    | _ -> ctx.PreludeGetTypeRef ctx scopeStore (LazyContainer.CreateFromValue elementType)
                {
                    TypeAliasRenderRef.Documentation = documentation
                    Metadata = metadata
                    Name = name
                    TypeParameters = typeParameters
                    Type =
                        let arrayPrefix =
                            RenderScopeStore.TypeRefRender.create scopeStore innerType.Value false Intrinsic.array
                        (arrayPrefix, [ elementRef ])
                        |> RenderScopeStore.TypeRefRender.create scopeStore innerType.Value false
                }
                |> TypeAliasRender.Alias
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
            // A named alias whose body is a TypeLiteral with EXACTLY ONE call signature and NOTHING
            // else is a FUNCTION TYPE — render it as an F# function-type abbreviation
            // (`type Name<tps> = a -> b -> r`) so an F# lambda satisfies it, NOT a nominal interface
            // with an `abstract Invoke:` member (which an F# callback lambda cannot be passed into).
            // The body's prerender (the single-call-signature TypeLiteral arm in prerender) already
            // produces the function-type molecule; `resolveInnerRef ()` returns that molecule, so the
            // alias just abbreviates to it. Anything else (>=2 call sigs / any non-call-sig member /
            // an intersection) falls through to the nominal TypeDefn arm below — an F# function-type
            // abbreviation cannot carry overloads or extra members.
            //
            // GUARD — `moleculeCollapsesOntoAlias`: a body that hoists a nameless `Anchored` transient
            // (a nested Intersection / object literal / template literal) would collapse onto the
            // alias's own name as a bare abbreviation (FS0033 self-cycle; `ExportedHandlerFetchHandler`
            // is the one handler with such an embedded literal). Keep those NOMINAL — converting trades
            // a usable nominal interface for a dangling self-cyclic abbreviation. Only bodies whose
            // molecule has no nameless transient convert (the ~13 clean callback handlers).
            | ResolvedType.TypeLiteral { Members = [ Member.CallSignature [ _ ] ] }
                when not (moleculeCollapsesOntoAlias (resolveInnerRef ())) ->
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
                // Each `Other` union member gets a positional `Case{i}` identity (see
                // `caseLiteralRef`). The index is global across the WHOLE union (others come first,
                // matching the prelude member order) so `Case{i}` is stable/unique.
                let renderCaseMember idx (other: ResolvedTypeOther) =
                    caseLiteralRef $"Case{idx}" other
                // Rebuild the FULL union molecule — NOT just the `others`. A union body frequently
                // mixes the collapse-prone structural members with primitives/enums/literals (e.g.
                // `WorkflowRetentionDuration = number | \`${number} second\` | ...`). Anchoring the
                // existing alias-ref collapses the structural members onto the alias name; we instead
                // route each structural member through `caseLiteralRef` (positional `Case{i}` def)
                // while keeping primitives/enums/the literal-union member as their normal prerendered
                // refs, preserving the same member SET and ORDER the prelude emits
                // (others, primitives, enums, then the literal sub-union). Dropping any of these
                // (the prior `others`-only build) would silently change the union arity.
                let prerenderMember (resolvedType: ResolvedType) =
                    ctx.PreludeGetTypeRef ctx scopeStore (LazyContainer.CreateFromValue resolvedType)
                let renderNamedCaseAlias (categories: ResolvedTypeCategories) =
                    let others = categories.Others
                    let memberRefs =
                        [
                            yield!
                                others
                                |> List.mapi (fun idx other -> renderCaseMember idx other |> fst)
                            for primitive in categories.Primitives do
                                prerenderMember primitive.AsResolvedType
                            for enum in categories.EnumLike do
                                prerenderMember enum.AsResolvedType
                            if not (List.isEmpty categories.LiteralLike) then
                                { Union.Types =
                                    categories.LiteralLike
                                    |> List.map (_.AsResolvedType >> LazyContainer.CreateTypeKeyDummy<ResolvedType>) }
                                |> ResolvedType.Union
                                |> prerenderMember
                        ]
                    {
                        TypeAliasRenderRef.Documentation = documentation
                        Metadata = metadata
                        Name = name
                        TypeParameters = typeParameters
                        Type =
                            match memberRefs with
                            | [ single ] -> single |> TypeRefRender.orNullable categories.Nullable
                            | _ ->
                                memberRefs
                                |> RenderScopeStore.TypeRefRender.create scopeStore innerType.Value categories.Nullable
                    }
                    |> TypeAliasRender.Alias
                // True only when at least one member will actually receive a `Case{i}` def — so we
                // never divert to the named-case path (losing the existing alias-ref) unless the
                // fix genuinely applies.
                let anyCaseNameable (others: ResolvedTypeOther list) =
                    others
                    |> List.mapi renderCaseMember
                    |> List.exists snd
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
                // The alias body is a union containing 'Others' (structural) members. If ANY of them
                // is an anonymous object literal with a real hoisted def it would collapse to the
                // alias name when anchored — emit positional `Case{i}` nested defs instead of the
                // cyclic ref. Other shapes keep the existing alias-ref behaviour.
                | { Others = others } as categories when anyCaseNameable others ->
                    renderNamedCaseAlias categories
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
                }
                |> TypeAliasRender.TypeDefn
        matchImpl innerType.Value
