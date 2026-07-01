[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Prelude

open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Control
open Xantham.Decoder.Types.Graph
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Types
open Xantham.Generator
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Fabulous.AST
open Xantham.Generator.NamePath

let private createConcreteTypeRef (path: TypePath) =
    RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
    |> RenderScopeStore.TypeRef.Unsafe.createAtom
    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false

// Canonical home for hoisted literal-union enums: a fixed top-level module, named from the
// union's literal VALUES (its identity), NOT from whichever owner references it. Rooting the
// enum here (absolute ConcretePath) means structurally-identical unions — which share one
// ResolvedType — resolve to ONE name and ONE emission, and every reference is the same
// re-anchor-invariant ConcretePath. Name and emission cannot diverge (the decoupling that
// made per-owner hoisting dangle). The emission itself is driven by `emitCanonicalUnions`.
let literalUnionModule = ModulePath.init "LiteralUnions"

/// A content-STABLE short hash (not `.GetHashCode`, which is per-process randomized and would
/// break regen determinism). FNV-1a over the UTF-8 bytes -> 8 lowercase hex chars. Deterministic
/// by content, so the same value-set always yields the same suffix across runs and processes.
let private stableShortHash (s: string) : string =
    let mutable h = 2166136261u
    for b in System.Text.Encoding.UTF8.GetBytes s do
        h <- (h ^^^ uint32 b) * 16777619u
    h.ToString("x8")

/// Bound an underscore-joined token string to a legible identifier. A short name (<=40 char) is
/// kept verbatim; a long one is truncated to a readable prefix (first 3 tokens, capped 28 char)
/// plus a content-stable FNV-1a hash of the FULL string for UNIQUENESS. Deterministic and
/// collision-safe (distinct full strings differ in the hash). Shared by every canonical-home name
/// path (literal-union enums AND shared object-literals) so the two cannot diverge — an unbounded
/// SharedLiterals name (335-char keyof-Array smash) was exactly such a divergence.
let boundName (full: string) : string =
    if full.Length <= 40 then full
    else
        let prefix =
            full.Split('_')
            |> Array.truncate 3
            |> String.concat "_"
            // Cap the readable prefix too (a single very long token can blow the budget).
            |> fun p -> if p.Length > 28 then p.Substring(0, 28) else p
        $"{prefix}_{stableShortHash full}"

let literalUnionName (literals: ResolvedTypeLiteralLike list) : Name<Case.pascal> =
    let token =
        function
        | ResolvedTypeLiteralLike.Literal (TsLiteral.String v) -> v
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Int v) -> $"i{v}"
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Float v) -> $"f{v}"
        | ResolvedTypeLiteralLike.Literal (TsLiteral.Bool v) -> (if v then "true" else "false")
        | ResolvedTypeLiteralLike.Literal (TsLiteral.BigInt v) -> $"n{v}"
        | ResolvedTypeLiteralLike.Literal TsLiteral.Null -> "null"
        | ResolvedTypeLiteralLike.EnumCase ec -> Name.Case.valueOrSource ec.Name
        | ResolvedTypeLiteralLike.TypeQuery tq ->
            tq.FullyQualifiedName |> List.tryLast |> Option.map (fun p -> p.Value) |> Option.defaultValue "Q"
    // The canonical name is the union's IDENTITY (the dedup key): same SORTED value-set -> same
    // name -> one emission -> all references resolve there. Concatenating EVERY member value works
    // but a many-member union (e.g. a 30-string-literal enum, or the WebSocket readyState constants)
    // produces an unbounded, unusable public name (200-335 chars). BOUND it: when the full name
    // exceeds the threshold, keep the first few tokens for READABILITY and append a content-stable
    // hash of the FULL sorted token list for UNIQUENESS — still deterministic and collision-safe
    // (two distinct value-sets differ in the hash), just bounded.
    // The canonical name is a legible identifier bounded via the shared `boundName` helper (40 keeps
    // short enums verbatim; long ones become prefix+hash). Pascal-casing roughly preserves length.
    let sortedTokens = literals |> List.map token |> List.sort
    sortedTokens |> String.concat "_" |> boundName |> Name.Pascal.create

let literalUnionTypePath (literals: ResolvedTypeLiteralLike list) : TypePath =
    literalUnionModule |> TypePath.createWithName (literalUnionName literals)

// Canonical home for hoisted object-LITERALS that are SHARED across more than one owner
// context. Mirrors `literalUnionModule`: a fixed top-level module whose member type is named
// from the literal's STRUCTURAL identity (computed by the counting pre-pass), NOT from any
// referencing owner. A shared literal is ONE `ResolvedType.TypeLiteral` (the decoder interned
// the structurally-identical TS literals into one node) reached through N>1 owners; rooting it
// here (absolute ConcretePath) makes every reference the same re-anchor-invariant ConcretePath
// and lets the single def be emitted once. The per-literal name is assigned into
// `ctx.SharedLiteralHomes` by `markSharedLiterals`; emission is driven by the same
// PreludeRenders/(Anchored+Transient) driver in `processExports` that emits LiteralUnions.
let sharedLiteralModule = ModulePath.init "SharedLiterals"

/// A readable base token for a shared object-literal's canonical name, derived from its member
/// NAMES (properties/methods/accessors), like `literalUnionName` derives from a union's values.
/// Member names alone do NOT uniquely identify a literal (two literals with identical property
/// names but different property TYPES are distinct ResolvedTypes), so `markSharedLiterals`
/// disambiguates collisions deterministically — this only supplies the human-readable stem.
let sharedLiteralNameStem (typeLiteral: TypeLiteral) : string =
    // A call/construct signature has no member NAME, so name it from its PARAMETER names instead —
    // a callback's params are its most meaningful identifier (`that/locales/options` for a
    // localeCompare overload). Without this an overloaded-call-signature literal (which is genuinely
    // nominal — F# function types cannot carry overloads) falls back to the machine name `Lit<N>`.
    let paramNames (parameters: Parameter list) =
        parameters |> List.map (fun p -> Name.Case.valueOrSource p.Name) |> String.concat "_"
    let memberName =
        function
        | Member.Property p -> ValueSome (Name.Case.valueOrSource p.Name)
        | Member.Method (m :: _) -> ValueSome (Name.Case.valueOrSource m.Name)
        | Member.GetAccessor g -> ValueSome (Name.Case.valueOrSource g.Name)
        | Member.SetAccessor s -> ValueSome (Name.Case.valueOrSource s.Name)
        | Member.CallSignature (c :: _) -> ValueSome (paramNames c.Parameters)
        | Member.ConstructSignature (c :: _) -> ValueSome (paramNames c.Parameters)
        | Member.Method []
        | Member.CallSignature []
        | Member.IndexSignature _
        | Member.ConstructSignature [] -> ValueNone
    let names =
        typeLiteral.Members
        |> List.choose (memberName >> ValueOption.toOption)
        |> List.filter (fun s -> s <> "")
        |> List.sort
    match names with
    | [] -> "Lit"
    // Bound via the same shared helper as `literalUnionName`: an unbounded member-name smash
    // produced a 335-char `SharedLiterals.``0[SymbolIterator]...With``` (keyof-Array) that broke F#.
    | _ -> names |> String.concat "_" |> boundName

/// True when a `TypeLiteral` renders as a HOISTED named object-type (the `_, _` arm of the
/// TypeLiteral prerender below) rather than inlining to `obj` (empty) or a bare function signature
/// (single inline call-signature). Only such literals are placed under a per-owner transient and
/// can therefore become owner-dependent shared-literal dangles — so only these are candidates for
/// the union-member reference-owner counting.
let isHoistableObjectLiteral (typeLiteral: TypeLiteral) : bool =
    let callSignatures, rest =
        typeLiteral.Members
        |> List.partition _.IsCallSignature
        ||> fun sigs rest ->
            sigs
            |> List.map (function
                | Member.CallSignature callSignature -> callSignature
                | _ -> failwith "Unreachable guaranteed by guard in partition")
            , rest
    // MUST mirror the TypeLiteral render arm below: a literal whose ONLY member is a single call
    // signature inlines as an F# function type (any arity, incl. spread — the param Type is already
    // the array), and an index-signature-ONLY literal inlines as an `IDictionary<K,V>` map — both are
    // therefore NOT hoisted object-literals. Lockstep keeps the SharedLiterals counting pre-pass
    // consistent with what actually gets hoisted.
    match callSignatures, rest with
    | [], [] -> false
    | [ [ _singleSig ] ], [] -> false
    | [], [ Member.IndexSignature _ ] -> false
    | _ -> true

/// During the counting pre-pass, walk a top-level owner's ResolvedType graph (rooted at `rootRt`)
/// and record — against the current owner (`ctx.CurrentRefOwner`) — every hoistable object-literal
/// it REFERENCES (in any position: direct property/array element, union member, etc). This is a
/// STRUCTURAL walk over the ResolvedType graph, deliberately independent of `prerender`'s cache: a
/// shared literal's whole reference subtree (`ResizeArray<Lit>`, `option<ResizeArray<U2<Lit,Lit>>>`)
/// is interned and shared across owners, so `prerender`/anchoring only descend into it under the
/// FIRST owner (every later owner hits the cache and never sees the literal as a def-home). That is
/// exactly why the def-VISIT gate misses these — both the union-wrapped literals AND direct
/// array-element shared literals whose subtree is interned. Walking the resolved graph per owner
/// instead captures the FULL set of distinct referencing owners; `markSharedLiterals` canonicalizes
/// any literal referenced through >1 distinct owner (a single-owner literal stays nested — the
/// control case). Cycle-safe via a per-walk visited set (the graph is cyclic). No-op outside the
/// counting pass.
let recordUnionMemberRefOwners (ctx: GeneratorContext) (rootRt: ResolvedType) =
    match ctx.SharedLiteralRefOwners with
    | ValueNone -> ()
    | ValueSome _ ->
        let visited = System.Collections.Generic.HashSet<ResolvedType>(HashIdentity.Reference)
        // A reference's STRUCTURAL (anonymous) inner head — Union/Array/Intersection/Tuple/etc — IS
        // part of THIS owner's territory and must be descended (the union frequently sits behind a
        // thin `TypeReference{Type=Union; ResolvedType=None}` wrapper). A NOMINAL head (Interface/
        // Class/Enum/another named reference) belongs to a DIFFERENT owner and must NOT be entered.
        let isStructuralHead =
            function
            | ResolvedType.Union _ | ResolvedType.Array _ | ResolvedType.Intersection _
            | ResolvedType.Tuple _ | ResolvedType.TypeLiteral _ | ResolvedType.Optional _
            | ResolvedType.ReadOnly _ -> true
            | _ -> false
        let rec walk (rt: ResolvedType) =
            if visited.Add rt then
                // Record THIS literal (if hoistable) against the current owner — a literal reached in
                // ANY position is referenced by this owner; the >1-distinct-owner gate decides sharing.
                (match rt with
                 | ResolvedType.TypeLiteral tl when isHoistableObjectLiteral tl ->
                    GeneratorContext.SharedLiterals.recordRefOwner ctx rt
                 | _ -> ())
                match rt with
                | ResolvedType.Union union ->
                    for lz in union.Types do walk lz.Value
                | ResolvedType.Array inner -> walk inner
                | ResolvedType.ReadOnly inner -> walk inner
                | ResolvedType.Optional typeRef -> walk (ResolvedType.TypeReference typeRef)
                | ResolvedType.TypeReference tr ->
                    tr.TypeArguments |> List.iter (fun a -> walk a.Value)
                    tr.ResolvedType |> Option.iter (fun r -> walk r.Value)
                    // Descend the head ONLY if it is a structural/anonymous wrapper (the literal/union
                    // often hides here); skip nominal named heads (another owner's body).
                    if isStructuralHead tr.Type.Value then walk tr.Type.Value
                | ResolvedType.Intersection intersection ->
                    intersection.Types |> List.iter (fun t -> walk t.Value)
                | ResolvedType.Tuple tuple ->
                    tuple.Types |> List.iter (fun t -> walk t.Type.Value)
                | ResolvedType.TypeLiteral typeLiteral ->
                    // Descend a literal's MEMBER types (e.g. the per-owner outer literal whose
                    // property is the shared inner literal/union) so they are reached per owner.
                    for m in typeLiteral.Members do
                        match m with
                        | Member.Property p -> walk p.Type.Value
                        | Member.GetAccessor g -> walk g.Type.Value
                        | _ -> ()
                | _ -> ()
        walk rootRt

// The numeric typed arrays + ArrayBufferView are GENERIC in recent TS (`Uint8Array<ArrayBufferLike>`)
// but NON-generic in Fable.Core.JS (their Fable name equals their TS name, so they are NOT
// name-substituted). A `TypeReference` to one carries the TS type argument, and arg-alignment uses
// the TS interface's declared arity (1), emitting `Uint8Array<X>` -> FS0033 "the non-generic type
// ... does not expect any type arguments, but here is given 1". `fableDeclaredArity` reports their
// Fable arity (0) so arg-alignment truncates the spurious arg away, emitting bare `Uint8Array`.
// (Contrast `ArrayLike`/`ConcatArray`/`ReadonlyArray`, which ARE generic in Fable and keep their arg.)
// NB: the BigInt64Array/BigUint64Array variants are deliberately EXCLUDED — they are NOT valid
// bare names in this Fable.Core.JS version, so stripping their arg dangles the name (FS0039).
// They render fine as-is at HEAD; only the listed arrays have a Fable-non-generic bare equivalent.
let private nonGenericTypedArrays =
    set [
        "Int8Array"; "Uint8Array"; "Uint8ClampedArray"
        "Int16Array"; "Uint16Array"
        "Int32Array"; "Uint32Array"
        "Float32Array"; "Float64Array"
        "ArrayBufferView"
    ]

// The iterator interfaces are 3-parameter in recent TS (`Iterator<T, TReturn, TNext>`,
// `IterableIterator<T, TReturn, TNext>`, ...) but substitute (LibEsSubstitution) to F#/Fable `seq`,
// which is SINGLE-parameter (`seq<'T>`). Applying all 3 TS args emits `seq<T, TReturn, TNext>` ->
// FS0033 "the type 'seq<_>' expects 1 type argument(s) but is given 3". Their Fable arity is 1, so
// arg-alignment keeps only the first (element) arg and truncates `TReturn`/`TNext`. (`ArrayIterator`
// is already 1-parameter in the IR, so it needs no override.)
let private singleParamIterators =
    set [ "Iterator"; "Iterable"; "IterableIterator"; "AsyncIterableIterator" ]

/// The Fable-side declared arity for a named type whose Fable equivalent's arity differs from its TS
/// declaration: 0 for the numeric typed arrays / ArrayBufferView (non-generic in Fable), 1 for the
/// iterator interfaces (3-param in TS but map to single-param `seq`). `None` => use the TS arity.
let private fableDeclaredArity (name: string) : int option =
    if nonGenericTypedArrays.Contains name then Some 0
    elif singleParamIterators.Contains name then Some 1
    else None

[<Struct>]
type private Registered = Registered of TypeRefRender

/// A reference to a generic mapped/utility TYPE ALIAS (`Without<U,T>`, `Params<P>`) in a
/// member/union/heritage position is resolved by the encoder to the alias's bare structural BODY
/// with the application's `TypeArguments` DROPPED — they are unrecoverable at the render site
/// (gone from the IR; confirmed by instrumentation: every such site reaches the remap as the bare
/// body, no `TypeReference` wrapper, no args). The remap renders the body as the bare alias NAME,
/// but the alias is emitted as `type Without<'U,'T>`, so the bare name is FS0033 ("expects N type
/// argument(s) but is given 0"). When the remap target is a bare NAME atom and the alias declares
/// `arity > 0` type parameters, pad it to `name<obj, ...>` (one `obj` per declared parameter) so
/// the reference is well-formed. `obj` is a trivial intrinsic atom — building it eagerly is
/// cycle-safe (it never re-enters `prerender`), so this preserves the remap cycle-break. A target
/// that is ALREADY a generic application (`Prefix`) carries real args (the cdc0108 arm built it)
/// and is left untouched — no double-wrap.
let private padAliasNameToArity (ctx: GeneratorContext) (key: ResolvedType) (ref: TypeRefRender) : TypeRefRender =
    // Pad ONLY a bare NAME atom (the remapped alias name — a `ConcretePath`). Any molecule —
    // a `Prefix` already carrying real args (cdc0108), or a Function/Tuple/Union that is the
    // alias's OWN structural body re-substituted by `Render.TypeAlias.resolveInnerRef` at its
    // definition site — must pass through untouched (appending `<obj,..>` to a function type or a
    // union is invalid F#).
    match ref.Kind with
    | TypeRefKind.Atom (TypeRefAtom.ConcretePath _) ->
        match GeneratorContext.Prelude.tryGetTypeAliasArity ctx key with
        | ValueSome arity when arity > 0 ->
            let objArg =
                RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic Intrinsic.obj
                |> RenderScopeStore.TypeRef.Unsafe.createAtom
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
            let nameOnly = { ref with Nullable = false }
            RenderScopeStore.TypeRefMolecule.Unsafe.createPrefix nameOnly (List.replicate arity objArg)
            |> RenderScopeStore.TypeRef.Unsafe.createMolecule
            |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind ref.Nullable
        | _ -> ref
    | _ -> ref

let rec prerender (ctx: GeneratorContext) (scope: RenderScopeStore) (lazyResolvedType: LazyResolvedType): TypeRefRender =
    let remap = function
            | { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Value) ->
                ctx.TypeAliasRemap[lazyResolvedType.Value]
                |> TypeRefRender.orNullable nullable
                |> padAliasNameToArity ctx lazyResolvedType.Value
            | ref -> ref
    let inline addOrReplaceScope ctx resolvedType renderScope =
        let renderScope = ctx.Customisation.Interceptors.ResolvedTypePrelude ctx resolvedType renderScope
        GeneratorContext.Prelude.addOrReplace ctx resolvedType renderScope
        Registered (remap renderScope.TypeRef)
    let valueIsCreated = lazyResolvedType.IsValueCreated
    let cachedRenderValue = GeneratorContext.Prelude.tryGet ctx lazyResolvedType.Value
    // a significant portion of the branching logic will not initially register the
    // type ref before proceeding.
    if valueIsCreated && cachedRenderValue.IsSome then
        let resolvedType = lazyResolvedType.Value
        match cachedRenderValue.Value with
        | { Root = ValueSome (TypeLikePath.Transient path); TypeRef = ref } ->
            scope
            |> RenderScopeStore.tryAdd resolvedType path
            remap ref
            
        | { TypeRef = ref } ->
            remap ref
    // a first visit to a type will either see the 'resolved type' as having been
    // lazily created but not yet processed (so it will not have a value in the
    // cache), or not created and not yet processed.
    // We protect against stack overflows by registering resolved types that have been
    // created on the first pass into the 'InFlight' set. This prevents infinite recursion.
    elif valueIsCreated && not(GeneratorContext.Prelude.canFlight ctx lazyResolvedType.Value) then
        eprintfn $"Stack overflow would be caused by rendering the type ref for {lazyResolvedType.Raw}"
        let (Registered ref) =
            RenderScopeStore.TypeRefRender.create scope lazyResolvedType.Value true Intrinsic.obj
            |> RenderScope.createRootless lazyResolvedType.Value
            |> addOrReplaceScope ctx lazyResolvedType.Value
        ref
    else
    let resolvedType = lazyResolvedType.Value
    let inline lift value = RenderScopeStore.TypeRefRender.create scope resolvedType false value
    let inline liftNullable value = RenderScopeStore.TypeRefRender.create scope resolvedType true value
    let inline liftWithNullable nullable value = if nullable then liftNullable value else lift value
    // Build the `Prefix (head, realArgs)` generic application from a prerendered prefix head and
    // the real type arguments. A bare reference to a GENERIC lib.es collection (`Array<'T>`,
    // `ReadonlyArray<'T>`) is substituted (in the ResolvedTypePrelude interceptor) to a Prefix
    // carrying the collection's OWN declared type params as PLACEHOLDER args
    // (`ResizeArray<'T>`) — so a bare reference renders with its element arg. When that same
    // substituted head is consumed HERE as the head of a real generic application
    // (`TypeReference{Array,[elem]}`), wrapping it again would double-apply
    // (`ResizeArray<'T><elem>`, invalid F#). Collapse the placeholder by reusing the
    // substituted head's OWN head and applying the real args instead: `ResizeArray<elem>`.
    let applyArgsToCollectionHead (prefix: TypeRefRender) (realArgs: TypeRefRender list) =
        match prefix.Kind with
        | TypeRefKind.Molecule (TypeRefMolecule.Prefix(innerHead, _placeholderArgs)) ->
            (innerHead, realArgs)
        | _ ->
            (prefix, realArgs)
    match resolvedType with
    | ResolvedType.GlobalThis ->
        lift Intrinsic.globalThis
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Conditional conditionalType ->
        if [
            conditionalType.True
            conditionalType.False
        ] |> List.contains lazyResolvedType
        then
            liftNullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        else
        // `T extends U ? X : Y` resolves to the union `X | Y`. Route the two branches through
        // the SAME Union categorization/simplify path (below) — which dedupes identical members
        // (so `X | X` collapses to `X`) and lifts nullability (so `X | null` becomes `option<X>`)
        // — rather than building a raw erased union that keeps `U2<X,X>` / `U2<X,unit>`.
        ResolvedType.Union {
            Types = [ conditionalType.True; conditionalType.False ]
        }
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        
    | ResolvedType.Interface ``interface`` ->
        let scope = RenderScopeStore.create()
        let path = Path.Interceptors.pipeInterface ctx ``interface``
        let ref = path |> createConcreteTypeRef
        {
            RenderScope.Type = resolvedType
            Root = TypeLikePath.create path |> ValueSome
            TypeRef = ref
            Render =
                lazy
                    Interface.render ctx scope ``interface``
                    |> Concrete.TypeRender.TypeDefn
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
        // |> executeRender
    | ResolvedType.Class ``class`` ->
        let scope = RenderScopeStore.create()
        let path = Path.Interceptors.pipeClass ctx ``class``
        let ref = path |> createConcreteTypeRef
        {
            RenderScope.Type = resolvedType
            Root = path |> TypeLikePath.create |> ValueSome
            TypeRef = ref
            Render =
                lazy
                    Class.render ctx scope ``class``
                    |> Concrete.TypeRender.TypeDefn
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
        // |> executeRender
    | ResolvedType.Predicate _ ->
        lift Intrinsic.bool
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Primitive typeKindPrimitive ->
        match typeKindPrimitive with
        | TypeKindPrimitive.Unknown 
        | TypeKindPrimitive.Any -> liftNullable Intrinsic.obj
        | TypeKindPrimitive.NonPrimitive 
        | TypeKindPrimitive.ESSymbol -> lift Intrinsic.obj
        | TypeKindPrimitive.Never 
        | TypeKindPrimitive.Void 
        | TypeKindPrimitive.Undefined 
        | TypeKindPrimitive.Null -> lift Intrinsic.unit
        | TypeKindPrimitive.String -> lift Intrinsic.string
        | TypeKindPrimitive.Integer -> lift Intrinsic.int
        | TypeKindPrimitive.Number -> lift Intrinsic.float
        | TypeKindPrimitive.Boolean -> lift Intrinsic.bool
        | TypeKindPrimitive.BigInt -> lift Intrinsic.bigint
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Union _ ->
        match ResolvedTypeCategories.create resolvedType with
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            liftWithNullable nullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | { Others = []; EnumLike = []; Primitives = primitives; LiteralLike = []; Nullable = nullable } ->
            primitives
            |> List.map (
                _.AsResolvedType
                >> LazyContainer.CreateFromValue
                >> prerender ctx scope
                )
            |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | { Others = [ ResolvedTypeCategories.AsResolvedType t ]; LiteralLike = []; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = [ResolvedTypeCategories.AsResolvedType t]; EnumLike = []; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = [ResolvedTypeCategories.AsResolvedType t]; Primitives = []; Nullable = nullable } 
        | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [ResolvedTypeCategories.AsResolvedType t]; Nullable = nullable } ->
            prerender ctx scope (LazyContainer.CreateFromValue t)
            |> TypeRefRender.orNullable nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        // direct type render of literals union
        | { LiteralLike = literals; Others = []; EnumLike = []; Primitives = []; Nullable = nullable } when lazyResolvedType.Raw = LazyContainer<_, _>.DummyTypeKey ->
            // Dummy-key unions are inline children of a larger union molecule — keep transient.
            let path = Name.Pascal.create "Literals" |> TransientTypePath.AnchoredAndMoored
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType nullable path
            {
                Transient.RenderScope.Type = resolvedType
                Root = TypeLikePath.create path |> ValueSome
                TypeRef = ref
                Render =
                    lazy Union.renderLiterals ctx scope literals
                    |> Render.create ref
                TransientChildren = ValueSome <| RenderScopeStore.create()
            }
            |> addOrReplaceScope ctx resolvedType
        | { LiteralLike = literals; Others = []; EnumLike = []; Primitives = []; Nullable = nullable } ->
            // Canonical hoisted enum at LiteralUnions.<identity-name>. Absolute ConcretePath ref
            // (identity-invariant) for references; Anchored root + Transient render emitted once
            // by emitCanonicalUnions (anchorPreludeAnchorScope's Anchored+Transient branch).
            let typePath = literalUnionTypePath literals
            let ref = createConcreteTypeRef typePath |> TypeRefRender.orNullable nullable
            let scope = RenderScopeStore.create()
            {
                RenderScope.Type = resolvedType
                Root = TypeLikePath.create typePath |> ValueSome
                TypeRef = ref
                Render =
                    (lazy Union.renderLiterals ctx scope literals)
                    |> Render.createFromTransientLazy ref
                TransientChildren = ValueSome scope
            }
            |> addOrReplaceScope ctx resolvedType
        | { Others = others; LiteralLike = literals; EnumLike = enumLike; Primitives = primitives; Nullable = nullable } ->
            seq {
                if not <| List.isEmpty literals then
                    { Union.Types =
                        literals
                        |> List.map (_.AsResolvedType >> LazyContainer.CreateTypeKeyDummy<ResolvedType>) }
                    |> ResolvedType.Union
                for other in others do other.AsResolvedType
                for primitive in primitives do primitive.AsResolvedType
                for enum in enumLike do enum.AsResolvedType
            }
            |> Seq.map (LazyContainer.CreateFromValue >> prerender ctx scope)
            |> Seq.toList
            |> RenderScopeStore.TypeRefRender.create scope resolvedType nullable
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Intersection intersection ->
        let path = TransientTypePath.Anchored
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        {
            Transient.RenderScope.Type = resolvedType
            Root = TypeLikePath.create path |> ValueSome
            TypeRef = ref
            Render =
                lazy Intersection.render ctx scope intersection
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Literal tsLiteral ->
        // A single-value literal type (e.g. `""`, `"0"`, `0`, `false`) is a TS compile-time
        // refinement with no distinct runtime representation in an erased binding. Render it
        // as its BASE PRIMITIVE inline (mirroring the Primitive case) rather than hoisting a
        // per-property named nested type. Hoisting named these per-property (certIssuerDN ->
        // CertIssuerDN) but deduped by ResolvedType — so N properties sharing one literal
        // value (16 share `""`) collapsed to one emitted type and the other N-1 references
        // dangled (FS0039). Erasing to the primitive removes the hoist, the dedup, and the
        // dangle entirely. Primitives/named-interface refs are unaffected (different branches).
        match tsLiteral with
        | TsLiteral.String _ -> lift Intrinsic.string
        | TsLiteral.Int _ -> lift Intrinsic.int
        | TsLiteral.Float _ -> lift Intrinsic.float
        | TsLiteral.Bool _ -> lift Intrinsic.bool
        | TsLiteral.BigInt _ -> lift Intrinsic.bigint
        | TsLiteral.Null -> lift Intrinsic.unit
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.IndexedAccess indexAccessType ->
        let suffixes =
            [
                indexAccessType.Object
                indexAccessType.Index
            ]
            |> List.map (prerender ctx scope)
        let prefix = lift Intrinsic.proptypekey
        (prefix, suffixes)
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Index index ->
        (
            lift Intrinsic.keyof,
            index.Type
            |> List.singleton
            |> List.map (prerender ctx scope)
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType; TypeArguments = (_ :: _) as typeArguments }
        when (match innerResolvedType.Value with
              // CASE 1 — the resolved body is a genuinely GENERIC NAMED head: an Interface/Class
              // declaring >=1 type parameter (e.g. a generic class instantiation). Apply the args.
              | ResolvedType.Interface i when i.TypeParameters.Length > 0 -> true
              | ResolvedType.Class c when c.TypeParameters.Length > 0 -> true
              // An already-instantiated `TypeReference<..>` carries its own args; re-applying the
              // outer args would double-wrap (`Foo<A><B>`, invalid F#). Render the body alone.
              | ResolvedType.TypeReference { TypeArguments = (_ :: _) } -> false
              // CASE 2 — a generic TYPE ALIAS reference (`Without<U,T>`, `Params<P>`,
              // `CfProperties<C>`): the encoder records the alias BODY (a TypeLiteral/Union/
              // Intersection/...) as `ResolvedType` and the alias-application args as
              // `TypeArguments`. The alias body is a key in `ctx.TypeAliasRemap`, so rendering it
              // resolves to the alias NAME (not the inlined structure). Apply the args so the
              // reference emits `Without<A,B>` instead of bare `Without` (the FS0033 arg-drop).
              // An INLINE structural type (anonymous union/object with a phantom arg) is NOT in
              // the remap, so it correctly falls through and renders alone (no double-wrap).
              | bodyValue -> ctx.TypeAliasRemap.ContainsKey bodyValue) ->
        // The encoder attached BOTH a resolved instantiation body (`ResolvedType = Some`) AND
        // the original `TypeArguments`. The resolved body alone is the correct NAMED head
        // (e.g. `Without`, `Params`, `CfProperties`) — rendering it is cycle-safe because it
        // short-circuits to the body's own named scope. But rendering it ALONE drops the args,
        // emitting a bare generic name -> FS0033 "expects N args but is given 0".
        //
        // The fix builds the `Prefix (head, args)` application using that resolved body as the
        // head, mirroring the args-carrying arm below — but with one essential difference for
        // CYCLE SAFETY: this node's RenderScope is REGISTERED in PreludeRenders (carrying the
        // head ref) BEFORE the args are prerendered. A self-referential mapped-type
        // instantiation (e.g. `Without<U,T>` whose arg transitively references this same node)
        // re-enters `prerender` for this key while prerendering the args; with the scope already
        // registered, the re-entry hits the cache-hit arm (lines 67-76) and returns the head ref
        // instead of recursing forever. (Prior attempts that eagerly prerendered the args BEFORE
        // registration stack-overflowed on exactly this cycle.)
        let prefix =
            innerResolvedType
            |> prerender ctx scope
        // Register the scope NOW (head-only ref) so a cyclic arg re-entering this key resolves
        // to the head via the cache rather than recursing. The full Prefix scope replaces this
        // below once the args are safely prerendered.
        prefix
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> ignore
        // Align args to the resolved body's declared arity (truncate excess / pad with obj),
        // matching the args-carrying arm's well-formedness contract. A stdlib type whose Fable
        // equivalent is non-generic (the numeric typed arrays / ArrayBufferView) reports a Fable
        // arity override of 0 so its spurious TS type argument is truncated away (FS0033 fix).
        let declaredParamCount =
            match innerResolvedType.Value with
            | ResolvedType.Interface i ->
                fableDeclaredArity (Name.Case.valueOrSource i.Name)
                |> Option.defaultValue i.TypeParameters.Length
            | ResolvedType.Class c ->
                fableDeclaredArity (Name.Case.valueOrSource c.Name)
                |> Option.defaultValue c.TypeParameters.Length
            // CASE 2 — the resolved body is a generic TYPE ALIAS body (a TypeLiteral/Union/...
            // registered in TypeAliasRemap). Its declared arity lives on the ALIAS, not the body
            // structure, so honour the recorded `TypeAliasArity`. Without this, a `Without<'P>`
            // reference carrying only 1 of the alias's 2 declared parameters would pass through
            // unpadded (declaredParamCount=typeArguments.Length) and emit FS0033 "given 1".
            | bodyValue ->
                GeneratorContext.Prelude.tryGetTypeAliasArity ctx bodyValue
                |> ValueOption.defaultValue typeArguments.Length
        let alignedArguments =
            if typeArguments.Length = declaredParamCount then
                typeArguments
            elif typeArguments.Length > declaredParamCount then
                typeArguments |> List.truncate declaredParamCount
            else
                let padCount = declaredParamCount - typeArguments.Length
                let objArg =
                    LazyContainer.CreateFromValue
                        (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive)
                typeArguments @ List.replicate padCount objArg
        if List.isEmpty alignedArguments then
            // Declared arity is 0 (the resolved body is non-generic) — emit the head alone;
            // the placeholder scope already registered above is the final form.
            Registered (remap prefix)
        else
        let postfixArguments =
            alignedArguments
            |> List.map (prerender ctx scope)
        // Replace the placeholder with the full Prefix application (head + args). The args were
        // prerendered AFTER the placeholder registration, so any cyclic arg was cycle-broken.
        applyArgsToCollectionHead prefix postfixArguments
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = []; ResolvedType = None }
        when (let typeParams =
                match innerResolvedType.Value with
                | ResolvedType.Interface i -> i.TypeParameters
                | ResolvedType.Class c -> c.TypeParameters
                | _ -> []
              // A BARE reference (no args) to a GENERIC Interface/Class. TypeScript permits this
              // when EVERY type parameter has a DEFAULT (`EventTarget<EventMap = Record<...>>` then
              // `extends EventTarget`). F# has NO default type parameters, so the bare reference
              // emits `inherit EventTarget` against `type EventTarget<'EventMap>` -> FS0033 "expects
              // 1 type argument(s) but is given 0". Fire ONLY when the head is generic AND a Fable
              // arity override doesn't make it non-generic (typed arrays stay bare via the no-args
              // arm below). Synthesize the defaults below.
              not (List.isEmpty typeParams)
              && (match innerResolvedType.Value with
                  | ResolvedType.Interface i -> fableDeclaredArity (Name.Case.valueOrSource i.Name) |> Option.isNone
                  | ResolvedType.Class c -> fableDeclaredArity (Name.Case.valueOrSource c.Name) |> Option.isNone
                  | _ -> false)) ->
        // Materialise each type parameter's TS DEFAULT as the application argument (the encoder +
        // decoder preserve it: TypeParameter.Default, Arena.Interner.fs:262/720). A parameter
        // lacking a default falls back to `obj` (NonPrimitive). Build `head<default...>` with the
        // SAME cycle-safe discipline as the cdc0108 arm: register the head-only scope BEFORE
        // prerendering the (possibly self-referential) default args, so a cyclic default re-enters
        // via the cache-hit arm instead of recursing.
        let typeParams =
            match innerResolvedType.Value with
            | ResolvedType.Interface i -> i.TypeParameters
            | ResolvedType.Class c -> c.TypeParameters
            | _ -> []
        let prefix = innerResolvedType |> prerender ctx scope
        prefix
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
        |> ignore
        let defaultArgs =
            typeParams
            |> List.map (fun tp ->
                match tp.Value.Default with
                | Some d -> d
                | None -> LazyContainer.CreateFromValue (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive))
        let postfixArguments = defaultArgs |> List.map (prerender ctx scope)
        applyArgsToCollectionHead prefix postfixArguments
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType }
    | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = [] } ->
        innerResolvedType
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { TypeArguments = (_ :: _) as typeArguments; Type = innerResolvedType }
        when (match innerResolvedType.Value with
              | ResolvedType.TypeReference { TypeArguments = (_ :: _) } -> true
              | _ -> false) ->
        // The inner `Type` is ITSELF an already-instantiated generic application
        // (e.g. `Type` = `ReadableStream<Uint8Array<ArrayBuffer>>`), and this outer
        // reference *also* carries TypeArguments — a redundant re-instantiation the
        // encoder emitted. Applying the outer args again would double-wrap the
        // prefix (`ReadableStream<..><..>`, invalid F#). Render the inner type alone;
        // it already includes the instantiation. (Contrast: a legitimate generic ref
        // like `Promise<X>` has a bare `Interface`/`Class` as its `Type`, handled below.)
        ignore typeArguments
        innerResolvedType
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeReference { TypeArguments = typeArguments; Type = innerResolvedType } ->
        let innerResolvedTypeValue = innerResolvedType.Value
        let declaredParamCount =
            match innerResolvedTypeValue with
            | ResolvedType.Interface i ->
                fableDeclaredArity (Name.Case.valueOrSource i.Name)
                |> Option.defaultValue i.TypeParameters.Length
            | ResolvedType.Class c ->
                fableDeclaredArity (Name.Case.valueOrSource c.Name)
                |> Option.defaultValue c.TypeParameters.Length
            | _ -> typeArguments.Length
        // Encoder/TS may produce a TypeReference whose argument count doesn't
        // match the inner type's declared type parameter count — happens with
        // declaration merging where the encoder can't disambiguate which
        // declaration's arity to honour (e.g. lib.dom Body has 0 type params
        // but a downstream class `extends Body<X>` was authored against a
        // merged extension). Align the args to the declaration's arity so the
        // emitted F# is well-formed: truncate excess args, pad missing args
        // with `obj`. Both directions log a warning so the upstream encoder
        // discrepancy stays visible.
        let alignedArguments =
            if typeArguments.Length = declaredParamCount then
                typeArguments
            elif typeArguments.Length > declaredParamCount then
                // Diagnostics go to stderr (stdout carries the generated F#) and identify the
                // type by its bounded Name — NEVER %A the ResolvedType, whose graph is cyclic
                // and overflows the stack via ResolvedType.ToString().
                eprintfn "Warning: TypeReference application (type key %A) has %d arguments but declares %d type parameters; truncating to declared arity"
                    innerResolvedType.Raw typeArguments.Length declaredParamCount
                typeArguments |> List.truncate declaredParamCount
            else
                eprintfn "Warning: TypeReference application (type key %A) has %d arguments but declares %d type parameters; padding with obj to declared arity"
                    innerResolvedType.Raw typeArguments.Length declaredParamCount
                let padCount = declaredParamCount - typeArguments.Length
                let objArg =
                    LazyContainer.CreateFromValue
                        (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive)
                typeArguments @ List.replicate padCount objArg
        if List.isEmpty alignedArguments then
            innerResolvedType
            |> prerender ctx scope
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        else
        let prefix =
            innerResolvedType
            |> prerender ctx scope
        let postfixArguments =
            alignedArguments
            |> List.map (prerender ctx scope)
        applyArgsToCollectionHead prefix postfixArguments
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Array innerResolvedType ->
        (
            lift Intrinsic.array,
            LazyContainer.CreateFromValue innerResolvedType
            |> prerender ctx scope
            |> List.singleton
        )
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Enum enumType ->
        let path = Path.Interceptors.pipeEnum ctx enumType
        let ref = path |> createConcreteTypeRef
        let scope = RenderScopeStore.create()
        { RenderScope.Type = resolvedType
          Root = path |> TypeLikePath.create |> ValueSome
          TypeRef = ref
          Render =
              lazy Enum.render ctx enumType
              |> Render.create ref
          TransientChildren = ValueSome scope }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.EnumCase enumCase ->
        let path = TransientTypePath.AnchoredAndMoored enumCase.Name
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        let scope = RenderScopeStore.create()
        { RenderScope.Type = resolvedType
          Root = TypeLikePath.create path |> ValueSome
          TypeRef = ref
          Render =
              lazy EnumCase.render ctx scope enumCase
              |> Render.create ref
          TransientChildren = ValueSome scope }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeParameter typeParameter ->
        typeParameter.Name
        |> Name.Case.valueOrModified
        |> Ast.LongIdent
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.ReadOnly innerResolvedType ->
        innerResolvedType
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Tuple { Types = [] } ->
        // The EMPTY TS tuple type `[]` (a zero-length fixed tuple, e.g. zod's
        // `$ZodIssueInvalidUnionMultipleMatch.errors: []`) has NO F# tuple equivalent — a tuple
        // molecule with zero elements renders to nothing (`errors: with get`, FS0010). Emit `obj`.
        lift Intrinsic.obj
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Tuple tuple ->
        tuple.Types
        |> List.mapi (fun idx ->
            _.Type
            >> prerender ctx scope
            >> TypeRefRender.orNullable tuple.Types[idx].IsOptional
            )
        |> List.toArray
        |> RenderScopeStore.TypeRefRender.create scope resolvedType false
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeLiteral typeLiteral ->
        let callSignature, rest =
            typeLiteral.Members
            |> List.partition _.IsCallSignature
            ||> fun sigs rest ->
                sigs
                |> List.map (function
                    | Member.CallSignature callSignature -> callSignature
                    | _ -> failwith "Unreachable guaranteed by guard in partition"
                    )
                , rest
        // A literal whose ONLY member is a single call signature IS a function type — render it
        // as an F# function type `(a -> b -> r)` (below), NOT as a hoisted nominal interface with
        // an `abstract Invoke:` member. A nominal `Invoke` interface is the single most usability-
        // breaking output: an F# CALLBACK lambda cannot be passed where the binding demands it.
        // F# function types curry any arity (Ast.Funs handles N params), and a SPREAD/rest param's
        // `.Type` is ALREADY the array (`(...args: T[])` -> the param Type is `T[]`), so it renders
        // as a normal `(ResizeArray<T> -> r)` param — the `IsSpread` flag only matters for the
        // nominal `[<ParamArray>]` form. So EVERY single-call-signature literal inlines as a
        // function type. (The prior `< 3 params && no-spread` cap was artificial and forced common
        // multi-arg / variadic callbacks — ExportedHandlerQueueHandler, `(...args) => any` — to
        // nominal interfaces an F# lambda can't satisfy.)
        match callSignature, rest with
        | [], [] ->
            liftNullable Intrinsic.obj
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | [ [ singleSig ] ], [] ->
            let parameters =
                singleSig.Parameters
                |> List.mapi (fun idx ->
                    _.Type
                    >> prerender ctx scope
                    >> TypeRefRender.orNullable singleSig.Parameters[idx].IsOptional)
            let returnValue = prerender ctx scope singleSig.Type
            (parameters, returnValue)
            |> RenderScopeStore.TypeRefRender.create scope resolvedType false
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        // An index-signature-ONLY literal (`{ [k: K]: V }`) is a MAP, not an object with named
        // members. Hoisting it as a nominal interface produces an unnameable `SharedLiterals.Lit<N>`
        // (the stem finds no property/method members) that leaks a machine name into the public
        // surface. Render it INLINE as `IDictionary<K, V>` — the faithful F#/Fable map, keeping both
        // key and value types. (A read-only index sig maps the same; F# has no distinct interface.)
        | [], [ Member.IndexSignature idx ] ->
            let keyRef =
                match idx.Parameters with
                | keyParam :: _ -> prerender ctx scope keyParam.Type
                | [] -> lift Intrinsic.string
            let valueRef = prerender ctx scope idx.Type
            let head =
                Ast.LongIdent "System.Collections.Generic.IDictionary"
                |> RenderScopeStore.TypeRefAtom.Unsafe.createWidget
                |> RenderScopeStore.TypeRef.Unsafe.createAtom
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
            RenderScopeStore.TypeRefMolecule.Unsafe.createPrefix head [ keyRef; valueRef ]
            |> RenderScopeStore.TypeRef.Unsafe.createMolecule
            |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
            |> RenderScope.createRootless resolvedType
            |> addOrReplaceScope ctx resolvedType
        | _, _ ->
            match GeneratorContext.SharedLiterals.tryGetHome ctx resolvedType with
            | ValueSome typePath ->
                // SHARED across >1 owner (gated by the counting pre-pass): root this literal at
                // its canonical absolute home `SharedLiterals.<structural-name>`, mirroring the
                // LiteralUnions arm above. The ConcretePath ref (identity-invariant) makes every
                // reference resolve here regardless of owner; the Anchored root + Transient render
                // is emitted ONCE by the PreludeRenders driver in processExports (the same driver
                // that emits canonical literal-unions). Without the canonical home the per-owner
                // transient root below would place the single def under only the first owner and
                // dangle every other owner's reference (the deep-nested shared-literal FS0039 class).
                let ref = createConcreteTypeRef typePath
                let scope = RenderScopeStore.create()
                {
                    RenderScope.Type = resolvedType
                    Root = TypeLikePath.create typePath |> ValueSome
                    TypeRef = ref
                    Render =
                        (lazy TypeLiteral.render ctx scope typeLiteral)
                        |> Render.createFromTransientLazy ref
                    TransientChildren = ValueSome scope
                }
                |> addOrReplaceScope ctx resolvedType
            | ValueNone ->
            let path = TransientTypePath.Anchored
            let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
            let scope = RenderScopeStore.create()
            {
                RenderScope.Type = resolvedType
                Root = path |> TypeLikePath.create |> ValueSome
                TypeRef = ref
                Render =
                    lazy TypeLiteral.render ctx scope typeLiteral
                    |> Render.create ref
                TransientChildren = ValueSome scope
            }
            |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TemplateLiteral templateLiteral ->
        let path = TransientTypePath.Anchored
        scope |> RenderScopeStore.tryAdd resolvedType path
        let scope = RenderScopeStore.create()
        let ref = RenderScopeStore.TypeRefRender.create scope resolvedType false path
        {
            RenderScope.Type = resolvedType
            Root = path |> TypeLikePath.create |> ValueSome
            TypeRef = ref
            Render =
                lazy TemplateLiteral.render ctx scope templateLiteral
                |> Render.create ref
            TransientChildren = ValueSome scope
        }
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Optional typeReference ->
        ResolvedType.TypeReference typeReference
        |> LazyContainer.CreateFromValue
        |> prerender ctx scope
        |> TypeRefRender.nullable
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.TypeQuery typeQuery ->
        prerender ctx scope typeQuery.Type
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    | ResolvedType.Substitution substitutionType ->
        substitutionType.Base |> prerender ctx scope
        |> RenderScope.createRootless resolvedType
        |> addOrReplaceScope ctx resolvedType
    |> function
        | Registered { Nullable = nullable } when ctx.TypeAliasRemap.ContainsKey(lazyResolvedType.Value) ->
            ctx.TypeAliasRemap[lazyResolvedType.Value]
            |> TypeRefRender.orNullable nullable
            |> padAliasNameToArity ctx lazyResolvedType.Value
        | Registered ref -> ref

module TestHelper =
    let prerender ctx resolvedType =
        let scope = RenderScopeStore.create()
        prerender ctx scope (LazyContainer.CreateFromValue resolvedType)
    
type GeneratorContext with
    static member Empty = GeneratorContext.Create prerender
    static member EmptyWithCustomisation customisation = GeneratorContext.Create(prerender, Customisation.Create customisation)
    
module ArenaInterner =
    /// True when a resolved type is memoised by the interner to a single shared
    /// instance (primitives, literals, globalThis). Such a type must NOT be used
    /// as a `TypeAliasRemap` key: the remap substitutes the alias name wherever
    /// that resolved type occurs, so aliasing a shared instance (e.g.
    /// `type D1SessionBookmark = string`, `type Mode = "primary-only"`) would make
    /// *every* occurrence of that primitive/literal render as the alias name across
    /// the entire surface. Only nominal/structural alias bodies are safe to remap.
    let isShareableAliasBody =
        function
        | ResolvedType.Primitive _
        | ResolvedType.Literal _
        | ResolvedType.GlobalThis -> true
        | _ -> false

    let prerenderTypeAliases (ctx: GeneratorContext) (arena: ArenaInterner) =
        // Iterate the export-key-keyed resolved exports (populated eagerly when the arena is
        // built) rather than ExportMap, so each alias's EXPORT key is in hand.
        arena.ResolvedExports
        |> Seq.iter (fun (KeyValue(exportKey, export)) ->
            match export with
            | ResolvedExport.TypeAlias value when not (isShareableAliasBody value.Type.Value) ->
                let path = Path.Interceptors.pipeTypeAlias ctx value
                let aliasRef =
                    RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
                    |> RenderScopeStore.TypeRef.Unsafe.createAtom
                    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                // Record the alias's DECLARED arity alongside each remap key. A reference to a
                // generic mapped/utility alias (`Without<U,T>`) in a member/union/heritage position
                // is resolved by the encoder to the alias's bare structural BODY with the
                // application's `TypeArguments` DROPPED (verified: every such site reaches the remap
                // as the bare body with no args anywhere in the IR). The remap renders the body as
                // the bare alias name, but the alias is emitted as `type Without<'U,'T>`, so the
                // bare name is FS0033. The args are unrecoverable here, so the remap pads the name
                // to this arity with `obj` placeholders.
                //
                // Record arity ONLY when EVERY declared type parameter is UNCONSTRAINED. A
                // constrained parameter (`type EventListener<'T when 'T :> Event> = ...`) cannot accept
                // the `obj` placeholder — `EventListener<obj>` is FS0001 ("obj not compatible with
                // Event"). Leaving the arity UNRECORDED for such aliases means (a) the remap padding
                // skips them (the bare name stays as the pre-existing FS0033/FS0887 it was at HEAD — no
                // NEW error class), and (b) the cdc0108 arm falls back to `typeArguments.Length` so a
                // constrained alias that DOES carry real args is unaffected. `.Constraint` is an
                // `option` over a lazy, so `.IsNone` does NOT force resolution.
                let allParamsUnconstrained =
                    value.TypeParameters
                    |> List.forall (fun tp -> tp.Value.Constraint.IsNone)
                // An IDENTITY alias (`type Identity<T> = T`, body is a bare type parameter) must NOT
                // record arity: padding its remapped name to `Identity<obj>` collides with the real
                // application args, emitting the unparseable double-generic `Identity<obj><realArgs>`.
                // With NO recorded arity the name is left unpadded, so a reference `Identity<X>` applies
                // its real args to the bare name -> a well-formed `Identity<X>` (`type Identity<'T>='T`).
                // An IDENTITY alias (`type Identity<T> = T`, body is a bare type parameter) is
                // TRANSPARENT — `Identity<X>` IS `X` (the encoder already resolves applications to the
                // argument). Its body is `'T`; remapping that body to the nominal name `Identity` makes
                // every reference to the body render as a BARE `Identity` (no type argument) —
                // `type Identity<'T>='T` given 0 args is FS0033. The .NET `module rec` resolver masked
                // this but Fable's transpiler surfaces it (637 errors). Remap the identity body to
                // `obj` (the erased form) instead of the nominal name: a bare reference to the alias
                // body becomes `obj` (always valid); an APPLICATION `Identity<X>` resolves to `X` via
                // the encoder's own resolution and does not hit this remap. Skipping the remap outright
                // instead lets the raw `'T` body leak (unbound-typar) — `obj` is the safe erasure.
                let isIdentityAlias =
                    match value.Type.Value with ResolvedType.TypeParameter _ -> true | _ -> false
                let objRef =
                    RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic Intrinsic.obj
                    |> RenderScopeStore.TypeRef.Unsafe.createAtom
                    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                let bodyRef = if isIdentityAlias then objRef else aliasRef
                let recordArity key =
                    if allParamsUnconstrained && not isIdentityAlias && value.TypeParameters.Length > 0 then
                        GeneratorContext.Prelude.addTypeAliasArity ctx key value.TypeParameters.Length
                // (1) Remap the alias BODY instance (`value.Type.Value`) — this is the instance
                //     produced when the body's structural type is rendered directly.
                GeneratorContext.Prelude.addTypeAliasRemap ctx value.Type.Value bodyRef
                recordArity value.Type.Value
                // (2) After the extractor's self-reference decoupling, the alias body lives at a
                //     distinct (generated) key, so a union member that references the alias by its
                //     EXPORT key resolves (via `ResolveType exportKey`) to an instance that the
                //     body-keyed remap above does NOT catch. Register that instance too, so members
                //     referencing the alias by export key render as the alias name
                //     (e.g. `U2<ResponseInputText, ResponseInputImage>`) instead of re-emitting the
                //     raw structural body. The alias's OWN definition is protected from self-remap
                //     by Render.TypeAlias.resolveInnerRef, which replaces the remapped name back
                //     with the real body render. Skip shareable (primitive/literal) export-key
                //     instances. ResolveType is the plain (memoised) resolver — no deep structural
                //     forcing — so this cannot stall on recursive graphs.
                let exportKeyType = arena.ResolveType exportKey
                if not (isShareableAliasBody exportKeyType) then
                    GeneratorContext.Prelude.addTypeAliasRemap ctx exportKeyType bodyRef
                    recordArity exportKeyType
            | _ -> ())
    let private getTopologicalSort (_: ArenaInterner) (graph: Graph) =
        let degrees = ConcurrentDictionary graph.Degrees
        let dependencies =
            graph.Dependents
            |> Seq.map (fun (KeyValue(key, value)) ->
                KeyValuePair(key, HashSet(value)))
            |> Dictionary
        let cycles =
            graph.Cycles
            |> Seq.sortBy (fun (KeyValue(key, value)) -> key = value)
        
        taskSeq {
            for cycle in cycles do
                if cycle.Value = cycle.Key then
                    degrees[cycle.Value] <- degrees[cycle.Value] - 1
                else degrees[cycle.Value] <- degrees[cycle.Value] - 2
                yield cycle.Value
            while degrees.Count > 0 do
                let (KeyValue(key, _)) = degrees |> Seq.sortBy _.Value |> Seq.head
                match dependencies.TryGetValue key with
                | true, deps ->
                    for dep in deps do
                        match degrees.TryGetValue dep with
                        | true, value -> degrees[dep] <- value - 1
                        | _ -> ()
                    yield key
                    dependencies.Remove(key) |> ignore
                    degrees.Remove(key) |> ignore
                | _ ->
                    yield key
                    degrees.Remove(key) |> ignore
        }
        
    /// <summary>
    /// Performs prerendering of all types in the graph - the series of operations are performed in topological order,
    /// and provides guarantees of passing in deep transient filled type graphs such as with solid-js.
    /// The costs may outweigh the benefits in this scenario. Performance costs are significant.
    /// </summary>
    /// <remarks>
    /// <list type="number">
    /// <item><description>Type aliases from the export map in the <c>ArenaInterner</c> are used to seed
    /// the type reference map - TypeAliases are encapsulations of the contained type, so we must register
    /// the encapsulation before we encounter the types to ensure that we do not render the reference generated
    /// from the underlying type.</description></item>
    /// <item><description>The graph evaluation is forced from the lazy function in the <c>ArenaInterner</c>.</description></item>
    /// <item><description>The graph is traversed in topological order by first yielding the cyclical keys of a graph,
    /// before yielding keys in the order of the number dependencies they have such that a type with no dependencies is registered
    /// first.</description></item>
    /// </list>
    /// </remarks>
    let prerenderFromGraph (ctx: GeneratorContext) (interner: ArenaInterner) =
        prerenderTypeAliases ctx interner
        let renderScopes = ConcurrentDictionary<ResolvedType, RenderScopeStore>()
        let graph = interner.Graph.Value
        getTopologicalSort interner graph
        |> TaskSeq.iter (fun key ->
            let renderScope = RenderScopeStore.create()
            let renderType = interner.ResolveType key
            {
                Data = key
                Result = lazy renderType
            }
            |> prerender ctx renderScope
            |> ignore
            if renderScope.TypeStore.Count <> 0 then
                match renderScopes.TryGetValue renderType with
                | true, scope ->
                    for kv in renderScope.TypeStore do
                        scope.TypeStore.TryAdd(kv.Key, kv.Value) |> ignore
                | _ ->
                    renderScopes.TryAdd(renderType, renderScope) |> ignore
            )
        |> _.Wait()
        for kv in renderScopes do
            GeneratorContext.Prelude.tryGet ctx kv.Key
            |> ValueOption.iter (fun renderScope ->
                renderScope.TransientChildren
                |> ValueOption.iter (fun scope ->
                    for kv in kv.Value.TypeStore do
                        scope
                        |> RenderScopeStore.tryAdd kv.Key kv.Value
                    )
                )
        renderScopes.Clear()