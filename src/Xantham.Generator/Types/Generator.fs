namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator

exception EncoderInvariantViolation of string

/// Wrapper that chooses the type of dictionary (concurrent/normal)
/// depending on the presence of the CONCURRENT_DICT constant on compilation
type DictionaryImpl<'Key, 'Value> =
    #if CONCURRENT_DICT
    System.Collections.Concurrent.ConcurrentDictionary<'Key, 'Value>
    #else
    Dictionary<'Key, 'Value>
    #endif

type PreludeScopeStore = DictionaryImpl< ResolvedType, RenderScope >
type AnchorScopeStore = DictionaryImpl<Choice<ResolvedType, ResolvedExport>, Choice<Anchored.TypeRefRender, Anchored.RenderScope>>
type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> LazyResolvedType -> TypeRefRender
and InterceptorIgnorePathRender = {
    Source: ArenaInterner.QualifiedNamePart -> bool
    QualifiedName: QualifiedName -> bool
} with
    static member Default = {
        Source = fun _ -> false
        QualifiedName = fun _ -> false
    }
and InterceptorPaths = {
    TypePaths: GeneratorContext -> Choice<Interface, EnumType, Class, TypeAlias> -> TypePath -> TypePath
    MemberPaths: GeneratorContext -> Choice<Variable, Function> -> MemberPath -> MemberPath
} with
    static member Default = {
        TypePaths = fun _ _ -> id
        MemberPaths = fun _ _ -> id
    }
and Interceptors = {
    IgnorePathRender: InterceptorIgnorePathRender
    Paths: InterceptorPaths
    ResolvedTypePrelude: GeneratorContext -> ResolvedType -> RenderScope -> RenderScope
    AnchoredRender: GeneratorContext -> Choice<ResolvedType, ResolvedExport> -> Choice<Anchored.TypeRefRender, Anchored.RenderScope> -> Choice<Anchored.TypeRefRender, Anchored.RenderScope>
} with
    static member Default = {
        IgnorePathRender = InterceptorIgnorePathRender.Default
        Paths = InterceptorPaths.Default
        ResolvedTypePrelude = fun _ _ -> id
        AnchoredRender = fun _ _ -> id
    }
and Customisation = {
    Interceptors: Interceptors
} with
    static member Default = {
        Interceptors = Interceptors.Default
    }
    static member Create fn: Customisation = fn Customisation.Default
and GeneratorContext =
    {
        TypeAliasRemap: DictionaryImpl<ResolvedType, TypeRefRender>
        /// Declared type-parameter arity for each `TypeAliasRemap` key (the alias body / export-key
        /// instances). A generic mapped/utility alias (`Without<U,T>`, `Params<P>`) is referenced in
        /// member/union/heritage positions whose IR has ALREADY been resolved to the alias's bare
        /// structural BODY — the encoder dropped the application's `TypeArguments`. The remap then
        /// renders the body as the bare alias NAME, but the alias is emitted as `type Without<'U,'T>`,
        /// so the bare name is FS0033 ("expects N type argument(s) but is given 0"). The args are
        /// unrecoverable at these sites (gone from the IR), so the remap pads the name to this declared
        /// arity with `obj` placeholders (`Without<obj,obj>`). Populated alongside `TypeAliasRemap` in
        /// `prerenderTypeAliases`. Arity 0 (non-generic alias) needs no padding.
        TypeAliasArity: DictionaryImpl<ResolvedType, int>
        PreludeGetTypeRef: PreludeGetTypeRefFunc
        PreludeRenders: PreludeScopeStore
        AnchorRenders: AnchorScopeStore
        /// Canonical owner-independent home for a hoisted object-LITERAL that is reached
        /// through MORE THAN ONE distinct owner context (a shared `ResolvedType.TypeLiteral`
        /// the decoder's structural compression interned into one node). Populated by a
        /// counting pre-pass (`markSharedLiterals`). When present for a literal's ResolvedType,
        /// `prerender` roots that literal at this absolute `TypePath` (under `SharedLiterals`)
        /// instead of a per-owner transient — so every reference resolves to the same
        /// re-anchor-invariant ConcretePath and the single def is emitted once. This mirrors the
        /// proven `LiteralUnions` canonical-home mechanism, gated to genuinely-shared literals so
        /// single-owner depth-2 literals keep nesting under their owner.
        SharedLiteralHomes: DictionaryImpl<ResolvedType, TypePath>
        /// LAMBDA-LIFTED hoisted-home typars (stage 3b). A hoisted object literal grafted under
        /// its owning alias (`caseLiteralRef`) whose members reference the alias's typars must
        /// ABSTRACT over them: the emitted nested def declares these typars (the anchor stage
        /// overrides the def's TypeParameters from this table, growing the orphan-scrub inScope
        /// so member typar references SURVIVE), and the alias's abbreviation body APPLIES them
        /// (the caseRef is a Prefix over the case path). Keyed by the literal's ResolvedType —
        /// the same identity the TypeStore graft uses. Only single-owner (non-SharedLiterals)
        /// literals are lifted; multi-owner homes are the stage-4 extension.
        HoistedHomeTypars: DictionaryImpl<ResolvedType, Transient.TypeParameterRender list>
        /// The declared typars (decoder-level, with their TS Constraint/Default lazies) of each
        /// remap-registered alias, keyed like `TypeAliasArity`. Consumed by the arity padders:
        /// a CONSTRAINED typar position cannot accept the `obj` placeholder (`'T :> ZodTypeAny`
        /// given `obj` is FS0001), so pads synthesize the typar's TS DEFAULT where present —
        /// which TS guarantees exists for any elidable parameter. Populated write-once in
        /// prerenderTypeAliases phase 2 (real pass only).
        TypeAliasTypars: DictionaryImpl<ResolvedType, Lazy<TypeParameter> list>
        /// Phase-1 collector for the shared-literal counting pre-pass. When `ValueSome`, the
        /// anchoring walk records, per hoisted object-literal ResolvedType, the set of distinct
        /// anchored def-home paths it is visited under. A literal with >1 distinct home is shared
        /// across owners and is assigned a canonical `SharedLiteralHomes` entry. `ValueNone` in
        /// the real (phase-2) pass so the counting is a no-op there.
        SharedLiteralVisits: voption<Dictionary<ResolvedType, HashSet<TypePath>>>
        /// Phase-1 collector for UNION-WRAPPED shared object-literals — the def-VISIT gate
        /// (`SharedLiteralVisits`) cannot see these. A multi-member object-literal reached only
        /// inside a union molecule (`U2<Owner.Lit, Owner.Lit>`) is never visited as a def-home: the
        /// cached union molecule carries its reference, so no owner scope holds it as an anchoring
        /// recursion child. Its single def lands under one owner and every other owner's `Owner.Lit`
        /// reference dangles (FS0039). When `ValueSome`, `prerender` records — keyed by the
        /// union-member literal's ResolvedType — the set of distinct REFERENCING-OWNER paths (the
        /// top-level export currently being rendered, threaded via `CurrentRefOwner`). A literal
        /// referenced through >1 distinct owner is shared and is given a canonical `SharedLiterals`
        /// home by `markSharedLiterals`, exactly like the def-VISIT-gated literals. `ValueNone` in
        /// the real (phase-2) pass. The owner is keyed by `AnchorPath` (covers Variable/Function
        /// member-owners as well as type owners uniformly); only the COUNT of distinct owners is
        /// load-bearing for the >1 shared-gate.
        SharedLiteralRefOwners: voption<Dictionary<ResolvedType, HashSet<AnchorPath>>>
        /// The top-level export's canonical anchor currently being rendered during the counting
        /// pre-pass, threaded so `prerender`'s union-member recording knows the referencing owner.
        /// A mutable cell (the surrounding record is otherwise immutable); `ValueNone` outside the
        /// counting pass and between exports.
        CurrentRefOwner: AnchorPath voption ref
        InFlight: HashSet<ResolvedType>
        /// The surface's own top-level globals (excludes lib.es internals). Used to keep
        /// a `typescript`-sourced top-level export from being dropped by the source-ignore
        /// gate, so its definition is emitted at the global root where references resolve.
        TopLevelExports: HashSet<ResolvedExport>
        /// The recipe's publish-ordered top-level module names (unit boundaries), set in
        /// emission. When non-empty, shared synthetic homes mint under
        /// `<HostTopModule>.SharedLiterals.<name>` where the host is the EARLIEST unit
        /// among the literal's owners — visible to every referencer via the unit DAG, and
        /// placed into the host's compilation unit by the ordinary top-module split (no
        /// cross-assembly module collision: each unit's SharedLiterals nests under its own
        /// top module). Empty = root-level `SharedLiterals.<name>` (isolation-test default).
        SyntheticPlacementOrder: string list
        /// Top-level module names under erase-with-advisory policy (+ the internal
        /// chunk-stub module). A shared synthetic whose owners ALL live under these
        /// roots is itself erased content: its home mints under the first erased
        /// root, which the path substitution immediately rewrites to the Erased.*
        /// alias — definitions drop with the erased modules, references collapse.
        ErasedRoots: string list
        /// THE ERASURE LEDGER (docs/GOALS.md Done(L1): every deliberate degradation
        /// counted and classed). Keys are "<class>:<subject>" (e.g.
        /// "erased-module:JsonSchemaTyped", "erased-heritage:Executor"); emission
        /// dumps it, the partition gate reports it. Never silent.
        AdvisoryLedger: DictionaryImpl<string, int>
        /// HOME-CHILD SCRUB STATE (armed by `emitCanonicalPreludeScopes`' post-drive
        /// second round, empty = inactive). `SyntheticHomePaths` = the flattened paths
        /// of every canonical shared-literal home; `SyntheticHomeChildDefs` = every
        /// MATERIALIZED def path (scope roots + child anchors) once the fixpoint drive
        /// completes. A path atom under a home with no matching materialized def can
        /// never resolve (the multi-member shared-rt class: per-site leaf stamping vs
        /// the one-slot rt-keyed store) — the scrub degrades it to `obj`, ledgered.
        SyntheticHomePaths: HashSet<string>
        SyntheticHomeChildDefs: HashSet<string>
        /// Per-export RenderScopeStore, MEMOIZED across export passes: nested children
        /// register into this store only when their refs mint FRESH — a re-anchor pass
        /// (the scrub-armed second export pass) that rebuilt stores from scratch would
        /// cache-hit those renders and under-register, silently shrinking the child
        /// set. Reusing the pass-1 store lets pass 2 re-anchor the FULL child set with
        /// the scrubs armed.
        ExportScopeStores: Dictionary<ResolvedExport, RenderScopeStore>
        Customisation: Customisation

    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"
    static member internal Create(preludeGetTypeRefFunc, ?customisation) = {
        PreludeRenders = DictionaryImpl()
        AnchorRenders = DictionaryImpl()
        SharedLiteralHomes = DictionaryImpl()
        SharedLiteralVisits = ValueNone
        SharedLiteralRefOwners = ValueNone
        CurrentRefOwner = ref ValueNone
        PreludeGetTypeRef = preludeGetTypeRefFunc
        InFlight = HashSet()
        TopLevelExports = HashSet()
        SyntheticPlacementOrder = []
        ErasedRoots = []
        AdvisoryLedger = DictionaryImpl()
        SyntheticHomePaths = HashSet()
        SyntheticHomeChildDefs = HashSet()
        ExportScopeStores = Dictionary()
        TypeAliasRemap = DictionaryImpl()
        TypeAliasArity = DictionaryImpl()
        HoistedHomeTypars = DictionaryImpl()
        TypeAliasTypars = DictionaryImpl()
        Customisation = defaultArg customisation Customisation.Default
    }
    

module GeneratorContext =
    module private Operation =
        let inline getOrAdd func key (dict: DictionaryImpl<'Key, 'Value>) =
            #if CONCURRENT_DICT
            dict.GetOrAdd(key, System.Func<_,_>(func))
            #else
            match dict.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = func key
                dict[key] <- value
                value
            #endif
        let inline tryGet key (dict: DictionaryImpl<'Key, 'Value>) =
            match dict.TryGetValue(key) with
            | true, value -> ValueSome value
            | _ -> ValueNone
        let inline tryAdd key value (dict: DictionaryImpl<'Key, 'Value>) =
            dict.TryAdd(key, value)
        let inline addOrReplace key value (dict: DictionaryImpl<'Key, 'Value>) =
            dict[key] <- value
    
    module Advisory =
        /// Increment an erasure-ledger class counter (key = "<class>:<subject>").
        let incrementBy (ctx: GeneratorContext) (key: string) (count: int) =
            let current =
                ctx.AdvisoryLedger
                |> Operation.tryGet key
                |> ValueOption.defaultValue 0
            ctx.AdvisoryLedger |> Operation.addOrReplace key (current + count)
        let increment (ctx: GeneratorContext) (key: string) = incrementBy ctx key 1
        /// The ledger, sorted for deterministic reporting.
        let dump (ctx: GeneratorContext) : (string * int) list =
            ctx.AdvisoryLedger
            |> Seq.map (fun (KeyValue(k, v)) -> k, v)
            |> Seq.sortBy fst
            |> Seq.toList

    module SharedLiterals =
        /// The canonical owner-independent home (if any) for a shared hoisted object-literal.
        let tryGetHome ctx (key: ResolvedType) =
            ctx.SharedLiteralHomes
            |> Operation.tryGet key
        let addHome ctx (key: ResolvedType) (home: TypePath) =
            ctx.SharedLiteralHomes
            |> Operation.addOrReplace key home
        /// Record (during the counting pre-pass) that the union-member object-literal `key` is
        /// referenced by the current top-level owner. A `key` with >1 distinct referencing owner is
        /// shared and is canonicalized into `SharedLiterals` alongside the def-VISIT-gated literals.
        /// No-op outside the counting pass (`SharedLiteralRefOwners`/`CurrentRefOwner` unset).
        let recordRefOwner (ctx: GeneratorContext) (key: ResolvedType) =
            match ctx.SharedLiteralRefOwners, ctx.CurrentRefOwner.Value with
            | ValueSome owners, ValueSome ownerPath ->
                match owners.TryGetValue key with
                | true, set -> set.Add ownerPath |> ignore
                | _ ->
                    let set = HashSet<AnchorPath>()
                    set.Add ownerPath |> ignore
                    owners[key] <- set
            | _ -> ()

    module Prelude =
        let addTypeAliasRemap ctx key value =
            ctx.TypeAliasRemap
            |> Operation.addOrReplace key value
        let addTypeAliasArity ctx key (arity: int) =
            ctx.TypeAliasArity
            |> Operation.addOrReplace key arity
        /// Write-once arity recording. Decoder compress routinely makes TWO aliases share one
        /// body/export-key instance (the JSONSchema/JsonSchemaType story), and `TypeAliasArity` is
        /// instance-keyed — a silent last-writer-wins overwrite lets one alias's arity clobber
        /// another's (an explicit 0 from a pruned alias zeroing a generic twin's arity truncates
        /// the twin's REAL args to a bare name: an FS0033 storm). First write wins; a conflicting
        /// later write is skipped with a diagnostic (stderr — stdout carries the generated F#).
        let addTypeAliasArityOnce ctx key (arity: int) (aliasName: string) =
            match ctx.TypeAliasArity |> Operation.tryGet key with
            | ValueSome existing when existing <> arity ->
                eprintfn "Warning: TypeAliasArity conflict for '%s': instance already recorded %d, skipping %d"
                    aliasName existing arity
            | ValueSome _ -> ()
            | ValueNone ->
                ctx.TypeAliasArity
                |> Operation.addOrReplace key arity
        let tryGetTypeAliasArity ctx key =
            ctx.TypeAliasArity
            |> Operation.tryGet key
        let addHoistedHomeTypars ctx key (typars: Transient.TypeParameterRender list) =
            ctx.HoistedHomeTypars
            |> Operation.addOrReplace key typars
        let tryGetHoistedHomeTypars ctx key =
            ctx.HoistedHomeTypars
            |> Operation.tryGet key
        /// Write-once, mirroring `addTypeAliasArityOnce` (instance-sharing twins are routine).
        let addTypeAliasTyparsOnce ctx key (typars: Lazy<TypeParameter> list) =
            match ctx.TypeAliasTypars |> Operation.tryGet key with
            | ValueSome _ -> ()
            | ValueNone ->
                ctx.TypeAliasTypars
                |> Operation.addOrReplace key typars
        let tryGetTypeAliasTypars ctx key =
            ctx.TypeAliasTypars
            |> Operation.tryGet key
        let canFlight ctx key =
            #if CONCURRENT_DICT
            lock ctx.InFlight (fun () ->
            #endif
            ctx.InFlight.Add key
            #if CONCURRENT_DICT
                )
            #endif
        let tryGet ctx key =
            ctx.PreludeRenders
            |> Operation.tryGet key
        let addOrReplace ctx key value =
            ctx.PreludeRenders
            |> Operation.addOrReplace key value
    module Anchored =
        let tryGet (ctx: GeneratorContext) (key: Choice<ResolvedType, ResolvedExport>) =
            ctx.AnchorRenders
            |> Operation.tryGet key
        
        let tryGetResolvedType (ctx: GeneratorContext) (key: ResolvedType) =
            tryGet ctx (Choice1Of2 key)
            
        let tryGetResolvedExport (ctx: GeneratorContext) (key: ResolvedExport) =
            tryGet ctx (Choice2Of2 key)

        let addOrReplace (ctx: GeneratorContext) (key: Choice<ResolvedType, ResolvedExport>) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            let value = ctx.Customisation.Interceptors.AnchoredRender ctx key value
            // CHILD-PRESERVING REPLACEMENT: registration is last-wins by design, but a
            // RE-anchor (the scrub-armed second export pass above all) re-renders
            // against the PRELUDE CACHE, and cache-hits under-register nested children
            // that only FRESH minting places in the store — so the replacement scope's
            // Anchors can silently LOSE defs the first pass materialized (measured:
            // ServiceWorkerGlobalScope's nested globals vanished while their refs
            // survived the def/ref-closure scrub, whose def-set had honestly recorded
            // them). Union the previous registration's Anchors into the replacement:
            // re-anchoring may improve a scope's own render, never shrink its children.
            (match value, ctx.AnchorRenders |> Operation.tryGet key with
             | Choice2Of2 newScope, ValueSome (Choice2Of2 oldScope) ->
                 for KeyValue(rt, entry) in oldScope.Anchors do
                     if not (newScope.Anchors.ContainsKey rt) then
                         newScope.Anchors.Add(rt, entry)
             | _ -> ())
            ctx.AnchorRenders
            |> Operation.addOrReplace key value
            
        let addResolvedType (ctx: GeneratorContext) (key: ResolvedType) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice1Of2 key) value
            
        let addResolvedExport (ctx: GeneratorContext) (key: ResolvedExport) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice2Of2 key) value
        
            
            
