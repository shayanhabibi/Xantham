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
        TypeAliasRemap = DictionaryImpl()
        TypeAliasArity = DictionaryImpl()
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
        let tryGetTypeAliasArity ctx key =
            ctx.TypeAliasArity
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
            ctx.AnchorRenders
            |> Operation.addOrReplace key value
            
        let addResolvedType (ctx: GeneratorContext) (key: ResolvedType) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice1Of2 key) value
            
        let addResolvedExport (ctx: GeneratorContext) (key: ResolvedExport) (value: Choice<Anchored.TypeRefRender, Anchored.RenderScope>) =
            addOrReplace ctx (Choice2Of2 key) value
        
            
            
