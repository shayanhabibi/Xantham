namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
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

/// Default lib.es type substitutions applied to every Xantham consumer.
/// TS standard-library types that have F# equivalents get rendered as
/// references to those equivalents instead of as path-based references
/// to undefined `Typescript.X` names.
module LibEsDefaults =
    /// A substitution maps a TS lib.es type name to an F# target type
    /// name AND its target arity. Arity awareness is essential because
    /// TS and F# bindings diverge on type parameter count: TS 5.6+ gave
    /// `IterableIterator` three typars (`<TYield, TReturn, TNext>`) while
    /// the F# `IEnumerator<T>` analogue has one. TS 5.7+ made typed
    /// arrays generic (`Uint8Array<TBuf>`) while Fable's bindings remain
    /// non-generic. Without arity awareness the renderer mechanically
    /// keeps the source's arg list and emits invalid `IEnumerator<a,b,c>`
    /// or `Uint8Array<a>` (FS0033). The applier reconciles by truncating
    /// excess args, padding missing args with `obj`, or dropping all args
    /// when the target is non-generic.
    type Substitution = {
        Target: string
        Arity: int
    }

    /// Map from TS lib.es type name to its F# substitution. The F# name
    /// is emitted via Ast.LongIdent so multi-segment names (e.g.
    /// `System.Collections.Generic.IReadOnlyList`) work as-is.
    let substitutions =
        Map.ofList [
            // `Error` → `exn` (F# alias for System.Exception). Supports
            // `inherit exn()` for TS classes that extend Error.
            "Error", { Target = Intrinsic.exn; Arity = 0 }
            // `Array<T>` → `ResizeArray<T>` (F# alias for List<T>; Fable
            // maps to JS Array). Same intrinsic the generator uses for `T[]`.
            "Array", { Target = Intrinsic.array; Arity = 1 }
            // `PromiseLike<T>` → `Promise<T>` (Fable.Core.JS.Promise satisfies
            // PromiseLike's structural interface).
            "PromiseLike", { Target = "Promise"; Arity = 1 }
            // `Disposable` → `System.IDisposable` (direct semantic equivalent).
            "Disposable", { Target = "System.IDisposable"; Arity = 0 }
            // `Iterable<T>` → `seq<T>` (F# alias for IEnumerable<T>).
            "Iterable", { Target = "seq"; Arity = 1 }
            // `IterableIterator`/`ArrayIterator` → `IEnumerator<T>`. TS 5.6+
            // expanded these to three typars (`<TYield, TReturn, TNext>`);
            // the F# analogue keeps one. Excess args truncated.
            "IterableIterator", { Target = "System.Collections.Generic.IEnumerator"; Arity = 1 }
            "ArrayIterator", { Target = "System.Collections.Generic.IEnumerator"; Arity = 1 }
            "AsyncIterableIterator", { Target = "System.Collections.Generic.IAsyncEnumerator"; Arity = 1 }
            // `ReadonlyArray<T>` → `IReadOnlyList<T>` (read-only with index).
            "ReadonlyArray", { Target = "System.Collections.Generic.IReadOnlyList"; Arity = 1 }
            // `ReadonlyMap<K, V>` → `IReadOnlyDictionary<K, V>`.
            "ReadonlyMap", { Target = "System.Collections.Generic.IReadOnlyDictionary"; Arity = 2 }
            // `Map<K, V>` → `IDictionary<K, V>` (mutable keyed lookup).
            "Map", { Target = "System.Collections.Generic.IDictionary"; Arity = 2 }
            // `ReadonlySet<T>` → `IReadOnlySet<T>`.
            "ReadonlySet", { Target = "System.Collections.Generic.IReadOnlySet"; Arity = 1 }
            // `Set<T>` → `ISet<T>` (mutable set).
            "Set", { Target = "System.Collections.Generic.ISet"; Arity = 1 }
            // TS 5.7+ typed arrays: `Uint8Array<TBuf extends ArrayBufferLike>`.
            // Fable's `Fable.Core.JS.Uint8Array` etc. are non-generic; the
            // wrapper opens `Fable.Core.JS` so a bare name resolves. Drop
            // the typar arg at use sites.
            "Uint8Array", { Target = "Uint8Array"; Arity = 0 }
            "Uint8ClampedArray", { Target = "Uint8ClampedArray"; Arity = 0 }
            "Uint16Array", { Target = "Uint16Array"; Arity = 0 }
            "Uint32Array", { Target = "Uint32Array"; Arity = 0 }
            "Int8Array", { Target = "Int8Array"; Arity = 0 }
            "Int16Array", { Target = "Int16Array"; Arity = 0 }
            "Int32Array", { Target = "Int32Array"; Arity = 0 }
            "Float32Array", { Target = "Float32Array"; Arity = 0 }
            "Float64Array", { Target = "Float64Array"; Arity = 0 }
            "BigInt64Array", { Target = "BigInt64Array"; Arity = 0 }
            "BigUint64Array", { Target = "BigUint64Array"; Arity = 0 }
            "ArrayBufferView", { Target = "ArrayBufferView"; Arity = 0 }
            "DataView", { Target = "DataView"; Arity = 0 }
            // TS lib.es / lib.dom built-ins that Fable.Core.JS doesn't
            // provide a binding for. Without substitution these emit as
            // bare names (`URL`, `RegExp`, `AbortSignal`, ...) that F# can't
            // resolve (FS0039). `obj` is a placeholder — preserves
            // compilation while losing structural type info; consumers
            // refine via `Fable.Browser.Types` etc. at the binding layer.
            // Listed only for types with no augmentation path through the
            // declaration-merging escape (consumers of workers-types
            // augment Request/Response/RequestInit/Headers — those stay
            // unmapped here so the augmented bodies emit once the
            // declaration-merging fix lands).
            "PropertyKey", { Target = Intrinsic.obj; Arity = 0 }
            "RegExp", { Target = Intrinsic.obj; Arity = 0 }
            "URL", { Target = Intrinsic.obj; Arity = 0 }
            "AbortSignal", { Target = Intrinsic.obj; Arity = 0 }
            "AbortController", { Target = Intrinsic.obj; Arity = 0 }
            "WebSocket", { Target = Intrinsic.obj; Arity = 0 }
            "EventTarget", { Target = Intrinsic.obj; Arity = 0 }
            "Event", { Target = Intrinsic.obj; Arity = 0 }
            "EventListener", { Target = Intrinsic.obj; Arity = 0 }
            "EventListenerObject", { Target = Intrinsic.obj; Arity = 0 }
            "AddEventListenerOptions", { Target = Intrinsic.obj; Arity = 0 }
            "EventListenerOptions", { Target = Intrinsic.obj; Arity = 0 }
            "Blob", { Target = Intrinsic.obj; Arity = 0 }
            "File", { Target = Intrinsic.obj; Arity = 0 }
            "FormData", { Target = Intrinsic.obj; Arity = 0 }
            "Headers", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableStream", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableStreamDefaultReader", { Target = Intrinsic.obj; Arity = 0 }
            "WritableStream", { Target = Intrinsic.obj; Arity = 0 }
            "WritableStreamDefaultWriter", { Target = Intrinsic.obj; Arity = 0 }
            "TransformStream", { Target = Intrinsic.obj; Arity = 0 }
            "CustomEvent", { Target = Intrinsic.obj; Arity = 0 }
            "MessageEvent", { Target = Intrinsic.obj; Arity = 0 }
            // Workers-types augments these via TS declaration merging, but
            // the Map<Source, _>-keyed `interner.ExportMap` collapses
            // multiple lib.dom-rooted exports into one bucket — the
            // augmented bodies never emit. Substituting to `obj` here
            // preserves compilation; consumers refine via `Fable.Browser.Types`
            // or driver-side customisation. Once the Map-collision issue
            // gets a structural fix that emits all augmented exports, these
            // entries can drop and consumers get the full Cloudflare-extended
            // shapes (`Request.cf`, `Response.headers`, etc.).
            "Request", { Target = Intrinsic.obj; Arity = 0 }
            "Response", { Target = Intrinsic.obj; Arity = 0 }
            "RequestInit", { Target = Intrinsic.obj; Arity = 0 }
            "RequestInfo", { Target = Intrinsic.obj; Arity = 0 }
            "ResponseInit", { Target = Intrinsic.obj; Arity = 0 }
            "Body", { Target = Intrinsic.obj; Arity = 0 }
            // TS Streams API
            "QueuingStrategy", { Target = Intrinsic.obj; Arity = 0 }
            "QueuingStrategyInit", { Target = Intrinsic.obj; Arity = 0 }
            "QueuingStrategySize", { Target = Intrinsic.obj; Arity = 0 }
            "UnderlyingSource", { Target = Intrinsic.obj; Arity = 0 }
            "UnderlyingSink", { Target = Intrinsic.obj; Arity = 0 }
            "Transformer", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableByteStreamController", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableStreamBYOBReader", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableStreamBYOBRequest", { Target = Intrinsic.obj; Arity = 0 }
            "ReadableStreamDefaultController", { Target = Intrinsic.obj; Arity = 0 }
            "WritableStreamDefaultController", { Target = Intrinsic.obj; Arity = 0 }
            "TransformStreamDefaultController", { Target = Intrinsic.obj; Arity = 0 }
            "ByteLengthQueuingStrategy", { Target = Intrinsic.obj; Arity = 0 }
            "CountQueuingStrategy", { Target = Intrinsic.obj; Arity = 0 }
            // TS Encoding API
            "TextEncoder", { Target = Intrinsic.obj; Arity = 0 }
            "TextDecoder", { Target = Intrinsic.obj; Arity = 0 }
            "TextEncoderStream", { Target = Intrinsic.obj; Arity = 0 }
            "TextDecoderStream", { Target = Intrinsic.obj; Arity = 0 }
            "TextDecoderOptions", { Target = Intrinsic.obj; Arity = 0 }
            "TextDecoderStreamInit", { Target = Intrinsic.obj; Arity = 0 }
            // TS Crypto
            "Crypto", { Target = Intrinsic.obj; Arity = 0 }
            "SubtleCrypto", { Target = Intrinsic.obj; Arity = 0 }
            "CryptoKey", { Target = Intrinsic.obj; Arity = 0 }
            "CryptoKeyPair", { Target = Intrinsic.obj; Arity = 0 }
            "JsonWebKey", { Target = Intrinsic.obj; Arity = 0 }
            // TS DOM Event init / misc
            "EventInit", { Target = Intrinsic.obj; Arity = 0 }
            "CustomEventInit", { Target = Intrinsic.obj; Arity = 0 }
            "MessageEventInit", { Target = Intrinsic.obj; Arity = 0 }
            "ErrorEvent", { Target = Intrinsic.obj; Arity = 0 }
            "ErrorEventInit", { Target = Intrinsic.obj; Arity = 0 }
            "PromiseRejectionEvent", { Target = Intrinsic.obj; Arity = 0 }
            "DOMException", { Target = Intrinsic.obj; Arity = 0 }
            "URLSearchParams", { Target = Intrinsic.obj; Arity = 0 }
            "URLSearchParamsInit", { Target = Intrinsic.obj; Arity = 0 }
            // TS WebAssembly
            "WebAssembly.Module", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Instance", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Memory", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Table", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Global", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Tag", { Target = Intrinsic.obj; Arity = 0 }
            "WebAssembly.Exception", { Target = Intrinsic.obj; Arity = 0 }
        ]

    let private intrinsicRef (name: string) =
        RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic name
        |> RenderScopeStore.TypeRef.Unsafe.createAtom
        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false

    /// Treat a type as eligible for the lib.es substitution table if
    /// it's flagged IsLibEs OR its Source is the TypeScript compiler
    /// (`typescript`). The encoder's `LibEsExports` set doesn't always
    /// include every type from the TS standard libs (e.g. `Disposable`
    /// from lib.es2022.disposable has Source="typescript" but isn't in
    /// True when the declaration came from a TypeScript standard library
    /// file (lib.es*/lib.dom/etc.). The new `Source.LibEs fileName` case
    /// captures every case the old `IsLibEs || Source="typescript"`
    /// disjunction did: the encoder used to leave `IsLibEs=false` on some
    /// lib types whose Source was the typescript compiler, but the
    /// source-attribution refactor unified both into `Source.LibEs _`.
    let private isLibEsSource (source: ArenaInterner.Source) =
        match source with
        | ArenaInterner.Source.LibEs _ -> true
        | _ -> false

    /// Lookup a `Substitution` for a `ResolvedType`. Returns the
    /// substitution only if the type is lib.es-eligible AND its name is
    /// in the table. Used by `prerender` to read the target arity when
    /// reconciling TypeReference args; mirrors the eligibility check
    /// used by `resolvedTypePreludeInterceptor` so name-substitution and
    /// arity-reconciliation always agree on which types are substituted.
    let tryLookupSubstitution (resolvedType: ResolvedType) : Substitution voption =
        let lookup source (name: Name<_>) =
            if isLibEsSource source then
                let key = Name.Case.valueOrSource name
                match Map.tryFind key substitutions with
                | Some s -> ValueSome s
                | None -> ValueNone
            else ValueNone
        match resolvedType with
        | ResolvedType.Interface { Source = source; Name = name } ->
            lookup source name
        | ResolvedType.Class { Source = source; Name = name } ->
            lookup source name
        | ResolvedType.Enum { Source = source; Name = name } ->
            lookup source name
        | _ -> ValueNone

    /// Default `ResolvedTypePrelude` interceptor. For lib-sourced types
    /// with a known F# substitution, swaps the renderScope's TypeRef with
    /// the intrinsic ref so all reference sites resolve through the cache.
    /// For other lib-sourced types, falls back to RefOnly (don't render
    /// the body). Non-lib-sourced types fall through to normal emission —
    /// this is the declaration-merging escape: types like `Request` /
    /// `Response` that the consumer's package redefines/augments are
    /// flagged `Source.Package _`, not `Source.LibEs _`, and still get
    /// their full body emitted.
    let resolvedTypePreludeInterceptor _ resolvedType =
        let trySubstitute (name: Name<_>) =
            let key = Name.Case.valueOrSource name
            match Map.tryFind key substitutions with
            | Some s ->
                let ref = intrinsicRef s.Target
                ValueSome (fun renderScope ->
                    { renderScope with TypeRef = ref; Render = Render.RefOnly ref })
            | None -> ValueNone
        match resolvedType with
        | ResolvedType.Interface { Source = ArenaInterner.Source.LibEs _; Name = name }
        | ResolvedType.Class { Source = ArenaInterner.Source.LibEs _; Name = name } ->
            match trySubstitute name with
            | ValueSome f -> f
            | ValueNone ->
                fun renderScope ->
                    { renderScope with Render = Render.RefOnly renderScope.TypeRef }
        | ResolvedType.Enum { Source = ArenaInterner.Source.LibEs _ } ->
            fun renderScope ->
                { renderScope with Render = Render.RefOnly renderScope.TypeRef }
        | _ -> id

type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> LazyResolvedType -> TypeRefRender
and InterceptorIgnorePathRender = {
    Source: ArenaInterner.Source -> bool
    QualifiedName: QualifiedName -> bool
} with
    static member Default = {
        // Ignore TS toolchain packages — TS standard library (`Source.LibEs`)
        // and `@babel/*` (parser internals). These are infrastructure, not
        // consumer libraries; their declarations leak into encoder output
        // when the consumer pulls them transitively but should never reach
        // the F# binding. Post source-attribution refactor the discriminator
        // is structural rather than string-based: LibEs uniformly identifies
        // standard-library files; babel still needs the package-name match.
        Source = function
            | ArenaInterner.Source.LibEs _ -> true
            | ArenaInterner.Source.PackageInternal subModule ->
                subModule.Value.Package.Value.Name.Contains(
                    "babel", System.StringComparison.OrdinalIgnoreCase)
            | ArenaInterner.Source.Package collection ->
                collection.Canonical.SubModule.Value.Package.Value.Name.Contains(
                    "babel", System.StringComparison.OrdinalIgnoreCase)
        QualifiedName = fun _ -> false
    }
and InterceptorPaths = {
    TypePaths: GeneratorContext -> Choice<Interface, EnumType, Class, TypeAlias> -> TypePath -> TypePath
    MemberPaths: GeneratorContext -> Choice<Variable, Function> -> MemberPath -> MemberPath
} with
    static member Default = {
        // Prune `Typescript` parent unconditionally. The `Typescript` module
        // is an artifact of how the encoder represents TS lib.* types'
        // source — it is never emitted as an F# module, so any `Typescript.X`
        // reference is unresolvable. Pruning it everywhere makes references
        // bare so the lib.es substitution interceptors can fire on them.
        TypePaths = fun _ _ ->
            TypePath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript")
        MemberPaths = fun _ _ ->
            MemberPath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript")
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
        ResolvedTypePrelude = LibEsDefaults.resolvedTypePreludeInterceptor
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
        // Keyed by the inner type's encoder TypeKey rather than ResolvedType.
        // The encoder produces multiple ResolvedType.TypeLiteral instances for
        // the same conceptual TS type, so reference-equality lookups by
        // ResolvedType miss whenever a reference site receives a different
        // instance from the alias's registration site. TypeKey is the stable
        // identity that survives across reference sites.
        TypeAliasRemap: DictionaryImpl<TypeKey, TypeRefRender>
        // Keyed by the alias's TypeAliasRemap target ref (the ConcretePath
        // ref the remap would substitute references to). Tracking by target
        // ref instead of by body TypeKey is robust against the encoder
        // producing multiple TypeKeys for the same conceptual alias and
        // against intermediate wrappers (e.g. ResolvedType.Optional uses
        // LazyContainer.CreateFromValue which loses the original Raw).
        // A reference whose remap target matches an in-flight alias's
        // target is a recursive ref and gets substituted with `obj`.
        RenderingAliasTargetRefs: HashSet<TypeRefRender>
        PreludeGetTypeRef: PreludeGetTypeRefFunc
        PreludeRenders: PreludeScopeStore
        AnchorRenders: AnchorScopeStore
        InFlight: HashSet<ResolvedType>
        Customisation: Customisation
        // Stable concrete paths for interned synthetic literal types (Union of
        // literals, Intersection, multi-member TypeLiteral, TemplateLiteral)
        // that appear in multiple positions and would otherwise produce
        // mismatched references when each call site re-anchors the cached
        // transient atom against its own anchorPath. Populated by a pre-pass
        // before `prerenderFromGraph`. When prerender encounters a synthetic
        // literal whose ResolvedType is in this map, it produces a
        // ConcretePath ref + Anchored Root rather than a Transient pair.
        // Additive to the documented Transient/Anchor system; non-interned
        // single-position literals continue through the documented path.
        SyntheticPaths: DictionaryImpl<ResolvedType, TypePath>
    }
    override this.ToString() = $"GeneratorContext(%d{this.PreludeRenders.Count})"
    static member internal Create(preludeGetTypeRefFunc, ?customisation) = {
        PreludeRenders = DictionaryImpl()
        AnchorRenders = DictionaryImpl()
        PreludeGetTypeRef = preludeGetTypeRefFunc
        InFlight = HashSet()
        TypeAliasRemap = DictionaryImpl()
        RenderingAliasTargetRefs = HashSet()
        Customisation = defaultArg customisation Customisation.Default
        SyntheticPaths = DictionaryImpl()
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
    
    module Prelude =
        let addTypeAliasRemap ctx key value =
            ctx.TypeAliasRemap
            |> Operation.addOrReplace key value
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
        
            
            
