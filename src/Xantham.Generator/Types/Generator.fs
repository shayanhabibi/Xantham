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
    /// Map from TS lib.es type name to F# intrinsic name. The F# name is
    /// emitted via Ast.LongIdent so multi-segment names (e.g.
    /// `System.Collections.Generic.IReadOnlyList`) work as-is.
    let substitutions =
        Map.ofList [
            // `Error` → `exn` (F# alias for System.Exception). Supports
            // `inherit exn()` for TS classes that extend Error.
            "Error", Intrinsic.exn
            // `Array<T>` → `ResizeArray<T>` (F# alias for List<T>; Fable
            // maps to JS Array). Same intrinsic the generator uses for `T[]`.
            "Array", Intrinsic.array
            // `PromiseLike<T>` → `Promise<T>` (Fable.Core.JS.Promise satisfies
            // PromiseLike's structural interface).
            "PromiseLike", "Promise"
            // `Disposable` → `System.IDisposable` (direct semantic equivalent).
            "Disposable", "System.IDisposable"
            // `Iterable<T>` → `seq<T>` (F# alias for IEnumerable<T>).
            "Iterable", "seq"
            // `IterableIterator<T>` and `ArrayIterator<T>` → IEnumerator<T>.
            "IterableIterator", "System.Collections.Generic.IEnumerator"
            "ArrayIterator", "System.Collections.Generic.IEnumerator"
            "AsyncIterableIterator", "System.Collections.Generic.IAsyncEnumerator"
            // `ReadonlyArray<T>` → `IReadOnlyList<T>` (read-only with index).
            "ReadonlyArray", "System.Collections.Generic.IReadOnlyList"
        ]

    let private intrinsicRef (name: string) =
        RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic name
        |> RenderScopeStore.TypeRef.Unsafe.createAtom
        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false

    /// Default `ResolvedTypePrelude` interceptor. For lib.es types with a
    /// known F# substitution, swaps the renderScope's TypeRef with the
    /// intrinsic ref so all reference sites resolve through the cache. For
    /// other lib.es types, falls back to RefOnly (don't render the body —
    /// it's a TS lib type, not part of the consumer's binding surface).
    let resolvedTypePreludeInterceptor _ resolvedType =
        let trySubstitute (name: Name<_>) =
            let key = Name.Case.valueOrSource name
            if substitutions.ContainsKey key then
                let ref = intrinsicRef substitutions.[key]
                ValueSome (fun renderScope ->
                    { renderScope with TypeRef = ref; Render = Render.RefOnly ref })
            else
                ValueNone
        match resolvedType with
        | ResolvedType.Interface { IsLibEs = true; Name = name } ->
            match trySubstitute name with
            | ValueSome f -> f
            | ValueNone ->
                fun renderScope ->
                    { renderScope with Render = Render.RefOnly renderScope.TypeRef }
        | ResolvedType.Class { IsLibEs = true; Name = name } ->
            match trySubstitute name with
            | ValueSome f -> f
            | ValueNone ->
                fun renderScope ->
                    { renderScope with Render = Render.RefOnly renderScope.TypeRef }
        | ResolvedType.Enum { IsLibEs = true } ->
            fun renderScope ->
                { renderScope with Render = Render.RefOnly renderScope.TypeRef }
        | _ -> id

type PreludeGetTypeRefFunc = GeneratorContext -> RenderScopeStore -> LazyResolvedType -> TypeRefRender
and InterceptorIgnorePathRender = {
    Source: ArenaInterner.QualifiedNamePart -> bool
    QualifiedName: QualifiedName -> bool
} with
    static member Default = {
        // Ignore TS toolchain packages — `typescript` (compiler types) and
        // `@babel/*` (parser internals). These are infrastructure, not
        // consumer libraries; their declarations leak into encoder output
        // when the consumer pulls them transitively but should never reach
        // the F# binding.
        Source = function
            | QualifiedNamePart.Normal text
            | QualifiedNamePart.Abnormal(text, _) ->
                text.Contains("babel", System.StringComparison.OrdinalIgnoreCase)
                || text.Contains("typescript", System.StringComparison.OrdinalIgnoreCase)
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
        
            
            
