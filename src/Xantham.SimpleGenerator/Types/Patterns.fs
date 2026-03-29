module rec Xantham.SimpleGenerator.Patterns

open System.Collections.Generic
open Xantham
open Xantham.Decoder
open Xantham.SimpleGenerator

module SourceKey = KeyNodeHashing.SourceKey
module CyclicKey = KeyNodeHashing.CyclicKey

/// <summary>
/// A struct that holds the <c>KeyResolutionContext</c> and a value to be used in pattern matching.
/// This struct is used to pass the context and the value to the pattern matching functions, where
/// multiple values are not accepted.<br/><br/>
/// See <c>PatternContext</c> module for functions to create, destruct, and manipulate this struct.<br/>
/// Example: <c>PatternContext.prepare</c> for creating the struct.<br/><br/>
/// See <c>PatternContext.Array</c> module for functions to manipulate a struct containing an array, such
/// that regular functions like <c>Array.map</c> are automatically rewrapped in the pattern context.<br/>
/// </summary>
[<Struct>]
type PatternContextHolder<'T> = {
    Context: KeyResolutionContext
    Value: 'T
} with
    member inline this.Destructure = (this.Context, this.Value)

/// <summary>
/// <para>Functions to manipulate the <see cref="PatternContextHolder{T}"/> struct.</para>
/// <para>Function names <b>suffixed</b> with <c>c</c> are functions that take a <see cref="KeyResolutionContext"/>
/// as the first argument, and the value as the second.</para>
/// <para>Function names <b>prefixed</b> with <c>c</c> are functions that take a <see cref="PatternContextHolder{T}"/>
/// as the argument.</para>
/// </summary>
module PatternContext =
    let destructure<'T> (ctxHolder: PatternContextHolder<'T>) = ctxHolder.Destructure
    let value<'T> (ctxHolder: PatternContextHolder<'T>) = ctxHolder.Value
    /// <summary>
    /// Prepares a value for pattern matching by creating a struct that holds the context and the value.
    /// This allows us to use pattern matching expressions which can return multiple choices without
    /// running into issues because of multiple input values.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="value"></param>
    let inline prepare<'T> (ctx: KeyResolutionContext) (value: 'T) = { Context = ctx; Value = value }
    /// <summary>
    /// Extracts the context and the value from a <see cref="PatternContextHolder{T}"/> struct
    /// and passes it to the given function, return the result in a new <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let mapc<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U) (ctxHolder: PatternContextHolder<'T>) =
        fn ctxHolder.Context ctxHolder.Value |> prepare ctxHolder.Context
    let cmap<'T, 'U> (fn: PatternContextHolder<'T> -> 'U) (ctxHolder: PatternContextHolder<'T>) = fn ctxHolder
    /// <summary>
    /// Extracts the value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given function,
    /// return the result in a new <see cref="PatternContextHolder{T}"/> struct if it has a value, otherwise return ValueNone.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let choose<'T, 'U> (fn: 'T -> 'U voption) (ctxHolder: PatternContextHolder<'T>) =
        fn ctxHolder.Value
        |> ValueOption.map (prepare ctxHolder.Context)
    /// <summary>
    /// Extracts the context and value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given function,
    /// return the result in a new <see cref="PatternContextHolder{T}"/> struct if it has a value, otherwise return ValueNone.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let choosec<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U voption) (ctxHolder: PatternContextHolder<'T>) =
        fn ctxHolder.Context ctxHolder.Value
        |> ValueOption.map (prepare ctxHolder.Context)
    /// <summary>
    /// Extracts the value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given function,
    /// return the result in a new <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let map<'T, 'U> (fn: 'T -> 'U) (ctxHolder: PatternContextHolder<'T>) = fn ctxHolder.Value |> prepare ctxHolder.Context
    /// <summary>
    /// Extracts the value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given function,
    /// which returns a <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let bind<'T, 'U> (fn: 'T -> PatternContextHolder<'U>) (ctxHolder: PatternContextHolder<'T>): PatternContextHolder<'U> = fn ctxHolder.Value
    /// <summary>
    /// Extracts the context and value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given function,
    /// which returns a <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let bindc<'T, 'U> (fn: KeyResolutionContext -> 'T -> PatternContextHolder<'U>) (ctxHolder: PatternContextHolder<'T>): PatternContextHolder<'U> = fn ctxHolder.Context ctxHolder.Value
    let cbind<'T, 'U> (fn: PatternContextHolder<'T> -> PatternContextHolder<'U>) (ctxHolder: PatternContextHolder<'T>): PatternContextHolder<'U> = fn ctxHolder
    /// <summary>
    /// Extracts the value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given predicate function,
    /// return the original <see cref="PatternContextHolder{T}"/> struct if the predicate function returns true, otherwise return ValueNone.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let filter<'T>(fn: 'T -> bool) (ctxHolder: PatternContextHolder<'T>) =
        if ctxHolder.Value |> fn
        then ValueSome ctxHolder
        else ValueNone
    /// <summary>
    /// Extracts the context and value from a <see cref="PatternContextHolder{T}"/> struct and passes it to the given predicate function,
    /// return the original <see cref="PatternContextHolder{T}"/> struct if the predicate function returns true, otherwise return ValueNone.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let filterc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctxHolder: PatternContextHolder<'T>) =
        if ctxHolder.Destructure ||> fn
        then ValueSome ctxHolder
        else ValueNone
    /// <summary>
    /// Extract the value from a <see cref="PatternContextHolder{T}"/> struct and pass it to the given function.
    /// Return the result in a tuple with the original <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let yieldBy<'T, 'U>(fn: 'T -> 'U) (ctxHolder: PatternContextHolder<'T>): 'U * _ =
        (ctxHolder.Value |> fn), ctxHolder
    /// <summary>
    /// Extract the context and value from a <see cref="PatternContextHolder{T}"/> struct and pass it to the given function.
    /// Return the result in a tuple with the original <see cref="PatternContextHolder{T}"/> struct.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let yieldByc<'T, 'U>(fn: KeyResolutionContext -> 'T -> 'U) (ctxHolder: PatternContextHolder<'T>): 'U * _ =
        (ctxHolder.Destructure ||> fn), ctxHolder
    /// <summary>
    /// Extract the value from a <see cref="PatternContextHolder{T}"/> struct and pass it to the given function.
    /// Return the original <see cref="PatternContextHolder{T}"/> struct in a tuple with the result of the function.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let flipYieldBy<'T, 'U>(fn: 'T -> 'U) (ctxHolder: PatternContextHolder<'T>): _ * 'U =
        ctxHolder, (ctxHolder.Value |> fn)
    /// <summary>
    /// Extract the context and value from a <see cref="PatternContextHolder{T}"/> struct and pass it to the given function.
    /// Return the original <see cref="PatternContextHolder{T}"/> struct in a tuple with the result of the function.
    /// </summary>
    /// <param name="fn"></param>
    /// <param name="ctxHolder"></param>
    let flipYieldByc<'T, 'U>(fn: KeyResolutionContext -> 'T -> 'U) (ctxHolder: PatternContextHolder<'T>): _ * 'U =
        ctxHolder, (ctxHolder.Destructure ||> fn)

    module Array =
        open Microsoft.FSharp.Collections
        /// <summary>
        /// Apply the function to each element of the array in the context holder and return the result
        /// in a new context holder.
        /// </summary>
        /// <param name="fn"></param>
        /// <param name="ctxHolder"></param>
        let map<'T, 'U> (fn: 'T -> 'U) (ctxHolder: PatternContextHolder<'T []>) =
            PatternContext.map (Array.map fn) ctxHolder
        /// <summary>
        /// Apply the function to each element of the array in the context holder and return the resulting
        /// array of elements that pass the predicate function in a new context holder.
        /// </summary>
        /// <param name="fn"></param>
        let filter<'T>(fn: 'T -> bool) =
            PatternContext.map (Array.filter fn)
        /// <summary>
        /// Apply the function to each element of the array in the context holder and return the resulting
        /// array of elements that pass the predicate function in a new context holder.
        /// </summary>
        /// <param name="fn"></param>
        let choose<'T, 'U> (fn: 'T -> 'U option): _ -> PatternContextHolder<'U []> =
            PatternContext.map (Array.choose fn)
        /// <summary>
        /// Apply the function to each element of the array, threading an accumulator, in the context holder, and return the resulting
        /// state in a new context holder.
        /// </summary>
        /// <param name="fn"></param>
        /// <param name="state"></param>
        let fold<'T, 'State> (fn: 'State -> 'T -> 'State) (state: 'State) =
            PatternContext.map (Array.fold fn state)
        let mapc<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U) =
            PatternContext.mapc (fun ctx -> Array.map (fn ctx))
        let cmap<'T, 'U> (fn: PatternContextHolder<'T> -> 'U) =
            PatternContext.mapc (fun ctx -> Array.map (prepare ctx >> fn))
        let filterc<'T>(fn: KeyResolutionContext -> 'T -> bool) =
            PatternContext.mapc (fun ctx -> Array.filter (fn ctx))
        let cfilter<'T>(fn: PatternContextHolder<'T> -> bool) =
            PatternContext.mapc (fun ctx -> Array.filter (prepare ctx >> fn))
        let choosec<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U option): _ -> PatternContextHolder<'U []> =
            PatternContext.mapc (fun ctx -> Array.choose (fn ctx))
        let cchoose<'T, 'U> (fn: PatternContextHolder<'T> -> 'U option): _ -> PatternContextHolder<'U []> =
            PatternContext.mapc (fun ctx -> Array.choose (prepare ctx >> fn))
        let foldc<'T, 'State> (fn: 'State -> KeyResolutionContext -> 'T -> 'State) (state: 'State) =
            PatternContext.mapc (fun ctx -> Array.fold (fun state -> fn state ctx) state)
        let cfold<'T, 'State> (fn: 'State -> PatternContextHolder<'T> -> 'State) (state: 'State): PatternContextHolder<'T[]> -> PatternContextHolder<'State> =
            PatternContext.mapc (fun ctx -> Array.fold (fun state ele -> fn state (prepare ctx ele)) state)
        let collect<'T, 'U> (fn: 'T -> 'U[]) =
            PatternContext.map (Array.collect fn)
        let collectc<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U[]) =
            PatternContext.mapc (fun ctx -> Array.collect (fn ctx))
        let ccollect<'T, 'U> (fn: PatternContextHolder<'T> -> 'U[]) =
            PatternContext.mapc (fun ctx -> Array.collect (prepare ctx >> fn))
        let unzip<'T> (ctx: PatternContextHolder<'T[]>): PatternContextHolder<'T>[] =
            ctx.Value |> Array.map (prepare ctx.Context)
        let zip<'T> (ctx: PatternContextHolder<'T>[]): PatternContextHolder<'T[]> voption =
            if ctx |> Array.isEmpty then ValueNone else
            ctx
            |> Array.map _.Value
            |> prepare ctx[0].Context
            |> ValueSome
        let exists<'T> (fn: 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.exists fn
        let existsc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.exists (fn ctx.Context)
        let cexists<'T> (fn: PatternContextHolder<'T> -> bool) (ctx: PatternContextHolder<'T[]>): bool =
            ctx.Value |> Array.exists (prepare ctx.Context >> fn)
        let contains<'T when 'T:equality>(v: 'T) (ctx: PatternContextHolder<'T[]>): bool =
            ctx.Value
            |> Array.contains v
        let ccontains<'T when 'T:equality>(v: PatternContextHolder<'T>) (ctx: PatternContextHolder<'T[]>): bool =
            ctx.Value
            |> Array.contains v.Value
        let forall<'T> (fn: 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.forall fn
        let forallc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.forall (fn ctx.Context)
        let cforall<'T> (fn: PatternContextHolder<'T> -> bool) (ctx: PatternContextHolder<'T[]>): bool =
            ctx.Value |> Array.forall (prepare ctx.Context >> fn)
        let tryPick<'T, 'U> (fn: 'T -> 'U voption) (ctx: PatternContextHolder<'T[]>): PatternContextHolder<'U> voption =
            ctx.Value
            |> Array.tryPick (fn >> ValueOption.toOption)
            |> ValueOption.ofOption
            |> ValueOption.map (prepare ctx.Context)
        let tryPickc<'T, 'U> (fn: KeyResolutionContext -> 'T -> 'U voption) (ctx: PatternContextHolder<'T[]>): PatternContextHolder<'U> voption =
            ctx.Value
            |> Array.tryPick (fn ctx.Context >> ValueOption.toOption)
            |> ValueOption.ofOption
            |> ValueOption.map (prepare ctx.Context)
        let ctryPick<'T, 'U> (fn: PatternContextHolder<'T> -> 'U voption) (ctx: PatternContextHolder<'T[]>): PatternContextHolder<'U> voption =
            ctx.Value
            |> Array.tryPick (prepare ctx.Context >> fn >> ValueOption.toOption)
            |> ValueOption.ofOption
            |> ValueOption.map (prepare ctx.Context)
        let find<'T> (fn: 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.find fn |> prepare ctx.Context
        let findc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.find (fn ctx.Context) |> prepare ctx.Context
        let cfind<'T> (fn: PatternContextHolder<'T> -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.find (prepare ctx.Context >> fn) |> prepare ctx.Context
        let tryFind<'T> (fn: 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFind fn |> ValueOption.ofOption |> ValueOption.map (prepare ctx.Context)
        let tryFindc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFind (fn ctx.Context) |> ValueOption.ofOption |> ValueOption.map (prepare ctx.Context)
        let ctryFind<'T> (fn: PatternContextHolder<'T> -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFind (prepare ctx.Context >> fn) |> ValueOption.ofOption |> ValueOption.map (prepare ctx.Context)
        let tryFindIndex<'T> (fn: 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFindIndex fn |> ValueOption.ofOption 
        let tryFindIndexc<'T>(fn: KeyResolutionContext -> 'T -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFindIndex (fn ctx.Context) |> ValueOption.ofOption 
        let ctryFindIndex<'T> (fn: PatternContextHolder<'T> -> bool) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value |> Array.tryFindIndex (prepare ctx.Context >> fn) |> ValueOption.ofOption
        let bind<'T, 'U> (fn: 'T -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value
            |> Array.map (fn >> value)
            |> prepare ctx.Context
        let bindc<'T, 'U> (fn: KeyResolutionContext -> 'T -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value
            |> Array.map (fn ctx.Context >> value)
            |> prepare ctx.Context
        let cbind<'T, 'U> (fn: PatternContextHolder<'T> -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
            ctx.Value
            |> Array.map (prepare ctx.Context >> fn >> value)
            |> prepare ctx.Context
        let item<'T> (index: int) (ctx: PatternContextHolder<'T[]>) =
            ctx |> PatternContext.map (Array.item index)
        let tryItem<'T> (index: int) (ctx: PatternContextHolder<'T[]>) =
            ctx |> PatternContext.choose (Array.tryItem index >> ValueOption.ofOption)
        let length<'T> (ctx: PatternContextHolder<'T[]>) = ctx.Value.Length
        module Indexed =
            let inline fold<'T, 'State> (fn: 'State -> int -> 'T -> 'State) (state: 'State) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.map (
                    Seq.indexed
                    >> Seq.fold (fun state (index, ele) -> fn state index ele) state
                ) ctx
            let inline foldc<'T, 'State> (fn: 'State -> int -> KeyResolutionContext -> 'T -> 'State) (state: 'State) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.mapc (fun ctx ->
                    Seq.indexed
                    >> Seq.fold (fun state (index, ele) -> fn state index ctx ele) state
                    ) ctx
            let inline cfold<'T, 'State> (fn: 'State -> int -> PatternContextHolder<'T> -> 'State) (state: 'State) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.mapc (fun ctx ->
                    Seq.indexed
                    >> Seq.fold (fun state (index, ele) -> fn state index (prepare ctx ele)) state
                    ) ctx
            let inline map<'T, 'U> (fn: int -> 'T -> 'U) =
                PatternContext.map (Array.mapi fn)
            let inline mapc<'T, 'U> (fn: KeyResolutionContext -> int -> 'T -> 'U) =
                PatternContext.mapc (fun ctx -> Array.mapi (fn ctx) )
            let inline cmap<'T, 'U> (fn: int -> PatternContextHolder<'T> -> 'U) (container: PatternContextHolder<'T[]>) =
                let ctx = container.Context
                PatternContext.map (Array.mapi (fun idx -> prepare ctx >> fn idx)) container
            let inline bind<'T, 'U>(fn: int -> 'T -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.map (Array.mapi (fun idx -> fn idx >> value)) ctx
            let inline bindc<'T, 'U>(fn: KeyResolutionContext -> int -> 'T -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.mapc (fun ctx -> Array.mapi (fun idx -> fn ctx idx >> value)) ctx
            let inline cbind<'T, 'U>(fn: int -> PatternContextHolder<'T> -> PatternContextHolder<'U>) (ctx: PatternContextHolder<'T[]>) =
                PatternContext.mapc (fun ctx -> Array.mapi (fun idx -> prepare ctx >> fn idx >> value)) ctx
            
let value = PatternContext.value
let (|Prepare|) ctx = PatternContext.prepare ctx
let (|Value|) (ctxHolder: PatternContextHolder<_>) = ctxHolder.Value
let (|Unzip|) (ctxHolder: PatternContextHolder<_ []>) =
    PatternContext.Array.unzip ctxHolder
let (|DestructureContext|): PatternContextHolder<_> -> _ = _.Destructure
module MasterKey =
    let rec masterBuilder { Context = ctx; Value = master } =
        // Failures caused at this point are usually related
        // to keys not being created with the contexts helper functions
        // which automatically cache the keys, or not being paired
        // with their respective masterkey values.
        match Dictionary.item master ctx.cache.masters with
        | MasterBuilder.TypeKey typeKey ->
            Resolve.tryResolveMasterBuilder ctx typeKey
            |> ValueOption.defaultWith(fun _ ->
                failwith "Could not resolve master builder for type key")
            |> PatternContext.prepare ctx
        | MasterBuilder.Cyclic cyclicKey ->
            MasterBuilder.Cyclic cyclicKey
            |> KeyNodeHashing.MasterKey.create
            |> Dictionary.Flip.tryItem ctx.cache.cyclicRemaps
            |> ValueOption.map (PatternContext.prepare ctx >> masterBuilder)
            |> ValueOption.defaultWith (fun () -> failwith $"Could not resolve master builder for cyclic key: {cyclicKey} (master: {master})")
        | MasterBuilder.HashType hashTypeKey ->
            Dictionary.tryItem hashTypeKey ctx.cache.typeToMasterKeys
            |> ValueOption.map (PatternContext.prepare ctx >> masterBuilder)
            |> ValueOption.defaultWith (fun () -> failwith "Could not resolve master builder for hash key")
        | passThrough -> PatternContext.prepare ctx passThrough
    let resolveIndirection value = value |> masterBuilder |> PatternContext.mapc _.createMasterKey
    let visitationFlag value = value |> PatternContext.mapc (fun ctx -> Dictionary.Flip.tryItem ctx.cache.visitationFlags >> ValueOption.defaultValue VisitationFlags.None)
    let documentation value = PatternContext.mapc (fun ctx -> Dictionary.Flip.tryItem ctx.cache.documentation >> ValueOption.defaultValue Array.empty) value
    let documentationOrNull value =
        let result = documentation value |> PatternContext.value
        if Array.isEmpty result
        then ValueNone
        else ValueSome result
    let associatedLiterals value =
        PatternContext.choosec (fun ctx ->
            ctx.cache.literalAssociations.GetLiteralKeys
            >> ValueOption.filter (Array.isEmpty >> not)) value
    let members value = PatternContext.choosec (fun ctx -> Dictionary.Flip.tryItem ctx.cache.memberIndex) value
    let isNullish value =
        match value with
        | ResolveIndirection (Primitive.IsNullish | KeyType.Literal Literal.Null) -> true
        | _ -> false
    let isNullable value =
        match value with
        | ResolveIndirection (Primitive.IsNullable | KeyType.Literal Literal.Null) -> true
        | _ -> false
    let containsNullish value =
        match value with
        | ResolveIndirection value ->
            match value with
            | IsNullish
            | KeyType.Union (Union.Types Array.MasterKey.ContainsNullish)
            | KeyType.TypeAlias (TypeAlias.Type ContainsNullish) -> true
            | _ -> false
    let hasSourceKey value =
        match value with
        | KeyType.Class (Class.Source sourceKey)
        | KeyType.Interface (Interface.Source sourceKey)
        | KeyType.Enum (Enum.Source sourceKey)
        | KeyType.Function (Function.SourceKey sourceKey)
        | KeyType.Variable (Variable.Source sourceKey)
        | KeyType.EnumCase (EnumCase.ParentEnum (Enum.Source sourceKey))
        | KeyType.TypeAlias (TypeAlias.Source sourceKey) when sourceKey.Value <> SourceKey.nullKey -> ValueSome sourceKey
        | _ -> ValueNone
    let hasSource value = hasSourceKey value |> ValueOption.map (PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.sourceKeys))
    let hasNameKey value =
        match value with
        | KeyType.Class (Class.NameKey nameKey)
        | KeyType.Interface (Interface.NameKey nameKey)
        | KeyType.Enum (Enum.NameKey nameKey)
        | KeyType.Function (Function.NameKey nameKey)
        | KeyType.Variable (Variable.NameKey nameKey)
        | KeyType.EnumCase (EnumCase.NameKey nameKey)
        | KeyType.TypeAlias (TypeAlias.NameKey nameKey)
        | KeyType.Module (Module.NameKey nameKey)
        | KeyType.Parameter (Parameter.NameKey nameKey)
        | KeyType.Name (Name.ToNameKey nameKey)
        | KeyType.TypeParameter (TypeParameter.NameKey nameKey)
        | KeyType.TupleElement (TupleElement.NameKey nameKey)
        | KeyType.MemberKey (MemberKey.HasNameKey nameKey) -> ValueSome nameKey
        | _ -> ValueNone
    let hasName value =
        match value with
        | HasNameKey (NameKey.ToName name) -> ValueSome name
        | _ -> ValueNone
    let hasQualifiers value =
        match value with
        | KeyType.Class (Class.Qualifiers qualifiers)
        | KeyType.Interface (Interface.Qualifiers qualifiers)
        | KeyType.Enum (Enum.Qualifiers qualifiers)
        | KeyType.Function (Function.Qualifiers qualifiers)
        | KeyType.Variable (Variable.Qualifiers qualifiers)
        | KeyType.EnumCase (EnumCase.ParentEnum (Enum.Qualifiers qualifiers))
        | KeyType.TypeAlias (TypeAlias.Qualifiers qualifiers)
        | KeyType.Module (Module.Qualifiers qualifiers) -> ValueSome qualifiers
        | _ -> ValueNone
    let (|MasterBuilder|) value = masterBuilder value
    let (|ResolveIndirection|) value = resolveIndirection value
    let (|ResolveMasterKey|) = (|ResolveIndirection|)
    let (|VisitationFlag|) value = visitationFlag value
    /// <summary>
    /// </summary>
    /// <remarks>
    /// The presence or absence of documentation is not used as to filter our rendering, so we do not
    /// return an option here, but instead will default to an empty array.
    /// </remarks>
    let (|Documentation|) = documentation
    let (|AssociatedLiterals|_|) = associatedLiterals
    let (|Members|_|) = members
    let (|IsNullish|_|) (value: PatternContextHolder<MasterKey>): bool = isNullish value
    let (|IsNullable|_|) value = isNullable value
    let (|ContainsNullish|_|): _ -> bool = containsNullish
    let (|HasSourceKey|_|) value = hasSourceKey value
    let (|HasSource|_|) value = hasSource value
    let (|HasNameKey|_|): PatternContextHolder<MasterKey> -> PatternContextHolder<NameKey> voption = hasNameKey
    let (|HasName|_|) = hasName
    let (|HasQualifiers|_|) = hasQualifiers
    let (|IsDeclaration|IsTypeNode|IsNode|) = function
        | MasterBuilder builder ->
            match builder with
            | MasterBuilders.Class _
            | MasterBuilders.Interface _
            | MasterBuilders.Enum _
            | MasterBuilders.Function _
            | MasterBuilders.Variable _
            | MasterBuilders.EnumCase _
            | MasterBuilders.TypeAlias _
            | MasterBuilders.Module _ -> IsDeclaration
            | MasterBuilders.Predicate _
            | MasterBuilders.Array _
            | MasterBuilders.Tuple _
            | MasterBuilders.TypeLiteral _
            | MasterBuilders.Index _
            | MasterBuilders.Conditional _
            | MasterBuilders.Intersection _
            | MasterBuilders.Union _
            | MasterBuilders.TypeReference _
            | MasterBuilders.Global
            | MasterBuilders.Primitive _
            | MasterBuilders.Constructor _
            | MasterBuilders.Member _
            | MasterBuilders.IndexAccess _ -> IsTypeNode
            | _ -> IsNode
    module VisitationFlags =
        let has flags = visitationFlag >> Patterns.VisitationFlags.has flags
        let matchesMask mask = visitationFlag >> Patterns.VisitationFlags.matchesMask mask
        let isEsLib = has VisitationFlags.IsEsLib
        let isObsolete = has VisitationFlags.IsObsolete
        let yieldsLiterals = matchesMask VisitationFlags.WouldYieldLiteral
        let yieldsStrings = has VisitationFlags.WouldYieldString
        let yieldsInts = has VisitationFlags.WouldYieldInt
        let yieldsFloats = has VisitationFlags.WouldYieldFloat
        let yieldsBools = has VisitationFlags.WouldYieldBool
        let yieldsNull = has VisitationFlags.WouldYieldNull
        let isMaybeParamObject = has VisitationFlags.MaybeParamObject
        let hasTypeParameters = has VisitationFlags.HasTypeParameters
        let isTypeParameter = has VisitationFlags.IsTypeParameter
        let hasMembers = has VisitationFlags.HasMembers
        let hasName = has VisitationFlags.HasName
        let hasSource = has VisitationFlags.HasSource
        let isSymbolMember = has VisitationFlags.IsSymbolMember
        let isGeneric value = isTypeParameter value && not (has VisitationFlags.IsConstrained value) && has VisitationFlags.IsGeneric value
        let isConstrained value = has VisitationFlags.IsConstrained value && isTypeParameter value
        let isParamArray value = has VisitationFlags.ParamArray value
        let isFullyQualified value = has VisitationFlags.IsFullyQualified value
        let (|Has|_|) flags: PatternContextHolder<MasterKey> -> bool = has flags
        let (|MatchesMask|_|) mask: PatternContextHolder<MasterKey> -> bool = matchesMask mask
        let (|IsEsLib|_|): _ -> bool = isEsLib
        let (|IsObsolete|_|): _ -> bool = isObsolete
        let (|YieldsLiterals|_|): _ -> bool = yieldsLiterals
        let (|YieldsStrings|_|): _ -> bool = yieldsStrings
        let (|YieldsInt|_|): _ -> bool = yieldsInts
        let (|YieldsFloats|_|): _ -> bool = yieldsFloats
        let (|YieldsBool|_|): _ -> bool = yieldsBools
        let (|YieldsNull|_|): _ -> bool = yieldsNull
        let (|YieldsOnlyStrings|YieldsOnlyInts|YieldsOnlyFloats|YieldsOnlyBools|YieldsMixedLiterals|YieldsNoLiterals|) = function
            | VisitationFlag flags ->
                match flags with
                | Patterns.VisitationFlags.YieldsOnlyStrings -> YieldsOnlyStrings
                | Patterns.VisitationFlags.YieldsOnlyInts -> YieldsOnlyInts
                | Patterns.VisitationFlags.YieldsOnlyFloats -> YieldsOnlyFloats
                | Patterns.VisitationFlags.YieldsOnlyBools -> YieldsOnlyBools
                | Patterns.VisitationFlags.YieldsMixedLiterals -> YieldsMixedLiterals
                | Patterns.VisitationFlags.YieldsNoLiterals -> YieldsNoLiterals
        let (|IsMaybeParamObject|_|): _ -> bool = (|Has|_|) VisitationFlags.MaybeParamObject
        let (|HasTypeParameters|_|): _ -> bool = (|Has|_|) VisitationFlags.HasTypeParameters
        let (|IsTypeParameter|_|): _ -> bool = (|Has|_|) VisitationFlags.IsTypeParameter
        let (|HasMembers|_|): _ -> bool = (|Has|_|) VisitationFlags.HasMembers
        let (|HasName|_|): _ -> bool = (|Has|_|) VisitationFlags.HasName
        let (|HasSource|_|): _ -> bool = (|Has|_|) VisitationFlags.HasSource
        let (|IsSymbolMember|_|): _ -> bool = (|Has|_|) VisitationFlags.IsSymbolMember
        let (|IsGeneric|_|): _ -> bool = isGeneric
        let (|IsConstrained|_|): _ -> bool = isConstrained
        let (|IsParamArray|_|): _ -> bool = isParamArray
        let (|IsFullyQualified|_|): _ -> bool = isFullyQualified
    let primitives: PatternContextHolder<MasterKey> -> bool = function
        | Value key when Prelude.Primitive.Master.primitives.Contains key -> true
        | Primitive.Boolean -> true
        | _ -> false
    let (|Primitives|_|): PatternContextHolder<MasterKey> -> bool = primitives
    module Primitive =
        let any = value >> ((=) Prelude.Primitive.Master.anyKey)
        let unknown = value >> ((=) Prelude.Primitive.Master.unknownKey)
        let never = value >> ((=) Prelude.Primitive.Master.neverKey)
        let void' = value >> ((=) Prelude.Primitive.Master.voidKey)
        let undefined = value >> ((=) Prelude.Primitive.Master.undefinedKey)
        let null' = value >> ((=) Prelude.Primitive.Master.nullKey)
        let string = value >> ((=) Prelude.Primitive.Master.stringKey)
        let integer = value >> ((=) Prelude.Primitive.Master.integerKey)
        let number = value >> ((=) Prelude.Primitive.Master.numberKey)
        let boolean = function
            | Value key when key = Prelude.Primitive.Master.booleanKey -> true
            | Value key when Prelude.Union.Master.booleans.Contains key -> true
            | ResolveMasterKey (KeyType.Union Union.IsBoolean) -> true
            | _ -> false
        let bigint = value >> ((=) Prelude.Primitive.Master.bigIntKey)
        let essymbol = value >> ((=) Prelude.Primitive.Master.esSymbolKey)
        let nonPrimitive = value >> ((=) Prelude.Primitive.Master.nonPrimitiveKey)
        let isNullable: PatternContextHolder<MasterKey> -> bool = function
            | Primitive.Any
            | Primitive.Unknown
            | Primitive.Never
            | Primitive.Void
            | Primitive.Undefined
            | Primitive.Null -> true
            | _ -> false
        let isNullish: PatternContextHolder<MasterKey> -> bool = function
            | Primitive.Null
            | Primitive.Void
            | Primitive.Undefined -> true
            | _ -> false
        let (|Any|_|): _ -> bool = any
        let (|Unknown|_|): _ -> bool = unknown
        let (|Never|_|): PatternContextHolder<MasterKey> -> bool = never
        let (|Void|_|): PatternContextHolder<MasterKey> -> bool = void'
        let (|Undefined|_|): PatternContextHolder<MasterKey> -> bool = undefined
        let (|Null|_|): PatternContextHolder<MasterKey> -> bool = null'
        let (|String|_|): PatternContextHolder<MasterKey> -> bool = string
        let (|Integer|_|): PatternContextHolder<MasterKey> -> bool = integer
        let (|Number|_|): PatternContextHolder<MasterKey> -> bool = number
        let (|Boolean|_|): PatternContextHolder<MasterKey> -> bool = boolean
        let (|BigInt|_|): PatternContextHolder<MasterKey> -> bool = bigint
        let (|ESSymbol|_|): PatternContextHolder<MasterKey> -> bool = essymbol
        let (|NonPrimitive|_|): PatternContextHolder<MasterKey> -> bool = nonPrimitive
        let (|IsNullable|_|): PatternContextHolder<MasterKey> -> bool = isNullable
        let (|IsNullish|_|): PatternContextHolder<MasterKey> -> bool = isNullish
    module KeyType =
        let (|Global|_|) = function
            | MasterBuilder MasterBuilders.Global -> true
            | _ -> false
        [<return: Struct>]
        let (|Array|_|) = function
            | MasterBuilder (MasterBuilders.Array value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Union|_|) = function
            | MasterBuilder (MasterBuilders.Union value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Name|_|) = function
            | MasterBuilder (MasterBuilders.Name value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Source|_|) = function
            | MasterBuilder (MasterBuilders.Source value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Parameter|_|) = function
            | MasterBuilder (MasterBuilders.Parameter value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|TypeParameter|_|) = function
            | MasterBuilder (MasterBuilders.TypeParameter value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Primitive|_|) = (|MasterBuilder|) >> PatternContext.choose (function MasterBuilder.Primitive value -> ValueSome value | _ -> ValueNone)
        [<return: Struct>]
        let (|EnumCase|_|) = function
            | MasterBuilder (MasterBuilders.EnumCase value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Interface|_|) = function
            | MasterBuilder (MasterBuilders.Interface value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Member|_|): PatternContextHolder<MasterKey> -> PatternContextHolder<MemberBuilder> voption = function
            | MasterBuilder (MasterBuilders.Member value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|MemberKey|_|) = function
            | MasterBuilder (MasterBuilders.MemberKey value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Constructor|_|) = function
            | MasterBuilder (MasterBuilders.Constructor value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|TupleElement|_|) = function
            | MasterBuilder (MasterBuilders.TupleElement value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Variable|_|) = function
            | MasterBuilder (MasterBuilders.Variable value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Conditional|_|) = function
            | MasterBuilder (MasterBuilders.Conditional value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|TypeLiteral|_|) = function
            | MasterBuilder (MasterBuilders.TypeLiteral value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|IndexAccess|_|) = function
            | MasterBuilder (MasterBuilders.IndexAccess value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Index|_|) = function
            | MasterBuilder (MasterBuilders.Index value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Predicate|_|) = function
            | MasterBuilder (MasterBuilders.Predicate value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Intersection|_|) = function
            | MasterBuilder (MasterBuilders.Intersection value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Literal|_|) = function
            | MasterBuilder (MasterBuilders.Literal value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Tuple|_|) = function
            | MasterBuilder (MasterBuilders.Tuple value) -> ValueSome value
            | _ -> ValueNone
        let (|TypeReference|_|) = function
            | MasterBuilder (MasterBuilders.TypeReference value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Class|_|) = function
            | MasterBuilder (MasterBuilders.Class value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Enum|_|) = function
            | MasterBuilder (MasterBuilders.Enum value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Function|_|) = function
            | MasterBuilder (MasterBuilders.Function value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|Module|_|) = function
            | MasterBuilder (MasterBuilders.Module value) -> ValueSome value
            | _ -> ValueNone
        [<return: Struct>]
        let (|TypeAlias|_|) = function
            | MasterBuilder (MasterBuilders.TypeAlias value) -> ValueSome value
            | _ -> ValueNone
        // INVALID
        // [<return: Struct>]
        // let (|Cyclic|_|) = function
        //     | MasterBuilder (MasterBuilders.Cyclic value) -> ValueSome value
        //     | _ -> ValueNone
module MasterBuilders =
    let (|Global|_|) { Value = builder }: bool = match builder with MasterBuilder.Global -> true | _ -> false
    [<return: Struct>]
    let (|Array|_|) = PatternContext.choosec (fun _ctx -> function MasterBuilder.Array value -> ValueSome value | _ -> ValueNone)
    [<return: Struct>]
    let (|Union|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Union i -> ctx.cache.unionKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Name|_|): PatternContextHolder<MasterBuilder> -> PatternContextHolder<string> voption = PatternContext.choosec (fun ctx -> function MasterBuilder.Name key -> ctx.cache.nameKeys[key] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Source|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Source key -> ctx.cache.sourceKeys[key] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Parameter|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Parameter i -> ctx.cache.parameterKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|TypeParameter|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.TypeParameter i -> ctx.cache.typeParameterKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Primitive|_|) = PatternContext.choose (function MasterBuilder.Primitive value -> ValueSome value | _ -> ValueNone)
    [<return: Struct>]
    let (|EnumCase|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.EnumCase i -> ctx.cache.enumCaseKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Interface|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Interface i -> ctx.cache.interfaceKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Member|_|): PatternContextHolder<MasterBuilder> -> PatternContextHolder<MemberBuilder> voption = PatternContext.choosec (fun ctx -> function MasterBuilder.Member i -> ctx.cache.members[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|MemberKey|_|) = PatternContext.choose (function MasterBuilder.Member i -> ValueSome i | _ -> ValueNone)
    [<return: Struct>]
    let (|Constructor|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Constructor i -> ctx.cache.constructorKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|TupleElement|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.TupleElement i -> ctx.cache.tupleElementKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Variable|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Variable i -> ctx.cache.variableKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Conditional|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Conditional i -> ctx.cache.conditionalKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|TypeLiteral|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.TypeLiteral i -> ctx.cache.typeLiteralKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|IndexAccess|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.IndexAccess i -> ctx.cache.indexAccessKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Index|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Index i -> ctx.cache.indexKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Predicate|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Predicate i -> ctx.cache.predicateKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Intersection|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Intersection i -> ctx.cache.intersectionKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Literal|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Literal i -> ctx.cache.literalKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Tuple|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Tuple i -> ctx.cache.tupleKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|TypeReference|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.TypeReference i -> ctx.cache.typeReferenceKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Class|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Class i -> ctx.cache.classKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Enum|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Enum i -> ctx.cache.enumKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Function|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Function i -> ctx.cache.functionKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|Module|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.Module i -> ctx.cache.moduleKeys[i] |> ValueSome | _ -> ValueNone)
    [<return: Struct>]
    let (|TypeAlias|_|) = PatternContext.choosec (fun ctx -> function MasterBuilder.TypeAlias i -> ctx.cache.typeAliasKeys[i] |> ValueSome | _ -> ValueNone)
    // INVALID
    // [<return: Struct>]
    // let (|Cyclic|_|) = PatternContext.choosec (fun ctx -> function
    //     | MasterBuilder.Cyclic i -> CyclicKey.toHashTypeKey i |> fun i -> ctx.cache.typeToMasterKeys[i] |> ValueSome
    //     | _ -> ValueNone)
module VisitationFlags =
    let has flags = PatternContext.value >> _.HasFlag(flags)
    let matchesMask (mask: VisitationFlags) = PatternContext.value >> ((&&&) mask) >> (<>) VisitationFlags.None
    let (|Has|_|) flags: PatternContextHolder<VisitationFlags> -> bool = function
        | Value value -> value.HasFlag(flags)
    let (|MatchesMask|_|) mask: PatternContextHolder<VisitationFlags> -> bool = function
        | Value value -> (value &&& mask) <> VisitationFlags.None
    let (|IsEsLib|_|): _ -> bool = (|Has|_|) VisitationFlags.IsEsLib
    let (|IsObsolete|_|): _ -> bool = (|Has|_|) VisitationFlags.IsObsolete
    let (|YieldsLiterals|_|): _ -> bool = (|MatchesMask|_|) VisitationFlags.WouldYieldLiteral
    let (|YieldsStrings|_|): _ -> bool = (|Has|_|) VisitationFlags.WouldYieldString
    let (|YieldsInt|_|): _ -> bool = (|Has|_|) VisitationFlags.WouldYieldInt
    let (|YieldsFloats|_|): _ -> bool = (|Has|_|) VisitationFlags.WouldYieldFloat
    let (|YieldsBool|_|): _ -> bool = (|Has|_|) VisitationFlags.WouldYieldBool
    let (|YieldsNull|_|): _ -> bool = (|Has|_|) VisitationFlags.WouldYieldNull
    let (|YieldsOnlyStrings|YieldsOnlyInts|YieldsOnlyFloats|YieldsOnlyBools|YieldsMixedLiterals|YieldsNoLiterals|) = function
        | Value value ->
            let mask = VisitationFlags.WouldYieldString ||| VisitationFlags.WouldYieldInt ||| VisitationFlags.WouldYieldFloat ||| VisitationFlags.WouldYieldBool
            let value = value &&& mask
            if value = VisitationFlags.None then
                YieldsNoLiterals
            elif value = VisitationFlags.WouldYieldString then
                YieldsOnlyStrings
            elif value = VisitationFlags.WouldYieldInt then
                YieldsOnlyInts
            elif value = VisitationFlags.WouldYieldFloat then
                YieldsOnlyFloats
            elif value = VisitationFlags.WouldYieldBool then
                YieldsOnlyBools
            else YieldsMixedLiterals
    let (|IsMaybeParamObject|_|): _ -> bool = (|Has|_|) VisitationFlags.MaybeParamObject
    let (|HasTypeParameters|_|): _ -> bool = (|Has|_|) VisitationFlags.HasTypeParameters
    let (|IsTypeParameter|_|): _ -> bool = (|Has|_|) VisitationFlags.IsTypeParameter
    let (|HasMembers|_|): _ -> bool = (|Has|_|) VisitationFlags.HasMembers
    let (|HasName|_|): _ -> bool = (|Has|_|) VisitationFlags.HasName
    let (|HasSource|_|): _ -> bool = (|Has|_|) VisitationFlags.HasSource
    let (|IsSymbolMember|_|): _ -> bool = (|Has|_|) VisitationFlags.IsSymbolMember
    let (|IsGeneric|_|): _ -> bool = function
        | IsTypeParameter & Has VisitationFlags.IsConstrained -> false
        | IsTypeParameter & Has VisitationFlags.IsGeneric -> true
        | _ -> false
    let (|IsConstrained|_|): _ -> bool = function
        | IsTypeParameter & Has VisitationFlags.IsConstrained -> true
        | _ -> false
    let (|IsParamArray|_|): _ -> bool = (|Has|_|) VisitationFlags.ParamArray
    let (|IsFullyQualified|_|): _ -> bool = (|Has|_|) VisitationFlags.IsFullyQualified
module HashTypeKey =
    let (|ToMasterKey|) =
        PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.typeToMasterKeys)
        >> MasterKey.(|ResolveIndirection|)
module Name =
    let (|ToLiteral|) (name: PatternContextHolder<string>) =
        name |> PatternContext.map TsLiteral.String
    let (|ToLiteralKey|) (name: PatternContextHolder<string>): PatternContextHolder<LiteralKey> =
        match name with ToLiteral (Literal.ToLiteralKey key) -> key
    let (|ToLiteralMasterKey|) (name: PatternContextHolder<string>): PatternContextHolder<MasterKey> =
        match name with ToLiteralKey (LiteralKey.ToMasterKey key) -> key
    let (|ToNameKey|) (name: PatternContextHolder<string>): PatternContextHolder<NameKey> = name |> PatternContext.mapc _.createNameKey
    let (|ToMasterKey|) = (|ToNameKey|) >> PatternContext.mapc (fun ctx -> MasterBuilder.Name >> ctx.createMasterKey)
module NameKey =
    let (|ToName|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.nameKeys)
    let (|ToLiteralKey|) = (|ToName|) >> Name.(|ToLiteralKey|)
    let (|ToLiteralMasterKey|) = (|ToName|) >> Name.(|ToLiteralMasterKey|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Name >> ctx.createMasterKey)
module KeyType =
    let inline name<^T when ^T:(member Name: NameKey)> (value: PatternContextHolder<^T>) =
        Dictionary.item value.Value.Name value.Context.cache.nameKeys |> PatternContext.prepare value.Context
    let inline nameKey<^T when ^T:(member Name: NameKey)> (value: PatternContextHolder<^T>) = value.Value.Name |> PatternContext.prepare value.Context
    let inline (|Name|) value = name value 
    let inline (|NameKey|) value = nameKey value
module Literal =
    let (|ToLiteralKey|) (literal: PatternContextHolder<TsLiteral>): PatternContextHolder<LiteralKey> = literal |> PatternContext.mapc _.createLiteralKey
    let (|ToMasterKey|) = (|ToLiteralKey|) >> PatternContext.mapc (fun ctx -> MasterBuilder.Literal >> ctx.createMasterKey)
    let (|String|Int|Bool|Null|BigInt|Float|): PatternContextHolder<TsLiteral> -> _ = function
        | Value value ->
        match value with
        | TsLiteral.String s -> String s
        | TsLiteral.Int i -> Int i
        | TsLiteral.Bool b -> Bool b
        | TsLiteral.Null -> Null
        | TsLiteral.Float value -> Float value
        | TsLiteral.BigInt value -> BigInt value
module LiteralKey =
    let (|ToLiteral|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.literalKeys)
    let toLiteral = (|ToLiteral|)
    let (|String|Int|Bool|Null|BigInt|Float|) = function
        | ToLiteral key ->
            match key with
            | Literal.String s -> String s
            | Literal.Int i -> Int i
            | Literal.Bool b -> Bool b
            | Literal.Null -> Null
            | Literal.BigInt value -> BigInt value
            | Literal.Float value -> Float value
    let (|ToMasterKey|) (literalKey: PatternContextHolder<LiteralKey>): PatternContextHolder<MasterKey> =
        literalKey |> PatternContext.mapc (fun ctx -> MasterBuilder.Literal >> ctx.createMasterKey)
module MemberKey =
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Member >> ctx.createMasterKey)
    let (|ToMemberBuilder|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.members)
    let (|MethodKey|CallSignatureKey|IndexSignatureKey|ConstructSignatureKey|PropertyKey|GetAccessorKey|SetAccessorKey|) { Context = ctx; Value = memberKey: MemberKey } =
        match ctx[memberKey] with
        | MemberBuilder.Method i -> MethodKey (PatternContext.prepare ctx i)
        | MemberBuilder.CallSignature i -> CallSignatureKey (PatternContext.prepare ctx i)
        | MemberBuilder.ConstructSignature i -> ConstructSignatureKey (PatternContext.prepare ctx i)
        | MemberBuilder.Property i -> PropertyKey (PatternContext.prepare ctx i)
        | MemberBuilder.GetAccessor i -> GetAccessorKey (PatternContext.prepare ctx i)
        | MemberBuilder.SetAccessor i -> SetAccessorKey (PatternContext.prepare ctx i)
        | MemberBuilder.IndexSignature i -> IndexSignatureKey (PatternContext.prepare ctx i)
    let (|Method|CallSignature|IndexSignature|ConstructSignature|Property|GetAccessor|SetAccessor|) = function
        | MethodKey key ->
            Method(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.methodKeys))
        | CallSignatureKey key ->
            CallSignature(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.callSignatureKeys))
        | ConstructSignatureKey key ->
            ConstructSignature(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.constructSignatureKeys))
        | PropertyKey key ->
            Property(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.propertyKeys))
        | GetAccessorKey key ->
            GetAccessor(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.getAccessorKeys))
        | SetAccessorKey key ->
            SetAccessor(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.setAccessorKeys))
        | IndexSignatureKey key ->
            IndexSignature(key |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.indexSignatureKeys))
    let inline (|HasName|_|) (value: PatternContextHolder<MemberKey>): PatternContextHolder<string> voption =
        match value with
        | GetAccessor (KeyType.Name name)
        | SetAccessor (KeyType.Name name)
        | Property (KeyType.Name name)
        | Method (KeyType.Name name) -> ValueSome name
        | _ -> ValueNone
    let (|HasNameKey|_|): PatternContextHolder<MemberKey> -> PatternContextHolder<NameKey> voption = function
        | GetAccessor (KeyType.NameKey name)
        | SetAccessor (KeyType.NameKey name)
        | Property (KeyType.NameKey name)
        | Method (KeyType.NameKey name) -> ValueSome name
        | _ -> ValueNone
    let toMemberBuilder = (|ToMemberBuilder|)
    let toMasterKey = (|ToMasterKey|)
    let isMethod = function MethodKey _ -> true | _ -> false
    let isProperty = function PropertyKey _ -> true | _ -> false
    let isGetAccessor = function GetAccessorKey _ -> true | _ -> false
    let isSetAccessor = function SetAccessorKey _ -> true | _ -> false
    let isCallSignature = function CallSignatureKey _ -> true | _ -> false
    let isIndexSignature = function IndexSignatureKey _ -> true | _ -> false
    let isConstructSignature = function ConstructSignatureKey _ -> true | _ -> false
    let hasName = (|HasNameKey|_|) >> ValueOption.isSome
module IndexSignature =
    let toIndexSignatureKey = PatternContext.mapc _.createIndexSignatureKey
    let toMemberKey = toIndexSignatureKey >> PatternContext.mapc (fun ctx -> MemberBuilder.IndexSignature >> ctx.createMemberKey)
    let toMasterKey: PatternContextHolder<KeyIndexSignature> -> PatternContextHolder<MasterKey> = toMemberKey >> PatternContext.mapc (fun ctx -> MasterBuilder.Member >> ctx.createMasterKey)
    let parameters: PatternContextHolder<KeyIndexSignature> -> _ = PatternContext.mapc (fun ctx -> _.Parameters >> Array.map (Dictionary.Flip.item ctx.cache.parameterKeys))
    let isReadOnly: PatternContextHolder<KeyIndexSignature> -> _ = PatternContext.map _.IsReadonly
    let type': PatternContextHolder<KeyIndexSignature> -> _ = PatternContext.map _.Type
    let (|ToIndexSignatureKey|) = toIndexSignatureKey
    let (|ToMemberKey|) = toMemberKey
    let (|ToMasterKey|) = toMasterKey
    let (|Parameters|) = parameters
    let (|IsReadOnly|) = isReadOnly
    let (|Type|) = type'
module ConstructSignature =
    let toConstructSignatureKey = PatternContext.mapc _.createConstructSignatureKey
    let toMemberBuilder = toConstructSignatureKey >> PatternContext.map MemberBuilder.ConstructSignature
    let toMemberKey = toMemberBuilder >> PatternContext.mapc _.createMemberKey
    let toMasterBuilder = toMemberKey >> PatternContext.map MasterBuilder.Member
    let toMasterKey = toMasterBuilder >> PatternContext.mapc _.createMasterKey
    let parameters (value: PatternContextHolder<KeyConstructSignature>) =
        value |> PatternContext.mapc (fun ctx -> _.Parameters >> Array.map (Dictionary.Flip.item ctx.cache.parameterKeys))
    let type' (value: PatternContextHolder<KeyConstructSignature>) =
        value |> PatternContext.map _.Type
    let (|ToConstructSignatureKey|) = toConstructSignatureKey
    let (|ToMemberBuilder|) = toMemberBuilder
    let (|ToMemberKey|) = toMemberKey
    let (|ToMasterBuilder|) = toMasterBuilder
    let (|ToMasterKey|) = toMasterKey
    let (|Parameters|) = parameters
    let (|Type|) = type'
module GetAccessor =
    let toGetAccessorKey = PatternContext.mapc _.createGetAccessorKey
    let toMemberBuilder = toGetAccessorKey >> PatternContext.map MemberBuilder.GetAccessor
    let toMemberKey = toMemberBuilder >> PatternContext.mapc _.createMemberKey
    let toMasterBuilder = toMemberKey >> PatternContext.map MasterBuilder.Member
    let toMasterKey = toMasterBuilder >> PatternContext.mapc _.createMasterKey
    let type' (value: PatternContextHolder<KeyGetAccessor>) = value |> PatternContext.map _.Type
    let nameKey (value: PatternContextHolder<KeyGetAccessor>) = value |> PatternContext.map _.Name
    let name (value: PatternContextHolder<KeyGetAccessor>) = nameKey value |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.nameKeys)
    let isStatic (value: PatternContextHolder<KeyGetAccessor>) = value.Value.IsStatic
    let isPrivate (value: PatternContextHolder<KeyGetAccessor>) = value.Value.IsPrivate
    let (|ToGetAccessorKey|) = toGetAccessorKey
    let (|ToMemberBuilder|) = toMemberBuilder
    let (|ToMemberKey|) = toMemberKey
    let (|ToMasterBuilder|) = toMasterBuilder
    let (|ToMasterKey|) = toMasterKey
    let (|Type|) = type'
    let (|NameKey|) = nameKey
    let (|Name|) = name
    let (|IsStatic|_|): PatternContextHolder<KeyGetAccessor> -> bool = isStatic
    let (|IsPrivate|_|): PatternContextHolder<KeyGetAccessor> -> bool = isPrivate
module SetAccessor =
    let toSetAccessorKey = PatternContext.mapc _.createSetAccessorKey
    let toMemberBuilder = toSetAccessorKey >> PatternContext.map MemberBuilder.SetAccessor
    let toMemberKey = toMemberBuilder >> PatternContext.mapc _.createMemberKey
    let toMasterBuilder = toMemberKey >> PatternContext.map MasterBuilder.Member
    let toMasterKey = toMasterBuilder >> PatternContext.mapc _.createMasterKey
    let type' (value: PatternContextHolder<KeySetAccessor>) = value |> PatternContext.map _.Type
    let nameKey (value: PatternContextHolder<KeySetAccessor>) = value |> PatternContext.map _.Name
    let name (value: PatternContextHolder<KeySetAccessor>) = nameKey value |> PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.nameKeys)
    let isStatic (value: PatternContextHolder<KeySetAccessor>) = value.Value.IsStatic
    let isPrivate (value: PatternContextHolder<KeySetAccessor>) = value.Value.IsPrivate
    let (|ToSetAccessorKey|) = toSetAccessorKey
    let (|ToMemberBuilder|) = toMemberBuilder
    let (|ToMemberKey|) = toMemberKey
    let (|ToMasterBuilder|) = toMasterBuilder
    let (|ToMasterKey|) = toMasterKey
    let (|Type|) = type'
    let (|NameKey|) = nameKey
    let (|Name|) = name
    let (|IsStatic|_|): PatternContextHolder<KeySetAccessor> -> bool = isStatic
    let (|IsPrivate|_|): PatternContextHolder<KeySetAccessor> -> bool = isPrivate
module Property =
    let (|Type|): PatternContextHolder<KeyProperty> -> _ = PatternContext.map _.Type
    let (|Name|) (value: PatternContextHolder<KeyProperty>) = value |> PatternContext.mapc (fun ctx -> _.Name >> Dictionary.Flip.item ctx.cache.nameKeys) 
    let (|Accessor|): PatternContextHolder<KeyProperty> -> _ = PatternContext.map _.Accessor
    let (|IsOptional|_|): PatternContextHolder<KeyProperty> -> _ = PatternContext.value >> _.IsOptional
    let (|IsStatic|_|): PatternContextHolder<KeyProperty> -> _ = PatternContext.value >> _.IsStatic
    let (|IsPrivate|_|): PatternContextHolder<KeyProperty> -> _ = PatternContext.value >> _.IsPrivate
    let (|IsReadOnly|IsReadAndWrite|IsWriteOnly|) = function
        | Accessor (Value value) ->
        match value with
        | TsAccessor.ReadOnly -> IsReadOnly
        | TsAccessor.ReadWrite -> IsReadAndWrite
        | TsAccessor.WriteOnly -> IsWriteOnly
    
    let (|CallSignatureAlias|_|) = function
        | Type (MasterKey.VisitationFlags.HasMembers & MasterKey.Members (Unzip [| MemberKey.CallSignature callSig |])) ->
            ValueSome callSig
        | _ -> ValueNone
    let (|ToPropertyKey|) = PatternContext.mapc _.createPropertyKey
    let (|ToMemberKey|) = function ToPropertyKey (PropertyKey.ToMemberKey key) -> key
    let (|ToMasterKey|) = (|ToMemberKey|) >> MemberKey.(|ToMasterKey|)
    let (|VisitationFlag|) = function ToPropertyKey (PropertyKey.VisitationFlag flags) -> flags
    let type' = (|Type|)
    let name = (|Name|)
    let accessor = (|Accessor|)
    let isOptional = (|IsOptional|_|)
    let isStatic = (|IsStatic|_|)
    let isPrivate = (|IsPrivate|_|)
    let callSignatureAlias = (|CallSignatureAlias|_|)
    let toPropertyKey = (|ToPropertyKey|)
    let toMemberKey = (|ToMemberKey|)
    let toMasterKey = (|ToMasterKey|)
    let visitationFlags = (|VisitationFlag|)
module CallSignature =
    let type': PatternContextHolder<KeyCallSignature> -> _ = PatternContext.map _.Type
    let parameters: PatternContextHolder<KeyCallSignature> -> _ = PatternContext.mapc (fun ctx -> _.Parameters >> Array.map (Dictionary.Flip.item ctx.cache.parameterKeys))
    let parameterKeys: PatternContextHolder<KeyCallSignature> -> _ = PatternContext.map _.Parameters
    let toCallSignatureKey = PatternContext.mapc _.createCallSignatureKey
    let toMasterKey =
        toCallSignatureKey
        >> PatternContext.mapc (fun ctx ->
        MemberBuilder.CallSignature
        >> ctx.createMemberKey
        >> MasterBuilder.Member
        >> ctx.createMasterKey
        )
    let (|Type|) = type'
    let (|Parameters|) = parameters
    let (|ToCallSignatureKey|) = toCallSignatureKey
    let (|ToMasterKey|) = toMasterKey
module PropertyKey =
    let (|ToProperty|): PatternContextHolder<PropertyKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.propertyKeys)
    let (|ToMemberKey|) = PatternContext.mapc (fun ctx -> MemberBuilder.Property >> ctx.createMemberKey)
    let (|ToMasterKey|) = (|ToMemberKey|) >> MemberKey.(|ToMasterKey|)
    let (|VisitationFlag|) = (|ToMasterKey|) >> MasterKey.(|VisitationFlag|)
    let (|Type|): PatternContextHolder<PropertyKey> -> _ = (|ToProperty|) >> PatternContext.map _.Type
    let (|Name|) (value: PatternContextHolder<PropertyKey>): _ = ((|ToProperty|) >>  PatternContext.map _.Name) value
    let (|Accessor|): PatternContextHolder<PropertyKey> -> _ = (|ToProperty|) >> PatternContext.map _.Accessor
    let (|IsOptional|_|): PatternContextHolder<PropertyKey> -> _ = (|ToProperty|) >> PatternContext.value >> _.IsOptional
    let (|IsStatic|_|): PatternContextHolder<PropertyKey> -> _ = (|ToProperty|) >> PatternContext.value >> _.IsStatic
    let (|IsPrivate|_|): PatternContextHolder<PropertyKey> -> _ = (|ToProperty|) >> PatternContext.value >> _.IsPrivate
    let (|IsReadOnly|IsReadAndWrite|IsWriteOnly|) = function
        | Accessor (Value value) ->
        match value with
        | TsAccessor.ReadOnly -> IsReadOnly
        | TsAccessor.ReadWrite -> IsReadAndWrite
        | TsAccessor.WriteOnly -> IsWriteOnly
    let (|CallSignatureAlias|_|) = function
        | Type (MasterKey.VisitationFlags.HasMembers & MasterKey.Members (Unzip [| MemberKey.CallSignature callSig |])) ->
            ValueSome callSig
        | _ -> ValueNone
    let type' = (|Type|)
    let name = (|Name|)
    let accessor = (|Accessor|)
    let isOptional = (|IsOptional|_|)
    let isStatic = (|IsStatic|_|)
    let isPrivate = (|IsPrivate|_|)
    let callSignatureAlias = (|CallSignatureAlias|_|)
    let toProperty = (|ToProperty|)
    let toMemberKey = (|ToMemberKey|)
    let toMasterKey = (|ToMasterKey|)
    let visitationFlags = (|VisitationFlag|)
module Parameter =
    let isOptional: PatternContextHolder<KeyParameter> -> bool = PatternContext.value >> _.IsOptional
    let isSpread: PatternContextHolder<KeyParameter> -> bool = PatternContext.value >> _.IsSpread
    let (|Type|): PatternContextHolder<KeyParameter> -> _ = PatternContext.map _.Type
    let (|Name|) (value: PatternContextHolder<KeyParameter>) = KeyType.name value
    let (|NameKey|) (value: PatternContextHolder<KeyParameter>) = KeyType.nameKey value
    let (|IsOptional|NotOptional|) keyParam =
        if isOptional keyParam then IsOptional else NotOptional
    let (|IsSpread|NotSpread|) keyParam =
        if isSpread keyParam then IsSpread else NotSpread
    let (|ToParameterKey|): PatternContextHolder<KeyParameter> -> _ = PatternContext.mapc _.createParameterKey
    let (|ToMasterKey|) = function ToParameterKey (ParameterKey.ToMasterKey value) -> value
    let (|VisitationFlag|) = function ToParameterKey (ParameterKey.VisitationFlag flags) -> flags
    let type' = (|Type|)
    let name value = (|Name|) value
    let nameKey value = (|NameKey|) value
    let toParameterKey = (|ToParameterKey|)
    let toMasterKey = (|ToMasterKey|)
    let visitationFlags = (|VisitationFlag|)
    [<return: Struct>]
    let (|IsCallSignatureAlias|_|): PatternContextHolder<KeyParameter> -> PatternContextHolder<_> voption = function
        | Type (MasterKey.KeyType.TypeLiteral (TypeLiteral.IsCallMethod value)) -> ValueSome value
        | _ -> ValueNone
    let isCallSignatureAlias = (|IsCallSignatureAlias|_|)
module ParameterKey =
    let (|IsCallSignatureAlias|_|): PatternContextHolder<ParameterKey> -> _ = (|ToParameter|) >> Parameter.(|IsCallSignatureAlias|_|)
    let isCallSignatureAlias = (|IsCallSignatureAlias|_|)
    let (|ToParameter|): PatternContextHolder<ParameterKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.parameterKeys)
    let (|ToMasterKey|): PatternContextHolder<ParameterKey> -> _ = PatternContext.mapc (fun ctx -> MasterBuilder.Parameter >> ctx.createMasterKey)
    let (|Type|): PatternContextHolder<ParameterKey> -> _ = (|ToParameter|) >> PatternContext.map _.Type
    let (|Name|) (value: PatternContextHolder<ParameterKey>)  = ((|ToParameter|) >>  KeyType.(|Name|)) value
    let (|NameKey|) (value: PatternContextHolder<ParameterKey>) = ((|ToParameter|) >>  KeyType.(|NameKey|)) value
    let (|IsOptional|_|): PatternContextHolder<ParameterKey> -> _ =(|ToParameter|) >>  PatternContext.value >> _.IsOptional
    let (|IsSpread|_|): PatternContextHolder<ParameterKey> -> _ =(|ToParameter|) >>  PatternContext.value >> _.IsSpread
    let (|VisitationFlag|) = (|ToMasterKey|) >> MasterKey.(|VisitationFlag|)
    let toParameter = (|ToParameter|)
    let toMasterKey = (|ToMasterKey|)
    let type' = (|Type|)
    let name (value: PatternContextHolder<ParameterKey>): PatternContextHolder<string> = (|Name|) value
    let nameKey (value: PatternContextHolder<ParameterKey>): PatternContextHolder<NameKey> = (|NameKey|) value
    let isOptional = (|IsOptional|_|)
    let isSpread = (|IsSpread|_|)
    let visitationFlags = (|VisitationFlag|)
module Method =
    let (|Type|): PatternContextHolder<KeyMethod> -> _ = PatternContext.map _.Type
    let (|NameKey|) (value: PatternContextHolder<KeyMethod>) = KeyType.(|NameKey|) value
    let (|Name|) (value: PatternContextHolder<KeyMethod>) = KeyType.(|Name|) value
    let (|ParameterKeys|): PatternContextHolder<KeyMethod> -> _ = PatternContext.map _.Parameters
    let (|Parameters|): PatternContextHolder<KeyMethod> -> _ = function
        | ParameterKeys value ->
            value |> PatternContext.Array.cmap (ParameterKey.(|ToParameter|) >> (|Value|))
    let (|IsStatic|_|): PatternContextHolder<KeyMethod> -> _ = PatternContext.value >> _.IsStatic
    let (|IsOptional|_|): PatternContextHolder<KeyMethod> -> _ = PatternContext.value >> _.IsOptional
    let (|ToMethodKey|) = PatternContext.mapc _.createMethodKey
    let (|ToMemberKey|) = function ToMethodKey (MethodKey.ToMemberKey key) -> key
    let (|ToMasterKey|) = (|ToMemberKey|) >> MemberKey.(|ToMasterKey|)
    let type' = (|Type|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let parameterKeys = (|ParameterKeys|)
    let parameters = (|Parameters|)
    let isStatic = (|IsStatic|_|)
    let isOptional = (|IsOptional|_|)
    let toMethodKey = (|ToMethodKey|)
    let toMemberKey = (|ToMemberKey|)
    let toMasterKey = (|ToMasterKey|)
module MethodKey =
    let (|ToMethod|): PatternContextHolder<MethodKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.methodKeys)
    let (|ToMemberKey|) = PatternContext.mapc (fun ctx -> MemberBuilder.Method >> ctx.createMemberKey)
    let (|ToMasterKey|) = function ToMemberKey (MemberKey.ToMasterKey key) -> key
    let (|Type|): PatternContextHolder<MethodKey> -> _ = (|ToMethod|) >> PatternContext.map _.Type
    let (|NameKey|) (value: PatternContextHolder<MethodKey>) = (|ToMethod|) value |> KeyType.(|NameKey|)
    let (|Name|) (value: PatternContextHolder<MethodKey>) = (|ToMethod|) value |> KeyType.(|Name|)
    let (|ParameterKeys|): PatternContextHolder<MethodKey> -> _ = (|ToMethod|) >> PatternContext.map _.Parameters
    let (|Parameters|): PatternContextHolder<MethodKey> -> _ = function
        | ParameterKeys value ->
            value
            |> PatternContext.Array.cmap (ParameterKey.(|ToParameter|) >> (|Value|))
    let (|IsStatic|_|): PatternContextHolder<MethodKey> -> _ = (|ToMethod|) >> PatternContext.value >> _.IsStatic
    let (|IsOptional|_|): PatternContextHolder<MethodKey> -> _ = (|ToMethod|) >> PatternContext.value >> _.IsOptional
    let toMethod = (|ToMethod|)
    let toMemberKey = (|ToMemberKey|)
    let toMasterKey = (|ToMasterKey|)
    let type' = (|Type|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let parameterKeys = (|ParameterKeys|)
    let parameters = (|Parameters|)
    let isStatic = (|IsStatic|_|)
    let isOptional = (|IsOptional|_|)
module UnionKey =
    let (|ToUnion|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.unionKeys)
    let (|IsBoolean|_|) = PatternContext.value >> fun key ->
        Prelude.Union.booleans |> Set.contains key
    let (|Types|) = function ToUnion (Union.Types types) -> types
    let (|IsTypeOrNullable|_|) = function ToUnion (Union.IsTypeOrNullable nestedKey) -> ValueSome nestedKey | _ -> ValueNone
    let toUnion = (|ToUnion|)
    let isBoolean = (|IsBoolean|_|)
    let types = (|Types|)
    let isTypeOrNullable = (|IsTypeOrNullable|_|)
module Union =
    let (|ToUnionKey|) = PatternContext.mapc _.createUnionKey
    let (|IsBoolean|_|): _ -> bool = (|ToUnionKey|) >> UnionKey.(|IsBoolean|_|)
    let (|Types|): PatternContextHolder<KeyUnion> -> _ = PatternContext.map _.Types
    [<return: Struct>]
    let (|IsTypeOrNullable|_|) = function
        | Types (Array.MasterKey.Filter.HadNullable (DestructureContext(ctx, [| value |]))) -> ValueSome (PatternContext.prepare ctx value)
        | _ -> ValueNone
    let toUnionKey = (|ToUnionKey|)
    let toMasterKey = toUnionKey >> PatternContext.mapc (fun ctx -> MasterBuilder.Union >> ctx.createMasterKey)
    let isBoolean = (|IsBoolean|_|)
    let types = (|Types|)
    let isTypeOrNullable = (|IsTypeOrNullable|_|)
module Intersection =
    let (|Types|): PatternContextHolder<KeyIntersection> -> _ = PatternContext.map _.Types
    let types = (|Types|)
module IntersectionKey =
    let (|ToIntersection|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.intersectionKeys)
    let toIntersection = (|ToIntersection|)
module IndexAccess =
    let toMemberKey = PatternContext.choosec KeyResolution.IndexAccess.tryResolveToMember
    let toMemberBuilder = toMemberKey >> ValueOption.map MemberKey.toMemberBuilder
    let (|Index|) = PatternContext.map _.Index
    let (|Object|) = PatternContext.map _.Object
    let (|ToMemberKey|_|) = toMemberKey
    let (|ToMemberBuilder|_|) = toMemberBuilder
    let (|ToIndexAccessKey|) = PatternContext.mapc _.createIndexAccessKey
    let (|ToMasterKey|) = function ToIndexAccessKey value -> match value with  IndexAccessKey.ToMasterKey key -> key
    let index = (|Index|)
    let object = (|Object|)
    let toIndexAccessKey = (|ToIndexAccessKey|)
    let toMasterKey = (|ToMasterKey|)
module IndexAccessKey =
    let (|ToIndexAccess|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.indexAccessKeys)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.IndexAccess >> ctx.createMasterKey)
    let (|Index|) = (|ToIndexAccess|) >> IndexAccess.(|Index|)
    let (|Object|) = (|ToIndexAccess|) >> IndexAccess.(|Object|)
    let (|ToMember|_|) = (|ToIndexAccess|) >> IndexAccess.toMemberKey
    let toIndexAccess = (|ToIndexAccess|)
    let toMasterKey = (|ToMasterKey|)
    let index = (|Index|)
    let object = (|Object|)
    let toMember = (|ToMember|_|)
module Module =
    let (|Source|_|): PatternContextHolder<KeyModule> -> _ = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Qualifiers|): PatternContextHolder<KeyModule> -> _ = PatternContext.map _.Qualifiers
    let (|Types|): PatternContextHolder<KeyModule> -> _ = PatternContext.map _.Types
    let (|Name|) (value: PatternContextHolder<KeyModule>): _ = (function KeyType.Name value -> value) value
    let (|NameKey|) (value: PatternContextHolder<KeyModule>): _ = value |> PatternContext.map _.Name 
    let (|IsNamespace|_|): PatternContextHolder<KeyModule> -> bool = _.Value.IsNamespace
    let (|IsRecursive|_|): PatternContextHolder<KeyModule> -> bool = _.Value.IsRecursive
    let (|ToModuleKey|) = PatternContext.mapc _.createModuleKey
    let (|ToMasterKey|): PatternContextHolder<KeyModule> -> PatternContextHolder<MasterKey> = function ToModuleKey (ModuleKey.ToMasterKey key) -> key
    let source = (|Source|_|)
    let qualifiers = (|Qualifiers|)
    let types = (|Types|)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let isNamespace = (|IsNamespace|_|)
    let isRecursive = (|IsRecursive|_|)
    let toModuleKey = (|ToModuleKey|)
    let toMasterKey = (|ToMasterKey|)
module ModuleKey =
    let (|ToModule|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.moduleKeys)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Module >> ctx.createMasterKey)
    let (|Source|_|): PatternContextHolder<ModuleKey> -> _ = (|ToModule|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Qualifiers|): PatternContextHolder<ModuleKey> -> _ = (|ToModule|) >> PatternContext.map _.Qualifiers
    let (|Types|): PatternContextHolder<ModuleKey> -> _ = (|ToModule|) >> PatternContext.map _.Types
    let (|Name|) (value: PatternContextHolder<ModuleKey>) = (|ToModule|) value |> KeyType.(|Name|)
    let (|NameKey|) (value: PatternContextHolder<ModuleKey>) = (|ToModule|) value |> KeyType.(|NameKey|)
    let (|IsNamespace|_|): PatternContextHolder<ModuleKey> -> bool = (|ToModule|) >> _.Value.IsNamespace
    let (|IsRecursive|_|): PatternContextHolder<ModuleKey> -> bool = (|ToModule|) >> _.Value.IsRecursive
    let source = (|Source|_|)
    let qualifiers = (|Qualifiers|)
    let types = (|Types|)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let isNamespace = (|IsNamespace|_|)
    let isRecursive = (|IsRecursive|_|)
    let toModule = (|ToModule|)
    let toMasterKey = (|ToMasterKey|)
module Tuple =
    let elementKeys: PatternContextHolder<KeyTuple> -> PatternContextHolder<TupleElementKey array> = PatternContext.map _.Elements
    let elements = elementKeys >> PatternContext.Array.cbind TupleElementKey.toTupleElement
    let fixedTuple = elements >> PatternContext.Array.cforall (function TupleElement.IsOptional | TupleElement.IsVariadic | TupleElement.IsRest -> false | _ -> true)
    let optionalTuple = elements >> fun eles ->
        eles |> PatternContext.Array.cexists (function TupleElement.IsOptional -> true | _ -> false)
        && eles |> PatternContext.Array.cforall (function TupleElement.IsVariadic | TupleElement.IsRest -> false | _ -> true)
    let variadicTuple = elements >> PatternContext.Array.cexists TupleElement.isVariadic
    let (|IsFixedTuple|IsOptionalTuple|IsVariadicTuple|) (value: PatternContextHolder<KeyTuple>) =
        if fixedTuple value then
            IsFixedTuple
        elif optionalTuple value then
            IsOptionalTuple
        elif variadicTuple value then
            IsVariadicTuple
        else failwith "Unknown tuple type. Not fixed optional or variadic."
    let (|ElementKeys|): PatternContextHolder<KeyTuple> -> _ = PatternContext.map _.Elements
    let (|Elements|): PatternContextHolder<KeyTuple> -> PatternContextHolder<KeyTupleElement array> = elements
    let (|MinRequired|): PatternContextHolder<KeyTuple> -> int = _.Value.MinRequired
    let (|FixedLength|): PatternContextHolder<KeyTuple> -> int  = _.Value.FixedLength
    let (|IsReadOnly|_|): PatternContextHolder<KeyTuple> -> bool = _.Value.IsReadOnly
    let (|ToTupleKey|) = PatternContext.mapc _.createTupleKey
    let (|ToMasterKey|) = function ToTupleKey (TupleKey.ToMasterKey key) -> key
    let minRequired = (|MinRequired|)
    let fixedLength = (|FixedLength|)
    let isReadOnly = (|IsReadOnly|_|)
    let toTupleKey = (|ToTupleKey|)
    let toMasterKey = (|ToMasterKey|)
module TupleKey =
    let (|ToTuple|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.tupleKeys)
    let (|Elements|): PatternContextHolder<TupleKey> -> _ = (|ToTuple|) >> PatternContext.map _.Elements
    let (|MinRequired|): PatternContextHolder<TupleKey> -> int = (|ToTuple|) >> _.Value.MinRequired
    let (|FixedLength|): PatternContextHolder<TupleKey> -> int  = (|ToTuple|) >> _.Value.FixedLength
    let (|IsReadOnly|_|): PatternContextHolder<TupleKey> -> bool = (|ToTuple|) >> _.Value.IsReadOnly
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Tuple >> ctx.createMasterKey)
    let elements = (|Elements|)
    let minRequired = (|MinRequired|)
    let fixedLength = (|FixedLength|)
    let isReadOnly = (|IsReadOnly|_|)
    let toTuple = (|ToTuple|)
    let toMasterKey = (|ToMasterKey|)
module TupleElement =
    let (|Name|_|) (value: PatternContextHolder<KeyTupleElement>): _ = value |> (PatternContext.choose _.Name >> ValueOption.map (PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.nameKeys)))
    let (|NameKey|_|) (value: PatternContextHolder<KeyTupleElement>): PatternContextHolder<NameKey> voption = value |> PatternContext.choose _.Name
    let (|Type|): PatternContextHolder<KeyTupleElement> -> _ = PatternContext.map _.Type
    let (|IsOptional|_|): PatternContextHolder<KeyTupleElement> -> bool = _.Value.IsOptional
    let (|IsRest|_|): PatternContextHolder<KeyTupleElement> -> bool = _.Value.IsRest
    let (|IsVariadic|_|): PatternContextHolder<KeyTupleElement> -> bool = _.Value.IsVariadic
    let (|ToTupleElementKey|) = PatternContext.mapc _.createTupleElementKey
    let (|ToMasterKey|) = function ToTupleElementKey (TupleElementKey.ToMasterKey key) -> key
    let name = (|Name|_|)
    let nameKey = (|NameKey|_|)
    let type' = (|Type|)
    let isOptional = (|IsOptional|_|)
    let isRest = (|IsRest|_|)
    let isVariadic = (|IsVariadic|_|)
    let toTupleElementKey = (|ToTupleElementKey|)
    let toMasterKey = (|ToMasterKey|)
module TupleElementKey =
    let (|ToTupleElement|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.tupleElementKeys)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.TupleElement >> ctx.createMasterKey)
    let (|Name|_|) value = value |> ((|ToTupleElement|) >> TupleElement.(|Name|_|))
    let (|NameKey|_|) value = value |> ((|ToTupleElement|) >> TupleElement.(|NameKey|_|))
    let (|Type|): PatternContextHolder<TupleElementKey> -> _ = (|ToTupleElement|) >> PatternContext.map _.Type
    let (|IsOptional|_|): PatternContextHolder<TupleElementKey> -> bool = (|ToTupleElement|) >> _.Value.IsOptional
    let (|IsRest|_|): PatternContextHolder<TupleElementKey> -> bool = (|ToTupleElement|) >> _.Value.IsRest
    let (|IsVariadic|_|): PatternContextHolder<TupleElementKey> -> bool = (|ToTupleElement|) >> _.Value.IsVariadic
    let name = (|Name|_|)
    let nameKey = (|NameKey|_|)
    let type' = (|Type|)
    let isOptional = (|IsOptional|_|)
    let isRest = (|IsRest|_|)
    let isVariadic = (|IsVariadic|_|)
    let toTupleElement = (|ToTupleElement|)
    let toMasterKey = (|ToMasterKey|)
module Index =
    let (|Type|): PatternContextHolder<KeyIndex> -> _ = PatternContext.map _.Type
    let (|ToIndexKey|) = PatternContext.mapc _.createIndexKey
    let (|ToMasterKey|): PatternContextHolder<KeyIndex> -> PatternContextHolder<MasterKey> = function ToIndexKey (IndexKey.ToMasterKey value) -> value
    [<return: Struct>]
    let (|Members|_|) = function
        | Type (MasterKey.Members members) -> members |> ValueSome
        | _ -> ValueNone
    [<return: Struct>]
    let (|MembersToLiterals|_|) = function
        | Members members ->
            members
            |> PatternContext.Array.cchoose (function
                | MemberKey.HasName (Name.ToLiteral (Value value)) -> Some value
                | _ -> None
                )
            |> ValueSome
        | _ -> ValueNone
    let type' = (|Type|)
    let toIndexKey = (|ToIndexKey|)
    let toMasterKey = (|ToMasterKey|)
    let members = (|Members|_|)
    let membersToLiterals = (|MembersToLiterals|_|)
module IndexKey =
    let (|ToIndex|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.indexKeys)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Index >> ctx.createMasterKey)
    let (|Type|): PatternContextHolder<IndexKey> -> _ = (|ToIndex|) >> PatternContext.map _.Type
    let (|Members|_|) = (|ToIndex|) >> Index.(|Members|_|)
    let (|MembersToLiterals|_|) = (|ToIndex|) >> Index.(|MembersToLiterals|_|)
    let toIndex = (|ToIndex|)
    let toMasterKey = (|ToMasterKey|)
    let type' = (|Type|)
    let members = (|Members|_|)
    let membersToLiterals = (|MembersToLiterals|_|)
module Predicate =
    let (|Type|): PatternContextHolder<KeyPredicate> -> _ = PatternContext.map _.Type
    let (|IsAssertion|_|): PatternContextHolder<KeyPredicate> -> bool = _.Value.IsAssertion
    let (|ToPredicateKey|) = PatternContext.mapc _.createPredicateKey
    let (|ToMasterKey|) = function ToPredicateKey (PredicateKey.ToMasterKey value) -> value
    let type' = (|Type|)
    let isAssertion = (|IsAssertion|_|)
    let toPredicateKey = (|ToPredicateKey|)
    let toMasterKey = (|ToMasterKey|)
module PredicateKey =
    let (|ToPredicate|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.predicateKeys)
    let (|Type|): PatternContextHolder<PredicateKey> -> _ = (|ToPredicate|) >> PatternContext.map _.Type
    let (|IsAssertion|_|): PatternContextHolder<PredicateKey> -> bool = (|ToPredicate|) >> _.Value.IsAssertion
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Predicate >> ctx.createMasterKey)
    let type' = (|Type|)
    let isAssertion = (|IsAssertion|_|)
    let toPredicate = (|ToPredicate|)
    let toMasterKey = (|ToMasterKey|)
module TypeReference =
    let (|Type|): PatternContextHolder<KeyTypeReference> -> _ = PatternContext.map _.Type
    let (|TypeArguments|): PatternContextHolder<KeyTypeReference> -> _ = PatternContext.map _.TypeArguments
    let (|ResolvedType|_|): PatternContextHolder<KeyTypeReference> -> _ = PatternContext.choose _.ResolvedType
    let (|ToTypeReferenceKey|) = PatternContext.mapc _.createTypeReferenceKey
    let (|ToMasterKey|) = function ToTypeReferenceKey (TypeReferenceKey.ToMasterKey value) -> value
    let type' = (|Type|)
    let typeArguments = (|TypeArguments|)
    let resolvedType = (|ResolvedType|_|)
    let toTypeReferenceKey = (|ToTypeReferenceKey|)
    let toMasterKey = (|ToMasterKey|)
module TypeReferenceKey =
    let (|ToTypeReference|): PatternContextHolder<TypeReferenceKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.typeReferenceKeys)
    let (|Type|): PatternContextHolder<TypeReferenceKey> -> _ = (|ToTypeReference|) >> PatternContext.map _.Type
    let (|TypeArguments|): PatternContextHolder<TypeReferenceKey> -> _ = (|ToTypeReference|) >> PatternContext.map _.TypeArguments
    let (|ResolvedType|_|): PatternContextHolder<TypeReferenceKey> -> _ = (|ToTypeReference|) >> PatternContext.choose _.ResolvedType
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.TypeReference >> ctx.createMasterKey)
    let type' = (|Type|)
    let typeArguments = (|TypeArguments|)
    let resolvedType = (|ResolvedType|_|)
    let toTypeReference = (|ToTypeReference|)
    let toMasterKey = (|ToMasterKey|)
module TypeParameter =
    let (|NameKey|) (value: PatternContextHolder<KeyTypeParameter>): _ = value |> PatternContext.map _.Name
    let (|Name|) (value: PatternContextHolder<KeyTypeParameter>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|Constraint|_|): PatternContextHolder<KeyTypeParameter> -> _ = PatternContext.choose _.Constraint
    let (|Default|_|): PatternContextHolder<KeyTypeParameter> -> _ = PatternContext.choose _.Default
    let (|ToTypeParameterKey|) = PatternContext.mapc _.createTypeParameterKey
    let (|ToMasterKey|) = function ToTypeParameterKey(TypeParameterKey.ToMasterKey value) -> value
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let constraint' = (|Constraint|_|)
    let default' = (|Default|_|)
    let toTypeParameterKey = (|ToTypeParameterKey|)
    let toMasterKey = (|ToMasterKey|)
module TypeParameterKey =
    let (|ToTypeParameter|): PatternContextHolder<TypeParameterKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.typeParameterKeys)
    let (|NameKey|) (value: PatternContextHolder<TypeParameterKey>) = (|ToTypeParameter|) value |> KeyType.(|NameKey|)
    let (|Name|) (value: PatternContextHolder<TypeParameterKey>) = (|ToTypeParameter|) value |> KeyType.(|Name|)
    let (|Constraint|_|): PatternContextHolder<TypeParameterKey> -> _ = (|ToTypeParameter|) >> PatternContext.choose _.Constraint
    let (|Default|_|): PatternContextHolder<TypeParameterKey> -> _ = (|ToTypeParameter|) >> PatternContext.choose _.Default
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.TypeParameter >> ctx.createMasterKey)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let constraint' = (|Constraint|_|)
    let default' = (|Default|_|)
    let toTypeParameter = (|ToTypeParameter|)
    let toMasterKey = (|ToMasterKey|)
module Function =
    let (|SourceKey|_|): PatternContextHolder<KeyFunction> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Source|_|) = (|SourceKey|_|) >> ValueOption.map (PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.sourceKeys))
    let (|Name|) (value: PatternContextHolder<KeyFunction>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyFunction>): _ = value |> PatternContext.map _.Name
    let (|Qualifiers|_|): PatternContextHolder<KeyFunction> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|ParameterKeys|): PatternContextHolder<KeyFunction> -> _ = PatternContext.map _.Parameters
    let (|Parameters|) = parameters
    let (|Type|): PatternContextHolder<KeyFunction> -> _ = PatternContext.map _.Type
    let (|TypeParameterKeys|_|): PatternContextHolder<KeyFunction> -> _ = PatternContext.choose (_.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|ToFunctionKey|) = PatternContext.mapc _.createFunctionKey
    let (|ToMasterKey|) = (|ToFunctionKey|) >> FunctionKey.(|ToMasterKey|)
    let (|ShouldRenderDelegate|DelegateNotRequired|) = function
        | ParameterKeys (Array.Length value) when value < 3 -> DelegateNotRequired
        | _ -> ShouldRenderDelegate
    let sourceKey = (|SourceKey|_|)
    let source = (|Source|_|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let parameterKeys = (|ParameterKeys|)
    let parameters (value: PatternContextHolder<KeyFunction>): PatternContextHolder<KeyParameter array> = parameterKeys value |> PatternContext.Array.cbind ParameterKey.toParameter
    let type' = (|Type|)
    let typeParameterKeys = (|TypeParameterKeys|_|)
    let typeParameters = typeParameterKeys >> ValueOption.map (PatternContext.Array.cbind TypeParameterKey.toTypeParameter)
    let toFunctionKey = (|ToFunctionKey|)
    let toMasterKey = (|ToMasterKey|)
module FunctionKey =
    let (|ToFunction|): PatternContextHolder<FunctionKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.functionKeys)
    let (|Source|_|): PatternContextHolder<FunctionKey> -> _ = (|ToFunction|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Name|) (value: PatternContextHolder<FunctionKey>): _ = value |> ((|ToFunction|) >> PatternContext.map _.Name)
    let (|Qualifiers|_|): PatternContextHolder<FunctionKey> -> _ = (|ToFunction|) >> PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Parameters|): PatternContextHolder<FunctionKey> -> _ = (|ToFunction|) >> PatternContext.map _.Parameters
    let (|Type|): PatternContextHolder<FunctionKey> -> _ = (|ToFunction|) >> PatternContext.map _.Type
    let (|TypeParameters|_|): PatternContextHolder<FunctionKey> -> _ = (|ToFunction|) >> PatternContext.choose (_.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Function >> ctx.createMasterKey)
    let (|ShouldRenderDelegate|DelegateNotRequired|) = function ToFunction Function.DelegateNotRequired -> DelegateNotRequired | _ -> ShouldRenderDelegate
    let toFunction = (|ToFunction|)
    let source = (|Source|_|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let parameters = (|Parameters|)
    let type' = (|Type|)
    let typeParameters = (|TypeParameters|_|)
    let toMasterKey = (|ToMasterKey|)
module TypeAlias =
    let (|Source|_|): PatternContextHolder<KeyTypeAlias> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Name|) (value: PatternContextHolder<KeyTypeAlias>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyTypeAlias>): _ = value |> PatternContext.map _.Name
    let (|Qualifiers|_|): PatternContextHolder<KeyTypeAlias> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Type|): PatternContextHolder<KeyTypeAlias> -> _ = PatternContext.map _.Type
    let (|TypeParameterKeys|_|): PatternContextHolder<KeyTypeAlias> -> _ = PatternContext.choose (_.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|ToTypeAliasKey|) = PatternContext.mapc _.createTypeAliasKey
    let (|ToMasterKey|) = (|ToTypeAliasKey|) >> TypeAliasKey.(|ToMasterKey|)
    let sourceKey = (|Source|_|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let type' = (|Type|)
    let typeParameterKeys = (|TypeParameterKeys|_|)
    let typeParameters = typeParameterKeys >> ValueOption.map (PatternContext.Array.cbind TypeParameterKey.toTypeParameter)
    let toTypeAliasKey = (|ToTypeAliasKey|)
    let toMasterKey = (|ToMasterKey|)
module TypeAliasKey =
    let (|ToTypeAlias|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.typeAliasKeys)
    let (|Source|_|): PatternContextHolder<TypeAliasKey> -> _ = (|ToTypeAlias|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Name|) (value: PatternContextHolder<TypeAliasKey>): _ = value |> ((|ToTypeAlias|) >> PatternContext.map _.Name)
    let (|Qualifiers|_|): PatternContextHolder<TypeAliasKey> -> _ = (|ToTypeAlias|) >> PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Type|): PatternContextHolder<TypeAliasKey> -> _ = (|ToTypeAlias|) >> PatternContext.map _.Type
    let (|TypeParameters|_|): PatternContextHolder<TypeAliasKey> -> _ = (|ToTypeAlias|) >> PatternContext.choose (_.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.TypeAlias >> ctx.createMasterKey)
    let toTypeAlias = (|ToTypeAlias|)
    let source = (|Source|_|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let type' = (|Type|)
    let typeParameters = (|TypeParameters|_|)
    let toMasterKey = (|ToMasterKey|)
module Enum =
    let memberKeys: PatternContextHolder<KeyEnum> -> PatternContextHolder<EnumCaseKey array> = PatternContext.map _.Members
    let members = memberKeys >> PatternContext.Array.cbind EnumCaseKey.toEnumCase
    let (|Name|) (value: PatternContextHolder<KeyEnum>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyEnum>): _ = value |> PatternContext.map _.Name
    let (|Qualifiers|_|): PatternContextHolder<KeyEnum> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Source|_|): PatternContextHolder<KeyEnum> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|MemberKeys|) = memberKeys
    let (|Members|) = members
    let (|ToEnumKey|) = PatternContext.mapc _.createEnumKey
    let (|ToMasterKey|) = (|ToEnumKey|) >> EnumKey.(|ToMasterKey|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let sourceKey = (|Source|_|)
    let toEnumKey = (|ToEnumKey|)
    let toMasterKey = (|ToMasterKey|)
module EnumKey =
    let (|ToEnum|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.enumKeys)
    let (|Name|) (value: PatternContextHolder<EnumKey>): _ = value |> ((|ToEnum|) >> PatternContext.map _.Name)
    let (|Qualifiers|_|): PatternContextHolder<EnumKey> -> _ = (|ToEnum|) >> PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Source|_|): PatternContextHolder<EnumKey> -> _ = (|ToEnum|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Members|): PatternContextHolder<EnumKey> -> _ = (|ToEnum|) >> PatternContext.map _.Members
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Enum >> ctx.createMasterKey)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let source = (|Source|_|)
    let members = (|Members|)
    let toEnum = (|ToEnum|)
    let toMasterKey = (|ToMasterKey|)
module EnumCase =
    let (|Name|) (value: PatternContextHolder<KeyEnumCase>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyEnumCase>): _ = value |> PatternContext.map _.Name
    let (|LiteralValue|): PatternContextHolder<KeyEnumCase> -> _ = PatternContext.mapc (fun ctx -> _.Value >> Dictionary.Flip.item ctx.cache.literalKeys)
    let (|LiteralValueKey|) = function LiteralValue (Literal.ToLiteralKey key) -> key
    let (|ToEnumCaseKey|) = PatternContext.mapc _.createEnumCaseKey
    let (|ToMasterKey|) = (|ToEnumCaseKey|) >> EnumCaseKey.(|ToMasterKey|)
    let (|StringCase|IntCase|NumberCase|BooleanCase|NullCase|) = function
        | LiteralValue (Value value) ->
        match value with
        | TsLiteral.Int value -> IntCase value
        | TsLiteral.BigInt value -> IntCase (int value)
        | TsLiteral.Float value -> NumberCase value
        | TsLiteral.String value -> StringCase value
        | TsLiteral.Bool value -> BooleanCase value
        | TsLiteral.Null -> NullCase
    let (|ParentEnum|) = (|ToEnumCaseKey|) >> EnumCaseKey.(|ParentEnum|)
    let (|ParentEnumKey|) = (|ToEnumCaseKey|) >> EnumCaseKey.(|ParentEnumKey|)
    let (|ParentEnumMasterKey|) = (|ParentEnumKey|) >> EnumKey.(|ToMasterKey|)
    let name = (|Name|)
    let literalValue = (|LiteralValue|)
    let literalValueKey = (|LiteralValueKey|)
    let toEnumCaseKey = (|ToEnumCaseKey|)
    let nameKey = (|NameKey|)
    let toMasterKey = (|ToMasterKey|)
    let parentEnum = (|ParentEnum|)
    let parentEnumKey = (|ParentEnumKey|)
    let parentEnumMasterKey = (|ParentEnumMasterKey|)
module EnumCaseKey =
    let (|ToEnumCase|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.enumCaseKeys)
    let (|Name|) (value: PatternContextHolder<EnumCaseKey>): _ = value |> ((|ToEnumCase|) >> EnumCase.(|Name|))
    let (|NameKey|) (value: PatternContextHolder<EnumCaseKey>): _ = value |> ((|ToEnumCase|) >> EnumCase.(|NameKey|))
    let (|LiteralValue|): PatternContextHolder<EnumCaseKey> -> _ = (|ToEnumCase|) >> EnumCase.(|LiteralValue|)
    let (|LiteralValueKey|) = function LiteralValue (Literal.ToLiteralKey key) -> key
    let (|StringCase|IntCase|NumberCase|BooleanCase|NullCase|) = function
        | ToEnumCase value ->
        match value with
        | EnumCase.StringCase value -> StringCase value
        | EnumCase.NumberCase value -> NumberCase value
        | EnumCase.BooleanCase value -> BooleanCase value
        | EnumCase.NullCase -> NullCase
        | EnumCase.IntCase value -> IntCase value
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.EnumCase >> ctx.createMasterKey)
    let (|ParentEnumKey|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.enumCaseToEnumKeys)
    let (|ParentEnum|) = (|ParentEnumKey|) >> EnumKey.(|ToEnum|)
    let (|ParentEnumMasterKey|) = (|ParentEnumKey|) >> EnumKey.(|ToMasterKey|)
    let name = (|Name|)
    let literalValue = (|LiteralValue|)
    let literalValueKey = (|LiteralValueKey|)
    let toEnumCase = (|ToEnumCase|)
    let nameKey = (|NameKey|)
    let toMasterKey = (|ToMasterKey|)
    let parentEnum = (|ParentEnum|)
    let parentEnumKey = (|ParentEnumKey|)
    let parentEnumMasterKey = (|ParentEnumMasterKey|)
module Variable =
    let (|Source|_|): PatternContextHolder<KeyVariable> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Qualifiers|_|): PatternContextHolder<KeyVariable> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Name|) (value: PatternContextHolder<KeyVariable>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyVariable>): _ = value |> PatternContext.map _.Name
    let (|Type|): PatternContextHolder<KeyVariable> -> _ = PatternContext.map _.Type
    let (|ToVariableKey|) = PatternContext.mapc _.createVariableKey
    let (|ToMasterKey|) = (|ToVariableKey|) >> VariableKey.(|ToMasterKey|)
    let sourceKey = (|Source|_|)
    let qualifiers = (|Qualifiers|_|)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let type' = (|Type|)
    let toVariableKey = (|ToVariableKey|)
    let toMasterKey = (|ToMasterKey|)
module VariableKey =
    let (|ToVariable|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.variableKeys)
    let (|Source|_|): PatternContextHolder<VariableKey> -> _ = (|ToVariable|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|Qualifiers|_|): PatternContextHolder<VariableKey> -> _ = (|ToVariable|) >> PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Name|) (value: PatternContextHolder<VariableKey>): _ = value |> ((|ToVariable|) >> Variable.(|Name|))
    let (|NameKey|) (value: PatternContextHolder<VariableKey>): _ = value |> ((|ToVariable|) >> Variable.(|NameKey|))
    let (|Type|): PatternContextHolder<VariableKey> -> _ = (|ToVariable|) >> Variable.(|Type|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Variable >> ctx.createMasterKey)
    let sourceKey = (|Source|_|)
    let qualifiers = (|Qualifiers|_|)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let type' = (|Type|)
    let toVariable = (|ToVariable|)
    let toMasterKey = (|ToMasterKey|)
module Interface =
    let (|NameKey|) (value: PatternContextHolder<KeyInterface>): _ = value |> PatternContext.map _.Name
    let (|Name|) (value: PatternContextHolder<KeyInterface>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|Qualifiers|_|): PatternContextHolder<KeyInterface> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|HeritageKeys|): PatternContextHolder<KeyInterface> -> _ = PatternContext.map _.Heritage
    let (|Heritage|) = heritage
    let (|Source|_|): PatternContextHolder<KeyInterface> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|TypeParameterKeys|_|): PatternContextHolder<KeyInterface> -> _ = PatternContext.choose (_.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|TypeParameters|_|) = typeParameters
    let (|Members|): PatternContextHolder<KeyInterface> -> _ = PatternContext.map _.Members
    let (|ToInterfaceKey|) = PatternContext.mapc _.createInterfaceKey
    let (|ToMasterKey|) = (|ToInterfaceKey|) >> InterfaceKey.(|ToMasterKey|)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let heritageKeys = (|HeritageKeys|)
    let heritage = heritageKeys >> PatternContext.Array.cbind TypeReferenceKey.toTypeReference
    let sourceKey = (|Source|_|)
    let typeParameterKeys: PatternContextHolder<KeyInterface> -> PatternContextHolder<TypeParameterKey array> voption = (|TypeParameterKeys|_|)
    let typeParameters: PatternContextHolder<KeyInterface> -> PatternContextHolder<KeyTypeParameter array> voption =
        typeParameterKeys >> ValueOption.map (PatternContext.Array.cbind TypeParameterKey.toTypeParameter)
    let members = (|Members|)
    let toInterfaceKey = (|ToInterfaceKey|)
    let toMasterKey = (|ToMasterKey|)
module InterfaceKey =
    let (|ToInterface|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.interfaceKeys)
    let (|NameKey|) (value: PatternContextHolder<InterfaceKey>): _ = value |> ((|ToInterface|) >> KeyType.(|NameKey|))
    let (|Name|) (value: PatternContextHolder<InterfaceKey>): _ = value |> ((|ToInterface|) >> KeyType.(|Name|))
    let (|Qualifiers|_|): PatternContextHolder<InterfaceKey> -> _ = (|ToInterface|) >> Interface.(|Qualifiers|_|)
    let (|Heritage|): PatternContextHolder<InterfaceKey> -> _ = (|ToInterface|) >> Interface.(|HeritageKeys|)
    let (|Source|_|): PatternContextHolder<InterfaceKey> -> _ = (|ToInterface|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|TypeParameters|_|): PatternContextHolder<InterfaceKey> -> _ = (|ToInterface|) >> Interface.(|TypeParameterKeys
                                                                                               |_|)
    let (|Members|): PatternContextHolder<InterfaceKey> -> _ = (|ToInterface|) >> Interface.(|Members|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Interface >> ctx.createMasterKey)
    let nameKey = (|NameKey|)
    let name = (|Name|)
    let qualifiers = (|Qualifiers|_|)
    let heritage = (|Heritage|)
    let sourceKey = (|Source|_|)
    let typeParameters = (|TypeParameters|_|)
    let members = (|Members|)
    let toInterface = (|ToInterface|)
    let toMasterKey = (|ToMasterKey|)
module Class =
    let (|Name|) (value: PatternContextHolder<KeyClass>): _ =
        value
        |> PatternContext.mapc (fun ctx value ->
            ctx.cache.nameKeys[value.Name])
    let (|NameKey|) (value: PatternContextHolder<KeyClass>): _ = value |> PatternContext.map _.Name
    let (|Qualifiers|_|): PatternContextHolder<KeyClass> -> PatternContextHolder<NameKey array> voption = PatternContext.map _.Qualifiers >> PatternContext.filter (Array.isEmpty >> not)
    let (|Source|_|): PatternContextHolder<KeyClass> -> PatternContextHolder<SourceKey> voption = PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|TypeParameterKeys|_|): PatternContextHolder<KeyClass> -> _ = PatternContext.choose( _.TypeParameters >> function [||] -> ValueNone | value -> ValueSome value)
    let (|Members|): PatternContextHolder<KeyClass> -> _ = PatternContext.map _.Members
    let (|ConstructorKeys|): PatternContextHolder<KeyClass> -> _ = PatternContext.map _.Constructors
    let (|HeritageKeys|): PatternContextHolder<KeyClass> -> _ = PatternContext.map _.Heritage
    let (|Heritage|) = heritage
    let (|ToClassKey|) = PatternContext.mapc _.createClassKey
    let (|ToMasterKey|) = (|ToClassKey|) >> ClassKey.(|ToMasterKey|)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let qualifiers = (|Qualifiers|_|)
    let sourceKey = (|Source|_|)
    let typeParameterKeys = (|TypeParameterKeys|_|)
    let typeParameters = typeParameterKeys >> ValueOption.map (PatternContext.Array.cbind TypeParameterKey.toTypeParameter)
    let members = (|Members|)
    let constructorKeys = (|ConstructorKeys|)
    let constructors = constructorKeys >> PatternContext.Array.cbind ConstructorKey.toConstructor
    let heritageKeys = (|HeritageKeys|)
    let heritage = heritageKeys >> PatternContext.Array.cbind TypeReferenceKey.toTypeReference
    let toClassKey = (|ToClassKey|)
    let toMasterKey = (|ToMasterKey|)
module Constructor =
    let toConstructorKey: PatternContextHolder<KeyConstructor> -> _ = PatternContext.mapc _.createConstructorKey
    let toMasterKey = toConstructorKey >> ConstructorKey.toMasterKey
    let parameterKeys: PatternContextHolder<KeyConstructor> -> PatternContextHolder<ParameterKey array> = PatternContext.map _.Parameters
    let parameters = parameterKeys >> PatternContext.Array.cbind ParameterKey.toParameter
module ConstructorKey =
    let toConstructor: PatternContextHolder<ConstructorKey> -> _ = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.constructorKeys)
    let toMasterKey: PatternContextHolder<ConstructorKey> -> _ = PatternContext.mapc (fun ctx -> MasterBuilder.Constructor >> ctx.createMasterKey)
    let parameterKeys = toConstructor >> Constructor.parameterKeys
    let parameters = toConstructor >> Constructor.parameters
module ClassKey =
    let (|ToClass|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.classKeys)
    let (|Name|) (value: PatternContextHolder<ClassKey>): _ = value |> ((|ToClass|) >> KeyType.(|Name|))
    let (|NameKey|) (value: PatternContextHolder<ClassKey>): _ = value |> ((|ToClass|) >> KeyType.(|NameKey|))
    let (|Qualifiers|_|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> Class.(|Qualifiers|_|)
    let (|Source|_|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> PatternContext.map _.Source >> PatternContext.filter ((<>) SourceKey.nullKey)
    let (|TypeParameters|_|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> Class.(|TypeParameterKeys|_|)
    let (|Members|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> Class.(|Members|)
    let (|Constructors|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> Class.(|ConstructorKeys|)
    let (|Heritage|): PatternContextHolder<ClassKey> -> _ = (|ToClass|) >> Class.(|HeritageKeys|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Class >> ctx.createMasterKey)
    let name = (|Name|)
    let nameKey = (|NameKey|)
    let qualifiers = (|Qualifiers|_|)
    let sourceKey = (|Source|_|)
    let typeParameters = (|TypeParameters|_|)
    let members = (|Members|)
    let constructors = (|Constructors|)
    let heritage = (|Heritage|)
    let toClass = (|ToClass|)
    let toMasterKey = (|ToMasterKey|)
module Conditional =
    let (|Check|): PatternContextHolder<KeyConditional> -> _ = PatternContext.map _.Check
    let (|TrueBranch|): PatternContextHolder<KeyConditional> -> _ = PatternContext.map _.True
    let (|FalseBranch|): PatternContextHolder<KeyConditional> -> _ = PatternContext.map _.False
    let (|Extends|): PatternContextHolder<KeyConditional> -> _ = PatternContext.map _.Extends
    let (|ToConditionalKey|) = PatternContext.mapc _.createConditionalKey
    let (|ToMasterKey|) = (|ToConditionalKey|) >> ConditionalKey.(|ToMasterKey|)
    let check = (|Check|)
    let trueBranch = (|TrueBranch|)
    let falseBranch = (|FalseBranch|)
    let extends' = (|Extends|)
    let toConditionalKey = (|ToConditionalKey|)
    let toMasterKey = (|ToMasterKey|)
module ConditionalKey =
    let (|ToConditional|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.conditionalKeys)
    let (|Check|): PatternContextHolder<ConditionalKey> -> _ = (|ToConditional|) >> Conditional.(|Check|)
    let (|TrueBranch|): PatternContextHolder<ConditionalKey> -> _ = (|ToConditional|) >> Conditional.(|TrueBranch|)
    let (|FalseBranch|): PatternContextHolder<ConditionalKey> -> _ = (|ToConditional|) >> Conditional.(|FalseBranch|)
    let (|Extends|): PatternContextHolder<ConditionalKey> -> _ = (|ToConditional|) >> Conditional.(|Extends|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.Conditional >> ctx.createMasterKey)
    let check = (|Check|)
    let trueBranch = (|TrueBranch|)
    let falseBranch = (|FalseBranch|)
    let extends' = (|Extends|)
    let toConditional = (|ToConditional|)
    let toMasterKey = (|ToMasterKey|)
module TypeLiteral =
    let (|Members|): PatternContextHolder<KeyTypeLiteral> -> _ = PatternContext.map _.Members
    let (|IsCallMethod|_|): PatternContextHolder<KeyTypeLiteral> -> PatternContextHolder<KeyCallSignature> voption = function
        | Members members ->
            members
            |> PatternContext.filter (Array.length >> (=) 1)
            |> ValueOption.bind (
                PatternContext.Array.unzip
                >> Array.exactlyOne
                >> function MemberKey.CallSignature callSignature -> ValueSome callSignature | _ -> ValueNone
                )
    let (|ToTypeLiteralKey|) = PatternContext.mapc _.createTypeLiteralKey
    let (|ToMasterKey|) = (|ToTypeLiteralKey|) >> TypeLiteralKey.(|ToMasterKey|)
    let members = (|Members|)
    let isCallMethod = (|IsCallMethod|_|)
    let toTypeLiteralKey = (|ToTypeLiteralKey|)
    let toMasterKey = (|ToMasterKey|)
module TypeLiteralKey =
    let (|ToTypeLiteral|) = PatternContext.mapc (fun ctx -> Dictionary.Flip.item ctx.cache.typeLiteralKeys)
    let (|Members|): PatternContextHolder<TypeLiteralKey> -> _ = (|ToTypeLiteral|) >> TypeLiteral.(|Members|)
    let (|IsCallMethod|_|): PatternContextHolder<TypeLiteralKey> -> PatternContextHolder<KeyCallSignature> voption = (|ToTypeLiteral|) >> TypeLiteral.(|IsCallMethod|_|)
    let (|ToMasterKey|) = PatternContext.mapc (fun ctx -> MasterBuilder.TypeLiteral >> ctx.createMasterKey)
    let members = (|Members|)
    let isCallMethod = (|IsCallMethod|_|)
    let toTypeLiteral = (|ToTypeLiteral|)
    let toMasterKey = (|ToMasterKey|)

module Array =
    let (|Length|)<'T> (value: PatternContextHolder<'T[]>): int = PatternContext.value value |> Array.length
    module MasterKey =
        module Filter =
            let (|HadBoolean|NoBoolean|IsBoolean|) (value: PatternContextHolder<MasterKey array>) =
                match value with
                | DestructureContext(ctx, arr) ->
                    let setArr = Set arr
                    if setArr |> Set.isProperSubset Prelude.Union.Master.booleans then
                        let result =
                            arr
                            |> Array.filter (Prelude.Union.Master.booleans.Contains >> not)
                            |> Array.insertAt 0 Prelude.Primitive.Master.booleanKey
                            |> PatternContext.prepare ctx
                        HadBoolean result
                    elif setArr = Prelude.Union.Master.booleans then
                        let result = Prelude.Primitive.Master.booleanKey |> PatternContext.prepare ctx
                        IsBoolean result
                    else
                        let result = PatternContext.prepare ctx arr
                        NoBoolean result
            let (|HadNullable|NoNullable|) (value: PatternContextHolder<MasterKey array>) =
                value |> PatternContext.Array.cfold (fun (hadNull,acc) -> function
                    | MasterKey.IsNullish -> true, acc
                    | Value value -> hadNull, value :: acc
                    ) (false, [])
                |> PatternContext.yieldBy fst
                ||> fun hadNull rvalue ->
                    let result = 
                        rvalue
                        |> PatternContext.map (snd >> Array.ofList)
                    if hadNull then HadNullable result
                    else NoNullable value
        let (|ContainsNullish|NoContainedNullish|) (value: PatternContextHolder<MasterKey array>) =
            if value |> PatternContext.Array.cexists MasterKey.(|IsNullish|_|)
            then ContainsNullish
            else NoContainedNullish
        let (|ContainsNullable|NoContainedNullable|) (value: PatternContextHolder<MasterKey array>) =
            if value |> PatternContext.Array.cexists MasterKey.(|ContainsNullish|_|)
            then ContainsNullable else NoContainedNullable
        let (|IsAllLiterals|_|): PatternContextHolder<MasterKey array> -> bool =
            PatternContext.Array.cforall (MasterKey.KeyType.(|Literal|_|) >> ValueOption.isSome)
        let (|AllLiterals|_|): PatternContextHolder<MasterKey array> -> _ = function
            | IsAllLiterals as value ->
                value
                |> PatternContext.Array.cmap (function MasterKey.KeyType.Literal value -> value.Value | _ -> failwith "Not a literal")
                |> ValueSome
            | _ -> ValueNone
        let (|IsAllPrimitives|_|): PatternContextHolder<MasterKey array> -> bool =
            PatternContext.Array.cforall (MasterKey.KeyType.(|Primitive|_|) >> ValueOption.isSome)
        let (|IsAllLiteralLike|_|): PatternContextHolder<MasterKey array> -> bool = fun context ->
            PatternContext.Array.cforall (function
                | MasterKey.KeyType.Literal _
                | MasterKey.KeyType.Enum _
                | MasterKey.KeyType.EnumCase _ -> true
                | _ -> false
                ) context
        let (|AllLiteralLike|_|): PatternContextHolder<MasterKey array> -> _ = function
            | IsAllLiteralLike as value ->
                value
                |> PatternContext.Array.cmap (function
                    | MasterKey.KeyType.EnumCase (EnumCase.ToMasterKey value)
                    | MasterKey.KeyType.Enum (Enum.ToMasterKey value)
                    | MasterKey.KeyType.Literal (Literal.ToMasterKey value) -> ValueSome value
                    | _ -> ValueNone
                    )
                |> ValueSome
            | _ -> ValueNone
        
