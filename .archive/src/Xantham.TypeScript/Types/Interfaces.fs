namespace Xantham.TypeScript

open System.ComponentModel
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable

type IUnwrappable<'UnderlyingType> = Xantham.IUnwrappable<'UnderlyingType>
type IIdentityKey<'T> = Xantham.IIdentityKey<'T>

/// <summary>
/// The interfacing type is a union where the first field of every case is the provided
/// underlying type. This can be used to automatically bind members such as <c>_.Value</c>
/// which will skip any pattern matching, and explicitly access the first field of the union.
/// </summary>
[<Interface>]
type IFastUnionUnwrappable<'UnderlyingType> = interface end

/// <summary>
/// The interfacing type has the type checker inlined into it using a symbol.
/// </summary>
[<Interface>]
type IInlinedTypeChecker = interface end

/// <summary>
/// The interfacing type has the program (and therefor the type checker) inlined into it.
/// </summary>
[<Interface>]
type IInlinedProgram = interface end

/// <summary>
/// <para>Used as an internal type constraint.</para>
/// The inheritor of this interface is used to provide hints as to the behaviour
/// of the type checker, and compiler. This can constrain type parameters, such as
/// with the <c>IEmbedded</c> interface.
/// </summary>
type ICompilerBehaviourHint = interface end

type INeverSymbol = inherit ICompilerBehaviourHint
type ICanHaveSymbol = inherit ICompilerBehaviourHint
type IAlwaysSymbol = inherit ICanHaveSymbol

/// <summary>
/// The interface type can have type parameters; the typar can be used to explicitly
/// declare what the output type should be.
/// </summary>
type ICanHaveTypeParameters<'T> =
    inherit ICompilerBehaviourHint
    abstract TypeParameters: NonEmptyArray<'T> option
type ICanHaveTypeParameters = ICanHaveTypeParameters<Ts.TypeParameterDeclaration>

type INeverType = inherit ICompilerBehaviourHint
type ICanHaveType = inherit ICompilerBehaviourHint
type IAlwaysType = ICanHaveType

/// <summary>
/// Ascribes a provided hint with regards to the object fields itself.
/// As an example, a type interfacing <c>IEmbedded&lt;IAlwaysSymbol></c> will always
/// have a symbol field in the object, regardless of whether the type checker will
/// provide a symbol when <c>getSymbolAtLocation</c> is called on the object itself or not.
/// </summary>
type IEmbedded<'EmbeddedHint when 'EmbeddedHint :> ICompilerBehaviourHint> =
    inherit ICompilerBehaviourHint

[<Interface; AllowNullLiteral>]
type ISymbol =
    static member inline op_Implicit(other: ISymbol): Ts.Symbol = unbox other

type ICanHaveSymbol<'SymbolKind when 'SymbolKind :> ISymbol> =
    inherit ICanHaveSymbol
type IAlwaysSymbol<'SymbolKind when 'SymbolKind :> ISymbol> =
    inherit IAlwaysSymbol

[<Interface; AllowNullLiteral>]
type INode =
    static member inline op_Implicit(other: INode): Ts.Node = unbox other

[<Interface; AllowNullLiteral>]
type IType =
    static member inline op_Implicit(other: IType): Ts.Type = unbox other

/// <summary>
/// A constructed erased wrapper, whereby the underlying object has had
/// the program and type checker inlined into it.
/// Interfacing types automatically bind helper members such as <c>_.MapWithProgram</c>
/// which allows mapping of the underlying type with the program provided as a parameter to the provided mapping function.
/// This is utilised to essentially abstract away the type checker and program from the user for our use case.
/// By ensuring returned objects are always wrapped in this type, we can ensure that the type checker and program are always
/// available to the user without explicitly passing them around.
/// </summary>
/// <remarks>
/// This allows tests to be written with a program being generated across different fixtures, but still being
/// able to collect all the nodes and types into one test fixture without having to risk contaminating the program/checker
/// state for that particular instance.
/// </remarks>
[<Interface>]
type IErasedWrapper<^UnderlyingType> =
    inherit IUnwrappable<^UnderlyingType>
    inherit IInlinedProgram

[<AutoOpen>]
module InterfaceExtensions =
    // implementation for some interfaces
    type Xantham.IUnwrappable<'T> with
        member inline this.Value = unbox<'T> this
    type IFastUnionUnwrappable<'T> with
        member inline this.Value: 'T = emitJsExpr this "$0.fields[0]"
    type Xantham.IIdentityKey<'T> with
        static member inline Create(key: int): 'T = unbox<'T> key
    type UnwrapSRTPHelper =
        static member inline unwrap(value: IUnwrappable<'T>): 'T = value.Value
        static member inline unwrap(value: IFastUnionUnwrappable<'T>): 'T = value.Value
        static member inline unwrap(value: ISymbol): Ts.Symbol = ISymbol.op_Implicit value
        static member inline unwrap(value: INode): Ts.Node = INode.op_Implicit value
        static member inline unwrap(value: IType): Ts.Type = IType.op_Implicit value
    
    let inline (|Unwrap|) value =
        ((^T or UnwrapSRTPHelper):(static member unwrap: ^T -> ^U) value)
    
    type IInlinedProgram with
        member inline this.Program = SymbolTypeKey.Program.unsafeGet this
        member inline this.TypeChecker = SymbolTypeKey.TypeChecker.unsafeGet this
        member inline this.Checker = this.TypeChecker
    
    module IFastUnionUnwrappable =
        let inline map (fn: 'T -> 'U) (value: #IFastUnionUnwrappable<'T>): 'U =
            fn value.Value
        let inline mapWithProgram (fn: Ts.Program -> 'T -> 'U) (value: 'Wrapper when 'Wrapper :> IFastUnionUnwrappable<'T> and 'Wrapper :> IInlinedProgram): 'U =
            fn value.Program value.Value
        let inline mapWithChecker (fn: Ts.TypeChecker -> 'T -> 'U) (value: 'Wrapper when 'Wrapper :> IFastUnionUnwrappable<'T> and 'Wrapper :> IInlinedProgram): 'U =
            fn value.TypeChecker value.Value
    module IErasedWrapper =
        /// Transforms the value to the wrapped interface by ensuring the program
        /// and type checker are inlined into the object.
        let inline create program (value: ^UnderlyingType): ^WrapperType when ^WrapperType :> IErasedWrapper<^UnderlyingType> =
            SymbolTypeKey.Program.addIfAbsent program value
            |> SymbolTypeKey.TypeChecker.addIfAbsent (program.getTypeChecker())
            |> unbox<'WrapperType>
        /// Standard map, using the type of the original object.
        let inline map (fn: ^UnderlyingType -> ^T) (value: #IErasedWrapper<^UnderlyingType>) = fn value.Value
        /// Wraps the outcome of the map by inlining the program and type checker
        let inline wrappedMap
            (fn: ^UnderlyingType -> ^T)
            (value: #IErasedWrapper<^UnderlyingType>)
            : ^U when ^U :> IErasedWrapper<^T> = value.Value |> fn |> create value.Program
        /// Wraps each element of the map with the program and type checker
        let inline wrappedArrayMap
            (fn: ^UnderlyingType -> ^TCol when ^TCol :> ^T seq)
            (value: #IErasedWrapper<^UnderlyingType>):
            ^U NonEmptyArray option when ^U :> IErasedWrapper<^T> =
            value.Value |> fn |> Seq.map (create value.Program) |> NonEmptyArray.create
        /// Wraps the element of the map if it gave Some value
        let inline wrappedMapMaybe
            (fn: ^UnderlyingType -> ^T option)
            (value: #IErasedWrapper<^UnderlyingType>):
            ^U option when ^U :> IErasedWrapper<^T> =
            fn value.Value
            |> Option.map (create value.Program)
        /// Wraps the elements of the map if it gave some sequential value
        let inline wrappedArrayMapMaybe
            (fn: ^UnderlyingType -> ^TCol option when ^TCol :> ^T seq)
            (value: #IErasedWrapper<^UnderlyingType>):
            ^U NonEmptyArray option when ^U :> IErasedWrapper<^T> =
            fn value.Value
            |> Option.bind (Seq.map (create value.Program) >> NonEmptyArray.create)
        /// Accesses (or initialises) the cache for the object, and adds the value for the given field.
        let inline setToCache
            (container: #IErasedWrapper<^UnderlyingType>)
            (field: string)
            (cacheValue: ^T) =
            let dict = SymbolTypeKey.Cache.getOrSet(fun () -> System.Collections.Generic.Dictionary()) container
            dict[field] <- cacheValue
        /// <summary>
        /// Alias for <c>setToCache</c> but returns the input value for convenience.
        /// </summary>
        let inline addToCache
            (container: #IErasedWrapper<^UnderlyingType>)
            (field: string)
            (cacheValue: ^T) =
            setToCache container field cacheValue
            cacheValue
        /// Tries to retrieve the value for the given field of the cache (if it exists) on the object.
        let inline tryGetFromCache<^T, ^UnderlyingType>
            (field: string)
            (container: IErasedWrapper<^UnderlyingType>) =
            let dict = SymbolTypeKey.Cache.getOrSet(fun () -> System.Collections.Generic.Dictionary()) container
            match dict.TryGetValue field with
            | true, v -> Some (unbox<'T> v)
            | _ -> None
        /// Gets the cache value for the given field, or applies the function to the input object to create the value,
        /// save it, and return it.
        let inline getFromCacheOrMap<^T, ^ErasedType, ^UnderlyingType when ^ErasedType :> IErasedWrapper<^UnderlyingType>>
            (field: string)
            (initFn: ^ErasedType -> ^T)
            (container: ^ErasedType) =
            match tryGetFromCache field container with
            | Some v -> v
            | None -> initFn container |> addToCache container field
        /// Gets the cache value for the given field, or runs the given function to initialise the value, and returns it.
        let inline getFromCacheOrInit<^T, ^UnderlyingType>
            (field: string)
            (initFn: unit -> ^T)
            (container: IErasedWrapper<^UnderlyingType>)= getFromCacheOrMap field (ignore >> initFn) container
        /// Alias for <c>setToCache</c>, but uses the name of the input value type as the field name.
        let inline setToCacheByType
            (container: #IErasedWrapper<^UnderlyingType>)
            (cacheValue: ^T) = setToCache container typeof<^T>.FullName cacheValue
        /// Alias for <c>addToCache</c>, but uses the name of the input value type as the field name.
        let inline addToCacheByType
            (container: #IErasedWrapper<^UnderlyingType>)
            (cacheValue: ^T) = addToCache container typeof<^T>.FullName cacheValue
        /// Tries to retrieve the value from the cache using the name of the return type as the field key.
        let inline tryGetFromCacheByType<^T, ^UnderlyingType>
            (container: IErasedWrapper<^UnderlyingType>)
            : ^T option = tryGetFromCache typeof<^T>.FullName container
        /// Tries to retrieve the value from the cache using the name of the return type as the field key, or applies
        /// the given function to the object to create the value, and then caches it.
        let inline getFromCacheOrMapByType<^T, ^ErasedType, ^UnderlyingType when ^ErasedType:>IErasedWrapper<^UnderlyingType>>
            (initFn: ^ErasedType -> ^T)
            (container: ^ErasedType)= getFromCacheOrMap typeof<^T>.FullName initFn container
        /// Tries to retrieve the value from the cache using the name of the return type as the field key, or runs
        /// the given function to create the value, and then caches it.
        let inline getFromCacheOrInitByType<^T, ^UnderlyingType>
            (initFn: unit -> ^T)
            (container: IErasedWrapper<^UnderlyingType>)= getFromCacheOrMapByType (ignore >> initFn) container
    
    type IErasedWrapper<^UnderlyingType> with
        member inline this.Map(fn: ^UnderlyingType -> ^T) = fn this.Value
        member inline this.MapWithProgram(fn: Ts.Program -> ^UnderlyingType -> ^T) = fn this.Program this.Value
        member inline this.CacheTypeMapWithProgram(fn: Ts.Program -> ^UnderlyingType -> ^T) = IErasedWrapper.getFromCacheOrInitByType (fun () -> fn this.Program this.Value) this
        member inline this.MapWithChecker(fn: Ts.TypeChecker -> ^UnderlyingType -> ^T) = fn this.TypeChecker this.Value
        member inline this.WrappedMap<^TWrapper, ^T when ^TWrapper :> IErasedWrapper<^T>>(fn: ^UnderlyingType -> ^T): 'TWrapper = IErasedWrapper.wrappedMap fn this
        member inline this.WrappedMap<'TWrapper, ^T when ^TWrapper :> IErasedWrapper<^T>>(fn: ^UnderlyingType -> ^T ResizeArray): 'TWrapper NonEmptyArray option = IErasedWrapper.wrappedArrayMap fn this
        member inline this.WrappedMap<'TWrapper, ^T when ^TWrapper :> IErasedWrapper<^T>>(fn: ^UnderlyingType -> ^T seq): 'TWrapper NonEmptyArray option = IErasedWrapper.wrappedArrayMap fn this
        member inline this.WrappedMap<'TWrapper, ^T when ^TWrapper :> IErasedWrapper<^T>>(fn: ^UnderlyingType -> ^T option): 'TWrapper option = IErasedWrapper.wrappedMapMaybe fn this
    

[<EditorBrowsable(EditorBrowsableState.Never)>]
type SRTPProgramHelper =
    static member inline program(value: IInlinedProgram) = value.Program
    static member inline program(value: Ts.Program) = value

type SRTPProgram<^T when (^T or SRTPProgramHelper):(static member program: ^T -> Ts.Program)> = ^T

module SRTPProgram =
    let inline get program = ((^T or SRTPProgramHelper):(static member program: ^T -> Ts.Program) program)
    