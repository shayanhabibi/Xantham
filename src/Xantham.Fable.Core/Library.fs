[<AutoOpen>]
module Fable.Core.JsInterop.XanthamExtensions

open System.Runtime.CompilerServices
open Fable.Core
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop


/// <summary>
/// Equivalent to <c>keyof</c> but keeps the return type.
/// </summary>
/// <remarks>
/// <para>This type can be used to create a SRTP constraint for a property key of an
/// unknown type.</para>
/// <para>The following example creates an SRTP type to reflect the
/// a typed accessor for any object with a member 'ofValue'</para>
/// <code lang="fsharp">
/// type OfValueAccessor =
///     static member inline create&lt;
///         ^T, ^ReturnType when ^T:(member ofValue: ^ReturnType)
///     >(?object: ^T): typekeyof&lt;^T, ^ReturnType> 
///         = unbox "ofValue"
///     static member inline access&lt;
///         ^T, ^ReturnType when ^T:(member ofValue: ^ReturnType)
///     >(object: ^T): typekeyof&lt;^T, ^ReturnType> 
///         = object.ofValue
/// </code>
/// <para>Now we can observe usage. If we were to create an untyped ofValue <c>typekeyof</c> by using
/// the unit method call, then the result type will be <c>typekeyof&lt;obj, obj></c></para>
/// <para>Subsequent usage would instantly resolve the type to the object it is used on, but this would
/// invalidate it for usage on other types with the same property.</para>
/// <code lang="fsharp">
/// type TestObject = {
///     ofValue: int
/// }
/// type TestObject2 = {
///     ofValue: string
/// }
/// let testObject = { TestObject.ofValue = 1 }
/// let testObject2 = { TestObject2.ofValue = "1" }
///
/// let ofValueAccessor = OfValueAccessor.create()
/// <br/>
/// // the line below will resolve ofValueAccessor to typekeyof&lt;TestObject, int>
/// TypeKeyOf.access ofValueAccessor testObject
/// 
/// // The line before can no longer be used on testObject2, as it is of a different type
/// // TypeKeyOf.access ofValueAccessor testObject2
/// </code>
/// <para>We can also alternatively use the SRTP static method to access the value directly, without restricting the
/// method to a single object type.</para>
/// <code lang="fsharp">
/// // Both are valid and are resolved by the compiler correctly
/// OfValueAccessor.access testObject // int
/// OfValueAccessor.access testObject2 // string
/// </code>
/// <para>Naturally, this is standard usage of SRTP that can be done by hand anytime, and is nothing unique to
/// Xantham. Providing these types and tools in the library is done to allow the Xantham generators to refer
/// to these types, or to create these SRTP types during their generation steps.</para>
/// </remarks>
[<Erase>]
type typekeyof<^T, ^ReturnType> =
    private Value__ of string with
    /// <summary>
    /// Convert a path key for a different type to the given type. UNSAFE.
    /// </summary>
    static member inline UnsafeCastFrom<^T>(value: typekeyof<_, ^ReturnType>): typekeyof<^T, ^ReturnType> =
        !!value
    /// <summary>
    /// The string representation of the path.
    /// </summary>
    [<Emit("$0")>]
    member inline this.Value: string = !!this
    /// <summary>
    /// Access a property of an object without type checking the object type. UNSAFE.
    /// </summary>
    [<Extension>]
    static member inline UnsafeAccess(accessedObject: obj, key: typekeyof<_, ^ReturnType>): ^ReturnType =
        accessedObject.Item(key.Value)
        |> unbox
    /// <summary>
    /// Access a property of an object using a path.
    /// </summary>
    [<Extension>]
    static member inline Access(accessedObject: ^T, key: typekeyof<^T, ^ReturnType>): ^ReturnType =
        accessedObject.Item(key.Value)
        |> unbox
    member inline this.Invoke(obj: ^T): ^ReturnType = obj.Item(this.Value) |> unbox
    
/// <summary>
/// Get a property key from a mapping function for the given type.
/// </summary>
/// <example><code>
/// type Book&lt;'T> = { Title: keyof&lt;'T> }
/// type LibraryHires = {
///     Witcher: int
///     Dune: int
/// }
/// let makeBook (key: keyof&lt;LibraryHires>): Book&lt;LibraryHires> = {
///     Title = key
/// }
/// let dune = makeBook (keyof&lt;_> _.Dune)
/// dune.Title.Value // "Dune"
/// </code></example>
[<Erase>]
type keyof<'T> =
    private Value__ of string with
    static member inline UnsafeCastFrom<'T>(value: keyof<_>): keyof<'T> = !!value
    static member inline UnsafeCastReturnType<'ReturnType>(key: keyof<'T>): typekeyof<'T, 'ReturnType> = !!key
    [<Emit("$0")>]
    member inline this.Value: string = !!this
    [<Extension>]
    static member inline Access(accessedObject: 'T, key: keyof<'T>): obj option =
        accessedObject.Item(key.Value) |> Option.ofObj
    [<Extension>]
    static member inline UnsafeAccess(accessedObject: obj, key: keyof<_>): obj option =
        accessedObject.Item(key.Value) |> Option.ofObj
    static member inline op_Implicit(key: typekeyof<'T, _>): keyof<'T> = !!key
    member inline this.Invoke(obj: ^T) = obj.Item(this.Value) |> Option.ofObj
    
[<Erase>]
type proptypekey<'T, 'ReturnType> =
    private | Value__ of (proptypelock<'T> -> 'ReturnType)
    [<Erase>]
    member inline this.unlock(value: proptypelock<'T>): 'ReturnType = unbox value
    [<Erase>]
    member inline this.lock(value: 'ReturnType) = unbox<proptypelock<'T>> value
    

/// <summary>
/// Used to represent a type can be any of the types of properties
/// for the given generic. Create a proptypekey with lambdas accessing each
/// of the properties to create a key which statically resolves the actual
/// types of the underlying value.
/// </summary>
and [<Erase>] proptypelock<'T> = private Value__ of obj with
    [<Emit("$0")>]
    member inline this.Item (value: proptypekey<'T, 'ReturnValue>): 'ReturnValue = unbox value

[<AutoOpen>]
type PropTypeBuilder =
    static member inline proptypekey(key: 'T -> 'ReturnType): proptypekey<'T, 'ReturnType> = unbox key
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B
        ): proptypekey<'T, U2<'A, 'B>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C
        ): proptypekey<'T, U3<'A, 'B, 'C>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C,
        _: 'T -> 'D
        ): proptypekey<'T, U4<'A, 'B, 'C, 'D>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C,
        _: 'T -> 'D,
        _: 'T -> 'E
        ): proptypekey<'T, U5<'A, 'B, 'C, 'D, 'E>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C,
        _: 'T -> 'D,
        _: 'T -> 'E,
        _: 'T -> 'F
        ): proptypekey<'T, U6<'A, 'B, 'C, 'D, 'E, 'F>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C,
        _: 'T -> 'D,
        _: 'T -> 'E,
        _: 'T -> 'F,
        _: 'T -> 'G
        ): proptypekey<'T, U7<'A, 'B, 'C, 'D, 'E, 'F, 'G>> = unbox ()
    static member inline proptypekey(
        _: 'T -> 'A,
        _: 'T -> 'B,
        _: 'T -> 'C,
        _: 'T -> 'D,
        _: 'T -> 'E,
        _: 'T -> 'F,
        _: 'T -> 'G,
        _: 'T -> 'H
        ): proptypekey<'T, U8<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H>> = unbox ()

module KeyOf =
    let inline fromPropertyKey (key: typekeyof<'T, 'ReturnType>): keyof<'T> = !!key
    let inline value (key: keyof<'T>): string = !!key
    let inline item (key: keyof<'T>) (obj: 'T): obj option = obj.Access(key)
    /// <summary>
    /// Alias for <c>item</c>
    /// </summary>
    let access = item
    
    let inline unsafeUnbox<'T, 'ReturnType> (key: keyof<'T>): typekeyof<'T, 'ReturnType> = !!key
    /// <summary>
    /// Unboxes the property key with the path of the given function.
    /// </summary>
    /// <param name="func"></param>
    /// <param name="key"></param>
    let inline tryUnbox<'T, 'ReturnType> (func: 'T -> 'ReturnType) (key: keyof<'T>) =
        if Experimental.nameofLambda func = value key
        then unsafeUnbox<'T, 'ReturnType> key |> Some
        else None

module TypeKeyOf =
    let inline value (key: typekeyof<'T, 'ReturnType>): string = !!key
    let inline box (key: typekeyof<'T, 'ReturnType>): keyof<'T> = !!key
    /// <summary>
    /// Create a path key. You should not access nested properties directly.
    /// </summary>
    let inline create(path: 'T -> 'ReturnType): typekeyof<'T, 'ReturnType> = Experimental.nameofLambda path |> unbox
    /// <summary>
    /// Access a property of an object using a path.
    /// </summary>
    /// <param name="key">The path key</param>
    /// <param name="obj">Object being accessed</param>
    let inline item (key: typekeyof<'T, 'ReturnType>) (obj: 'T): 'ReturnType = obj.Access(key)
    /// <summary>
    /// Alias for <c>item</c>
    /// </summary>
    let access = item

/// <summary>
/// Get a property key from a mapping function for the given type.
/// </summary>
/// <example><code>
/// type Book&lt;'T> = { Title: keyof&lt;'T> }
/// type LibraryHires = {
///     Witcher: int
///     Dune: int
/// }
/// let makeBook (key: keyof&lt;LibraryHires>): Book&lt;LibraryHires> = {
///     Title = key
/// }
/// let dune = makeBook (keyof&lt;_> _.Dune)
/// dune.Title.Value // "Dune"
/// </code></example>
let inline keyof<'T> (fn: 'T -> obj) = TypeKeyOf.create fn |> TypeKeyOf.box

/// <summary>Equivalent to <c>keyof</c> but keeps the return type.</summary>
let inline typekeyof<'T, 'ReturnType> (fn: 'T -> 'ReturnType): typekeyof<'T, 'ReturnType> = TypeKeyOf.create fn

    

/// <summary>Pattern matching against a property key using strings.</summary>
/// <example>
/// <code>
/// type TestObject = {
///     Field1: int
///     Field2: string
/// }
/// let fieldAccessor = keyof _.Field1
/// match fieldAccessor with
/// | KeyIs "Field1" -> true
/// | KeyIs "Field2" -> false
/// </code>
/// </example>
let inline (|KeyIs|_|) (value: string) (comp: keyof<'T>): bool = comp.Value = value

/// <summary>Pattern matching against a type property key using strings.</summary>
let inline (|TypeKeyIs|_|) (value: string) (comp: typekeyof<'T, 'U>): bool = comp.Value = value

/// <summary>A record implementation that allows you to access properties using a property key</summary>
type PropertyRecord<'T, 'K> =
    [<EmitIndexer>]
    abstract Item: keyof<'T> -> 'K with get,set