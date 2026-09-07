[<AutoOpen>]
module Fable.Core.JS.JS

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
type typekeyof< ^T, ^ReturnType> =
    private
    | Value__ of string

    /// <summary>
    /// Convert a path key for a different type to the given type. UNSAFE.
    /// </summary>
    static member inline UnsafeCastFrom< ^T>(value: typekeyof<_, ^ReturnType>) : typekeyof< ^T, ^ReturnType > = !!value

    /// <summary>
    /// The string representation of the path.
    /// </summary>
    [<Emit("$0")>]
    member inline this.Value: string = !!this

    /// <summary>
    /// Access a property of an object without type checking the object type. UNSAFE.
    /// </summary>
    [<Extension>]
    static member inline UnsafeAccess(accessedObject: obj, key: typekeyof<_, ^ReturnType>) : ^ReturnType =
        accessedObject.Item(key.Value) |> unbox

    /// <summary>
    /// Access a property of an object using a path.
    /// </summary>
    [<Extension>]
    static member inline Access(accessedObject: ^T, key: typekeyof< ^T, ^ReturnType >) : ^ReturnType =
        accessedObject.Item(key.Value) |> unbox

    member inline this.Invoke(obj: ^T) : ^ReturnType = obj.Item(this.Value) |> unbox

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
/// let dune = makeBook (keyof&lt;_, _> _.Dune)
/// dune.Title.Value // "Dune"
/// </code></example>
[<Erase>]
type keyof<'T> =
    private
    | Value__ of string

    static member inline UnsafeCastFrom<'T>(value: keyof<_>) : keyof<'T> = !!value
    static member inline UnsafeCastReturnType<'ReturnType>(key: keyof<'T>) : typekeyof<'T, 'ReturnType> = !!key

    [<Emit("$0")>]
    member inline this.Value: string = !!this

    [<Extension>]
    static member inline Access(accessedObject: 'T, key: keyof<'T>) : obj option =
        accessedObject.Item(key.Value) |> Option.ofObj

    [<Extension>]
    static member inline UnsafeAccess(accessedObject: obj, key: keyof<_>) : obj option =
        accessedObject.Item(key.Value) |> Option.ofObj

    static member inline op_Implicit(key: typekeyof<'T, _>) : keyof<'T> = !!key
    member inline this.Invoke(obj: ^T) = obj.Item(this.Value) |> Option.ofObj

[<Erase>]
type proptypekey<'T, 'ReturnType> =
    private
    | Value__ of (proptypelock<'T> -> 'ReturnType)

    [<Erase>]
    member inline this.unlock(value: proptypelock<'T>) : 'ReturnType = unbox value

    [<Erase>]
    member inline this.lock(value: 'ReturnType) = unbox<proptypelock<'T>> value


/// <summary>
/// Used to represent a type can be any of the types of properties
/// for the given generic. Create a proptypekey with lambdas accessing each
/// of the properties to create a key which statically resolves the actual
/// types of the underlying value.
/// </summary>
and [<Erase>] proptypelock<'T> =
    private
    | Value__ of obj

    [<Emit("$0")>]
    member inline this.Item(value: proptypekey<'T, 'ReturnValue>) : 'ReturnValue = unbox value

/// <summary>
/// Branded primitives: the F# rendering of TypeScript's intersection brands
/// (<c>type UserId = string &amp; { __brand: "UserId" }</c>).
/// </summary>
/// <remarks>
/// <para>A brand exists to make two values that share a runtime representation refuse to
/// substitute for one another. F#'s units of measure are exactly that tool, and they are
/// erased at compile time, so a branded value costs nothing at runtime - which is the whole
/// requirement for a binding: the JavaScript on the other side only ever sees the
/// primitive.</para>
/// <para>Numeric brands need nothing from this module. <c>float&lt;UserId&gt;</c> and
/// <c>int&lt;Ticks&gt;</c> are ordinary measure applications, because measures are built to
/// annotate numbers.</para>
/// <para>Non-numeric primitives are the gap this module fills. A measure cannot be applied
/// to <c>string</c> directly, but <c>MeasureAnnotatedAbbreviation</c> - the mechanism
/// FSharp.UMX is built on - defines an abbreviation that carries one anyway. The plain
/// <c>string</c> type is unaffected: an application with no measure argument still resolves
/// to the primitive, so this abbreviation can sit in scope over generated code that uses
/// <c>string</c> everywhere.</para>
/// <para>The brand is enforced in both directions, which is the property that makes it worth
/// emitting: a <c>string&lt;UserId&gt;</c> is not a <c>string&lt;OrderId&gt;</c>, and a raw
/// <c>string</c> is neither. Crossing the boundary is deliberate, and <c>tag</c>/<c>untag</c>
/// are the only ways to do it.</para>
/// </remarks>
[<AutoOpen>]
module Brands =

    /// <summary>
    /// A <c>string</c> carrying a unit of measure, so that differently branded strings do
    /// not substitute for one another. Erased: the runtime value is the string itself.
    /// </summary>
    [<MeasureAnnotatedAbbreviation>]
    type string<[<Measure>] 'm> = string

    /// <summary>
    /// A <c>bool</c> carrying a unit of measure. Same erasure, same enforcement.
    /// </summary>
    [<MeasureAnnotatedAbbreviation>]
    type bool<[<Measure>] 'm> = bool

    /// <summary>
    /// A <c>char</c> carrying a unit of measure. Same erasure, same enforcement.
    /// </summary>
    [<MeasureAnnotatedAbbreviation>]
    type char<[<Measure>] 'm> = char


/// <summary>
/// A TypeScript index signature (<c>{ [key: K]: V }</c>): a value read and written through
/// <c>Item</c> compiles to the same property access an index signature's own type would.
/// </summary>
type Record<'Key, 'Value> =
    [<EmitIndexer>]
    abstract Item: 'Key -> 'Value with get, set

/// <summary>
/// A readonly TypeScript index signature (<c>{ readonly [key: K]: V }</c>): the same shape
/// as <see cref="Record{Key,Value}"/>, without the setter a readonly index signature never
/// gave it.
/// </summary>
type ReadonlyRecord<'Key, 'Value> =
    [<EmitIndexer>]
    abstract Item: 'Key -> 'Value

/// <summary>A record implementation that allows you to access properties using a property key</summary>
type PropertyRecord<'T, 'K> =
    [<EmitIndexer>]
    abstract Item: keyof<'T> -> 'K with get, set

type NoInfer<'T> = 'T
