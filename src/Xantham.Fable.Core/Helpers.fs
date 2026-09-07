[<AutoOpen>]
module XanthamFableCore

open Fable.Core
open Fable.Core.JS
open Fable.Core.DynamicExtensions
open Fable.Core.JsInterop

[<AutoOpen>]
type PropTypeBuilder =
    static member inline proptypekey(key: 'T -> 'ReturnType) : proptypekey<'T, 'ReturnType> = unbox key
    static member inline proptypekey(_: 'T -> 'A, _: 'T -> 'B) : proptypekey<'T, U2<'A, 'B>> = unbox ()
    static member inline proptypekey(_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C) : proptypekey<'T, U3<'A, 'B, 'C>> = unbox ()

    static member inline proptypekey
        (_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C, _: 'T -> 'D)
        : proptypekey<'T, U4<'A, 'B, 'C, 'D>> =
        unbox ()

    static member inline proptypekey
        (_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C, _: 'T -> 'D, _: 'T -> 'E)
        : proptypekey<'T, U5<'A, 'B, 'C, 'D, 'E>> =
        unbox ()

    static member inline proptypekey
        (_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C, _: 'T -> 'D, _: 'T -> 'E, _: 'T -> 'F)
        : proptypekey<'T, U6<'A, 'B, 'C, 'D, 'E, 'F>> =
        unbox ()

    static member inline proptypekey
        (_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C, _: 'T -> 'D, _: 'T -> 'E, _: 'T -> 'F, _: 'T -> 'G)
        : proptypekey<'T, U7<'A, 'B, 'C, 'D, 'E, 'F, 'G>> =
        unbox ()

    static member inline proptypekey
        (_: 'T -> 'A, _: 'T -> 'B, _: 'T -> 'C, _: 'T -> 'D, _: 'T -> 'E, _: 'T -> 'F, _: 'T -> 'G, _: 'T -> 'H)
        : proptypekey<'T, U8<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H>> =
        unbox ()

module KeyOf =
    let inline fromPropertyKey (key: typekeyof<'T, 'ReturnType>) : keyof<'T> = !!key
    let inline value (key: keyof<'T>) : string = !!key

    let inline item (key: keyof<'T>) (obj: 'T) : obj option =
        let value = obj.Item(key.Value)
        if isNull value then None else Some value

    /// <summary>
    /// Alias for <c>item</c>. An inline function rather than a value binding: Fable cannot
    /// take an inline function as a first-class value, and the run gate is where that showed.
    /// </summary>
    let inline access (key: keyof<'T>) (obj: 'T) : obj option = item key obj

    let inline unsafeUnbox<'T, 'ReturnType> (key: keyof<'T>) : typekeyof<'T, 'ReturnType> = !!key

    /// <summary>
    /// Unboxes the property key with the path of the given function.
    /// </summary>
    /// <param name="func"></param>
    /// <param name="key"></param>
    let inline tryUnbox<'T, 'ReturnType> (func: 'T -> 'ReturnType) (key: keyof<'T>) =
        if Experimental.nameofLambda func = value key then
            unsafeUnbox<'T, 'ReturnType> key |> Some
        else
            None

module TypeKeyOf =
    let inline value (key: typekeyof<'T, 'ReturnType>) : string = !!key
    let inline box (key: typekeyof<'T, 'ReturnType>) : keyof<'T> = !!key
    /// <summary>
    /// Create a path key. You should not access nested properties directly.
    /// </summary>
    let inline create (path: 'T -> 'ReturnType) : typekeyof<'T, 'ReturnType> = Experimental.nameofLambda path |> unbox
    /// <summary>
    /// Access a property of an object using a path.
    /// </summary>
    /// <param name="key">The path key</param>
    /// <param name="obj">Object being accessed</param>
    let inline item (key: typekeyof<'T, 'ReturnType>) (obj: 'T) : 'ReturnType = obj.Item(key.Value) |> unbox
    /// <summary>
    /// Alias for <c>item</c>; inline for the reason <c>KeyOf.access</c> is.
    /// </summary>
    let inline access (key: typekeyof<'T, 'ReturnType>) (obj: 'T) : 'ReturnType = item key obj

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
let inline keyof<'T, 'ReturnType> (fn: 'T -> 'ReturnType) : keyof<'T> = TypeKeyOf.create fn |> TypeKeyOf.box

/// <summary>Equivalent to <c>keyof</c> but keeps the return type.</summary>
let inline typekeyof<'T, 'ReturnType> (fn: 'T -> 'ReturnType) : typekeyof<'T, 'ReturnType> = TypeKeyOf.create fn



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
let inline (|KeyIs|_|) (value: string) (comp: keyof<'T>) : bool = comp.Value = value

/// <summary>Pattern matching against a type property key using strings.</summary>
let inline (|TypeKeyIs|_|) (value: string) (comp: typekeyof<'T, 'U>) : bool = comp.Value = value


/// <summary>
/// Crossing the brand boundary. Every operation here is an erased cast: it changes what the
/// type checker will accept and nothing about the value.
/// </summary>
module Brand =

    /// <summary>Apply a brand to a raw string.</summary>
    let inline tagString<[<Measure>] 'm> (value: string) : string<'m> = unbox value

    /// <summary>Drop the brand from a string, recovering the primitive.</summary>
    let inline untagString<[<Measure>] 'm> (value: string<'m>) : string = unbox value

    /// <summary>Apply a brand to a raw bool.</summary>
    let inline tagBool<[<Measure>] 'm> (value: bool) : bool<'m> = unbox value

    /// <summary>Drop the brand from a bool, recovering the primitive.</summary>
    let inline untagBool<[<Measure>] 'm> (value: bool<'m>) : bool = unbox value

    /// <summary>Apply a brand to a raw char.</summary>
    let inline tagChar<[<Measure>] 'm> (value: char) : char<'m> = unbox value

    /// <summary>Drop the brand from a char, recovering the primitive.</summary>
    let inline untagChar<[<Measure>] 'm> (value: char<'m>) : char = unbox value
