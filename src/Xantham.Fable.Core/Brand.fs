namespace Xantham.Fable.Core

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
