/// The support-package half of the compile gate. Generated bindings will carry
/// `open Xantham.Fable.Core`, which brings the measure-annotated abbreviations into scope
/// over code that uses `string`, `bool` and `char` on nearly every line. That the primitives
/// still resolve under that open is a property of the abbreviation, not an obvious one, so
/// it is gated rather than assumed.
module Xantham.Generator.CompileGate.BrandIdioms

open Xantham.Fable.Core

[<Measure>]
type UserId

[<Measure>]
type OrderId

[<Measure>]
type Millis

// The primitives still resolve with no measure argument, under the open.
let plainString: string = "plain"
let plainBool: bool = true
let plainChar: char = 'x'

// Numeric brands need nothing from the support package - measures annotate numbers natively.
let elapsed: float<Millis> = 16.0<Millis>

// Non-numeric brands go through the abbreviation.
let user: string<UserId> = Brand.tagString<UserId> "u-1"
let order: string<OrderId> = Brand.tagString<OrderId> "o-1"
let flag: bool<UserId> = Brand.tagBool<UserId> true
let initial: char<UserId> = Brand.tagChar<UserId> 'u'

// And back out again.
let userRaw: string = Brand.untagString user
let flagRaw: bool = Brand.untagBool flag
let initialRaw: char = Brand.untagChar initial

// A branded string is still a string at every use site that untags it.
let describe () =
    sprintf
        "%s/%s/%s/%b/%c/%f"
        (Brand.untagString user)
        (Brand.untagString order)
        userRaw
        flagRaw
        initialRaw
        (float elapsed)

// The negative direction is what makes the brand worth emitting; these must not compile,
// and are recorded here as prose because the gate has no way to assert a non-compile:
//   let bad: string<OrderId> = user      // FS0001, UserId is not OrderId
//   let bad2: string<UserId> = "plain"   // FS0001, a raw string is not branded
