/// Roles for the integers the decoder passes around.
///
/// The blob is full of `int`s and `uint32`s that mean entirely different things - a node index, a
/// string index, a byte offset into one section or another, a bitmap, a shift distance - and
/// nothing but the parameter name distinguishes them. These measures make the wrong one a
/// compile error, and erase completely, so the decoder pays nothing for them.
///
/// The bitwise operators are not defined for measured integers: `mask &&& mask` does not compile,
/// let alone `field &&& mask`. That is the point rather than an obstacle - it forces the
/// operations to be spelled out here, with the roles in the signature, so a mask cannot be used
/// where a shift belongs and masking cannot silently change what a value is.
module Xantham.TypeScript.Wire.Measures

/// A bitmap over some other value's bits. Masks are consumed by masking, never produced by it.
[<Measure>]
type mask

/// A node's raw 32-bit `data` word, before its type bits have been read.
[<Measure>]
type word

/// A bit count, for shift distances.
[<Measure>]
type bits

/// A bit position in a node's child mask, i.e. a declared child slot.
[<Measure>]
type astSlot

/// An index into the blob's node section. `Ast.Root` is one of these.
[<Measure>]
type nodeIndex

/// An index into the string offsets table. Word-indexed, not byte-indexed.
[<Measure>]
type stringIndex

/// A byte offset into the extended-data section, or within one record in it.
[<Measure>]
type byteOffset

/// A byte offset into the structured-data (msgpack) section.
[<Measure>]
type structuredOffset

/// Tags a raw `uint32` with the role it plays. Erased; this is a compile-time assertion that the
/// caller knows which of the blob's many integers it is holding.
let inline tag<[<Measure>] 'role> (value: uint32) : uint32<'role> =
    LanguagePrimitives.UInt32WithMeasure value

/// Tags a raw `int` with the role it plays.
let inline tagInt<[<Measure>] 'role> (value: int) : int<'role> =
    LanguagePrimitives.Int32WithMeasure value

[<AutoOpen>]
module Operators =

    /// Masks a value, keeping its role: masking a `word` yields a `word`. Deliberately
    /// asymmetric, so the mask has to be the right-hand operand.
    let inline (&&&&) (value: uint32<'role>) (mask: uint32<mask>) : uint32<'role> =
        tag (uint32 value &&& uint32 mask)

    /// True when every bit of `mask` is set in `value`.
    let inline hasAll (mask: uint32<mask>) (value: uint32<'role>) =
        uint32 value &&& uint32 mask = uint32 mask

    /// True when any bit of `mask` is set in `value`.
    let inline hasAny (mask: uint32<mask>) (value: uint32<'role>) =
        uint32 value &&& uint32 mask <> 0u

    /// Right-shifts a value, keeping its role. The distance is a bit count, not a mask.
    let inline (>>>>) (value: uint32<'role>) (count: int<bits>) : uint32<'role> =
        tag (uint32 value >>> int count)

    /// Re-tags a value whose role has changed because bits were taken out of it - masking the
    /// string index out of a data word makes it a string index. Explicit, because this is the
    /// one operation the measures cannot check.
    let inline reinterpret<[<Measure>] 'from, [<Measure>] 'into> (value: uint32<'from>) : uint32<'into> =
        tag (uint32 value)

    /// The single-bit mask selecting `slot`.
    let inline slotBit (slot: int<astSlot>) : uint32<mask> = tag (1u <<< int slot)

    /// The mask of every slot below `slot`, for counting the slots that precede it.
    let inline slotsBelow (slot: int<astSlot>) : uint32<mask> = tag ((1u <<< int slot) - 1u)
