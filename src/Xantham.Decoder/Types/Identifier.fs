namespace Xantham.Decoder

/// <summary>
/// Sanitization that converts raw TS-source names into strings that can be
/// rendered as F# identifiers without backtick-quoting for the common cases.
/// </summary>
/// <remarks>
/// Wired into <c>Name.Normalization</c> as a <c>SafeCustom</c> setting so
/// that anything still illegal after sanitization (F# reserved words,
/// leading-digit names) gets backtick-wrapped by the Fantomas fallback —
/// preserving idiomatic F# practice for consumer-facing names.
/// </remarks>
module Identifier =
    /// Drop characters whose only role is decorative — the npm scope <c>@</c>
    /// and the JS-convention <c>$</c> prefix — so they don't force
    /// backtick-quoting downstream. Structural separators like <c>.</c> and
    /// <c>/</c> are left in place; the pascal/camel-case regex recognizes
    /// them as word boundaries.
    ///
    /// Also strip characters that can never appear in any F# identifier:
    /// <c>{</c>, <c>}</c>, <c>,</c>, whitespace, newlines. These show up
    /// when TS uses destructured parameters (<c>fn({type, payload}: T)</c>)
    /// — the encoder preserves the literal destructure text as the
    /// parameter name. Without this strip we emit multi-line backtick
    /// identifiers that F# can't parse.
    let sanitize (input: string) =
        let mutable s = input.Replace("@", "").Replace("$", "")
        // Destructure-pattern punctuation (TS `fn({type, payload}: T)`)
        s <- s.Replace("{", "").Replace("}", "").Replace(",", "")
        s <- s.Replace(" ", "").Replace("\n", "").Replace("\r", "").Replace("\t", "")
        // Computed property name brackets (TS `Symbol.iterator` becomes
        // `[Symbol.iterator]` etc. in `.d.ts`). F# accepts backticked
        // identifiers with `[` and `]` as MEMBER names but rejects them
        // for TYPE names (FS0883). Strip so the modified form is usable
        // in either position; the original survives in `[<CompiledName>]`.
        s <- s.Replace("[", "").Replace("]", "")
        s
