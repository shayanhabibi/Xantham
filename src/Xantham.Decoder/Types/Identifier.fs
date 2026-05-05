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
    let sanitize (input: string) =
        input.Replace("@", "").Replace("$", "")
