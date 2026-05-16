namespace Xantham.Decoder

/// <summary>
/// Sanitization that converts raw TS-source names into strings renderable
/// as F# identifiers WITHOUT backtick-quoting. Renames are recorded as
/// <c>Name.Modified(original, renamed)</c> so downstream emitters attach
/// <c>[&lt;CompiledName(original)&gt;]</c> automatically, preserving JS
/// interop while keeping the consumer-facing F# API clean.
/// </summary>
module Identifier =
    /// F# keywords that cause compile errors when used unescaped as
    /// identifiers. Names matching these get a trailing underscore so the
    /// emitted F# parses without backtick-quoting.
    /// The list covers both current F# keywords and reserved words the
    /// compiler still enforces (e.g. <c>const</c>, <c>constraint</c>).
    /// Reserved future-use words the compiler accepts today as identifiers
    /// (<c>event</c>, <c>method</c>, <c>mixin</c>, <c>object</c>, etc.)
    /// are NOT included — renaming them would mass-rename real TS API
    /// surface (e.g. <c>event</c> parameter names) for no compile gain.
    let private fsharpKeywords =
        System.Collections.Generic.HashSet<string>([
            "abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"
            "const"; "constraint"
            "default"; "delegate"; "do"; "done"; "downcast"; "downto"
            "elif"; "else"; "end"; "exception"; "extern"; "false"; "finally"
            "fixed"; "for"; "fun"; "function"; "global"; "if"; "in"
            "inherit"; "inline"; "interface"; "internal"; "lazy"; "let"
            "match"; "member"; "module"; "mutable"; "namespace"; "new"
            "not"; "null"; "of"; "open"; "or"; "override"; "private"
            "public"; "rec"; "return"; "select"; "static"; "struct"; "then"
            "to"; "true"; "try"; "type"; "upcast"; "use"; "val"; "void"
            "when"; "while"; "with"; "yield"
        ])

    /// Word-form names for characters that survive <c>sanitize</c> intact
    /// but would still force backtick-quoting (e.g., <c>.</c>) — or that
    /// <c>sanitize</c> would strip entirely (e.g., <c>\n</c>), leaving the
    /// caller with an empty string. Used as a fallback when the sanitized
    /// result is empty; the original survives via <c>[&lt;CompiledName&gt;]</c>.
    let private specialCharNames =
        Map.ofList [
            '\n', "Newline"; '\r', "CarriageReturn"; '\t', "Tab"
            '.', "Dot"; ',', "Comma"; ':', "Colon"; ';', "Semicolon"
            '/', "Slash"; '\\', "Backslash"; '-', "Dash"; '+', "Plus"
            '*', "Asterisk"; '%', "Percent"; '@', "At"; '$', "Dollar"
            '#', "Hash"; '&', "Ampersand"; '?', "Question"
            '!', "Exclamation"; '=', "Equals"; '<', "LessThan"
            '>', "GreaterThan"; '|', "Pipe"; '^', "Caret"; '~', "Tilde"
            '`', "Backtick"; '\'', "Quote"; '"', "DoubleQuote"; ' ', "Space"
            '(', "LParen"; ')', "RParen"; '[', "LBracket"; ']', "RBracket"
            '{', "LBrace"; '}', "RBrace"
        ]

    /// Render an input string as a sequence of word-form names for any
    /// chars in the special-char table, leaving other chars as-is.
    /// Used as the empty-result fallback in <c>toSafe</c>.
    let private nameSpecialChars (input: string) =
        let sb = System.Text.StringBuilder()
        for c in input do
            match Map.tryFind c specialCharNames with
            | Some n -> sb.Append(n) |> ignore
            | None when System.Char.IsLetterOrDigit c || c = '_' ->
                sb.Append(c) |> ignore
            | None -> ()
        sb.ToString()

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

    /// Sanitize, and if every character was stripped, fall back to
    /// word-form names derived from the original input (e.g. <c>"\n"</c> →
    /// <c>"Newline"</c>). Preserves pascal/camel-case boundary characters
    /// (<c>.</c>, <c>/</c>) so upstream casing can still fold them into a
    /// single identifier. Used by the casing pipeline in <c>Name.fs</c>
    /// before pascal/camel folding runs.
    let sanitizeOrName (input: string) =
        let sanitized = sanitize input
        if System.String.IsNullOrEmpty sanitized then
            let renamed = nameSpecialChars input
            if System.String.IsNullOrEmpty renamed then "Anon" else renamed
        else sanitized

    /// Convert an arbitrary input string into a clean F# identifier with
    /// NO backtick-quoting. Rename rules:
    ///   - empty after sanitize → word-form names from <c>specialCharNames</c>;
    ///     if still empty, fall back to <c>"Anon"</c>.
    ///   - contains chars outside the F# bare-identifier alphabet
    ///     (<c>[A-Za-z0-9_']</c>) → strip them so the result needs no backticks.
    ///   - F# reserved keyword → append <c>_</c> (e.g. <c>fixed</c> → <c>fixed_</c>).
    ///   - bare <c>_</c> → <c>"anon"</c> (F# parses <c>_</c> as the wildcard
    ///     pattern, not a member name).
    ///   - leading digit → prefix <c>_</c>.
    /// Callers store the original alongside the renamed form via
    /// <c>Name.Modified</c>, and <c>[&lt;CompiledName&gt;]</c> emission
    /// picks the original up automatically for JS-interop round-trip.
    let toSafe (input: string) =
        let sanitized = sanitizeOrName input
        let stripUnsafe (s: string) =
            // Drop anything outside the F# bare-identifier alphabet so the
            // result needs no backticks. `.` and `/` are kept — they're the
            // structural word boundaries that pascal/camel-case folds
            // downstream. Every render site applies casing before final
            // emission, so by the time the identifier reaches F# output
            // they've been folded into camel/pascal-cased words.
            let sb = System.Text.StringBuilder()
            for c in s do
                if System.Char.IsLetterOrDigit c
                   || c = '_' || c = '\''
                   || c = '.' || c = '/' || c = '-'
                then sb.Append c |> ignore
            sb.ToString()
        let stripped = stripUnsafe sanitized
        let normalized = if System.String.IsNullOrEmpty stripped then "Anon" else stripped
        if normalized = "_" then "anon"
        elif System.Char.IsDigit normalized.[0] then "_" + normalized
        elif fsharpKeywords.Contains normalized then normalized + "_"
        else normalized
