// A member key and the declaration it names are two different names. The member keeps the key
// verbatim (mapping doc §4.14) and backticks carry anything through; the declaration is
// synthesized, and F# admits far less of it - `` type ``Registry@cf/meta`` = `` is FS0883,
// "Invalid namespace, module, type or union case name", which backticks do not rescue.
//
// So a synthesized name is reduced to the plain identifier shape: each run of characters
// outside it separates segments, and the segment after it is capitalised. The reduction is
// reported per declaration as SY005.

// ---------------------------------------------------------------------------
// The keys that name an illegal declaration. Every one of these is a real
// `@cloudflare/workers-types` model key shape.
// ---------------------------------------------------------------------------

/** The owner of members whose JavaScript keys are not F# declaration names. */
export interface Registry {
    /** Names `RegistryCfMeta`, and the member keeps the key. */
    "@cf/meta": {
        model: string;
        /** The sanitised name opens a module of its own: `RegistryCfMeta.Limits`. */
        limits: { tokens: number };
    };
    /** Two keys reduce to two names, so the reduction separates rather than collides. */
    "@cf/meta/llama-3": { model: string };
    /** A leading `$`, which is legal in JavaScript and not in a declaration name. */
    "$ref": { target: string };
}

// ---------------------------------------------------------------------------
// The negatives. A key that already concatenates into a legal name is left
// exactly as it was, and an identifier-shaped key opens a module as always.
// ---------------------------------------------------------------------------

/** The owner of keys that need no reduction. */
export interface Settings {
    /** Not nestable - it opens with a digit - but `Settings2fa` is a legal name, so it stands. */
    "2fa": { enabled: boolean };
    /** Identifier-shaped, so it opens `Settings.Timeouts` the way any member key does. */
    timeouts: { connectMs: number };
}

/** Reads the sanitised shape back, so the reference position is gated too. */
export declare function modelOf(entry: Registry["@cf/meta"]): string;
