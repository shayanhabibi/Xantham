// The declarations behind docs/fable5-workarounds.md. Each one is the smallest TypeScript that
// produces a binding whose loss is Fable 5's compilation model rather than F#'s type system,
// and the run gate exercises the documented workaround against `index.js` for every one.

// --- An erased union over two object arms (§1 of the document). -------------------------------
// No shared discriminant property, so this is a `U2`, not a TypeScriptTaggedUnion.

export interface Ok {
    value: string;
}

export interface Err {
    reason: string;
}

export type Outcome = Ok | Err;

/** Returns an `Err` when `fail`, an `Ok` otherwise. */
export declare function run(fail: boolean): Outcome;

// --- A base and a narrowing extension (§2). ---------------------------------------------------

export interface Shape {
    area: number;
}

export interface Circle extends Shape {
    radius: number;
}

/** One `Shape` that is a `Circle` and one that is not. */
export declare function shapes(): Shape[];

// --- A settable class static (§3). ------------------------------------------------------------

export declare class Budget {
    constructor(spent: number);
    readonly spent: number;
    /** Assignable from JavaScript. */
    static limit: number;
}

// --- `string | null` beside an absent member (§4). --------------------------------------------

export interface Slot {
    value: string | null;
}

/** Three slots: a string, an explicit `null`, and an absent property. */
export declare function slots(): Slot[];

/** `"string"`, `"null"` or `"absent"`, read off the property as JavaScript sees it. */
export declare function describe(slot: Slot): string;

// --- An interface the consumer supplies (§5). -------------------------------------------------

export interface Listener {
    name: string;
    notify(count: number): string;
}

/** `JSON.stringify` of the listener, then its own enumerable keys, then `notify(1)`. */
export declare function invite(listener: Listener): string;

// --- Two distinct objects with the same fields, one of them cyclic (§6). ----------------------

/** A fresh object every call. */
export declare function fresh(): Ok;

/** A fresh object every call, holding a reference to itself. */
export declare function cyclic(): Ok;
