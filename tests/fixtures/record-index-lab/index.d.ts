// ---------------------------------------------------------------------------
// 4.10, an anonymous pure index signature: no interface declares it, so it
// resolves through the support package's Record/ReadonlyRecord rather than
// minting a name of its own.
// ---------------------------------------------------------------------------

/** An inline string-keyed index signature, in property position. */
export interface Cache {
    entries: { [key: string]: number };
}

/** An inline numeric-keyed index signature, in property position. */
export interface Grid {
    rows: { [index: number]: string };
}

/** An inline readonly index signature, in property position - `ReadonlyRecord`. */
export interface Frozen {
    values: { readonly [key: string]: boolean };
}

// ---------------------------------------------------------------------------
// 4.10, a named declaration that is nothing but an index signature: it is a
// declaration, not an anonymous shape, so it keeps the name its own consumers
// use rather than collapsing to a bare `Record` reference.
// ---------------------------------------------------------------------------

/** A pure index signature declared under its own name. */
export interface Bag {
    [key: string]: number;
}

// ---------------------------------------------------------------------------
// 4.10, an index signature carried beside a real member: the support package
// expresses the index signature alone, not the rest of the shape, so this
// still mints a name.
// ---------------------------------------------------------------------------

/** An index signature beside a real member. */
export interface Config {
    name: string;
    [key: string]: string;
}

// ---------------------------------------------------------------------------
// 4.10, a pure index signature over a generic operand this fixture's own
// scope does not close.
// ---------------------------------------------------------------------------

/** An inline index signature keyed by a generic function's own type parameter. */
export declare function tag<T>(value: { [key: string]: T }): void;

// ---------------------------------------------------------------------------
// 4.10, a package that declares its own `Record`: the generated reference to
// the support package's `Record` must not resolve to this one instead.
// ---------------------------------------------------------------------------

/** The package's own `Record`, unrelated to the support package's. */
export interface Record {
    id: string;
}

/** An inline index signature alongside the package's own `Record`. */
export interface Ledger {
    entries: { [key: string]: number };
}
