// The absence alphabet, per docs/plans/generator-type-mapping.md D1.
//
// TypeScript spells absence five ways and F# offers two forms for all five: `option` and
// `unit`. The rendered binding cannot separate them, so the manifest carries the separation.
// Each declaration below is one spelling; what the fixture pins is which findings the site
// carries, because that pair - the `?` fact and the hoisted-member fact - is the whole
// distinction available to a consumer.

// ---------------------------------------------------------------------------
// D1, the five shapes, one per member of one interface, so the owner strings differ only in
// the member name.
// ---------------------------------------------------------------------------

export interface Absence {
    /** `x?: T`. The `?` marker alone. */
    optionalOnly?: string;
    /** `x: T | undefined`. Required, and the type admits `undefined`. */
    unionUndefined: string | undefined;
    /** `x: T | null`. Required, and the type admits `null` - the KV `get` miss. */
    unionNull: string | null;
    /** `x?: T | null`. Both facts on one member. */
    optionalNull?: string | null;
    /** `x: T | null | undefined`. Both spellings in the union, and no `?` marker. */
    unionBoth: string | null | undefined;
    /** A `void`-returning method. `void` sits in a return position, not in a union. */
    returnsVoid(): void;
}

// ---------------------------------------------------------------------------
// The same alphabet away from object members: a bare function's return and its parameters.
// ---------------------------------------------------------------------------

/** A `void` return on a bare exported function. */
export declare function fireAndForget(topic: string): void;

/** A `null` return: the KV miss, at a call boundary. */
export declare function getOrNull(key: string): string | null;

/** An `undefined` return: the Durable Object storage miss. */
export declare function getOrUndefined(key: string): string | undefined;

/** An optional parameter - the `?` fact at a parameter position. */
export declare function withOptional(key: string, fallback?: string): string;

/** A parameter typed `T | null` - required, and the type admits `null`. */
export declare function withNullable(key: string, fallback: string | null): string;

/** A union carrying `void` beside a real member, so the third flag has a live site. */
export declare function voidOrValue(key: string): string | void;

// ---------------------------------------------------------------------------
// The negatives. None of these carries an absence fact, and none may be read as one.
// ---------------------------------------------------------------------------

export interface Present {
    /** Required and non-nullable. */
    always: string;
    /** A method returning a required value. */
    read(): string;
}
