// Branding intersections, per docs/plans/generator-type-mapping.md 4.6 and D11.
//
// A brand is a primitive intersected with an object that has no runtime existence: the
// object's members are there only to make the type nominal. F# says the same thing with a
// unit of measure, which the compiler enforces and Fable erases - so the mapping is exact in
// spirit. What this fixture pins is the *detection*: which intersections are brands, which
// are ordinary object intersections, and which are neither.

// ---------------------------------------------------------------------------
// 4.6, branding intersections. The marker styles found in the wild.
// ---------------------------------------------------------------------------

/** The common hand-rolled brand: a primitive and a literal-typed marker property. */
export type UserId = string & { readonly __brand: "UserId" };

/** A second brand over the same primitive - distinct from the first, and that is the point. */
export type SessionId = string & { readonly __brand: "SessionId" };

/** The marker a unique symbol keys, exported so that the brand can be spelled elsewhere. */
export declare const orderIdTag: unique symbol;

/** The type-fest style: the marker is keyed by a unique symbol, so nothing can name it. */
export type OrderId = string & { readonly [orderIdTag]: true };

/** A branded number. Measures apply to numbers natively, with no support package at all. */
export type Millis = number & { readonly __brand: "Millis" };

/** A branded boolean. */
export type Verified = boolean & { readonly __brand: "Verified" };

/** A brand whose marker carries `never` - the marker is unconstructible, not just unnamed. */
export type Nonce = string & { readonly __nonce: never };

/** A brand over a literal union rather than a bare primitive. */
export type Mode = ("read" | "write") & { readonly __brand: "Mode" };

// ---------------------------------------------------------------------------
// Brands at use sites: the whole reason for detecting them.
// ---------------------------------------------------------------------------

export interface Store {
    /** A brand in a parameter and a plain primitive as the return. */
    get(id: UserId): string;
    /** Two different brands over two different primitives in one signature. */
    put(id: UserId, at: Millis): void;
    /** A brand under an array. */
    ids(): UserId[];
    /** A brand under an optional. */
    find(id?: SessionId): UserId | undefined;
    /** A branded property. */
    readonly owner: UserId;
}

/** A brand as a bare exported function's parameter and return. */
export declare function mint(seed: string): UserId;

// ---------------------------------------------------------------------------
// The negative cases. None of these is a brand and none may be read as one.
// ---------------------------------------------------------------------------

/** An intersection of two object types: a real shape, not a brand. */
export type Merged = { a: string } & { b: number };

/** A primitive intersected with an object that has a *real* member - not a brand. */
export type Counted = string & { count: number };

/** An object intersected with a branded primitive: still not a branded object. */
export type Wrapped = { id: UserId } & { at: Millis };
