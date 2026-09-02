// The `(X & {})` autocomplete idiom, per docs/plans/generator-type-mapping.md 4.6.
//
// `"in" | "out" | (string & {})` is TypeScript's spelling for "one of these literals, or any
// string, and keep the literals in autocomplete". The object operand declares nothing, so the
// intersection is the other operand, and a union carrying one keeps every arm it had. What
// this fixture pins is which operands reduce away and which are a shape the mapping owes the
// reader.

// ---------------------------------------------------------------------------
// The idiom, in the spellings found in the wild.
// ---------------------------------------------------------------------------

/** The idiom itself: an empty object operand beside a primitive, inside a union. */
export type Ease = "in" | "out" | (string & {});

/** A reference position, so the reduction is visible where the alias is used. */
export declare const ease: Ease;

/** The idiom on its own, outside a union. */
export type Loose = string & {};

/** `Record<never, never>` is the same empty operand under another name (type-fest's `LiteralUnion`). */
export type Size = "small" | "large" | (string & Record<never, never>);

/** A number carries the idiom the same way. */
export type Weight = 100 | 200 | (number & {});

// ---------------------------------------------------------------------------
// The negatives. None of these has an empty operand to drop.
// ---------------------------------------------------------------------------

/** A real member on the object operand: both operands stand. */
export type Counted = string & { count: number };

/** A marker-only operand is a brand (§4.6, D11), and the measure survives. */
export type UserId = string & { readonly __brand: "UserId" };

/** Two object operands, one of them empty: the flattened interface still declares the member. */
export type Padded = { width: number } & {};
