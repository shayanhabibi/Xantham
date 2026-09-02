// Intersections of callable operands, per docs/plans/generator-type-mapping.md 4.6.
//
// `typeof round & Chained` is TypeScript's spelling for an overload set assembled out of two
// declarations. Both operands are object types and both carry call signatures alone, so the
// intersection has signatures and no properties. What this fixture pins is that the two
// positions agree: an export already renders the signatures, and a member must reach them too.

declare function round(value: number, length: number): number;

/** The chained form of the same helper: one argument, applied later. */
export type Chained = (length: number) => number;

/** Member position: the property's type is an intersection of two callables. */
export interface Utils {
    round: typeof round & Chained;
}

export declare const utils: Utils;

/** Export position: the same intersection, where the signatures already render. */
export declare const roundPad: typeof round & Chained;

// ---------------------------------------------------------------------------
// The negative. A hybrid carrying properties is a shape, not a callback.
// ---------------------------------------------------------------------------

/** Call signatures beside a property: the member set flattens and the signature is the loss. */
export interface Timers {
    schedule: (() => void) & { cancel(): void };
}
