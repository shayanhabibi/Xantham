// Uninhabited intersections, per docs/plans/generator-type-mapping.md 4.6 and lane CC's
// wave-thirteen remeasurement of TR018 (docs/.ai/handovers/lane-cc.md §2).
//
// `animejs`'s "make not thenable" idiom intersects a class's own instance type with a
// `{ then: null }` marker, so that `await`ing an instance never resolves it as a promise.
// TypeScript reduces that whole intersection to `never`, not merely to an object whose `then`
// property is unreachable: both operands are object types, both carry members, and the
// checker still empties the aggregate's member list rather than typing `then` itself `never`.
// The pre-existing fallback misreads this as "intersection over a non-object operand" - false,
// since neither operand is a non-object. What this fixture pins is the correct read: the
// operand that did not mark the shared property nullable is the type a caller can reach.

/** The positive: a method's own class collides with a `{ then: null }` marker on itself. */
export declare class Timer {
    then(callback?: (self: Timer & { then: null }) => any): Promise<any>;
}

/** The same reduction spelled as an alias, at both the alias's own declaration and a use. */
export interface Named {
    then(): void;
}

export type Reduced = Named & { then: null };

export declare const reduced: Reduced;

// ---------------------------------------------------------------------------
// Negatives. Each differs from `Timer` in exactly one place.
// ---------------------------------------------------------------------------

/** No name collision: the marker names a property `Ticking` does not declare. */
export declare class Ticking {
    then(callback?: (self: Ticking & { paused: boolean }) => any): Promise<any>;
}

/** The collided name is not a member of the operand it is intersected against. */
export declare class Player {
    play(callback?: (self: Player & { then: null }) => any): void;
}

/** The collision is not unit-typed: TypeScript does not empty the whole intersection, so the
 *  loss (if any) belongs to the `then` member itself, not to `Chained`'s own declaration. */
export declare class Chained {
    then(callback?: (self: Chained & { then: string }) => any): Promise<any>;
}
