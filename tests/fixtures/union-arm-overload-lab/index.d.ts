// Lab: `expand-union-arms`. One export per predicate outcome, so the pass's refusals are
// observable separately from its expansions.

/** Expands: two distinct arms, under the cap. */
export declare function label(value: string | number): string;

/**
 * Expands, and is the load-bearing case: inside the union a bare lambda has no target type to
 * infer against, so `!^ (fun a b -> "x")` is FS0002. The arm overload gives it one.
 */
export declare function apply(handler: string | ((a: number, b: number) => string)): string;

/**
 * Not a candidate by the time the pass runs: `number[]` and `ReadonlyArray<number>` are one F#
 * type, so the arms are already deduped to a single `float[]` and there is no union left. The
 * arms-collapse guard is exercised in Shape.test.fs, on a model TypeScript cannot express.
 */
export declare function collapse(items: number[] | ReadonlyArray<number>): string;

/**
 * Declines: the second declared overload already occupies `(value: string)`, which is what
 * expanding the first would synthesize.
 */
export declare function tint(value: string | number): string;
export declare function tint(value: string): string;

/**
 * Declines: the `(x: string)` arm is a prefix of the second declared overload, whose tail is
 * optional, so a call supplying `x` alone selects either.
 */
export declare function prefix(x: string | number): string;
export declare function prefix(x: string, y?: number): string;

/** Declines: five arms against a cap of four. */
export declare function wide(value: string | number | boolean | number[] | string[]): string;

/** Not a candidate: two union parameters is what `policy: "linear"` is for. */
export declare function pair(left: string | number, right: string | boolean): string;

/** Not a candidate: an optional union parameter. */
export declare function maybe(value?: string | number): string;
