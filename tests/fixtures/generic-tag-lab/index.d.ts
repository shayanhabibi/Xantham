// Tagged unions over type parameters. A discriminated union declares the parameters its arms
// read - the alias's own, or the signature's where the union is written inline - and every
// reference applies them back, so no payload widens to `obj`.

export interface Base {
    readonly id: string;
}

// Two parameters, each read by one arm and both by the third.
export type Pair<T, U> =
    | { readonly kind: "left"; readonly value: T }
    | { readonly kind: "right"; readonly value: U }
    | { readonly kind: "both"; readonly left: T; readonly right: U };

// A parameter under an array and under a promise.
export type Batch<T> =
    | { readonly kind: "many"; readonly items: T[] }
    | { readonly kind: "later"; readonly pending: Promise<T> };

// A constrained parameter, read by one arm.
export type Owned<T extends Base> =
    | { readonly kind: "owned"; readonly owner: T }
    | { readonly kind: "orphan" };

// A parameter no arm reads.
export type Marker<T> = { readonly kind: "on" } | { readonly kind: "off" };

// A recursive reference re-applies the alias's own parameter.
export type Tree<T> =
    | { readonly kind: "leaf"; readonly value: T }
    | { readonly kind: "node"; readonly children: Tree<T>[] };

// Named interface arms: lib.es2020's `PromiseSettledResult<T>` form.
export interface Fulfilled<T> {
    readonly status: "fulfilled";
    readonly value: T;
}
export interface Rejected {
    readonly status: "rejected";
    readonly reason: string;
}
export type Settled<T> = Fulfilled<T> | Rejected;

// An intersection distributed over tagged arms, with a default: the Agents SDK's
// `Schedule<T = string>` form.
export type Job<T = string> = { readonly id: string; readonly payload: T } & (
    | { readonly type: "once"; readonly at: number }
    | { readonly type: "every"; readonly seconds: number }
);

// A nullable tagged union stays an abbreviation, of the application under `option`.
export type MaybeJob<T> = Job<T> | null;

// A second export of a generic tagged union abbreviates to it.
export { Job as Task };

// A new alias over an application is a declaration of its own.
export type JobAlias<T> = Job<T>;

// A transformed subset of a tagged union is its own union, not an application of the source.
export type Sided<T, U> = Extract<Pair<T, U>, { readonly kind: "left" | "right" }>;

// A literal union with a parameter it never reads stays a non-generic string enum.
export type Mode<T> = "fast" | "slow";

export interface Queue<T> {
    readonly head: Job<T>;
    readonly items: Job<T>[];
    readonly last?: Job<T>;
    readonly fallback: Job;
    readonly mode: Mode<string>;
    next(): Promise<Job<T>>;
    peek(): Job<T> | undefined;
    concrete(): Job<Base>;
    settle<U>(value: U): Settled<U>;
    sided<U>(value: U): Sided<T, U>;
    state(): { readonly kind: "idle" } | { readonly kind: "busy"; readonly job: Job<T> };
}

// Applications reached only beside `null` or `undefined`: a phantom parameter's argument, and an
// application no other member reaches. Two applications of a phantom parameter reduce to the
// shared arms, with no application left to read an argument from.
export interface Holder<T> {
    readonly marker?: Marker<string>;
    readonly nullable: Marker<number> | null;
    readonly either: Marker<string> | Marker<number>;
    poll(): Job<T> | undefined;
}

export declare function accept<T>(input: { readonly kind: "a"; readonly value: T } | { readonly kind: "b" }): T | undefined;
