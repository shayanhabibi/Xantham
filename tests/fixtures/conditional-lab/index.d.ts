// Conditional types (§4.11). TypeScript defers `T extends U ? X : Y` until it has an
// argument to test, and F# has no form that defers a type. Three shapes reach the shaper
// and they do not get the same answer: two of them name a branch, the third stays `obj`.

export interface Marker {
    readonly id: string;
}
export interface Tagged extends Marker {
    readonly tag: string;
}

// 1. The condition holds for every argument the head admits: `T`'s bound is already
//    assignable to what `T` is tested against, so the true branch is the answer whatever
//    `T` turns out to be. TypeScript still defers it - it tests the parameter without its
//    bound - so this arrives as a conditional and the run decides it.
export type Proven<T extends Tagged> = T extends Marker ? T : string;
export declare function proven<T extends Tagged>(value: Proven<T>): void;

// 2. One branch is `never`, so no application lands in it and the other branch is the type.
//    `solid-js`'s `RequiredParameter`, which reaches 8 member positions on that rung.
export type Inhabited<T> = T extends () => unknown ? never : T;
export declare function inhabited<T>(value: Inhabited<T>): void;

// 3. Two inhabited branches with no shared F# form. This one stays `obj`.
export type Divergent<T> = T extends string ? number : boolean;
export declare function divergent<T>(value: Divergent<T>): void;

// `undefined` is not `never`: an application does land in that branch, so the pair still
// diverges and the mapping is still `obj`.
export type OrUndefined<T> = T extends string ? number : undefined;

// Negatives.
// A condition over no type variable never reaches the shaper at all - the checker answers
// it and hands over the branch, so this is `float` with no finding of any kind.
export type Decided = string extends string ? number : boolean;
// A generic alias with no condition in it, which must keep its own shape.
export type Box<T> = {
    value: T;
};
