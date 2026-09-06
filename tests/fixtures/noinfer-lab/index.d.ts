// `NoInfer<T>` (§4.11's carve-out): TypeScript blocks inference through it, and the type
// denotes exactly `T`. No compiler ships the name here - a package that wants it declares its
// own, the way `solid-js` does, and the checker expands the reference to an indexed access
// before the shaper ever sees a name to widen on.
export type NoInfer<T> = [T][T extends any ? 0 : never];

export declare function widen<T>(seed: T, guard: NoInfer<T>): T;

export interface Options<T> {
    value: NoInfer<T>;
}

// Negative: an unrelated indexed access over a tuple still widens - the idiom is the name, not
// the shape alone.
export type FirstOf<T extends unknown[]> = T[0];
export declare function first<T extends unknown[]>(value: T): FirstOf<T>;
