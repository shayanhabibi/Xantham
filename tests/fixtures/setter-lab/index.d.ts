// Type parameters hoisted onto a callback alias (§4.9). F# has no rank-2 form, so a generic
// call signature's variables are written on the alias around it - and F# rejects a parameter
// list that names one variable twice.

/**
 * The shape of `solid-js`'s `Setter`: four call signatures, each declaring `U extends T`.
 * One name, one bound, one head slot.
 */
export type Setter<T> = {
  <U extends T>(...args: undefined extends T ? [] : [value: U | ((prev: T) => U)]): undefined extends T
    ? undefined
    : U;
  <U extends T>(value: (prev: T) => U): U;
  <U extends T>(value: U): U;
};

/** Negative: signatures naming different variables keep one head slot each. */
export type Distinct<T> = {
  <A extends T>(value: A): A;
  <B extends T>(value: B): B;
};

/** Negative: a single signature has nothing to collapse. */
export type Single<T> = {
  <U extends T>(value: U): U;
};

/** One name, two bounds: one variable cannot stand for both. */
export type DivergentBound<T> = {
  <U extends T>(value: U): U;
  <U extends string>(value: U): U;
};

/** Reference positions, including the empty-tuple rest `Setter<string | undefined>` reaches. */
export interface Holder {
  setter: Setter<string>;
  optional: Setter<string | undefined>;
  distinct: Distinct<string>;
  single: Single<string>;
  divergent: DivergentBound<string>;
}
