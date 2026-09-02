// A constraint TypeScript reads structurally and F# reads nominally (§4.9). `Lengthy` is a
// named interface, so the head keeps `'T :> Lengthy`; every argument below satisfies it in
// TypeScript and none of them is an F# subtype of it.

export interface Lengthy {
  readonly length: number;
}

export interface Holder<T extends Lengthy> {
  readonly held: T;
}

/** A primitive argument. */
export type HeldString = Holder<string>;

/** A tuple argument. */
export type HeldTuple = Holder<[string, number]>;

/** An array argument. */
export type HeldArray = Holder<number[]>;

/** Negative: a named type that inherits the bound. */
export interface Sized extends Lengthy {
  readonly name: string;
}

export type HeldSized = Holder<Sized>;

/** The same four in member position. */
export interface Shelf {
  str: Holder<string>;
  tup: Holder<[string, number]>;
  arr: Holder<number[]>;
  sized: Holder<Sized>;
}
