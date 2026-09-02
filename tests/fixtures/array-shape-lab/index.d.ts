/** An array reached through an interface of its own: the symbol is `Chapters`, not `Array`. */
export interface Chapters extends Array<string> {}

/** An array intersected with a shape: the symbol is the intersection's. */
export type Tagged = readonly number[] & { readonly kind: "tagged" };

/** A tuple the checker cannot collapse before the mapped type applies. */
type TupleOf<Length extends 0 | 1 | 2, Fill> = [[], [Fill], [Fill, Fill]][Length];

/** A mapped type over a deferred tuple operand: the symbol is `__type`. */
export type ReadonlyTuple<Element, Length extends 0 | 1 | 2> = Readonly<TupleOf<Length, Element>>;

/** Indexable by number and carrying `length`, with none of `Array`'s members. */
export interface Register {
    [slot: number]: string;
    length: number;
}

export declare const chapters: Chapters;
export declare const tagged: Tagged;
export declare const pair: ReadonlyTuple<string, 2>;
export declare const register: Register;
