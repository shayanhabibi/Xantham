// `three`'s BufferGeometry, reduced: a structural `extends` constraint whose default
// argument satisfies it structurally but not nominally. TypeScript's `extends` on a type
// parameter is structural; F#'s `:>` is nominal, so `Geometry<Narrow>` is FS0001 against
// a constraint of `Wide` even though TypeScript accepts it - 328 FS0001 on the `three`
// rung, every one of them a use of `BufferGeometry<NormalBufferAttributes, ...>`.
// Reduced from `three`'s `src/core/BufferGeometry.d.ts`.
export interface Attr {
    readonly kind: "attr";
}
export interface GLAttr {
    readonly kind: "gl";
}

export type Narrow = Record<string, Attr>;
export type Wide = Record<string, Attr | GLAttr>;

export class Geometry<Attributes extends Wide = Narrow> {
    attributes: Attributes;
}

// The use site: the default argument is written out, and is not nominally `Wide`.
export declare const g: Geometry;

// Negatives: constraints whose nominal relation the run can actually state, and which
// must keep their rendered `:>`.
export interface Base {
    readonly tag: string;
}
export interface Derived extends Base {
    readonly extra: number;
}
// The argument is nominally the constraint itself.
export declare function exact<T extends Base>(value: T): T;
// The argument `inherit`s the constraint, so `:>` holds.
export declare const derived: Derived;
