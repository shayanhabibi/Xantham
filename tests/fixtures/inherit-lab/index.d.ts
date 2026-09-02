// Hand-authored fixture for §4.4's heritage rule: a declared `extends` base becomes an F#
// `inherit`, so the derived type upcasts to it, while its members stay declared in full.
//
// The positives and the negatives are the point. F# can only state the is-a relation when the
// base is a type this run itself declares as an interface; a base that widened away, and a base
// that names something outside this run, stay flattened, and each says which of the two it is.
// `inherit obj` is FS0887 and a cyclic inherit is FS0954, so neither may ever be emitted.

/** A base this run declares. */
export interface Base {
    name: string;
    at: number;
}
/** Inherits `Base` and still declares its members: F# admits the redeclaration, and it is what
 *  keeps the member list and a synthesized `Create` exact. */
export interface Derived extends Base {
    extra: boolean;
}
/** A member redeclared at a narrower type. The checker reports the narrowed one, and the
 *  `inherit` stands beside it. */
export interface Narrowed extends Base {
    name: "fixed";
}

/** The diamond: `Both` extends an interface that already extends the other, and both operands
 *  declare `volume`. F# admits both inherits and the shared member. */
export interface Loud {
    volume: number;
}
export interface Pitched extends Loud {
    pitch: number;
}
export interface Both extends Loud, Pitched {
    label: string;
}

/** A generic base: the argument is applied at the `inherit`, not dropped. */
export interface Box<T> {
    value: T;
}
export interface Labelled<T> extends Box<T> {
    label: string;
}
/** A generic base at a fixed argument. */
export interface Tagged extends Box<string> {
    tag: string;
}

/** A class base: the instance side inherits the base class's instance side. */
export declare class Node {
    constructor(id: number);
    readonly id: number;
}
export declare class Leaf extends Node {
    constructor(id: number);
    readonly leafy: boolean;
}

/** Not inherited: `Error` is the compiler lib's, and nothing shipped binds it, so the base has
 *  no F# name at this position at all and only its members survive. */
export interface Failure extends Error {
    code: number;
}

/** Not inherited: `Promise` is the compiler lib's too, but a shipped Fable package does bind
 *  it - under a name this run did not declare, so the members flatten and say so. */
export interface Deferred extends Promise<string> {
    tag: string;
}

/** A utility-type base: TypeScript's surgery over `Base` rather than a name it can inherit. */
export interface Slim extends Partial<Base> {
    slim: boolean;
}
