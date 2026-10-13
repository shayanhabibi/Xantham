/** A resource released by `using`. */
export interface Disposable {
    [Symbol.dispose](): void;
}

/** An object whose `Object.prototype.toString` tag is its own. */
export interface Tagged {
    readonly [Symbol.toStringTag]: string;
    readonly name: string;
}

/** A finite list iterated by `for…of`. */
export interface Bag {
    readonly size: number;
    [Symbol.iterator](): Iterator<string>;
}

/** Iterates as its base does. */
export interface LabelledBag extends Bag {
    readonly label: string;
}

/** Iterates its own type parameter. */
export interface Box<T> {
    [Symbol.iterator](): IterableIterator<T>;
}

/** Iterates a union element. */
export interface Values {
    [Symbol.iterator](): Iterator<string | number>;
}

/** A step that yields a string. */
export interface Yielded {
    readonly done: false;
    readonly value: string;
}

/** The step past the last string. */
export interface Finished {
    readonly done: true;
    readonly value: undefined;
}

/** An iterator the package declares, read through its `next` result. */
export interface StringCursor {
    next(): Yielded | Finished;
}

/** Narrows its base's element: F# admits one seq instantiation per interface. */
export interface Strings extends Values {
    [Symbol.iterator](): StringCursor;
}

declare const key: unique symbol;

/** Keyed by the package's own unique symbol. */
export interface Keyed {
    readonly [key]: number;
    readonly name: string;
}

declare const iterator: unique symbol;

/** Keyed by a unique symbol spelled like a well-known one. */
export interface Impostor {
    [iterator](): Iterator<string>;
}

/** A writable well-known key, and a well-known method taking an optional argument. */
export interface Convertible {
    [Symbol.toStringTag]: string;
    [Symbol.toPrimitive](hint?: string): string;
}

/** Narrows a lib iterable's element. */
export interface Narrowed extends Iterable<string | number> {
    [Symbol.iterator](): Iterator<string>;
}

declare global {
    interface SymbolConstructor {
        readonly observable: symbol;
    }
}

/** Keyed by a package's augmentation of `SymbolConstructor`, absent without a polyfill. */
export interface Observed {
    [Symbol.observable](): Observed;
}

export declare const resource: Disposable;
export declare const tagged: Tagged;
export declare const bag: Bag;
export declare const convertible: Convertible;
