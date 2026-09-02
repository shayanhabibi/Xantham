// Hand-authored fixture for constructor objects: the static side of a class, and the type
// `typeof X` names at a member position (§4.4). F# has no first-class type for one, so the
// object is declared as an interface of its own whose construct signatures are
// `[<EmitConstructor>] Create` members - `$0` is the object the member is read off, so
// `scope.Gauge.Create(3)` reaches JavaScript as `new scope.Gauge(3)` rather than as a call.
//
// The two TypeScript spellings are both here, because the checker names them differently: a
// `declare class` gives its static side the class's own name, while `interface C` plus
// `declare const C: { new (...): C }` gives it `__type`. The negatives are here too: a
// constructor object nothing references must not be declared, and `typeof` over a plain value
// is just that value's type.

/** The instance side, written as an interface, of the `declare const` spelling. */
export interface Widget {
    readonly label: string;
    resize(by: number): Widget;
}

/** The `interface` + `declare const` spelling of a class - the shape every DOM class has in
 *  `lib.dom.d.ts`. The checker calls this anonymous object `__type`, so its name has to come
 *  from somewhere else: here, from the export it is the value of. `prototype` is the instance
 *  side, which is a declaration of its own and must not be repeated as a member. */
export declare const Widget: {
    readonly prototype: Widget;
    new (label: string): Widget;
    /** A property of the constructor object is a static of the class. */
    readonly DEFAULT_LABEL: string;
};

/** The `declare class` spelling. Its static side is reached only through `typeof Gauge` below. */
export declare class Gauge {
    constructor(size: number);
    readonly size: number;
    static readonly UNIT: string;
}

/** `typeof Gauge` at a member position - the construct the whole `ServiceWorkerGlobalScope`
 *  constructor table is made of. */
export interface Scope {
    readonly Gauge: typeof Gauge;
}

export declare const scope: Scope;

/** An interface whose only members are construct signatures, generic and overloaded. Without a
 *  mapping for them it has nothing to declare and abbreviates to `obj`. */
export interface ParcelFactory {
    new <T>(value: T): Parcel<T>;
    new (): Parcel<string>;
}

export interface Parcel<T> {
    readonly value: T;
}

export declare const parcels: ParcelFactory;

/** A negative: a class no `typeof` ever names. Its static side is `shape-classes`'s work, and
 *  a second interface for it would be referenced by nothing. */
export declare class Solo {
    constructor();
    tag(): string;
}

export declare const VERSION: string;

/** A negative: `typeof` over a plain value is that value's type, not a constructor object. */
export interface Meta {
    readonly version: typeof VERSION;
}
