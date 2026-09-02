// Hand-authored fixture for class statics: the members that live on the constructor object
// rather than on an instance. The checker reports them as properties of a class export's
// *value* type, which is why `shape-classes` - the pass that already reads that type for its
// construct signatures - is where they are shaped.
//
// Two things this fixture exists to pin. Fable binds a static through a dotted selector
// (`[<Import("Counter.MAX", "statics-lab")>]` emits `import { Counter }` and `Counter.MAX`),
// and F# admits a static beside an abstract member only in one of the four name-collision
// cases. Both are verified by the run gate against `index.js`, not asserted from memory.

/** A class carrying the const-like statics that almost every DOM class does. */
export declare class Counter {
    constructor(start: number);
    /** The count so far. */
    readonly value: number;
    bump(by: number): Counter;

    /** A const-like static: `readonly`, so F#'s get-only static is exact. */
    static readonly MAX: number;
    /** A static factory method. */
    static from(value: number): Counter;
    /** Static overloads: one JavaScript name, two signatures. */
    static of(value: number): Counter;
    static of(text: string): Counter;
    /** A settable static. Fable compiles an assignment to an imported static as a *call*, so
     *  this reads only, with a finding. */
    static tick: number;
}

/** JavaScript inherits statics down the prototype chain, and the checker reports the base's
 *  statics as the subclass's own - so `Doubling.MAX` is real and is emitted again here. */
export declare class Doubling extends Counter {
    double(): Doubling;
}

/** A static on a generic declaration. F# warns at a *use* that cannot infer the instantiation
 *  (`Box<_>.EMPTY`), but the declaration itself is legal and the value is reachable. */
export declare class Box<T> {
    constructor(value: T);
    readonly value: T;
    /** A static that mentions none of the class's parameters. */
    static readonly EMPTY: number;
}

/** The four ways a static's name can meet an instance member's. Only method-over-method is
 *  legal F#; the other three are compile errors, so those statics are dropped with a finding. */
export declare class Clash {
    /** Method over method: legal, and the shape `Response.json` has in the wild. */
    json(): number;
    static json(body: number): Clash;

    /** Property over property: FS0441, "duplicate property". */
    status: number;
    static status: number;

    /** Property over method: the method shadows it, and the use is FS3214. */
    text(): string;
    static text: number;

    /** Method over property: FS0434, "has the same name as a method". */
    url: string;
    static url(value: string): string;
}
