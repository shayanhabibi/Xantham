// No import and no export anywhere in this file: that is what makes it a global script rather
// than a module, and what sends the generator down `harvest-globals` instead of
// `harvest-exports`. Adding an `export` to any declaration below changes what is under test.

/** A global type. A type reads the same wherever its name lives. */
declare interface Widget {
    /** The widget's label. */
    label: string;
    size?: number;
}

/** A global value: already on `globalThis`, so it binds with `[<Global>]`, not `[<Import>]`. */
declare const registry: Widget;

/** A mutable global. */
declare var counter: number;

/** A global function. */
declare function ping(target: string, retries?: number): boolean;

/** A global class: `[<Global>]` plus `[<EmitConstructor>]`. */
declare class Gadget {
    constructor(widget: Widget);
    readonly widget: Widget;
    spin(turns: number): this;
}

/** A nominal brand: the property can never hold a value, so F# gives it no setter. */
declare interface Branded {
    __brand: never;
    id: string;
}

/** A generic alias whose target never mentions its parameter - F# has no such abbreviation. */
declare type Loose<P> = { [key: string]: string };

/** A user of that alias: the reference has to widen when the alias goes. */
declare interface Bag {
    loose: Loose<Widget>;
}

/** An ambient module: importable from its specifier, which is not a global name at all. */
declare module "globals-lab:extra" {
    export const extra: number;
}
