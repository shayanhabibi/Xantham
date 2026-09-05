/**
 * A retained callback becomes a named delegate whose parameters read as TypeScript spelled
 * them. The runtime half reports the arity and the values each position received, so the run
 * gate reads the JavaScript arity against the F# declaration.
 */

/** A two-argument callback in parameter position. */
export declare function callTwo(handler: (x: number, y: number) => string): string;

/** A three-argument callback in parameter position. */
export declare function callThree(handler: (x: number, y: number, label: string) => string): string;

/** A two-argument callback returning nothing, which rendered `Action<A, B>`. */
export declare function callVoidTwo(handler: (x: number, y: number) => void): string;

/** A named callback alias, so the abbreviation is what crosses. */
export type TickHandler = (x: number, y: number) => string;

/** The alias in parameter position. */
export declare function callNamed(handler: TickHandler): string;

/** Two members of the same declared shape, and one nullary for contrast. */
export interface EventTarget {
    onTick: (x: number, y: number) => string;
    onDrag: (x: number, y: number) => string;
    onDone?: () => void;
}

/** Reports the arity of each member of a target built in F#. */
export declare function fire(target: EventTarget): string;

/** The same target built in JavaScript, for reading the members back into F#. */
export declare const target: EventTarget;

/** A retained callback in return position. */
export interface Factory {
    readonly pair: (x: number, y: number) => string;
    make(seed: number): (x: number, y: number) => string;
}

/** A factory built in JavaScript, so the delegate crosses inward. */
export declare const factory: Factory;

/** A factory built in F#, so the delegate crosses outward. */
export declare function drive(source: Factory): string;

/** A unary callback returning a unary callback: retained at the outer level by the nesting rule. */
export declare function callNesting(outer: (seed: number) => (x: number) => string): string;
