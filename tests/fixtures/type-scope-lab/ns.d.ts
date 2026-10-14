import type { Shape } from "./entries.js";
/** Returns its argument. */
export declare function identity<T>(value: T): T;
/** A constrained namespace function. */
export declare function measure<T extends Shape>(shape: T): T;
/** A constant stays a property. */
export declare const version: string;
/** A generic function-typed constant stays a property: the declaration kind decides. */
export declare const format: <T>(value: T) => string;
export { relabel } from "./relabel.js";
/** A function merged with a namespace stays a property; the namespace's function is a method. */
export declare function tagged(value: string): string;
export declare namespace tagged {
    const kind: string;
    function strip(value: string): string;
}
