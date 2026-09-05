/**
 * A callback in every position the corpus uses. Each runtime export reports the `length` of the
 * function it received, so the gate reads the arity JavaScript sees rather than the arity F#
 * declared.
 */

/** A callback of arity 0 in parameter position. */
export declare function callNone(callback: () => string): string;

/** A callback of arity 1 in parameter position. */
export declare function callOne(callback: (a: number) => string): string;

/** A callback of arity 2 in parameter position. */
export declare function callTwo(callback: (a: number, b: number) => string): string;

/** A callback of arity 3 in parameter position. */
export declare function callThree(callback: (a: number, b: number, c: number) => string): string;

/** A callback returning `void`: the arm that renders `Action` today. */
export declare function callVoid(callback: (a: number) => void): number;

/** A named callback type, which abbreviates on its own. */
export type Formatter = (value: number, digits: number) => string;

/** The named callback in parameter position, so the abbreviation is what crosses. */
export declare function callNamed(formatter: Formatter): string;

/** A callback carried by an interface member, required and optional. */
export interface Handlers {
    onTick: (a: number, b: number) => string;
    onDone?: (a: number) => void;
}

/** Reports the arity of each member of a handler object built in F#. */
export declare function fire(handlers: Handlers): string;

/** A handler object built in JavaScript, for reading a callback member back into F#. */
export declare const handlers: Handlers;

/** A method member, which the ParamObject pass binds as a callback-typed Create parameter. */
export interface Options {
    label: string;
    transform(a: number, b: number): string;
    finish(): void;
}

/** Reports the arity of the callback the Create literal carried. */
export declare function build(options: Options): string;

/** A callback in return position, at arity 2 and at arity 0. */
export interface Factory {
    make(seed: number): (a: number, b: number) => string;
    makeOne(seed: number): (a: number) => string;
    makeNone(seed: number): () => string;
    makeThree(seed: number): (a: number, b: number, c: number) => string;
    readonly ready: () => string;
    readonly pair: (a: number, b: number) => string;
}

export declare const factory: Factory;
