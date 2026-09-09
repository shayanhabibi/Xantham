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

/** The same arm at arity 2, where `Action<A, B>` has more than one argument to guarantee. */
export declare function callVoidTwo(callback: (a: number, b: number) => void): number;

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

/**
 * A callback whose own return is a callback. Where only some arities convert, this is the
 * nesting the rule has to decide: the outer function and the inner one need not agree.
 */
export declare function callNesting(outer: (seed: number) => (a: number, b: number) => string): string;

/** The same nesting with a unary inner callback, where both levels are alike. */
export declare function callNestingOne(outer: (seed: number) => (a: number) => string): string;

/** A `Factory` built in F#, so its callback members cross outward rather than back. */
export declare function drive(factory: Factory): string;

/**
 * A union of a callback arm and a non-callback arm, the shape the corpus carries as
 * `EventListenerOrEventListenerObject`. The erased union unwraps at runtime, so what crosses is
 * whichever arm was supplied.
 */
export type Listener = ((a: number) => string) | string;

/** The named union in parameter position, so the abbreviation is what crosses. */
export declare function callUnionNamed(listener: Listener): string;

/** A union arm of arity 0 in parameter position. */
export declare function callUnionNone(listener: (() => string) | string): string;

/** A union arm of arity 1 in parameter position. */
export declare function callUnionOne(listener: ((a: number) => string) | string): string;

/** A union arm of arity 2 in parameter position, where the delegate is retained. */
export declare function callUnionTwo(listener: ((a: number, b: number) => string) | string): string;

/** A union arm in return position at arity 1. */
export declare function makeUnionOne(seed: number): ((a: number) => string) | string;

/** A union arm in return position at arity 2. */
export declare function makeUnionTwo(seed: number): ((a: number, b: number) => string) | string;

/** Union-typed members, at both arities and with the non-callback arm supplied. */
export interface UnionHandlers {
    one: ((a: number) => string) | string;
    two: ((a: number, b: number) => string) | string;
    text: ((a: number) => string) | string;
}

/** A union-typed member object built in JavaScript, for reading a callback arm back into F#. */
export declare const unionHandlers: UnionHandlers;

/** Reports the arity of each union-typed member of an object built in F#. */
export declare function fireUnion(handlers: UnionHandlers): string;

/** The object arm of `EventListenerOrEventListenerObject`, whose method carries the same arity. */
export interface ListenerObject {
    handleEvent(a: number): string;
}

/** The corpus shape exactly: a callback arm beside an object arm rather than beside a primitive. */
export declare function callUnionObject(listener: ((a: number) => string) | ListenerObject): string;

/** The same union built in JavaScript, for reading the callback arm back into F#. */
export declare const objectUnion: ((a: number) => string) | ListenerObject;

/**
 * A union-typed member one level deeper: behind an array, matching
 * `U2<ScopeConstructorCallback, (Scope -> Tickable)>[]` on `animejs`'s `Scope`.
 */
export interface ArrayUnionHandlers {
    steps: (((a: number) => string) | string)[];
}

/** Reports the arity of each element of the array member built in F#. */
export declare function fireArrayUnion(handlers: ArrayUnionHandlers): string;

/** The same array member built in JavaScript, for reading its callback arm back into F#. */
export declare const arrayUnionHandlers: ArrayUnionHandlers;

/**
 * A union-typed member one level deeper: behind an `option`, matching
 * `U2<bool, (ScrollObserver -> bool)> option` on `animejs`'s `repeat`.
 */
export interface OptionUnionHandlers {
    step?: ((a: number) => string) | string;
}

/** Reports the arity of the optional member built in F#, or its absence. */
export declare function fireOptionUnion(handlers: OptionUnionHandlers): string;

/** The optional member present, built in JavaScript. */
export declare const optionUnionHandlersSome: OptionUnionHandlers;

/** The optional member absent, built in JavaScript. */
export declare const optionUnionHandlersNone: OptionUnionHandlers;

/** The object arm of the nested union below, matching `EventListenerObject<Event>` in shape. */
export interface UnionListenerObject {
    handleEvent(x: number): void;
}

/**
 * A two-argument void callback whose second parameter is itself a union of a function arm and an
 * object arm - the shape `Action<'Type, U2<(obj -> unit), EventListenerObject<Event>>, ...>`
 * carries on `EventTarget`'s `Create` in `@cloudflare/workers-types`. The outer callback converts
 * to `Action` by the arity rule already measured; whether the inner union arm keeps its own arity
 * once nested inside the delegate's own type parameter is what this measures.
 */
export declare function addListener(register: (kind: string, listener: ((x: number) => void) | UnionListenerObject) => void): void;

/** Generic callable identity: F# functions do not inherit JS.Function. */
export declare function keepFunction<T extends Function>(callback: T): T;

/** The same bound on a generic interface and one of its methods. */
export interface CallbackBox<T extends Function> {
    callback: T;
    keep<U extends Function>(callback: U): U;
}

/** A concrete callback argument must not be replaced with the dropped bound. */
export declare function boxedFunction(box: CallbackBox<(value: number) => number>): number;
