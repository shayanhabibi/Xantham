// The runtime behind callback-function-lab. Every export reports `fn.length` beside the result of
// calling the callback with all of its arguments at once, so a curried chain is visible twice: the
// length reads 1, and the call returns a function instead of a string.

const report = (fn, ...args) => `${fn.length}:${String(fn(...args))}`;

export function callNone(callback) {
    return report(callback);
}

export function callOne(callback) {
    return report(callback, 1);
}

export function callTwo(callback) {
    return report(callback, 1, 2);
}

export function callThree(callback) {
    return report(callback, 1, 2, 3);
}

// Arity 4, in both directions. `index.d.ts` declares neither: the arity-4 pair exists for the
// hand-written bindings in `RunGate/Program.fs`, which compare a named F# delegate against
// `System.Func` at an arity the generated golden does not reach.
export function callFour(callback) {
    return report(callback, 1, 2, 3, 4);
}

export function callVoid(callback) {
    callback(7);
    return callback.length;
}

export function callVoidTwo(callback) {
    callback(7, 8);
    return callback.length;
}

export function callNamed(formatter) {
    return report(formatter, 1.5, 2);
}

export function fire(handlers) {
    const done = handlers.onDone ? handlers.onDone.length : -1;
    return `${report(handlers.onTick, 1, 2)}:${done}`;
}

export const handlers = {
    onTick: (a, b) => `js:${a}:${b}`,
    onDone: (a) => undefined,
};

export function build(options) {
    return `${options.label}:${report(options.transform, 1, 2)}:${options.finish.length}`;
}

export const factory = {
    make: (seed) => (a, b) => `made:${seed}:${a}:${b}`,
    makeOne: (seed) => (a) => `one:${seed}:${a}`,
    makeNone: (seed) => () => `none:${seed}`,
    makeThree: (seed) => (a, b, c) => `three:${seed}:${a}:${b}:${c}`,
    ready: () => "ready",
    pair: (a, b) => `pair:${a}:${b}`,
};

// The read-back half of the arity-4 pair above.
export function makeFour(seed) {
    return (a, b, c, d) => `four:${seed}:${a}:${b}:${c}:${d}`;
}

export function callNesting(outer) {
    const inner = outer(5);
    return `${outer.length}:${inner.length}:${String(inner(1, 2))}`;
}

export function callNestingOne(outer) {
    const inner = outer(5);
    return `${outer.length}:${inner.length}:${String(inner(1))}`;
}

export function drive(factory) {
    const made = factory.make(5);
    return [
        factory.make.length,
        made.length,
        String(made(1, 2)),
        factory.ready.length,
        String(factory.ready()),
        factory.pair.length,
        String(factory.pair(1, 2)),
    ].join(":");
}

// A union arm reports the same pair as a bare callback - `length` beside the result of calling it
// with all its arguments - and reports the value itself when the non-callback arm arrived.
const describe = (value, ...args) =>
    typeof value === "function" ? `${value.length}:${String(value(...args))}` : `text:${String(value)}`;

export function callUnionNamed(listener) {
    return describe(listener, 1);
}

export function callUnionNone(listener) {
    return describe(listener);
}

export function callUnionOne(listener) {
    return describe(listener, 1);
}

export function callUnionTwo(listener) {
    return describe(listener, 1, 2);
}

export function makeUnionOne(seed) {
    return (a) => `one:${seed}:${a}`;
}

export function makeUnionTwo(seed) {
    return (a, b) => `two:${seed}:${a}:${b}`;
}

export const unionHandlers = {
    one: (a) => `js1:${a}`,
    two: (a, b) => `js2:${a}:${b}`,
    text: "plain",
};

export function fireUnion(handlers) {
    return [describe(handlers.one, 1), describe(handlers.two, 1, 2), describe(handlers.text, 1)].join("|");
}

export function callUnionObject(listener) {
    return typeof listener === "function"
        ? `${listener.length}:${String(listener(1))}`
        : `object:${listener.handleEvent.length}:${String(listener.handleEvent(1))}`;
}

export const objectUnion = (a) => `js:${a}`;

// A union arm behind an array, matching `U2<...>[]` on animejs's `Scope`.
export function fireArrayUnion(handlers) {
    return handlers.steps.map((step) => describe(step, 1)).join("|");
}

export const arrayUnionHandlers = {
    steps: [(a) => `js1:${a}`, "plain"],
};

// A union arm behind an `option`, matching `U2<...> option` on animejs's `repeat`.
export function fireOptionUnion(handlers) {
    return handlers.step === undefined ? "none" : describe(handlers.step, 1);
}

export const optionUnionHandlersSome = {
    step: (a) => `js1:${a}`,
};

export const optionUnionHandlersNone = {};

// A union nested inside a two-argument delegate's own type parameter, matching cloudflare's
// `Action<'Type, U2<(obj -> unit), EventListenerObject<Event>>, ...>` on `EventTarget`'s `Create`.
export function addListener(register) {
    register("fn-arm", (x) => {});
    register("obj-arm", { handleEvent: (x) => {} });
}
