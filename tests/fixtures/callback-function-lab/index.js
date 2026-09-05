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

export function callVoid(callback) {
    callback(7);
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
