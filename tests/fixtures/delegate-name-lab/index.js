// The runtime half: every export reports the arity it received and the values each position
// carried, so the run gate reads the JavaScript call against the F# declaration.

const report = (fn, ...args) => `${fn.length}:${fn(...args)}`;

export function callTwo(handler) {
    return report(handler, 1, 2);
}

export function callThree(handler) {
    return report(handler, 1, 2, "z");
}

export function callVoidTwo(handler) {
    handler(1, 2);
    return String(handler.length);
}

export function callNamed(handler) {
    return report(handler, 3, 4);
}

export function fire(t) {
    const done = t.onDone ? `${t.onDone.length}` : "none";
    return `${report(t.onTick, 1, 2)}|${report(t.onDrag, 3, 4)}|${done}`;
}

export const target = {
    onTick: (x, y) => `tick${x}${y}`,
    onDrag: (x, y) => `drag${x}${y}`,
    onDone: () => {},
};

export const factory = {
    pair: (x, y) => `pair${x}${y}`,
    make: (seed) => (x, y) => `made${seed}${x}${y}`,
};

export function drive(source) {
    return `${report(source.pair, 1, 2)}|${report(source.make(9), 1, 2)}`;
}

export function callNesting(outer) {
    const inner = outer(7);
    return `${outer.length}:${inner.length}:${inner(5)}`;
}
