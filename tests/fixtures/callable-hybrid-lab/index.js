// The runtime behind callable-hybrid-lab. Every export is a plain JavaScript function carrying
// extra properties, exactly what `Invoke` proves: `x.Invoke(a)` must run the function `x` is,
// not a method named `Invoke` on it.

export const widget = {
    handler: Object.assign((event) => `handled:${event}`, { enabled: true }),
};

export const trigger = Object.assign((value) => value * 2, { label: "double" });

export const multi = Object.assign((x, y) => (y === undefined ? x + 1 : x + y), { tag: "multi" });

export const ambiguous = Object.assign((x) => x * 10, { note: "ambiguous" });

export const identity = Object.assign((value) => value, { calls: 0 });

export const boxedNumber = Object.assign(() => 42, { value: 42 });

export const collides = Object.assign((x) => x + 1, { Invoke: "not-a-function" });
