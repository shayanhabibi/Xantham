// The runtime behind union-arm-overload-lab, for the run gate's delegate-arm check. `apply`
// reports a callback's `length` beside the result of calling it with both arguments at once, so a
// curried callback reads `1:` and returns a function instead of a string.

export function apply(handler) {
    return typeof handler === "function" ? `${handler.length}:${String(handler(1, 2))}` : `text:${handler}`;
}
