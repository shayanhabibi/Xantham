// The runtime half of the workaround lab. The run gate resolves the `fable-workaround-lab`
// specifier here, so every documented workaround is checked against JavaScript that behaves the
// way the `.d.ts` declares rather than against the F# type alone.

export function run(fail) {
    return fail ? { reason: "no" } : { value: "yes" };
}

export function shapes() {
    return [{ area: 4 }, { area: 12.56, radius: 2 }];
}

export class Budget {
    static limit = 100;

    constructor(spent) {
        this.spent = spent;
    }
}

export function slots() {
    return [{ value: "a" }, { value: null }, {}];
}

export function describe(slot) {
    if (!("value" in slot)) return "absent";
    return slot.value === null ? "null" : slot.value === undefined ? "undefined" : "string";
}

export function invite(listener) {
    const own = Object.keys(listener);
    return `${JSON.stringify(listener)}|${own.join(",")}|${listener.notify(1)}`;
}

export function fresh() {
    return { value: "same" };
}

export function cyclic() {
    const o = { value: "same" };
    o.self = o;
    return o;
}
