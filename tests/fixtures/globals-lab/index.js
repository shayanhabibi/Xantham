// The runtime half of the globals lab. `index.d.ts` declares these as ambient globals, so the
// Fable run gate (tests/Xantham.Generator.RunGate) installs them on `globalThis` before the
// generated bindings run - a `[<Global>]` binding that reaches something else is the bug the
// gate exists to catch.

globalThis.registry = { label: "root", size: 3 };
globalThis.counter = 41;
globalThis.ping = (target, retries) => target === "up" && (retries === undefined || retries > 0);
globalThis.Gadget = class Gadget {
    static SPEED = 9;
    #turns = 0;
    constructor(widget) {
        this.widget = widget;
    }
    get turns() {
        return this.#turns;
    }
    spin(turns) {
        this.#turns += turns;
        return this;
    }
};
