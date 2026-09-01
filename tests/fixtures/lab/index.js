// The runtime half of the phase B lab. `index.d.ts` is what the generator reads; this is what
// the Fable run gate (tests/Xantham.Generator.RunGate) executes the generated bindings
// against, so every declaration there has one deliberately small implementation here.
// Behaviour is chosen to be checkable, not realistic: a timer that records what it was told.

export * as utils from "./utils.js";

export const version = "0.1.0-lab";

export const defaults = Object.freeze({ duration: 1000, unit: "ms", labels: [] });

export class Timer {
    #progress = 0;
    #calls = [];
    speed = 1;
    constructor(options) {
        this.options = { ...defaults, ...(options ?? {}) };
    }
    get progress() {
        return this.#progress;
    }
    /** What the chainable methods were called with, in order - the gate's witness. */
    get calls() {
        return this.#calls;
    }
    play() {
        this.#calls.push(["play"]);
        this.#progress = 1;
        if (this.options.onTick) this.options.onTick(this.#progress, this.#calls.length);
        return this;
    }
    seek(time, muteCallbacks) {
        this.#calls.push(["seek", time, muteCallbacks]);
        return this;
    }
    tween(...values) {
        this.#calls.push(["tween", ...values]);
        return this;
    }
}

export function createTimer(options) {
    return new Timer(options);
}

export function round(value, decimals) {
    if (typeof value === "string") return Number(value).toFixed(decimals);
    return Math.round(value);
}

/** The last settings `configure` received, for the gate to read back. */
export let configured = undefined;

export function configure(settings) {
    configured = settings;
}

/** Reads a discriminated shape the way TypeScript code would: off its tag. */
export function area(shape) {
    switch (shape.kind) {
        case "circle":
            return Math.PI * shape.radius * shape.radius;
        case "round-rect":
            return shape.width * shape.height;
        default:
            throw new Error(`unknown shape kind: ${String(shape.kind)}`);
    }
}

/** Hands back a tagged object built on the JavaScript side, for F# to match on. */
export function makeRoundRect(width, height, radius) {
    return { kind: "round-rect", width, height, radius };
}
