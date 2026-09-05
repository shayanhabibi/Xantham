// The runtime half of the nested-name lab. `index.d.ts` is what the generator reads; this is
// what the Fable run gate (tests/Xantham.Generator.RunGate) executes the generated bindings
// against. Every implementation here is the smallest one that witnesses a value crossing the
// boundary as a type declared inside a nested module.

/** The gate's witness that a nested StringEnum arrives as its compiled string. */
export function backoffOf(retry) {
    return retry.backoff;
}

/** A nested shape built on the JavaScript side, for the F# side to read members off. */
export function defaultRetry() {
    return { attempts: 3, backoff: "exponential" };
}

/** Records what it was told on the global, so the gate can read it back. */
export function configure(settings) {
    globalThis.__nestedNameLabConfigure = settings;
}
