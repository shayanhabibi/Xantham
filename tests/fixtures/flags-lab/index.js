// The runtime half of the flags lab. The run gate (tests/Xantham.Generator.RunGate) resolves
// the `flags-lab` specifier here, so the `bigint` mapping is proven against a JavaScript side
// that refuses anything but a native `BigInt` - a compile gate can only say the F# type was
// spelled, not what Fable's erasure handed over.

/** Throws unless the value really is a native JavaScript BigInt. */
function requireBigInt(where, value) {
    if (typeof value !== "bigint") {
        throw new TypeError(`${where} expected a bigint, got ${typeof value}`);
    }

    return value;
}

export function total(amounts) {
    if (!Array.isArray(amounts)) {
        throw new TypeError(`total expected an array, got ${typeof amounts}`);
    }

    return amounts.reduce((sum, amount) => sum + requireBigInt("total", amount), 0n);
}

export function ledger(start) {
    let balance = requireBigInt("ledger", start);

    return {
        get balance() {
            return balance;
        },
        credit(amount) {
            balance += requireBigInt("credit", amount);
            return balance;
        },
    };
}

export function normalize(name) {
    if (typeof name !== "string") {
        throw new TypeError(`normalize expected a string, got ${typeof name}`);
    }

    return name.startsWith("on") ? name : `on${name}`;
}

export function shout(text) {
    return text.toUpperCase();
}

export function freeze(value) {
    return Object.freeze(value);
}

export function describe(key) {
    return typeof key;
}

export const brandTag = Symbol("flags-lab");
