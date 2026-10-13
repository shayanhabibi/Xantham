// The runtime behind symbol-member-lab: each value answers to the well-known symbol key its declaration names.
export const resource = { disposed: false, [Symbol.dispose]() { this.disposed = true; } };
export const tagged = { name: "lab", get [Symbol.toStringTag]() { return "Lab"; } };
export const convertible = { [Symbol.toStringTag]: "Lab", [Symbol.toPrimitive](hint) { return hint ?? "none"; } };
export const bag ={ size: 2, *[Symbol.iterator]() { yield "a"; yield "b"; } };
