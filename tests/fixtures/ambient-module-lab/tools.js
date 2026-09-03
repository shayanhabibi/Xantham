// The runtime behind `declare module "ambient-lab:tools"`. `register.mjs` resolves the
// specifier here, so a generated `[<Import(name, "ambient-lab:tools")>]` that reaches anything
// else is the bug the run gate exists to catch.

export class Hammer {
    static LIMIT = 12;
    constructor(weight) {
        this.weight = weight;
    }
    strike(payload) {
        return `${payload.label}:${this.weight}`;
    }
}

export function measure(payload) {
    return payload.label.length;
}
