// The runtime behind `declare module "error-lab:faults"`. `register.mjs` resolves the specifier
// here, so what the run gate raises and catches by type is this file's class.

export class Fault extends Error {
    constructor(message) {
        super(message);
        this.name = "Fault";
        this.retryable = true;
    }
    describe(detail) {
        return `${this.message}:${detail}`;
    }
}

export class Halt extends Error {
    constructor(message, code) {
        super(message);
        this.name = "Halt";
        this.code = code;
    }
}

export class Runner {
    constructor(label) {
        this.label = label;
    }
    run(detail) {
        return `base:${this.label}:${detail}`;
    }
}

export function reason(fault) {
    return new Error(fault.message);
}
