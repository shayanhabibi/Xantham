// The runtime behind `declare module "hook-lab:runtime"`. Every class here is the JavaScript a
// generated `[<Import(name, "hook-lab:runtime")>]` binds, and no base carries a hook - exactly
// what the declaration's `?` says.

export class Station {
    constructor(label) {
        this.label = label;
    }
    run(signal) {
        return `base:${signal.label}`;
    }
}

export class Relay {
    constructor(seed) {
        this.seed = seed;
    }
}

export class Hub {
    constructor(depth) {
        this.depth = depth;
    }
}

export class Annex extends Hub {
    constructor(depth, tag) {
        super(depth);
        this.tag = tag;
    }
}
