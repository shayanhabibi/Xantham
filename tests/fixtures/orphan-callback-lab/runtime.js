// The runtime behind `declare module "orphan-lab:runtime"`. Each base leaves its hooks to the
// subclass, which is what the declaration's `?` says.

export class Station {
    constructor(label) {
        this.label = label;
    }
    run(signal) {
        return `station:${signal.label}`;
    }
}

export class Solo {
    constructor(seed) {
        this.seed = seed;
    }
    start(signal) {
        return `solo:${signal.label}`;
    }
}

export class Relay {
    constructor(tag) {
        this.tag = tag;
    }
}

export class Hub {
    constructor(depth) {
        this.depth = depth;
    }
}

export class Depot extends Hub {
    constructor(depth, code) {
        super(depth);
        this.code = code;
    }
}
