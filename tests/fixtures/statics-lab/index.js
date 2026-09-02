// The runtime half of the statics lab. The run gate (tests/Xantham.Generator.RunGate) resolves
// the `statics-lab` specifier here, so a static binding that reaches the wrong property - the
// constructor itself, or an instance member of the same name - fails the gate rather than
// passing as a compile-checked claim.

export class Counter {
    static MAX = 100;
    static tick = 7;

    constructor(start) {
        this.value = start;
    }

    static from(value) {
        return new Counter(value);
    }

    static of(value) {
        return new Counter(typeof value === "string" ? value.length : value);
    }

    bump(by) {
        return new Counter(this.value + by);
    }
}

export class Doubling extends Counter {
    double() {
        return new Doubling(this.value * 2);
    }
}

export class Box {
    static EMPTY = 0;

    constructor(value) {
        this.value = value;
    }
}

export class Clash {
    static status = 1;
    static text = 2;

    constructor() {
        this.status = 10;
        this.url = "instance";
    }

    static json(body) {
        return new Clash(body);
    }

    static url(value) {
        return "static:" + value;
    }

    json() {
        return 42;
    }

    text() {
        return "instance";
    }
}
