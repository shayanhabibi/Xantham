// The runtime half of the constructor-object lab. The run gate resolves the `ctor-lab`
// specifier here, so an `[<EmitConstructor>] Create` that compiles to a *call* rather than to
// `new` fails the gate instead of passing as a compile-checked claim.

export class Gauge {
    static UNIT = "px";

    constructor(size) {
        this.size = size;
    }
}

export const scope = { Gauge };

export const Widget = class Widget {
    static DEFAULT_LABEL = "widget";

    constructor(label) {
        this.label = label;
    }

    resize(by) {
        return new Widget(this.label + ":" + by);
    }
};

export const parcels = class Parcel {
    constructor(value = "empty") {
        this.value = value;
    }
};

export class Solo {
    tag() {
        return "solo";
    }
}

export const VERSION = "1.0.0";
