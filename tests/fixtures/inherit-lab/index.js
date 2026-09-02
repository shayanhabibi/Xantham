// The runtime half of the inherit lab. The run gate (tests/Xantham.Generator.RunGate) resolves
// the `inherit-lab` specifier here, so §4.4's is-a relation is proved to survive erasure rather
// than only to type-check: an upcast to an inherited base has to be the same object, and a
// class whose F# type gained an `inherit` has to keep the JavaScript prototype chain it had.

export class Node {
    constructor(id) {
        this.id = id;
    }
}

export class Leaf extends Node {
    constructor(id) {
        super(id);
        this.leafy = true;
    }
}
