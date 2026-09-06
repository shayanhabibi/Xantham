// Hand-authored fixture for inherited members. The checker reports every interface with its
// full apparent member list, base members included. A member a declared base carries at the
// same signature is written once, on the base; a member narrowed at a level stays on it.

export interface Attr {
    value: string;
}

/** Root of the chain. */
export interface Node {
    nodeName: string;
    cloneNode(deep?: boolean): Node;
}

/** Second level: inherits `Node`, narrows `cloneNode`, adds its own. */
export interface Element extends Node {
    getAttributeNode(name: string): Attr | null;
    cloneNode(deep?: boolean): Element;
}

/** Third level: inherits `Element`, narrows `cloneNode` again. */
export interface HTMLElement extends Element {
    hidden: boolean;
    cloneNode(deep?: boolean): HTMLElement;
}

/** Leaf: everything above is inherited; only `align` and the narrowed `cloneNode` are its own. */
export interface HTMLDivElement extends HTMLElement {
    align: string;
    cloneNode(deep?: boolean): HTMLDivElement;
}

/** A diamond: both arms carry `volume` from `Loud`. */
export interface Loud {
    volume: number;
}
export interface Pitched extends Loud {
    pitch: number;
}
export interface Both extends Loud, Pitched {
    both: boolean;
}
