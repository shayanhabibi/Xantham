import { Signal } from "outer-lab";

/** A reading the entry package declares over a dependency's type. */
export interface Reading {
    signal: Signal;
    taken: string;
}

export declare function read(signal: Signal): Reading;
