// Each declaration here exists to pin one part of the node data word: literal text and flags
// come out of extended data, the rest out of the six commonData bits.
import type { Point } from "./main.js";

export const greeting = `hello ${"world"} and ${true}`;
export const plain = "quoted";
export const count = 0x2a;
export const negated = -count;

export const shape = {
    x: true,
    y: false,
};

export const flat = { z: true };

export declare const origin: Point;
