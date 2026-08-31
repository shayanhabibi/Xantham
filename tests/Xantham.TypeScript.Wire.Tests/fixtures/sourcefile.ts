/// <reference path="./main.ts" />
/// <reference types="node" />

// Every declaration here pins one field of the SourceFile extended-data record: the reference
// directives above, an import, a module augmentation, an ambient module and a directive comment.
import type { Point } from "./main.js";

declare module "./main.js" {
    interface Point {
        z: number;
    }
}

declare module "ambient-only" {
    export const version: string;
}

// @ts-expect-error - the directive itself is the fixture; the error is incidental.
export const bad: number = "not a number";

export const origin: Point = { x: 0, y: 0, z: 0 };
