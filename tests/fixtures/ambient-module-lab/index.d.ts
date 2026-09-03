// No import and no export at the top level of this file: that is what makes it a global script
// rather than a module, so `harvest-globals` runs and the ambient module declarations below are
// what is under test. Adding a top-level `export` changes what this fixture pins.

/** A namespace declaring only types: nothing puts `Shapes` on `globalThis`. */
declare namespace Shapes {
    /** A type reached only through a reference to it. */
    interface Point {
        x: number;
    }
}

/** A global type every module below refers to. */
declare interface Payload {
    /** The payload's label. */
    label: string;
    at: Shapes.Point;
}

/** A namespace declaring a value: `globalThis.Telemetry` is real, so it keeps `[<Global>]`. */
declare namespace Telemetry {
    const level: number;
}

/** A global class sharing its name with `ambient-lab:runtime`'s. Two declarations, two types. */
declare class Session {
    readonly kind: string;
}

/** An ambient module declaring its own class, function and interface. */
declare module "ambient-lab:tools" {
    /** A class exported from a specifier rather than from `globalThis`. */
    export class Hammer {
        constructor(weight: number);
        readonly weight: number;
        strike(payload: Payload): string;
        static readonly LIMIT: number;
    }

    /** A function exported from a specifier. */
    export function measure(payload: Payload): number;

    /** A type declared inside the module block. */
    export interface Reading {
        depth: number;
    }
}

/** An ambient module re-exporting a module-local declaration under another name. */
declare module "ambient-lab:sockets" {
    function _connect(label: string): Payload;
    export { _connect as connect };
}

/** The body of `ambient-lab:runtime`: a namespace re-exported whole, never a global. */
declare namespace AmbientLabRuntime {
    export const version: string;

    export class Session {
        constructor(label: string);
        readonly label: string;
    }
}

declare module "ambient-lab:runtime" {
    export = AmbientLabRuntime;
}

/** An ambient module exporting nothing. */
declare module "ambient-lab:empty" {}

/** A wildcard specifier: no import expression can name the module it stands for. */
declare module "ambient-lab:*" {
    export const wildcard: number;
}
