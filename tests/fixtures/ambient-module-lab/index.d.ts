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

/** An abstract class no specifier exports: `globalThis.Anvil` is not a module entrypoint, so it
 * keeps the interface form and its `Create`. */
declare abstract class Anvil {
    readonly mass: number;
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

    /**
     * A class written to be derived from: `abstract`, and exported from a specifier. F# reaches
     * this one as a class, because an interface admits no `inherit`.
     */
    export abstract class Workbench {
        constructor(label: string);
        readonly label: string;
        /** The slot a derived class fills. */
        run(payload: Payload): string;
    }

    /**
     * A class with a base the compiler library owns. Not abstract, and still the class form:
     * `extends` is what says a consumer derives from it.
     */
    export class Snag extends Error {
        constructor(message: string);
    }

    /** A class whose base this run declares. An F# class reaches its base through a constructor
     * call, and an interface has none, so this keeps the interface form. */
    export class Vise extends Hammer {
        constructor(weight: number, jaw: number);
        readonly jaw: number;
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
