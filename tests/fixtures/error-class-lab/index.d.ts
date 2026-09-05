// No import and no export at the top level of this file: that is what makes it a global script
// rather than a module, so the ambient module below declares the entrypoint classes under test.

/**
 * A class no specifier exports: `globalThis.Mishap` is not a module entrypoint, so it keeps the
 * interface form and the flattened members of its base.
 */
declare class Mishap extends Error {
    readonly at: number;
}

declare module "error-lab:faults" {
    /**
     * The positive. A class an ambient module exports whose base is the compiler library's
     * `Error`: the class form, and `inherit exn`, so a consumer raises it and catches it by type.
     */
    export class Fault extends Error {
        constructor(message: string);
        readonly retryable: boolean;
        describe(detail: string): string;
    }

    /**
     * A second entrypoint over the same base, declared `abstract`. Both halves of the entrypoint
     * rule hold at once, and the `inherit` is the same one.
     */
    export abstract class Halt extends Error {
        constructor(message: string, code: number);
        readonly code: number;
    }

    /**
     * The negative for the class form: an entrypoint with no base at all, which carries no
     * `inherit` line.
     */
    export abstract class Runner {
        constructor(label: string);
        readonly label: string;
        run(detail: string): string;
    }

    /**
     * The negative for the heritage rule: an interface over the same base keeps its flattened
     * members and stays an interface.
     */
    export interface Warned extends Error {
        note: string;
    }

    /** `Error` in a reference position, which the compiler-lib table now answers. */
    export function reason(fault: Fault): Error;
}
