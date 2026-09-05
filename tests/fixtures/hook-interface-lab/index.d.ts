// No import and no export at the top level of this file: that is what makes it a global script
// rather than a module, so the ambient module below is harvested by specifier and its exported
// classes reach F# through `[<Import(name, "hook-lab:runtime")>]`.

/** The value every hook below is handed. */
declare interface Signal {
    label: string;
}

declare module "hook-lab:runtime" {
    /**
     * An entrypoint a consumer derives from. `run` is the mandatory slot; `fetch` and `alarm` are
     * the platform's lifecycle hooks, called where the object provides them.
     */
    export abstract class Station {
        constructor(label: string);
        readonly label: string;
        run(signal: Signal): string;
        fetch?(signal: Signal): string;
        alarm?(): string;
        /** An optional property rather than an optional method: nothing calls this one. */
        tag?: string;
    }

    /** An entrypoint whose hook mentions the class's own type parameter. */
    export abstract class Relay<T> {
        constructor(seed: T);
        readonly seed: T;
        forward?(value: T): T;
    }

    /** An exported class that is neither abstract nor derived: the interface form, where an
     * optional method is an option property because there is no class to hang a hook off. */
    export class Hub {
        constructor(depth: number);
        readonly depth: number;
        probe?(signal: Signal): string;
    }

    /** A class whose base this run declares. An F# interface admits no `inherit` of a class, so
     * this keeps the interface form and its inherited optional method stays an option. */
    export class Annex extends Hub {
        constructor(depth: number, tag: string);
        readonly tag: string;
    }
}

/** A plain interface carrying an optional method. Nothing derives from it, so the method stays
 * an option property. */
declare interface Listener {
    ping?(signal: Signal): string;
}
