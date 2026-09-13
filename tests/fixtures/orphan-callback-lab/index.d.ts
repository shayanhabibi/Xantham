// No import and no export at the top level of this file: that is what makes it a global script
// rather than a module, so the ambient module below is harvested by specifier and its exported
// classes reach F# through `[<Import(name, "orphan-lab:runtime")>]`.

/** The value every callback below is handed. */
declare interface Signal {
    label: string;
}

/**
 * A plain interface carrying an optional callback method of two arguments. The arity rule
 * retains the callback as a delegate, the member is an option property, and the delegate name
 * is written there.
 */
declare interface Watcher {
    onClose?(signal: Signal, code: number): void | Promise<void>;
}

declare module "orphan-lab:runtime" {
    /**
     * An entrypoint a consumer derives from. `onClose` is a lifecycle hook of the same shape as
     * `Watcher.onClose`, emitted as `Station.IOnCloseHandler`, so the delegate the callback pass
     * declares for it is written at zero sites.
     */
    export abstract class Station {
        constructor(label: string);
        readonly label: string;
        run(signal: Signal): string;
        onClose?(signal: Signal, code: number): void | Promise<void>;
    }

    /**
     * An entrypoint whose hook shape this run declares once. The delegate behind it is written
     * at zero sites too, under a name carrying no suffix.
     */
    export abstract class Solo {
        constructor(seed: number);
        readonly seed: number;
        start(signal: Signal): string;
        onDrop?(signal: Signal, depth: number): void;
    }

    /**
     * An entrypoint whose hook takes one argument: written as an F# function type, so the
     * callback pass declares nothing for it.
     */
    export abstract class Relay {
        constructor(tag: string);
        readonly tag: string;
        tick?(signal: Signal): string;
    }

    /**
     * An exported class that is neither abstract nor derived: the interface form, where the
     * optional callback method is an option property and the delegate name is written there.
     */
    export class Hub {
        constructor(depth: number);
        readonly depth: number;
        probe?(signal: Signal, depth: number): string;
    }

    /**
     * A class whose base this run declares. F# admits no `inherit` of an interface, so the
     * declaration keeps the interface form and its optional callback method is an option
     * property reading the delegate name.
     */
    export class Depot extends Hub {
        constructor(depth: number, code: string);
        readonly code: string;
        settle?(signal: Signal, at: number): string;
    }
}
