declare global {
    interface SymbolConstructor {
        readonly dispose: unique symbol;
        readonly asyncDispose: unique symbol;
        /** Not declared by the compiler's lib: undefined without a polyfill. */
        readonly observable: unique symbol;
    }

    interface Disposable {
        [Symbol.dispose](): void;
    }

    interface AsyncDisposable {
        [Symbol.asyncDispose](): PromiseLike<void>;
    }
}

/** Disposed through its base. */
export interface Handle extends Disposable {
    close(): void;
}

/** Keyed by an augmented well-known key and by an augmented key the compiler's lib lacks. */
export interface Stream {
    [Symbol.asyncDispose](): Promise<void>;
    [Symbol.observable](): unknown;
}

export declare const own: unique symbol;

/** Keyed by the package's own unique symbol. */
export interface Keyed {
    [own](): void;
}

export declare const handle: Handle;
export declare const stream: Stream;

/** Keyed through a namespace-local `Symbol` whose `asyncDispose` is the namespace's own unique symbol. */
export declare namespace Shadowed {
    const Symbol: { readonly asyncDispose: unique symbol };

    interface Resource {
        [Symbol.asyncDispose](): Promise<void>;
    }
}
