/** An event key that is either a name or a symbol. */
export interface Emitter {
    on(eventName: string | symbol, listener: (...args: any[]) => void): this;
    readonly key: symbol;
}

/** A package-owned singleton symbol. */
export declare const tag: unique symbol;

export declare function keyOf(e: Emitter): string | symbol;

export declare function make(description?: string): symbol;

/** Negative: a union with a real obj arm still collapses. */
export declare function loose(value: symbol | object): void;
