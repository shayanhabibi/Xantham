export interface Message<Meta = unknown, Data extends Record<string, unknown> = Record<string, unknown>> {
    meta: Meta;
    data: Data;
}
/** The alias declares its parameter's bound as the defaulted `Message`. */
export type StreamOptions<M extends Message> = {
    onMessage?: (message: M) => void;
};
/** A use site whose parameter has a different bound: a separate F# declaration. */
export declare function createStream<Meta = unknown, M extends Message<Meta, never> = Message<Meta, never>>(
    options: { timeout?: number } & StreamOptions<M>
): void;
/** A use site repeating the declared bound: an abbreviation of the declaration. */
export declare function stream<M extends Message>(options: { retries?: number } & StreamOptions<M>): void;
