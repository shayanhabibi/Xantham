// Hand-authored fixture for wave fourteen lane CP: docs/.ai/plans/generator-wave-fourteen-dispatch.md.
// Today's baseline for the exclusive-arm shape lane CN's batch-two fold (§ carried-forward item
// 5 of wave thirteen) targets - a union whose arms differ only in which member each declares
// `?: never`, mirroring `@cloudflare/workers-types`' `AiSearchSearchRequest`. Each arm mints its
// own interface today, with its own `ParamObject` `Create`; the fold merges them into one type,
// carrying exclusivity on the `Create` overloads instead. This fixture pins the unfolded shape
// so the fold's diff, once built, lands on a fixture small enough to read in full.
export interface QueryOptions {
    query?: string;
    messages?: never;
    shared: number;
}

export interface MessagesOptions {
    query?: never;
    messages?: string[];
    shared: number;
}

export type SearchOptions = QueryOptions | MessagesOptions;

export declare function search(options: SearchOptions): void;
