// The compiler-lib group, per docs/plans/generator-architecture.md O7 and the mapping
// document's 4.8.
//
// Compiler-library names are written as their Xantham.Fable.Core.TS declarations,
// except Promise and PromiseLike (Fable.Core's JS.Promise) and Error (exn). This
// fixture covers their names, generic arguments, and the losses those exceptions retain.

// ---------------------------------------------------------------------------
// Written as the same declaration Core.TS emits: same runtime object, same
// shape, same number of parameters.
// ---------------------------------------------------------------------------

/** The one that matters most: nearly every asynchronous API in a `.d.ts` returns one. */
export declare function fetchOne(url: string): Promise<string>;

/** A promise of a promise, so the argument is shaped at its position rather than widened. */
export declare function fetchAll(urls: string[]): Promise<Promise<string>[]>;

/** Both parameters carried. */
export declare function index(entries: Map<string, number>): Map<string, string[]>;

/** And the one-parameter collections. */
export declare function unique(values: Set<string>): Set<number>;

/** The weak collections. */
export declare function cache(store: WeakMap<object, string>, seen: WeakSet<object>): void;

/** Non-generic lib types: no arguments to shape, just a name to write. */
export declare function stamp(at: Date): Date;

/** Binary data - the buffer types a worker or a stream API traffics in. */
export declare function view(buffer: ArrayBuffer, over: DataView): ArrayBufferView;

/** An async iterable. */
export declare function stream(source: AsyncIterable<string>): AsyncIterator<number>;

/** A bare `Function` in a `.d.ts` is untyped by construction; Core.TS's `Function` says the same. */
export declare function invoke(fn: Function): Object;

// ---------------------------------------------------------------------------
// TypeScript made the typed arrays generic in their backing buffer. Core.TS
// carries the parameter, so no argument is dropped.
// ---------------------------------------------------------------------------

/** A typed array. Under a modern lib this is `Uint8Array<ArrayBufferLike>`. */
export declare function bytes(input: Uint8Array): Float64Array;

// ---------------------------------------------------------------------------
// A thenable reads as JS.Promise, and the finding keeps that from passing as
// exact. The readonly views are Core.TS's own declarations.
// ---------------------------------------------------------------------------

/** A thenable is the structural supertype of a promise, not a promise. */
export declare function thenable(): PromiseLike<string>;

/** Readonly views over the collections. */
export declare function frozen(entries: ReadonlyMap<string, number>, values: ReadonlySet<string>): void;

// ---------------------------------------------------------------------------
// The DOM half is generated from the same TypeScript pin as this input.
// ---------------------------------------------------------------------------

/** The one the group's widening used to cost most: a DOM name in an ordinary position. */
export declare function handle(target: EventTarget): void;

/** Elements and events, which is most of what a browser-facing `.d.ts` traffics in. */
export declare function mount(host: HTMLElement, on: Event): void;

/** Binary and URL types from the same Core.TS DOM module. */
export declare function upload(body: Blob, to: URL): FormData;

/** A generic DOM declaration retains its argument. */
export declare function emit(detail: CustomEvent<string>): void;

// ---------------------------------------------------------------------------
// Core.TS, not Fable.Core: written under their Xantham.Fable.Core.TS names.
// ---------------------------------------------------------------------------

/** Core.TS binds the synchronous iteration protocol as its own JS interface, not as `seq<'T>`. */
export declare function each(values: Iterable<string>): void;

/** Core.TS supplies the DOM Range without the former Browser package ambiguity. */
export declare function select(over: Range): void;

/** Fetch declarations are part of the shipped DOM module. */
export declare function respond(): Response;

// ---------------------------------------------------------------------------
// The lib names in the positions a binding actually puts them: on members, under
// an array, under an option, as a type argument to a generated declaration.
// ---------------------------------------------------------------------------

/** A generated interface whose members are lib types throughout. */
export interface Store {
    /** A promise-returning member, which is what the group's widening used to cost most. */
    load(key: string): Promise<Uint8Array>;
    /** Under an array. */
    all(): Promise<string>[];
    /** Optional, so the promise arrives wrapped in an option. */
    pending?: Promise<void>;
    /** A lib type as the argument of a generic this package declares. */
    boxed: Box<Date>;
    /** A property whose type is a lib collection. */
    index: Map<string, ArrayBuffer>;
}

/** A generic of our own, so a lib type can be seen passing through one. */
export interface Box<T> {
    value: T;
}
