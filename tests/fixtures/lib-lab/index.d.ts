// The compiler-lib group, per docs/plans/generator-architecture.md O7 and the mapping
// document's 4.8.
//
// Every name below is declared by the compiler's own `lib.*.d.ts`, not by this package. O7
// widens that group to `obj` for want of a shipped binding - but for the ECMAScript half of
// the lib there is one, it is `Fable.Core.JS`, and every generated file already opens it.
// What this fixture pins is where that line falls: which lib names come back as `JS.*`, what
// the arity rule does when TypeScript's lib and Fable's binding disagree, and which names are
// still honestly `obj` because binding them would mean taking a dependency.

// ---------------------------------------------------------------------------
// Bound by Fable.Core, arities agreeing. These are exact: same runtime object,
// same shape, same number of parameters.
// ---------------------------------------------------------------------------

/** The one that matters most: nearly every asynchronous API in a `.d.ts` returns one. */
export declare function fetchOne(url: string): Promise<string>;

/** A promise of a promise, so the argument is shaped at its position rather than widened. */
export declare function fetchAll(urls: string[]): Promise<Promise<string>[]>;

/** Both parameters carried. */
export declare function index(entries: Map<string, number>): Map<string, string[]>;

/** And the one-parameter collections. */
export declare function unique(values: Set<string>): Set<number>;

/** The weak collections, which Fable binds under the same names. */
export declare function cache(store: WeakMap<object, string>, seen: WeakSet<object>): void;

/** Non-generic lib types: no arguments to shape, just a name to write. */
export declare function stamp(at: Date): Date;

/** Binary data - the buffer types a worker or a stream API traffics in. */
export declare function view(buffer: ArrayBuffer, over: DataView): ArrayBufferView;

/** An async iterable, which Fable.Core binds (the *synchronous* one it does not - see below). */
export declare function stream(source: AsyncIterable<string>): AsyncIterator<number>;

/** A bare `Function` in a `.d.ts` is untyped by construction; `JS.Function` says the same. */
export declare function invoke(fn: Function): Object;

// ---------------------------------------------------------------------------
// Where TypeScript's lib and Fable's binding disagree on arity. TypeScript made
// the typed arrays generic in their backing buffer; Fable's abbreviations are
// not. The extra argument is dropped, and the manifest says so.
// ---------------------------------------------------------------------------

/** A typed array. Under a modern lib this is `Uint8Array<ArrayBufferLike>`. */
export declare function bytes(input: Uint8Array): Float64Array;

// ---------------------------------------------------------------------------
// Bound, but not exactly. The name is carried; the restriction it expressed is
// not, and the finding is what keeps that from passing as exact.
// ---------------------------------------------------------------------------

/** A thenable is the structural supertype of a promise, not a promise. */
export declare function thenable(): PromiseLike<string>;

/** Readonly views over the collections. F# has the mutable binding and only that. */
export declare function frozen(entries: ReadonlyMap<string, number>, values: ReadonlySet<string>): void;

// ---------------------------------------------------------------------------
// Not bound, and this fixture exists partly to hold that line. Fable.Core binds
// the async iteration protocol but not the synchronous one, and the DOM has no
// binding here at all without a `Fable.Browser.*` dependency - which is a
// decision about what this generator depends on, not a table entry.
// ---------------------------------------------------------------------------

/** `seq<'T>` is not a JS iterable, whatever the two have in common. Widened, and noted. */
export declare function each(values: Iterable<string>): void;

/** A DOM name. Real, ubiquitous in `.d.ts` files, and still `obj` here. */
export declare function handle(target: EventTarget): void;

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
