export * from "./entries.js";
import type { Shape, TaggedEntry, EntryLike, EntryList } from "./entries.js";
export * as ns from "./ns.js";

/** W7.1: a delegate whose only use of T is the owner's declared form; interface constraint. */
export interface Grid<T extends Shape> {
    each(visit: (cell: number, index: number, grid: Grid<T>) => void): void;
}

export interface PlainBuffer { readonly byteLength: number; }
export interface SharedBuffer { readonly byteLength: number; readonly growable: boolean; }
export type BufferLike = PlainBuffer | SharedBuffer;

/** W7.2: an erased-union constraint stays off the delegate, as it is off the owner. */
export interface Store<B extends BufferLike = BufferLike> {
    readonly buffer: B;
    each(visit: (value: number, index: number, store: Store<B>) => boolean): void;
}

/** W7.3 on a class's merged namespace. */
export declare class Registry { readonly size: number; }
export declare namespace Registry {
    function find<T extends Shape>(shape: T): T;
}

export interface Container {
    /** W7.4: E is used only in the return. */
    queryAll<E extends TaggedEntry = TaggedEntry>(selectors: string): EntryList<E>;
    /** Negative: a structural-only bound still widens to the constraint. */
    queryLike<E extends EntryLike = EntryLike>(selectors: string): EntryList<E>;
}

/** W7.1 extension: NoInfer, the solid-js idiom. */
export type NoInfer<T> = [T][T extends any ? 0 : never];
export type Options<T, S = unknown> = { initialValue?: T; source?: S };
export declare function create<T>(fetch: () => T, options?: Options<NoInfer<T>, true>): T;
