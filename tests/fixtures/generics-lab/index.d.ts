/** A generic interface that is not exported: it is reached only through instantiations. */
interface Ready<T> {
    latest: T;
    settled: boolean;
}
/** A second one, so the union below has two generic arms. */
interface Pending<T> {
    previous: T | undefined;
}
/** Two instantiations of one generic declaration - the arms are applications of it, not re-expansions. */
export type Resource<T> = Ready<T> | Pending<T>;
/** A concrete instantiation of the same declaration. */
export type StringResource = Ready<string>;
/** A generic union alias: the parameter is bound on the alias, and the arms read it. */
export type Ref<T> = T | ((value: T) => void);
/** A generic union alias with a nullish arm, hoisted to option around the erased union. */
export type Source<S> = S | (() => S) | undefined;
/** An anonymous object type inside a generic function - it binds nothing itself and reads the function's parameters. */
export declare function each<T, U>(props: { items: T[]; fallback?: string; render: (item: T, index: number) => U }): U[];
/** An anonymous object type inside a generic alias. */
export type Handle<T> = [() => T, { set: (next: T) => void; reset(): void }];
/** A named bound: the only kind of constraint F# can state. */
export interface Named {
    name: string;
}
/** A constrained parameter that is not the last: F# writes one `when` clause after the parameter list. */
export interface Labelled<T extends Named, U> {
    subject: T;
    label: U;
}
/** An application of a constrained generic whose argument is a `typekeyof` result: the argument is written as the constraint. */
export interface Registry<M extends Record<string, Named>> {
    pick<K extends keyof M>(key: K): Labelled<M[K], K>;
}
/** An optional parameter ahead of a rest parameter: F# has no tail for the `?`, so it stays required, of option type. */
export declare function schedule(callback: (...args: number[]) => void, delay?: number, ...args: number[]): number;
/** An index-only anonymous object, written in the compiler lib: an interface of one indexer, not `obj`. */
export interface Manifest {
    flags: Record<string, boolean>;
}
