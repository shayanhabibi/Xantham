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
