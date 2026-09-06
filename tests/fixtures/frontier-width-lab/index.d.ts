// Wave fifteen, lane DI, and wave sixteen. `Frontier<T>`'s own generic methods each return the
// interface applied to a fresh method-level type parameter (`map<U>(...): Frontier<U>`, the
// `Array<T>` shape). The checker clones the parameter per instantiation, so deriving each
// instantiation's members re-derived the same methods under a new id every generation, and the
// frontier's width doubled generation over generation until the width cutoff (RT003) deferred a
// whole generation. `Resolve.fs` now derives such an instantiation as identity, and the walk
// closes in a handful of generations with `map` rendered as `Frontier<'U>`.
interface Frontier<T> {
    every<S extends T>(
        predicate: (value: T, index: number, array: Frontier<T>) => value is S,
        thisArg?: any,
    ): this is Frontier<S>;
    map<U>(callbackfn: (value: T, index: number, array: Frontier<T>) => U, thisArg?: any): Frontier<U>;
    filter<S extends T>(predicate: (value: T, index: number, array: Frontier<T>) => value is S, thisArg?: any): Frontier<S>;
    reduce<U>(
        callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Frontier<T>) => U,
        initialValue: U,
    ): U;
    reduceRight<U>(
        callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Frontier<T>) => U,
        initialValue: U,
    ): U;
    [n: number]: T;
}

declare const frontier: Frontier<number>;

export { frontier };
