// Wave fifteen, lane DI. `Frontier<T>`'s own generic methods each return the interface applied
// to a fresh method-level type parameter (`map<U>(...): Frontier<U>`, the `Array<T>` shape) -
// every generation the walk re-derives the same members under a new instantiation id, so the
// frontier's width doubles generation over generation at any depth. This is the smallest shape
// that outgrows the width cutoff (RT003) inside a few seconds, standing in for the `lib.dom`
// measurement only a live run against the real library reproduces.
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
