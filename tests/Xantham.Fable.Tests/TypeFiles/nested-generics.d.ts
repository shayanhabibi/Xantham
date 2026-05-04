export interface Map<K, V> {
    get(key: K): V;
    set(key: K, value: V): void;
}

// Test with nested generics
export interface Nested<T, U> {
    first: T;
    second: U;
}

export type IntStringPair = Nested<number, string>;

export type StringMap = Map<string, string>;

export type NestedStringMap = Nested<StringMap, IntStringPair>
