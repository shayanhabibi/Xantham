// TEST TARGET: type parameter names and counts on generic interfaces
//
// TypeKey values for type parameters are runtime-generated IDs and are NOT
// asserted here.  Only TsTypeParameter.Name (stable across runs) and
// InlinedTsTypeParameter list length are checked.
//
// Expected:
//   Box  → 1 type parameter named "T"
//   Pair → 2 type parameters named "A" and "B"

// Single type parameter — Box<T>
export interface Box<T> {
    value: T;
}

// Two type parameters — Pair<A, B>
export interface Pair<A, B> {
    first: A;
    second: B;
}
