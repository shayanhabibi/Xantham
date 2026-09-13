// Overloaded generic call signatures whose hoisted type parameters share a name under two
// different bounds (`hasIncompatibleOverloadedTypeParameters`, §4.4) route to an interface, each
// call signature reaching its own `Invoke` overload.

/** Two call signatures under the same name, `U`, bound differently — to the interface's own `T`
 *  and to `number` — and separated by arity: each reaches its own `Invoke` overload. The outer
 *  `T` inside a call-signature bound mirrors `solid-js`'s `SetStoreFunction<T>`, whose every
 *  bound reads `T`. */
export interface Coalesce<T> {
    <U extends T>(value: U): U;
    <U extends number>(value: U, fallback: U): U;
}

/** Negative: one generic call signature; its type parameter hoists onto the delegate head beside
 *  the interface's own. */
export interface OneShot<T> {
    <U extends T>(value: U): U;
}

/** Negative: two non-generic call signatures separated by arity; the delegate takes its shape
 *  from the first signature, and the second is recorded as dropped (TR031). */
export interface Multiplex {
    (value: number): number;
    (value: number, extra: number): number;
}

/** Negative: a member beside the call signature routes through the existing hybrid path (§4.4),
 *  regardless of the call signature's own shape. */
export interface Ledger {
    (id: number): string;
    count: number;
}

/** `Coalesce` at a member position: both call signatures recover as separable overloads under
 *  the member's own name (§4.2 extended to callbacks), reachable through `holder.coalesce`. */
export interface Holder {
    coalesce: Coalesce<string>;
}

/** `Coalesce<T>` at a tuple return position, mirroring `solid-js`'s `createStore` returning
 *  `[Store<T>, SetStoreFunction<T>]`: the reference applies the function's own free `T`, keeps
 *  its name at arity 1, and each call signature reaches its own `Invoke` overload. */
export declare function makeCoalescer<T>(): [T, Coalesce<T>];

export declare const coalesce: Coalesce<string>;
export declare const oneShot: OneShot<string>;
export declare const multiplex: Multiplex;
export declare const ledger: Ledger;
export declare const holder: Holder;
