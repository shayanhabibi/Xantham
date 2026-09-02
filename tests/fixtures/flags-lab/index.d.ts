// The TypeScript type flags the shape tier used to refuse - one section each, with the
// negatives that must not be read as the same mapping. See docs/plans/generator-type-mapping.md
// §4.1 (primitives), §4.2 (literal types) and §4.11 (template literals).
//
// Every section is deliberately a handful of declarations: this file is the primary evidence
// for the mapping, so it has to be readable end to end. `index.js` beside it is the run gate's
// runtime, which is where the `bigint` claim is proven - the F# type says nothing about what
// Fable's erasure did with it.

// ---------------------------------------------------------------------------
// Template literal types. A template literal *is* a string at runtime, so `string` is the
// honest read (§4.11): the pattern is lost, the type is not. `obj` lost both. Three positions,
// because the widening used to cost each of them differently.
// ---------------------------------------------------------------------------

/** Alias position: the declaration itself is a template literal. */
export type EventName = `on${string}`;

export interface Emitter {
    /** Member position, through the alias. */
    event: EventName;
    /** Member position, written inline over two open operands. */
    channel: `${string}-${string}`;
    /** Parameter position on a member, returning one through the alias. */
    resolve(scope: `${string}:${string}`): EventName;
}

/** Parameter position on an exported function, and a template literal returned. */
export declare function normalize(name: `on${string}`): EventName;

/**
 * A negative. A *closed* template literal over finite unions is expanded by the checker into
 * its union of literals, which takes the StringEnum path and stays Exact - it must never reach
 * the widening at all.
 */
export type Mode = "read" | "write";
export type ModeEvent = `on${Capitalize<Mode>}`;

/**
 * A negative for the other side. A template literal over a type parameter is a type-level
 * computation the checker cannot finish, so it stays the erased phantom of D9's rung rather
 * than collapsing to a bare `string` and losing its arity.
 */
export type Tagged<T extends string> = `x-${T}`;

/** An intrinsic string mapping over an open operand: a string, with the transform lost. */
export declare function shout(text: string): Uppercase<string>;

// ---------------------------------------------------------------------------
// bigint. Exact - Fable 5 compiles F# `bigint` to the native JavaScript `BigInt` - which is
// why this is the section with a runtime half.
// ---------------------------------------------------------------------------

export interface Ledger {
    /** A member of bigint type, and a method taking and returning one. */
    balance: bigint;
    credit(amount: bigint): bigint;
}

/** Under an array at a parameter, bare at a return. */
export declare function total(amounts: bigint[]): bigint;

export declare function ledger(start: bigint): Ledger;

/** A bigint *literal* type is the widening its string and number counterparts already are. */
export type Two = 2n;

// ---------------------------------------------------------------------------
// `object` - TypeScript for "anything that is not a primitive". `obj` is the mapping §4.1
// asks for and there is no closer one, but it is still a widening: `obj` admits exactly the
// primitives `object` was written to exclude.
// ---------------------------------------------------------------------------

export interface Registry {
    holder: object;
}

export declare function freeze(value: object): object;

// ---------------------------------------------------------------------------
// `symbol` and `unique symbol`. Nothing shipped binds either: Fable.Core 5.2.0 declares no
// `JS.Symbol`, checked against the assembly. Both widen, each saying which construct it was
// and why - a `unique symbol` loses its identity on top of losing its type.
// ---------------------------------------------------------------------------

export declare const brandTag: unique symbol;

export interface Keyed {
    id: symbol;
}

export declare function describe(key: symbol): string;

// ---------------------------------------------------------------------------
// The neighbours, so a mapping above cannot quietly swallow one of them: a plain string stays
// Exact, a string literal keeps its own widening (TR006), and `number` is not `bigint`.
// ---------------------------------------------------------------------------

export interface Plain {
    name: string;
    kind: "static";
    count: number;
}
