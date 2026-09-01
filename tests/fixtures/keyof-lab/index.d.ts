/** The operand every keyof in this fixture is taken over. */
export interface Options {
    /** How long, in milliseconds. */
    duration: number;
    /** What to call it. */
    label: string;
    /** Whether to loop. */
    loop: boolean;
}

// ---------------------------------------------------------------------------
// 4.10, closed/concrete regime: the checker can finish these.
// ---------------------------------------------------------------------------

/** `keyof` over a known interface - a finished union of string literals. */
export type OptionKey = keyof Options;

/** Concrete indexed access - the checker resolves this to `number`. */
export type Duration = Options["duration"];

/** Concrete indexed access over a union of keys - resolves to `number | string`. */
export type DurationOrLabel = Options["duration" | "label"];

/** A closed keyof in member position. */
export interface Selection {
    key: keyof Options;
    value: Options[keyof Options];
}

/** A closed keyof in parameter and return position. */
export declare function pick(key: keyof Options): Options[keyof Options];

// ---------------------------------------------------------------------------
// 4.10, open/generic regime: these cannot be closed, and are what the support
// package exists for.
// ---------------------------------------------------------------------------

/** `K extends keyof T` plus `T[K]` - the canonical typed accessor. */
export declare function get<T, K extends keyof T>(source: T, key: K): T[K];

/** A bare `keyof T` parameter over an unresolved operand. */
export declare function keys<T>(source: T): (keyof T)[];

/** `T[keyof T]` - the value-of idiom. */
export declare function values<T>(source: T): T[keyof T][];

/** A generic accessor carried as a member rather than a function. */
export interface Accessor<T> {
    read<K extends keyof T>(key: K): T[K];
    all(): (keyof T)[];
}

// ---------------------------------------------------------------------------
// 4.10, index signatures and Record.
// ---------------------------------------------------------------------------

/** A string index signature. */
export interface Bag {
    [key: string]: number;
}

/** A numeric index signature. */
export interface Slots {
    [index: number]: string;
}

/** `Record` over concrete operands. */
export type Registry = Record<string, number>;

/** A readonly string index signature. */
export interface FrozenBag {
    readonly [key: string]: string;
}

// ---------------------------------------------------------------------------
// 4.10, mapped types.
// ---------------------------------------------------------------------------

/** A mapped type over a concrete operand - the checker expands it (D6). */
export type PartialOptions = Partial<Options>;

/** Another concrete expansion, over a chosen key set. */
export type OptionsHead = Pick<Options, "duration" | "label">;

/** A concrete omission. */
export type OptionsTail = Omit<Options, "duration">;

/** Readonly over a concrete operand. */
export type FrozenOptions = Readonly<Options>;

/** A generic mapped type at an unresolved operand - cannot be expanded. */
export type DeepPartial<T> = {
    [K in keyof T]?: T[K];
};

/** A mapped type that changes the value type rather than the modifiers. */
export type Flags<T> = {
    [K in keyof T]: boolean;
};

// ---------------------------------------------------------------------------
// 4.11, conditional and template-literal types.
// ---------------------------------------------------------------------------

/** A conditional over a concrete operand - already resolved by the checker. */
export type ConcreteBranch = Options extends { duration: number } ? "yes" : "no";

/** A conditional in a generic signature - unresolved. */
export type Unwrap<T> = T extends Array<infer E> ? E : T;

/** A template literal over concrete operands - a finished literal union. */
export type EventName = `on${"Start" | "End"}`;

/** A template literal in a generic signature - unresolved. */
export type Prefixed<T extends string> = `x-${T}`;

/** Uppercase over a concrete operand: an intrinsic string mapping. */
export type ShoutedKey = Uppercase<"duration">;
