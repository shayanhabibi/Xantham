// Test file for Plan A - encoder-side type argument padding

// Simple generic with default
export interface Container<T = number> {
    item: T;
}

// Bare uninstantiated generic (no args supplied)
export type BareContainer = Container;

// Partial supply (Container with one arg for Container<T, U = number>)
export interface Box<T, U = number> {
    value: T;
    size: U;
}

// Bare Box (should pad to [_, number])
export type BareBox = Box;

// Partial Box (should pad to [string, number])
export type PartialBox = Box<string>;

// Full Box (should stay [string, number])
export type FullBox = Box<string, number>;

// Type alias with default
export interface GenericDefault<T = string> {
    data: T;
}

// Bare generic with default in alias
export type DefaultAlias = GenericDefault;

// Aliased reference retaining alias name
export type AliasReference = Container<string>;
