// Hand-authored fixture for D3 (docs/.ai/plans/generator-type-mapping.md): a required member
// whose type admits `null` keeps an omittable `Create` parameter, and the Emit writes the key,
// as `null` where the argument is absent or `undefined` - SP006.

// Two required nullable members of different value types, beside a required member and an
// optional one.
export interface Record2 {
    id: string;
    label: string | null;
    score: number | null;
    note?: string;
}

// A type parameter admitting `null`.
export interface Box<T> {
    value: T | null;
}

// Negatives: an optional nullable member, and a required member admitting `undefined` too,
// stay omittable keys - no SP006.
export interface Loose {
    maybe?: string | null;
    either: string | null | undefined;
}

// An exclusive-arm fold whose shared member is required and nullable: each arm's `Create`
// writes the key.
export interface LeftArm {
    owner: string | null;
    left: string;
    right?: never;
}
export interface RightArm {
    owner: string | null;
    left?: never;
    right: number;
}
export type Side = LeftArm | RightArm;

// A tagged union whose case carries a required nullable field: the case's `Create` writes it.
export interface Opened {
    kind: "opened";
    by: string | null;
}
export interface Closed {
    kind: "closed";
}
export type Door = Opened | Closed;
