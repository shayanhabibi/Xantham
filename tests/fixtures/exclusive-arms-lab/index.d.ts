// Hand-authored fixture, wave fourteen lane CN: docs/.ai/plans/generator-wave-fourteen-dispatch.md
// batch two, item 4. Pairs one to three pin the fold's three outcomes; pair four pins the fold
// over source-named arms.

// Pair one: both arms carry a required member of their own (`query` against `messages`),
// mirroring `AiSearchSearchRequest`. Folds - TR060.
export interface QueryArm {
    query: string;
    messages?: never;
    shared: number;
}
export interface MessagesArm {
    query?: never;
    messages: string[];
    shared: number;
}
export type SeparableOptions = QueryArm | MessagesArm;

// Pair two: `image` is required on `ImageArm`, but `SnapshotArm`'s own distinguisher
// (`snapshot`) is optional in source, mirroring `Container.Start.Options`/`Options2`. Still
// folds - TR060 - because `image` gives F# a required-arity fact to resolve on.
export interface ImageArm {
    enableInternet: boolean;
    image: string;
    snapshot?: never;
}
export interface SnapshotArm {
    enableInternet: boolean;
    image?: never;
    snapshot?: string;
}
export type AnchoredOptions = ImageArm | SnapshotArm;

// Pair three: otherwise exclusive-arm shaped (`label`/`count` each `never` on the arm that
// does not own it), but `kind` is a member both arms declare outright, with disagreeing types
// and neither `never`. This is not the exclusive-arm construct - it only resembles one.
// Declines silently: no TR060, no TR061, each arm keeps its own interface as it does today.
export interface WidgetArm {
    kind: string;
    label: string;
    count?: never;
}
export interface GadgetArm {
    kind: number;
    label?: never;
    count: number;
}
export type DisagreeingOptions = WidgetArm | GadgetArm;

export function search(options: SeparableOptions): void;
export function start(options: AnchoredOptions): void;
export function build(options: DisagreeingOptions): void;

// Pair four: source-named arms in a namespace merged with the union alias. Folds - TR060 - and
// both arms stay declared. A top-level `Left` sits beside `Contested.Left`. Each arm is read
// from an interface, a callback, a function of another namespace and a top-level function.
export type Contested = Contested.Left | Contested.Right;
export namespace Contested {
    export interface Left {
        left: string;
        right?: never;
        tag: number;
    }
    export interface Right {
        left?: never;
        right: string[];
        tag: number;
    }
}
export interface Left {
    other: boolean;
}
export interface ContestedHolder {
    left: Contested.Left;
    right: Contested.Right;
    top: Left;
}
export type ContestedCallback = (left: Contested.Left) => void;
export namespace Elsewhere {
    export function read(arm: Contested.Right): Contested.Left;
}
export function contest(options: Contested, callback: ContestedCallback, holder: ContestedHolder): void;
export function pickLeft(left: Contested.Left): Contested.Right;

// A shared member readonly on one arm only: either fold would declare the head arm's setter
// or its lack for both, so neither folds.
export type ReadonlyFirst =
    | { readonly id: string; a: number; b?: never }
    | { id: string; b: string; a?: never };
export type ReadonlySecond =
    | { id: string; a: number; b?: never }
    | { readonly id: string; b: string; a?: never };
export function readonlyPairs(first: ReadonlyFirst, second: ReadonlySecond): void;
