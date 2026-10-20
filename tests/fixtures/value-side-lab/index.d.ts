// Hand-authored fixture: the name a merged export's value side is declared under.

// A tagged union merged with a namespace: its arms are dropped, its value side declared.
export type Shape = { kind: "circle"; radius: number } | { kind: "square"; side: number };
export namespace Shape {
    const version: string;
    function unit(): Shape;
}

// An interface merged with a namespace. The inline shape a namespace value owns nests under
// the namespace. The namespace type `Point.Anchor`, read by `pinned` ahead of `anchor`, keeps
// its spelling, and the value-owned shape `anchor` takes the collision number. The top-level
// `Anchor` makes `Point.Anchor` nest under today's naming as well.
export interface Point { x: number }
export interface Anchor { other: boolean }
export namespace Point {
    function origin(): Point;
    const defaults: { x: number };
    interface Anchor { at: number }
    const pinned: Anchor;
    const anchor: { at: string };
}

// An enum: the type side is the F# enum, the value side the enum object.
export enum Level {
    Low = 0,
    High = 1,
}

// The role name is itself an exported type, so the value side takes the next number.
export type Mode = { fast: boolean };
export namespace Mode {
    const fallback: Mode;
}
export interface ModeNamespace { reserved: string }

// A const merged with a type: no source namespace, so its value-owned shape nests under the
// minted value side.
export type Token = { value: string };
export declare const Token: { parse(text: string): Token; options: { strict: boolean } };

// Negative: a namespace alone keeps its name.
export namespace Config {
    const path: string;
}
