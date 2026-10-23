// A named arm is a declaration in its own right: the case carries the interface, so a `Ping`
// is a `Message` with no copy of its fields.
export interface Ping {
    op: "ping";
    at: number;
}
export interface Pong {
    op: "pong";
    at?: number;
}
export type Message = Ping | Pong;
export declare function send(message: Message): void;
export declare function ping(): Ping;

// Generic named arms: the case applies the union's parameter.
export interface Ready<T> {
    state: "ready";
    value: T;
}
export interface Failed {
    state: "failed";
    error: string;
}
export type Result<T> = Ready<T> | Failed;

// Named arms sharing a tag value, told apart by the member each carries alone.
export interface Done {
    state: "done";
    value: number;
}
export interface Aborted {
    state: "done";
    reason: string;
}
export type Outcome = Done | Aborted | { state: "queued" };

// An arm with a method is more than data: the case carries its interface.
export type Handle =
    | { kind: "file"; path: string; close(): void }
    | { kind: "none" };

// An arm with an index signature is more than its fields.
export type Bag =
    | { kind: "bag"; [key: string]: unknown }
    | { kind: "empty" };

// An arm past the case field budget.
export type Wide =
    | {
          kind: "wide";
          a1: number; a2: number; a3: number; a4: number; a5: number; a6: number; a7: number;
          a8: number; a9: number; a10: number; a11: number; a12: number; a13: number;
      }
    | { kind: "narrow" };

// A callable arm: constructing the case would copy the function into a plain object, so the
// union stays erased.
export type Thunk =
    | { kind: "fn"; (): void }
    | { kind: "none" };

// Anonymous plain-data arms keep their fields.
export type Shape =
    | { kind: "circle"; radius: number }
    | { kind: "square"; side: number };

// A class arm: constructing the case would copy the instance without its internal state, so
// the case keeps its fields.
export declare class Token {
    kind: "token";
    value: string;
}
export type Lexeme = Token | { kind: "eof" };
