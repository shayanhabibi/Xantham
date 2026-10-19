// Issue #158. `detect-tagged-unions` declares these unions as DUs, and the arm interfaces
// `synthesize-anonymous` named for them are declared too, read by nothing.

/** The outcome of recovering a fiber. */
export type Recovery =
    | {
          status: "completed";
          /** The state captured when the fiber finished. */
          snapshot?: unknown;
          /** @deprecated Read `snapshot` instead. */
          metadata?: Record<string, unknown>;
      }
    | {
          status: "aborted";
          /** Why the fiber was aborted. */
          reason?: string;
          /** Which party aborted it. */
          type: "user" | "system";
      }
    | { status: "pending" };

// Every field required: the DU case alone constructs the arm.
export type Shape =
    | { kind: "circle"; radius: number }
    | { kind: "square"; side: number };

// A literal field beside the discriminant. The arm's Create writes `channel`; the DU case
// takes it as a `string` parameter.
export type Signal =
    | { channel: "bus"; verb: "start"; at: number }
    | { channel: "bus"; verb: "stop"; reason: string };

// An arm read by another declaration through `Extract`: its interface stays declared.
export type Event =
    | { type: "open"; url: string }
    | { type: "close"; code?: number };
export interface Opener {
    open(event: Extract<Event, { type: "open" }>): void;
}

// Named arms are declarations in their own right and stay.
/** A ping message. */
export interface Ping {
    op: "ping";
    /** Milliseconds since the epoch. */
    at: number;
}
export interface Pong {
    op: "pong";
    at?: number;
}
export type Message = Ping | Pong;

// A helper name another case already holds: `Create` + `X` spells the case `CreateX`.
export type Clash =
    | { kind: "x"; note?: string }
    | { kind: "createX" };

// Arms sharing `state: "done"` are cases of their own, told apart by the literal `ok`: the
// cases' `CaseTest` reads `$0.ok === true` and `$0.ok === false`, and `Create` writes `ok`.
export type Task =
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: true; note: { text: string } }
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: false; note: { text: number } }
    | { state: "queued" };

// Arms sharing a tag value, told apart by the member each carries alone.
export type Run =
    | { state: "done"; result: { value: number } }
    | { state: "done"; failure: { message: string } }
    | { state: "queued"; position: number };

// The exclusive-arm fold over an inline object both arms share.
export type Choice =
    | { meta: { id: string }; a: number; b?: never }
    | { meta: { id: string }; b: string; a?: never };

// A case named `Cases` would shadow the case modules, so these arms nest under their own names.
export type Shelf =
    | { kind: "cases"; size: "s" | "m" }
    | { kind: "box"; size: "l" | "xl" };

// Arms sharing a tag value where only the second carries a member of its own: the first is
// told apart by lacking it.
export type Job =
    | { state: "done"; level: "info" | "warn" }
    | { state: "done"; level: "info" | "warn"; code: number }
    | { state: "queued" };

// An arm named by a type alias the module does not export is a declaration of its own: it and
// its nested declarations stay, under its own name.
type Hello = { kind: "hello"; meta: { at: number } };
export type Greeting = Hello | { kind: "bye" };

// An exclusive-folded union whose arms repeat an inline member: the fold reads the first arm's
// declaration, and the second arm's copy is read by no declaration left.
export type Pick =
    | { meta: { id: string }; a: number; b?: never }
    | { meta: { id: string }; b: string; a?: never };

// A namespace member merged under an exclusive-folded union is API, read or not. A top-level
// interface takes the bare name, so the member nests under the folded union, and the fold drops
// the call signature and index signature that read it.
export interface Extra { other: number }
export type T1 = { (e: T1.Extra): void; a: number; b?: never } | { b: string; a?: never };
export namespace T1 { export interface Extra { note: string } }

export interface Idx { other: number }
export type T2 = { [key: string]: T2.Idx | number; a: number; b?: never } | { b: string; a?: never };
export namespace T2 { export interface Idx { note: string } }

// Arms sharing a tag value and an optional member, told apart by `value` and `error`.
export type Report =
    | { state: "done"; note?: string; value: number }
    | { state: "done"; note?: string; error: string }
    | { state: "queued" };

// Arms sharing a tag value where the second extends the first by `error`.
export type Outcome =
    | { state: "done"; value: number; note?: string }
    | { state: "done"; value: number; note?: string; error: string }
    | { state: "queued" };

// Arms sharing a tag value with nothing to tell them apart: `x` is an object on both, so the
// union stays an erased union. The two arms differ only inside `x`'s own union.
export type Mirror =
    | { state: "done"; x: { n: number } | { n: number } }
    | { state: "done"; x: { n: number } | { s: string } }
    | { state: "queued" };

// The exclusive-arm fold compares a shared member's unions both ways: `x` reads differently on
// the two arms, in either order, so neither union folds.
export type W =
    | { x: { n: number } | { n: number }; a: number; b?: never }
    | { x: { n: number } | { s: string }; b: string; a?: never };
export type Y =
    | { x: { n: number } | { s: string }; a: number; b?: never }
    | { x: { n: number } | { n: number }; b: string; a?: never };

// An arm carrying a symbol-keyed member or an index signature is more than its case's fields,
// so its interface stays declared.
export type F =
    | { kind: "a"; [Symbol.toStringTag]: (x: number, y: string) => void; n: number }
    | { kind: "b" };
export type G =
    | { kind: "a"; n: number; [key: string]: unknown }
    | { kind: "b" };

// Arms sharing a tag value and every member name, told apart by the `typeof` of `payload`.
export type Mixed =
    | { state: "done"; payload: string }
    | { state: "done"; payload: number }
    | { state: "queued" };
