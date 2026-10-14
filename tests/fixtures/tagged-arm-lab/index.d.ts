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

// Folded arms agreeing on an inline object member keep it: equal shapes, distinct type ids.
// `ok` differs by literal and `note` differs inside its object, so both fold away.
export type Task =
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: true; note: { text: string } }
    | { state: "done"; owner: { name: string; tags: { id: number }[] }; ok: false; note: { text: number } }
    | { state: "queued" };

// Members only one folded arm carries fold away.
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

// Arms folding into one case nest under that case.
export type Job =
    | { state: "done"; level: "info" | "warn" }
    | { state: "done"; level: "info" | "warn"; code: number }
    | { state: "queued" };
