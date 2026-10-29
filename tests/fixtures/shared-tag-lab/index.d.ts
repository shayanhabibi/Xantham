// Tag values that collide. Arms sharing a tag value are each a case of their own, told apart
// by a `CaseTest`; a union whose shared arms no test separates stays an erased union under a
// `DT003` finding.
//
// Reduced from `@cloudflare/workers-types`' `TailStream.EventType`, where an intersection over
// a union distributes into two members both tagged `"log"`.

// The regression guard: uniformly tagged, every value distinct. Stays a discriminated union.
export type Distinct =
    | { readonly kind: "circle"; readonly radius: number }
    | { readonly kind: "square"; readonly side: number }
    | { readonly kind: "point" };

// The second candidate. `channel` is the first arm's first property and collides across the
// arms; `verb` is the second and discriminates cleanly, so the union is a discriminated union
// on `verb` and the collision on `channel` is reported nowhere.
export type Signal =
    | { readonly channel: "bus"; readonly verb: "start"; readonly at: number }
    | { readonly channel: "bus"; readonly verb: "stop"; readonly reason: string };

// Two arms share `state: "done"` and a third discriminates. The shared pair are two cases,
// told apart by `'value' in $0` and `'error' in $0`.
export type Terminal =
    | { readonly state: "done"; readonly value: number }
    | { readonly state: "done"; readonly error: string }
    | { readonly state: "pending" };

// Every arm carries `type: "log"`, and the literal `truncated` tells them apart: one case per
// arm, the optional `false` arm also matching an absent `truncated`. Written flat: this is the
// pair the checker produces from the `Log` intersection below.
export type Onset =
    | { readonly type: "log"; readonly level: string; readonly message: object; readonly truncated?: false }
    | { readonly type: "log"; readonly level: string; readonly message: string; readonly truncated: true };

// The `TailStream.EventType` shape verbatim. The checker distributes the intersection over the
// union, and the two arms it produces are separated as `Onset`'s are.
export type Log = { readonly type: "log"; readonly level: string } & (
    | { readonly message: object; readonly truncated?: false }
    | { readonly message: string; readonly truncated: true }
);

// A negative: `kind` is a bare string on one arm, so no candidate is uniform and the union was
// never a tagged one to refuse.
export type Loose =
    | { readonly kind: "one"; readonly a: number }
    | { readonly kind: string; readonly b: number };

// A union written inline at a member position. `detect-tagged-unions` reads the declaration
// names, so a union with no name of its own was never offered to it; `synthesize-anonymous`
// names it under its owner and the pass claims it from there.
//
// Reduced from `@cloudflare/workers-types`' `TailStream.Onset.info`, ten arms uniformly tagged
// on `type`.
export interface FetchInfo {
    readonly type: "fetch";
    readonly url: string;
}
export interface AlarmInfo {
    readonly type: "alarm";
    readonly scheduledTime: number;
}
export interface JsRpcInfo {
    readonly type: "jsrpc";
    readonly methodName: string;
}
export interface ScheduledInfo {
    readonly type: "scheduled";
}
export interface QueueInfo {
    readonly type: "queue";
}
export interface EmailInfo {
    readonly type: "email";
}
export interface TraceInfo {
    readonly type: "trace";
}
export interface SocketInfo {
    readonly type: "socket";
}
export interface CustomInfo {
    readonly type: "custom";
}
export interface ConnectInfo {
    readonly type: "connect";
}

// Above the erased-union cap, so the member read `obj` until the union carried a name.
export interface Wide {
    readonly event:
        | FetchInfo
        | AlarmInfo
        | JsRpcInfo
        | ScheduledInfo
        | QueueInfo
        | EmailInfo
        | TraceInfo
        | SocketInfo
        | CustomInfo
        | ConnectInfo;
}

// The same union at a callback parameter, which is the position `TailStream.EventType` reaches
// the manifest at. The checker hash-conses the two spellings onto one type id, so this reads
// the declaration `Wide.event` claimed.
export type OnWide = (event:
    | FetchInfo
    | AlarmInfo
    | JsRpcInfo
    | ScheduledInfo
    | QueueInfo
    | EmailInfo
    | TraceInfo
    | SocketInfo
    | CustomInfo
    | ConnectInfo) => void;

// A union another declaration already answers for by member set is left to that name, so the
// inline spelling reads `Named` and mints nothing. `@cloudflare/workers-types` declares
// `ResponseInputContent` in two of its files and the second reads as the first.
export type Named = FetchInfo | AlarmInfo | JsRpcInfo;
export interface Alias {
    readonly event: FetchInfo | AlarmInfo | JsRpcInfo;
}

// Below the cap the mapping is the same one: an erased union is written but not read, since
// Fable type-tests no interface arm, so a union the checker discriminates is claimed at any
// width.
export interface Narrow {
    readonly event: ScheduledInfo | QueueInfo | EmailInfo;
}

// The `TailStream.EventType` spelling: nine arms and the `Log` intersection, which the checker
// distributes into two members flagged `Intersection`. No candidate discriminant is uniform, so
// the union is offered no name and the parameter widens.
export type OnEvent = (event:
    | FetchInfo
    | AlarmInfo
    | JsRpcInfo
    | ScheduledInfo
    | QueueInfo
    | EmailInfo
    | TraceInfo
    | SocketInfo
    | CustomInfo
    | Log) => void;

export type Scheduled<T> =
    | { readonly kind: "once"; readonly payload: T }
    | { readonly kind: "repeat"; readonly payload: T; readonly previous?: T };

export interface Scheduler {
    schedule<T>(payload: T): Scheduled<T>;
    current<T>():
        | { readonly kind: "ready"; readonly payload: T }
        | { readonly kind: "waiting"; readonly previous?: T };
}
