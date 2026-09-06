// ---------------------------------------------------------------------------
// A concrete operand indexed by a variable confined to its own keys.
// ---------------------------------------------------------------------------

/** The event the `fetch` key selects. */
export interface FetchEvent {
    request: string;
}

/** The event the `scheduled` key selects. */
export interface ScheduledEvent {
    cron: string;
}

/** The event the `queue` key selects. */
export interface QueueEvent {
    batch: string[];
}

/** The concrete operand every access in this section is taken over. */
export interface WorkerEventMap {
    fetch: FetchEvent;
    scheduled: ScheduledEvent;
    queue: QueueEvent;
}

/**
 * `Concrete[Type]` under `Type extends keyof Concrete`, in parameter position and again
 * inside the callback. Both resolve to the union of the map's value types.
 */
export declare function addEventListener<Type extends keyof WorkerEventMap>(
    type: Type,
    handler: (event: WorkerEventMap[Type]) => void,
): void;

/** The same access returned rather than taken. */
export declare function lastEvent<Type extends keyof WorkerEventMap>(type: Type): WorkerEventMap[Type];

/** The same access on a member. */
export interface Registrar {
    on<Type extends keyof WorkerEventMap>(type: Type, handler: (event: WorkerEventMap[Type]) => void): void;
}

// ---------------------------------------------------------------------------
// A bounded type parameter indexed by its own whole key set.
// ---------------------------------------------------------------------------

/** What every value of a dispatch target's map is at least. */
export interface WorkerEvent {
    type: string;
}

/**
 * `EventMap[keyof EventMap]` over a parameter bounded by an index signature: the bound's
 * value type is what the access selects, whichever key it lands on.
 */
export declare class Target<EventMap extends Record<string, WorkerEvent> = Record<string, WorkerEvent>> {
    dispatchEvent(event: EventMap[keyof EventMap]): boolean;
}

// ---------------------------------------------------------------------------
// A value union past the erased-union cap: the loss re-keys to TR036.
// ---------------------------------------------------------------------------

export interface E01 {
    a01: string;
}
export interface E02 {
    a02: string;
}
export interface E03 {
    a03: string;
}
export interface E04 {
    a04: string;
}
export interface E05 {
    a05: string;
}
export interface E06 {
    a06: string;
}
export interface E07 {
    a07: string;
}
export interface E08 {
    a08: string;
}
export interface E09 {
    a09: string;
}
export interface E10 {
    a10: string;
}
export interface E11 {
    a11: string;
}
export interface E12 {
    a12: string;
}

/** Twelve distinct value types, three past the cap Fable's erased unions stop at. */
export interface WideEventMap {
    e01: E01;
    e02: E02;
    e03: E03;
    e04: E04;
    e05: E05;
    e06: E06;
    e07: E07;
    e08: E08;
    e09: E09;
    e10: E10;
    e11: E11;
    e12: E12;
}

/** The access resolves, and the union it resolves to is wider than the cap. */
export declare function onWide<Type extends keyof WideEventMap>(
    type: Type,
    handler: (event: WideEventMap[Type]) => void,
): void;

// ---------------------------------------------------------------------------
// The soundness boundary: two shapes that stay widened.
// ---------------------------------------------------------------------------

/** A model map whose values are indexed again. */
export interface ModelMap {
    alpha: { inputs: string; outputs: number };
    beta: { inputs: number; outputs: string };
}

/**
 * A nested access, in parameter and in return position. The outer operand is `ModelMap[Name]`,
 * which declares no keys of its own, so nothing names what `"inputs"` selects and both
 * accesses stay widened.
 */
export declare function runModel<Name extends keyof ModelMap>(
    name: Name,
    input: ModelMap[Name]["inputs"],
): ModelMap[Name]["outputs"];

/** A dispatch target whose map is only ever bounded, never given. */
export declare class Feed<EventMap extends Record<string, WorkerEvent> = Record<string, WorkerEvent>> {
    /** A literal index over a bound that declares no keys: the access stays widened. */
    take(event: EventMap["fetch"]): void;
}
