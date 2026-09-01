/** Time unit for durations. */
export type TimeUnit = "ms" | "s";
/** Playback direction. */
export type Direction = 0 | 1;
/** A mixed literal union (D12). */
export type Mixed = "auto" | 1.5 | false;
/** Called every tick. */
export type TickCallback = (progress: number, count?: number) => void;
/** Options for a timer. */
export interface TimerOptions {
    /** Total duration. */
    duration?: number;
    unit?: TimeUnit;
    onTick?: TickCallback;
    labels: string[];
}
/** A minimal chainable class. */
export declare class Timer {
    constructor(options?: TimerOptions);
    readonly progress: number;
    speed: number;
    play(): this;
    seek(time: number, muteCallbacks?: boolean): this;
    tween(...values: number[]): this;
}
/** Creates a timer. */
export declare function createTimer(options?: TimerOptions): Timer;
/** Overloaded rounding. */
export declare function round(value: number): number;
export declare function round(value: string, decimals: number): string;
/** The library version. */
export declare const version: string;
/** Default options, exported as a value. */
export declare const defaults: TimerOptions;
/** A parameter-position object literal. */
export declare function configure(settings: { fps: number; muted?: boolean }): void;
/** A homogeneous tuple (D7). */
export type Coords = [number, number];
/** A heterogeneous tuple (D7). */
export type Entry = [string, number];
/** An optional tail element - the checker hands it over as `number | undefined`. */
export type Span = [number, number?];
/** A rest element - no fixed F# tuple form, so it widens to an array. */
export type Segments = [string, ...number[]];
/** A heterogeneous union, erased (D4). */
export type Sizeish = number | string;
/** An erased union over two named shapes. */
export type Subject = Timer | TimerOptions;
/** Wider than the erased-union arity, so it widens to obj. */
export type Anything = string | number | boolean | Timer | TimerOptions;
/** One arm of a discriminated union. */
export interface CircleShape {
    kind: "circle";
    radius: number;
}
/** Another arm, whose tag needs a CompiledName. */
export interface RoundRectShape {
    kind: "round-rect";
    width: number;
    height: number;
    radius: number;
}
/** A union the checker proves is discriminated (D4). */
export type Shape = CircleShape | RoundRectShape;
/** Reads a discriminated union off its tag - the run gate's proof the erasure agrees with JavaScript. */
export declare function area(shape: Shape): number;
/** Hands back a tagged object built on the JavaScript side, for the F# side to match on. */
export declare function makeRoundRect(width: number, height: number, radius: number): Shape;
/** A generic declaration, referenced from its own members (§4.9). */
export interface Box<T> {
    value: T;
    map(next: T): Box<T>;
}
/** An instantiation of a generic declaration - written as an application, not re-expanded. */
export type StringBox = Box<string>;
/** A constraint F# can state: the bound is another generated interface. */
export interface Holder<T extends Timer> {
    held: T;
}
/** A constraint F# has no form for, dropped with a finding. */
export interface Keyed<K extends string> {
    key: K;
}
/** A generic callback alias (D5). */
export type Mapper<T> = (input: T) => T;
export * as utils from "./utils.js";
