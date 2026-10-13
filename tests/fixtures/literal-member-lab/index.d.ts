/** An event-shaped type: methods beside numeric constants. */
export interface PhaseEvent {
    readonly eventPhase: number;
    readonly type: string;
    /** The event is not being processed. */
    readonly NONE: 0;
    readonly CAPTURING_PHASE: 1;
    readonly AT_TARGET: 2;
    readonly BUBBLING_PHASE: 3;
    stopPropagation(): void;
}

/** Inherits the constants. */
export interface PointerPhaseEvent extends PhaseEvent {
    readonly pointerId: number;
}

/** A string discriminant. */
export interface MethodSignal {
    readonly kind: "method";
    name: string;
}

/** A writable string discriminant. */
export interface WritableSignal {
    kind: "writable";
    name: string;
}

/** Constants only. */
export interface Phases {
    readonly NONE: 0;
    readonly DONE: 1;
}

/** A generic declaration with a discriminant. */
export interface Tagged<T> {
    readonly kind: "tagged";
    value: T;
}

/** Values that need escaping, a negative and a fraction. */
export interface Quoted {
    readonly marker: "it's \"q\" \\ $0 {{x}}";
    readonly NEGATIVE: -1;
    readonly HALF: 0.5;
    note?: string;
}

export declare enum Level {
    Low = 0,
    High = 1,
}

/** None of these is a literal member. */
export interface NotLiterals {
    mode?: "fast";
    readonly optional?: 1;
    readonly flag: true;
    readonly level: Level.High;
}

export declare const event: PhaseEvent;

export declare function inspect(value: object): string;
