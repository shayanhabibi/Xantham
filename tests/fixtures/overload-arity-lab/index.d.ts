// Lab: `dedupe-overloads` over signatures sharing their first three parameters.

export interface Writer {
    /** Separated by the fourth parameter: both overloads survive. */
    write(a: string, b: string, c: string, d: string): void;
    write(a: string, b: string, c: string, d: number): void;

    /** Negative: the fourth parameters widen to one F# type, so the second is dropped (DO001). */
    pad(a: string, b: string, c: string, d: string[]): void;
    pad(a: string, b: string, c: string, d: ReadonlyArray<string>): void;
}

/** The same separation on call signatures, reached through `Invoke`. */
export interface Setter {
    (a: string, b: string, c: string, d: string): void;
    (a: string, b: string, c: string, d: number): void;
    count: number;
}

export declare const writer: Writer;
export declare const setter: Setter;
