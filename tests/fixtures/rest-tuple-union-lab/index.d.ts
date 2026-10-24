export interface Step<T, TNext> {
    next(...[value]: [] | [TNext]): T;
}
export declare function range(...args: [end: number] | [start: number, end: number] | [start: number, end: number, step: number]): number[];
/** Instantiated with a unit-mapping argument: the arm collapsing onto `unit -> string` is written once. */
export interface VoidStep extends Step<string, undefined> {}
export declare class Cursor {
    move(...args: [offset: number] | [line: number, column: number]): void;
}
/** A variadic arm keeps the rest form. */
export declare function tail(...args: [string] | [string, ...number[]]): void;
