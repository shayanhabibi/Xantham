/** One shape. */
export interface Named {
    name: string;
}
/** Another, with an optional readonly member. */
export interface Timed {
    at: number;
    readonly stamp?: string;
}
/** Two named shapes intersected: both member sets, flattened into one interface. */
export type NamedTimed = Named & Timed;
/** A named shape intersected with an anonymous one. */
export type Extended = Named & { extra: boolean };
/** An intersection reached only at a parameter position: hoisted and named by path. */
export declare function label(target: Named & { id: number }): void;
/** A generic intersection alias: `T` is bound on the alias and a member reads it. */
export type WithValue<T> = Named & { value: T };
/** Operands that overlap on a member: the checker hands over one `volume`, not two. */
export interface Loud {
    volume: number;
}
export interface Pitched {
    volume: number;
    pitch: number;
}
export type LoudPitched = Loud & Pitched;
/** An index-signature operand: the flattened interface carries the indexer beside the member. */
export type Bag = Named & { [key: string]: unknown };
/** A mapped operand: `Partial` expands under D6, so its members arrive optional. */
export type Loose = Named & Partial<Timed>;
/** A callable operand: the properties flatten and the call signature is the hybrid finding. */
export type Cancelable = (() => void) & { cancel(): void };
/** Not flattened: a type-parameter operand has no members to read, so it widens loudly. */
export declare function merge<T>(base: T & { id: number }): T;
