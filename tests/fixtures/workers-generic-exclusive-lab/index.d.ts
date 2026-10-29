export interface Instance<P> { params: P; }
export declare abstract class Workflow<P = unknown> { createBatch(options: Options<P>): void; }
type Options<P = unknown> =
    { count: number; params?: P; instances?: never } |
    { instances: Instance<P>[]; count?: never; params?: never };

export interface CountArm<P> { count: number; params?: P; instances?: never; }
export type NamedOptions<P> = CountArm<P> | { instances: Instance<P>[]; count?: never; params?: never; };
export interface NamedWorkflow<P> {
    create(options: NamedOptions<P>): void;
    count(options: CountArm<P>): void;
}
export type ExtractedCount<P> = Extract<Options<P>, { count: number }>;
type BooleanArm<T> = T extends unknown ? { [K in keyof T]: Exclude<T[K], undefined> extends never ? never : boolean } : never;
export type BooleanOptions = BooleanArm<Options<string>>;
