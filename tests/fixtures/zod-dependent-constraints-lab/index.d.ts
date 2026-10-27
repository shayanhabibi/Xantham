export interface Internals<O = unknown, I = unknown> { output: O; input: I; }
export interface Schema<O = unknown, I = unknown, T extends Internals<O, I> = Internals<O, I>> { internals: T; }
export interface ArrayConsumer<T extends Schema<unknown[], unknown[]> = Schema<unknown[], unknown[]>> { schema: T; }
export interface ExplicitConsumer<T extends Schema<string, number, Internals<string, number>>> { schema: T; }
export interface GenericConsumer<O, I, T extends Internals<O, I>> extends Schema<O, I, T> { value: T; }
export interface Methods { use<T extends Schema<string, number>>(value: T): T; }
export interface Nested<A, B, T extends Schema<B, A, Internals<B, A>>> { value: T; }
export interface NestedConsumer { value: Nested<string, number, Schema<number, string, Internals<number, string>>>; }
export interface Wide { left: string; }
export interface Narrow { left: string; extra: number; }
export interface Forward<A extends Internals<B, B>, B extends Wide> { a: A; b: B; }
export interface ForwardConsumer { value: Forward<Internals<Narrow, Narrow>, Narrow>; }
export interface Box<T> { value: T; }
export interface Self<T extends Box<T>> { value: T; }
export type AnySelf = Self<any>;
export interface Recursive extends Box<Recursive> { label: string; }
export interface SelfConsumer { value: Self<Recursive>; }
export interface Cycle<A extends Internals<B, B>, B extends Internals<A, A>> { a: A; b: B; }
export interface Left extends Internals<Right, Right> { label: string; }
export interface Right extends Internals<Left, Left> { label: string; }
export interface CycleConsumer { value: Cycle<Left, Right>; }
