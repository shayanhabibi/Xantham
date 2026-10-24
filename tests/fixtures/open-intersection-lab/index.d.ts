export interface Base {
    id: number;
}
export interface Wide {
    req: Base;
    label: string;
}
/** The literal restates a member the bound declares: the intersection is `R`. */
export type Listener<R extends Wide> = (res: R & { req: Base }) => void;
/** The literal adds members over the open operand: both sides are kept. */
export type Handler<E, T> = (e: E & { currentTarget: T; target: Base }) => void;
/** A generic alias over an open operand and a literal. */
export type WithChildren<P> = P & { children?: string };
export declare function wrap<P>(props: WithChildren<P>): P;
export declare function listen(handler: Handler<Base, Wide>): void;
/** Two open operands. */
export declare function assign<T, U>(target: T, source: U): T & U;
/** Three operands nest. */
export type Counted<P, Q> = P & Q & { count: number };
/** An empty literal restates nothing: the intersection is `T`. */
export type Defined<T> = T & {};
/** Negative: a mapped operand fills its members in at each use, so it is never read as restating the bound. */
export type Without<T, U> = { [K in Exclude<keyof T, keyof U>]?: never };
export type Exclusive<T, U> = Without<T, U> & U;
/** A primitive beside a type parameter keeps the parameter. */
export interface Pool<T> {
    narrow<U>(other: U): Pool<T & U>;
}
export interface Names extends Pool<string> {}
/** Negative: a primitive operand has no type parameter to keep, and widens as before. */
export type Tally = string & { count: number };
