export type Mask<T> = NoInfer<T>;
export type { NoInfer as CompilerNoInfer };
export function choose<T>(value: T, other: NoInfer<T>): T;
export namespace Indexed {
    export type NoInfer<T> = [T][T extends any ? 0 : never];
    export interface Options<T> { guard: NoInfer<T>; }
}
export namespace User {
    export type NoInfer<T> = { value: T };
    export type Mask<T> = NoInfer<T>;
}
