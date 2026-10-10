export interface Def { id: string; }
export type Result<T extends Def = Def> = { definition: T };
export interface Middleware<U extends Def = never> { callback?: () => Result<U>; }
export interface Defaults { middleware: Middleware; }
export interface Bound<T> { value: T; }
export interface Applied<U extends Bound<string> = never> { value: U; }
export type AnyApplied = Applied<any>;
export type NeverApplied = Applied<never>;
export interface InheritedApplied extends Applied {}
export interface StringBound extends Bound<string> {}
export interface NumberBound extends Bound<number> {}
export type StringApplied = Applied<StringBound>;
export interface Erased<U extends Bound<unknown> = never> { value: U; }
export type ErasedString = Erased<StringBound>;
export interface Recursive<T extends Recursive<T>> { next: T; }
export interface RecursiveValue extends Recursive<RecursiveValue> {}
export type RecursiveUse = Recursive<RecursiveValue>;
