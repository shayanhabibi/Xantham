export interface $ZodType<O = unknown> { _zod: { output: O } }
export type output<T> = T extends { _zod: { output: any } } ? T["_zod"]["output"] : unknown;
export type $ZodFlattenedError<T, U = string> = _FlattenedError<T, U>;
type _FlattenedError<T, U = string> = { formErrors: U[]; fieldErrors: { [P in keyof T]?: U[] } };
export declare function flattenError<T>(error: T): _FlattenedError<T>;
