import * as core from "./core/index";
export type inferFlattenedErrors<T extends core.$ZodType, U = string> = core.$ZodFlattenedError<core.output<T>, U>;
export type partialFlattened<T, U = string> = core.$ZodFlattenedError<Partial<T>, U>;
