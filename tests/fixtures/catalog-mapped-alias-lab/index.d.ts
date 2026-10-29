// Hand-authored fixture: aliases applying one generic object alias, whose member is a mapped
// type, to different arguments. The catalogue keys each alias by its own declaration: a mapped
// type over `T`, over `output<T>` and over `Partial<T>` must not share an identity, or the
// later aliases are written as the first.
export * as core from "./core/index";
export * from "./errors";
export * from "./compat";
