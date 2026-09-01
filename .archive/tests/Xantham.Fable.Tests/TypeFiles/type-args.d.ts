export interface Box<T> {
    value: T;
}

export interface Container<T = number> {
    item: T;
}

export type StringBox = Box<string>;
export type IntBox = Box<number>;
export type StringContainer = Container<string>;
export type NumberContainer = Container;

export interface DefaultTypeParams<T, U = number, Y = string> {
    first: T;
    second: U;
    third: Y;
}

export type DefaultTypeParamsSingleArg = DefaultTypeParams<string>;
export type DefaultTypeParamsDoubleArg = DefaultTypeParams<string, string>;
export type DefaultTypeParamsDoubleSplit<U> = DefaultTypeParams<string, U, number>;
