export declare namespace Original {
    type Compute<T, U extends keyof T> = Pick<T, Exclude<keyof T, U>>;
}
export { Original as Mirror };
export declare namespace Separate {
    type Compute<T, U extends keyof T> = Pick<T, Exclude<keyof T, U>>;
}
export declare namespace Structural {
    type Compute<T, U> = { left: T; right: U };
}
