export declare namespace v3 {
    namespace util {
        type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;
        type OmitKeys<T, K extends string> = Pick<T, Exclude<keyof T, K>>;
        type MakePartial<T, K extends keyof T> = Omit<T, K> & Partial<Pick<T, K>>;
    }
}
export type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;
export declare namespace other {
    namespace util {
        type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;
    }
}
