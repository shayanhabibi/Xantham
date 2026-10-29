export type Store<T> = T;
export declare const Store: string;
export type Plain = "same";
export type Phantom<T> = "same";
export interface Existing { value: string }
export type ExistingPhantom<T> = Existing;
export type FirstPhantom<T> = "reverse";
export type LaterPlain = "reverse";
export type Reordered<A, B> = [B, A];
export type NoInfer<T extends any> = [T][T extends any ? 0 : never];
export declare function store<T>(value: T): Store<T>;
export declare function noInfer<T>(value: T): NoInfer<T>;
