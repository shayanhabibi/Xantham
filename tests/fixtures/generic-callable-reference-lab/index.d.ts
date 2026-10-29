export interface PairCallback<T> { (left: T, right: T): T; }
export type Pair<T> = [T, PairCallback<T>];
export declare function pair(value: string): Pair<string>;
export declare function callback(): PairCallback<string>;
export declare function accept(value: PairCallback<string>): string;
