export interface Marker<T> {
    _?: T;
    read(_: string): T;
    write(_?: T): void;
}

export declare function _(_: Marker<string>): string;
