export interface Keys {
    readonly None: string;
    readonly _None: string;
    readonly Error?: string;
}
export declare function inspect(value: Keys): string;
export declare function join(None: string, Error: string): string;
