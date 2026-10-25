export interface Legacy {
    readonly "$`": string;
}

export interface EscapedKeys {
    "__`required": string;
    "__`optional"?: string;
}

export interface Options {
    "`required": string;
    "required`": number;
    "`optional"?: string;
}

export interface DerivedOptions extends Options {
    extra: string;
}

export interface RenamedBase {
    "`value": string;
}

export interface RenamedDerived extends RenamedBase {
    Value: number;
}

export type Choice =
    { "`tag": string; a: string; b?: never } |
    { "`tag": string; a?: never; b: number };

export interface Base {
    readonly Item: string;
    readonly Value: string;
}

export interface Collision extends Base {
    __promisify__(event: "close", value: string): string;
    __promisify__(event: "end", value: number): number;
    "$`": string;
    "`": string;
    "`value": string;
    "value`": string;
    "`Invoke"(value: string): string;
    Invoke(value: number): number;
    "`Create": string;
    Create: string;
    "`SymbolToPrimitive"(value: string): string;
    "`SymbolTo"(event: "primitive", value: string): string;
    "`SymbolTo"(event: "other", value: number): number;
    [Symbol.toPrimitive](hint: string): string;
    "`on"(event: "end", value: string): string;
    "`on"(event: "close", value: number): number;
    OnEnd: string;
    "quote\"slash\\$1`": string;
}

export interface Synthetic {
    (): string;
    new(): Options;
    "`Create": string;
    "`Invoke": string;
    "$`": string;
    [key: string]: unknown;
}

export declare class KeyedClass {
    constructor();
    "$`": string;
    "`call"(value: string): string;
    static "$`": string;
    static ordinary: string;
    static "__`property": string;
    static "`call"(value: string): string;
}

export declare const legacy: Legacy;
export declare const collision: Collision;
export declare const renamedDerived: RenamedDerived;
export declare function inspectOptions(value: Options): string;
export declare function inspectChoice(value: Choice): string;
export declare function inspectEscapedKeys(value: EscapedKeys): string;
