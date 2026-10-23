export interface LeftCall { (value: number): number; left: number; right?: never; }
export interface RightCall { (value: number): number; right: string; left?: never; }
export type Calls = LeftCall | RightCall;
export interface RightData { right: string; left?: never; }
export type Mixed = LeftCall | RightData;
export interface TextCall { (value: string): string; right: string; left?: never; }
export type DifferentCalls = LeftCall | TextCall;

export interface Instance { value: number; }
export interface LeftConstructor { new(value: number): Instance; left: number; right?: never; }
export interface RightConstructor { new(value: number): Instance; right: string; left?: never; }
export type Constructors = LeftConstructor | RightConstructor;

export interface LeftGeneric<T> { (value: T): T; left: number; right?: never; }
export interface RightGeneric<T> { (value: T): T; right: string; left?: never; }
export type Generic<T> = LeftGeneric<T> | RightGeneric<T>;
export declare function generic(): Generic<string>;

export interface LeftMethod { <T>(value: T): T; left: number; right?: never; }
export interface RightMethod { <U>(value: U): U; right: string; left?: never; }
export type GenericMethod = LeftMethod | RightMethod;
export declare function genericMethod(): GenericMethod;
export interface LeftOverload { (value: "a"): number; (value: "b"): number; left: number; right?: never; }
export interface RightOverload { (value: "a"): number; (value: "b"): number; right: string; left?: never; }
export type Overloaded = LeftOverload | RightOverload;
export declare function overloaded(): Overloaded;

export type Ambiguous =
    { (value: number): number; left?: number; right?: never } |
    { (value: number): number; right?: string; left?: never };
export type UndefinedRequired =
    { (value: number): number; left: number | undefined; right?: never } |
    { (value: number): number; right: string | undefined; left?: never };
export type Escaped =
    { (value: number): number; "left-$0": number; right?: never } |
    { (value: number): number; right: string; "left-$0"?: never };

export declare function left(): LeftCall;
export declare function right(): RightCall;
export declare function incomingLeft(): Calls;
export declare function incomingRight(): Calls;
export declare function constructor(): Constructors;
export declare function escaped(): Escaped;
