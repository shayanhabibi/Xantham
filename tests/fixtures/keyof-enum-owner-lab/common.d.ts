export declare const Parsed: { string: "string"; number: "number"; object: "object" };
export type Parsed = keyof typeof Parsed;
export declare const OtherParsed: { string: "string"; number: "number"; object: "object" };
export type OtherParsed = keyof typeof OtherParsed;
export interface OtherParsedValue { existing: boolean; }
export type Alias = Parsed;
export type Identity<T> = T;
export interface Wrapped { generic: Identity<Parsed>; nullable: Parsed | null; }
export interface Invalid { code: "invalid"; expected: Parsed; received: Parsed; other: OtherParsed; raw: "string" | "number" | "object"; alias: Alias; }
export interface Other { code: "other"; message: string; }
export type Issue = Invalid | Other;
