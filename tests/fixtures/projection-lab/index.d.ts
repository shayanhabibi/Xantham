export type Choice = "auto" | "manual" | number | null | undefined;
export type EqualA = "auto" | "manual";
export type EqualB = "auto" | "manual";
export type OverlapC = "auto" | "off";
export type Weird = "" | "Number" | "Null" | "Undefined" | "Tags" | "Tag" | "IsTag" | "ToString" | "auto-mode" | "auto_mode" | "quote\"and\\slash" | number | null | undefined;
export function echo(value: Choice): Choice;
export interface TextContent { type: "text"; text: string; textSignature?: string; }
export interface ImageContent { type: "image"; data: string; mimeType: string; }
export type Input = string | (TextContent | ImageContent)[];
export interface Session {
    readonly id: string;
    submit(input: Input, options?: { whenBusy?: "followUp" | "steer" }): Input;
    configure(change: { thinkingLevel?: Choice; note?: string }, context: string): Choice;
    inspect(input: { "__proto__": string; constructor: string; optional?: string | undefined }): string;
}
export function session(): Session;
export interface OtherSession {
    submit(input: number, options?: { whenBusy?: "followUp" | "steer" }): number;
}
