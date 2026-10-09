export type Choice = "auto" | "manual" | number | null | undefined;
export type EqualA = "auto" | "manual";
export type EqualB = "auto" | "manual";
export type OverlapC = "auto" | "off";
export type Weird = "" | "Number" | "Null" | "Undefined" | "Tags" | "Tag" | "IsTag" | "ToString" | "auto-mode" | "auto_mode" | "quote\"and\\slash" | number | null | undefined;
export function echo(value: Choice): Choice;
