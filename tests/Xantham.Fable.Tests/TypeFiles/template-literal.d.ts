// TEST TARGET: template literal type emission
//
// Verifies TsTemplateLiteralType.Texts (fixed string segments) and
// TsTemplateLiteralType.Types (interpolated TypeKeys).
// Invariant: Texts.Length = Types.Length + 1.
// EventName  — 1 interpolation: texts = ["on", ""],        types = [string]
// KeyValuePair — 2 interpolations: texts = ["", ":", ""],  types = [string, string]

export type EventName = `on${string}`;
export type KeyValuePair = `${string}:${string}`;
