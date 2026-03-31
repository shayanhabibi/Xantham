// TEST TARGET: union type emission
//
// Verifies that a type alias declared with | produces TsAstNode.Alias whose
// Type resolves to TsAstNode.Union.  Tests check the member count and that
// expected TypeKindPrimitive TypeKeys appear in the Types list.  Note: boolean
// is intentionally avoided here — TypeScript expands it to `true | false`
// internally, which would produce 4 union members instead of 3.

export type StringOrNumber = string | number;
export type ThreeWay = string | number | null;
