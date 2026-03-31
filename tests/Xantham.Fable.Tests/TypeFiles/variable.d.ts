// TEST TARGET: variable (const/let) declaration emission
//
// Verifies that top-level exported const/let declarations produce
// TsAstNode.Variable nodes with the correct Name and Type TypeKey.
// Primitive types (string, number) are asserted using TypeKindPrimitive.TypeKey.

export declare const VERSION: string;
export declare let count: number;
export declare const PI: number;
