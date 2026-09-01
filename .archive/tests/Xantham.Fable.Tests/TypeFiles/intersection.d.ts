// TEST TARGET: intersection type emission
//
// Verifies that a type alias declared with & produces TsAstNode.Alias whose
// Type resolves to TsAstNode.Intersection.  Tests check member count and that
// both constituent interface nodes are present independently in the result.

export interface Named { name: string; }
export interface Aged { age: number; }
export type Person = Named & Aged;
