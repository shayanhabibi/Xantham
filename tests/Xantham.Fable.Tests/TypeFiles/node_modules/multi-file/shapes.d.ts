// TEST TARGET: cross-file type reference (provider)
//
// Defines Point2D and Scalar as base types referenced by vectors.d.ts.
// Verifies that types from a non-entry file are still present in the result
// when both files are provided to TypeScriptReader.createFor.

export interface Point2D {
    x: number;
    y: number;
}

export type Scalar = number;
