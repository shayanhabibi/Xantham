// TEST TARGET: tuple type emission
//
// Verifies TsTuple.FixedLength, TsTuple.MinRequired, and TsTuple.Types list
// length for concrete, optional-element, and rest-element tuple forms.
// Exact TypeKey values are not asserted — only counts and element flags.

export type Coord = [number, number];
export type Triple = [string, number, boolean];
export type WithOptional = [string, number?];
export type WithRest = [string, ...number[]];
