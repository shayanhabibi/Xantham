// TEST TARGET: enum emission with non-sequential and negative numeric values
//
// Verifies that TsEnumCase.Value preserves the exact numeric literal assigned,
// including large non-sequential values (HTTP status codes) and negative values.
// Member order is not asserted — tests find cases by Name.

export enum HttpStatus {
    Ok = 200,
    NotFound = 404,
    ServerError = 500
}

export enum Temperature {
    AbsoluteZero = -273,
    Freezing = 0,
    Boiling = 100
}
