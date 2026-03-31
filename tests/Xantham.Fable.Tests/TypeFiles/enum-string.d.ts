// TEST TARGET: string enum emission
//
// Verifies that TsEnumCase.Value is TsLiteral.String for enums with string
// initialisers.  Tests check the exact string value for each member.

export enum Color {
    Red = "RED",
    Green = "GREEN",
    Blue = "BLUE"
}
