// TEST TARGET: TsEnumType and TsEnumCase emission
//
// Verifies that enum member names, total count, and explicit numeric TsLiteral
// values are preserved exactly.  Member order in the emitted list is not
// asserted — tests find members by Name.
//
// Expected:
//   Direction.Name = "Direction"
//   Direction.Members has 4 entries
//   Up.Value   = TsLiteral.Int 0
//   Down.Value = TsLiteral.Int 1
//   Left.Value = TsLiteral.Int 2
//   Right.Value= TsLiteral.Int 3

export enum Direction {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
}
