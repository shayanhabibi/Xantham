interface AliasLeft { kind: "left"; value: string; }
interface AliasRight { kind: "right"; value: string; }
type NamedFirst = AliasLeft | AliasRight;
type NamedSecond = AliasLeft | AliasRight;
interface AliasContracts { first: NamedFirst; second: NamedSecond; }
