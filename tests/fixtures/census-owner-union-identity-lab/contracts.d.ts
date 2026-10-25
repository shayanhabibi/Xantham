interface UnionLeft<T> { kind: "left"; value: T; }
interface UnionRight<T> { kind: "right"; value: T; }
interface OtherLeft { kind: "left"; value: string; }
interface OtherRight { kind: "right"; value: string; }
interface Contracts {
  first: UnionLeft<string> | UnionRight<string>;
  reversed: UnionRight<string> | UnionLeft<string>;
  other: OtherLeft | OtherRight;
  numeric: UnionLeft<number> | UnionRight<number>;
  primitive: UnionLeft<string> | string;
}
