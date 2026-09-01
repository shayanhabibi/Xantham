// A number literal type whose value is not finite. `1e999` overflows to Infinity, and the
// server's JSON encoder cannot write that (`+Inf`), so any response carrying this type is
// refused as a whole. The mailbox tests use it to prove a refused batch is replayed member by
// member. Not in tsconfig.json's `files`: the program tests count what that lists.
export type Infinite = 1e999;
export type NegativeInfinite = -1e999;
export type Finite = 1;
export type Text = string;
export type Flag = boolean;
export type Words = "a" | "b";
export type Count = number;
export type Nothing = undefined;
