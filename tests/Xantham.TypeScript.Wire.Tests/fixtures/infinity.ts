// Non-finite numeric literals share a batch with ordinary types.
// Kept outside tsconfig.json's files because the program tests count those roots.
export type Infinite = 1e999;
export type NegativeInfinite = -1e999;
export type Finite = 1;
export type Text = string;
export type Flag = boolean;
export type Words = "a" | "b";
export type Count = number;
export type Nothing = undefined;
