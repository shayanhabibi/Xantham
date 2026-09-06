// Wave sixteen. An instantiation of a generic declaration is derived as identity where it
// stands at a reference position and in full where a union or an intersection reads it as an
// operand. `Ok<string>` is met first as the type of `Api.last` (identity) and only a
// generation later, through `run`'s return type, as an arm of `Outcome`; the walk re-derives it
// in full, so the tagged-union pass reads its members and `Outcome` folds the same way it would
// had the arm been met as an operand first.
interface Ok<T> {
    kind: "ok";
    value: T;
}

interface Err {
    kind: "err";
    message: string;
}

type Outcome = Ok<string> | Err;

export interface Api {
    last: Ok<string>;
    run(): Outcome;
}
