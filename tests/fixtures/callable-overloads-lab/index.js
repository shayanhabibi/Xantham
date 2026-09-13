// The runtime behind callable-overloads-lab. `coalesce` dispatches on arity, reached both
// through `makeCoalescer`'s tuple return and through `holder.coalesce`. `oneShot`, `multiplex`
// and `ledger` are plain functions, reached through the negatives' delegate path.
export const coalesce = (value, fallback) => (fallback === undefined ? value : value + fallback);

export const makeCoalescer = () => ["seed", coalesce];

export const oneShot = (value) => value;

export const multiplex = (value, extra) => (extra === undefined ? value + 1 : value + extra);

export const ledger = Object.assign((id) => `entry:${id}`, { count: 0 });

export const holder = { coalesce };
