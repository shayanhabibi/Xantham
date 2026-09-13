// runtime behind callable-overloads-lab. `coalesce` proves both Invoke overloads reach the same
// underlying function with the arity each call signature declared; `oneShot`, `multiplex` and
// `ledger` are plain functions the negatives' delegates or hybrid Invoke call straight through to.
export const coalesce = (value, fallback) => (fallback === undefined ? value : value + fallback);

export const oneShot = (value) => value;

export const multiplex = (value, extra) => (extra === undefined ? value + 1 : value + extra);

export const ledger = Object.assign((id) => `entry:${id}`, { count: 0 });
