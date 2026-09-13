// The runtime behind callable-overloads-lab. `coalesce`, read off `holder`, dispatches on arity:
// both overloads reach the same underlying function. `oneShot`, `multiplex` and `ledger` are
// plain functions, reached through the negatives' delegates or through the hybrid path's own
// `Invoke`.
export const coalesce = (value, fallback) => (fallback === undefined ? value : value + fallback);

export const oneShot = (value) => value;

export const multiplex = (value, extra) => (extra === undefined ? value + 1 : value + extra);

export const ledger = Object.assign((id) => `entry:${id}`, { count: 0 });

export const holder = { coalesce };
