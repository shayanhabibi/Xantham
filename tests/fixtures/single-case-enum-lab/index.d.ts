// Single-case string enums, minted where overloads split on a literal parameter
// (DO002): each arm becomes a one-case enum. A single-literal type alias (e.g.
// `type Mode = "strict"`) widens to `string` instead and never reaches this path.
export type Level = "low" | "high";
export function pick(kind: "fast"): number;
export function pick(kind: "slow"): string;
export function run(mode: "ok"): string;
export function run(mode: "error"): number;
export function level(value: Level): Level;
