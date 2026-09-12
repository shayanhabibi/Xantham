// The runtime behind `declare module "layout-lab"`; `register.mjs` resolves the package name
// here from `package.json`. `strict.js` and `aliases.js` back the two ambient subpaths.

export function check(value) {
    return `root:${value}`;
}

export const mode = "root";

export function echo(value) {
    return value;
}

export function convert(value) {
    return value;
}

export function pick(value) {
    return value === "number" ? 2 : value;
}

export function dispatch(kind) {
    return kind;
}
