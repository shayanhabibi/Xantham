// Runtime of `utils.d.ts`; see index.js.
export function clamp(value, min, max) {
    return Math.min(Math.max(value, min), max);
}
export const epsilon = 1e-9;
