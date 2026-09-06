// The runtime half of the record-index lab. `index.d.ts` declares `tag` as a named export
// taking a plain object keyed by strings; the run gate's `[<Import("tag", "record-index-lab")>]`
// binding resolves here and records what it was called with, so the gate can confirm a
// `Record<string, 'T>` parameter reaches this function as the same object literal it was built
// from, with no wrapper the index signature's own type would not have produced.

globalThis.__recordIndexLabTagCalls = [];

export function tag(value) {
    globalThis.__recordIndexLabTagCalls.push(value);
}
