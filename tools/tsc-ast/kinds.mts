/**
 * The SyntaxKind numbering rule, kept apart from the emitter because it is the one piece of
 * this generator that has to be right: everything else is spelling, but a wrong ordinal
 * silently mis-reads every blob.
 *
 * Named elements are numbered in declaration order; comment-only entries are section headings
 * and do not consume a value. This mirrors the Go enum's `iota`
 * (upstream/tools/scripts/tsc/generate-go-ast.ts:1024-1039), which is what makes the values
 * comparable with the `kind` word of a binary AST blob.
 */
import { api } from "./upstream/tools/scripts/tsc/schema.ts";

/** Kind name to ordinal, in schema order. */
export function kindValues(): Map<string, number> {
  const values = new Map<string, number>();
  for (const element of api.kindElements()) {
    if (!element.name) continue;
    if (values.has(element.name)) throw new Error(`kind element ${element.name} is declared twice`);
    values.set(element.name, values.size);
  }
  return values;
}

/**
 * Values that must hold for the numbering to be wire-compatible. `307` and `79` are the
 * magic numbers hard-coded in the live tests today.
 */
export const ANCHORS: ReadonlyArray<readonly [string, number]> = [
  ["Unknown", 0],
  ["EndOfFile", 1],
  ["Identifier", 79],
  ["SourceFile", 307],
];
