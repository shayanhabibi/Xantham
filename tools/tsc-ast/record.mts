/**
 * The `SourceFile` extended-data record layout, parsed out of the vendored `encoder.go`.
 *
 * Every other layout in this generator comes from `ast.json`. This one does not exist there:
 * the record is written by a hand-written Go function (`recordExtendedData_SourceFile`) and
 * read by hand-written TypeScript, so the only statement of its shape is the format
 * documentation in `encoder.go`'s header comment - a markdown table, in prose.
 *
 * Generating from a comment is worth it only if the parse is strict, so it is: the field list,
 * the order, the contiguity of the byte spans and the word count are all checked, and anything
 * unexpected is a problem the caller turns into a failed run. The alternative - nineteen
 * hand-typed offsets guarded by a checksum over a 42kb file that churns constantly - fails
 * silently instead.
 */
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));

const ENCODER_GO = path.join(here, "upstream/tsc/internal/api/encoder/encoder.go");

/** Bytes per word. Every field of the record is a `uint32`. */
const WORD = 4;

/**
 * The fields, in record order, mapped to the F# names they are emitted under.
 *
 * Explicit rather than derived from the table's English, so that an upstream rewording is a
 * failed run rather than a renamed - or worse, reordered - accessor.
 */
const FIELD_NAMES: ReadonlyArray<readonly [string, string]> = [
  ["text", "Text"],
  ["fileName", "FileName"],
  ["path", "Path"],
  ["languageVariant", "LanguageVariant"],
  ["scriptKind", "ScriptKind"],
  ["referencedFiles", "ReferencedFiles"],
  ["typeReferenceDirectives", "TypeReferenceDirectives"],
  ["libReferenceDirectives", "LibReferenceDirectives"],
  ["imports", "Imports"],
  ["moduleAugmentations", "ModuleAugmentations"],
  ["ambientModuleNames", "AmbientModuleNames"],
  ["externalModuleIndicator", "ExternalModuleIndicator"],
  ["originalText", "OriginalText"],
  ["spanMap", "SpanMap"],
  ["supplementalSourceFileNames", "SupplementalSourceFileNames"],
  ["canonicalSourceFileName", "CanonicalSourceFileName"],
  ["contentMapper", "ContentMapper"],
  ["virtualFileName", "VirtualFileName"],
  ["diagnosticDirectives", "DiagnosticDirectives"],
];

export interface RecordField {
  /** The name upstream calls it, from the table. */
  readonly go: string;
  /** The name it is emitted under. */
  readonly name: string;
  /** Byte offset into the record. */
  readonly offset: number;
  /** The table's own description of the field, carried through as the XML doc. */
  readonly doc: string;
}

export interface SourceFileRecord {
  readonly fields: readonly RecordField[];
  /** Total record size in bytes. */
  readonly size: number;
  /** The sentinel a structured-data offset or string index carries when the field is absent. */
  readonly noStructuredData: string;
}

/** The rows of the markdown table introduced by `heading`, as arrays of trimmed cells. */
function tableAfter(source: string, heading: string, problems: string[]) {
  const lines = source.split("\n");
  const start = lines.findIndex(line => line.includes(heading));
  if (start === -1) {
    problems.push(`encoder.go: no format table introduced by "${heading}"`);
    return [];
  }

  const rows: string[][] = [];
  for (let i = start + 1; i < lines.length; i++) {
    const line = lines[i].trim();
    if (line === "//") continue;
    if (!line.startsWith("// |")) {
      if (rows.length) break;
      continue;
    }
    const cells = line.slice("//".length).trim().split("|").slice(1, -1).map(cell => cell.trim());
    // The header row and its underline carry no data.
    if (cells[0] === "Byte offset" || /^-+$/.test(cells[0])) continue;
    rows.push(cells);
  }

  if (!rows.length) problems.push(`encoder.go: the table after "${heading}" has no rows`);
  return rows;
}

/**
 * The number of words the writer actually appends, as a second opinion on the table. Counted
 * from the `appendUint32s` call at the end of `recordExtendedData_SourceFile`, less its first
 * argument, which is the buffer.
 */
function appendedWordCount(source: string, problems: string[]) {
  const body = source.match(
    /func recordExtendedData_SourceFile\([^)]*\) \{([\s\S]*?)\n\}/,
  )?.[1];
  const call = body?.match(/appendUint32s\((.*)\)\s*$/m)?.[1];
  if (!call) {
    problems.push("encoder.go: no appendUint32s call found in recordExtendedData_SourceFile");
    return undefined;
  }

  // Arguments are calls themselves (`uint32(sf.ScriptKind)`), so only commas at depth 0 split.
  let depth = 0;
  let count = 1;
  for (const char of call) {
    if (char === "(") depth++;
    else if (char === ")") depth--;
    else if (char === "," && depth === 0) count++;
  }
  return count - 1;
}

/** Parses the record layout, pushing anything unexpected onto `problems`. */
export function sourceFileRecord(problems: string[]): SourceFileRecord {
  const source = fs.readFileSync(ENCODER_GO, "utf8");
  const rows = tableAfter(source, "and for `SourceFile` is:", problems);

  const fields: RecordField[] = [];
  let expectedOffset = 0;

  for (const [rowIndex, cells] of rows.entries()) {
    const [span, type, doc] = cells;
    const where = `encoder.go: SourceFile record row ${rowIndex}`;

    if (type !== "uint32") {
      problems.push(`${where} is a ${type}, and the reader assumes every field is a uint32`);
      continue;
    }

    const [from, to] = (span.match(/^(\d+)-(\d+)$/)?.slice(1) ?? []).map(Number);
    if (from === undefined) {
      problems.push(`${where} has an unreadable byte span "${span}"`);
      continue;
    }
    if (from !== expectedOffset || to !== from + WORD) {
      problems.push(
        `${where} spans ${span}, expected ${expectedOffset}-${expectedOffset + WORD}: the ` +
          `record is no longer a run of contiguous words`,
      );
      continue;
    }
    expectedOffset = to;

    // The field is the first thing the description names in backticks.
    const go = doc.match(/`([^`]+)`/)?.[1];
    const name = FIELD_NAMES[rowIndex]?.[1];
    if (go === undefined) {
      problems.push(`${where} names no field: "${doc}"`);
      continue;
    }
    if (go !== FIELD_NAMES[rowIndex]?.[0]) {
      problems.push(
        `${where} is \`${go}\`, expected \`${FIELD_NAMES[rowIndex]?.[0]}\`: the record's fields ` +
          `were reordered, renamed or reworded upstream`,
      );
      continue;
    }

    fields.push({ go, name, offset: from, doc });
  }

  if (fields.length !== FIELD_NAMES.length) {
    problems.push(
      `encoder.go: read ${fields.length} SourceFile record fields, expected ${FIELD_NAMES.length}`,
    );
  }

  const appended = appendedWordCount(source, problems);
  if (appended !== undefined && appended !== rows.length) {
    problems.push(
      `encoder.go: recordExtendedData_SourceFile writes ${appended} words but the format table ` +
        `documents ${rows.length}`,
    );
  }

  const noStructuredData = source.match(/const noStructuredData = (0x[0-9A-Fa-f]+)/)?.[1];
  if (!noStructuredData) {
    problems.push("encoder.go: no `noStructuredData` constant found");
  }

  return { fields, size: expectedOffset, noStructuredData: noStructuredData ?? "0xFFFFFFFF" };
}
