/**
 * Generates `src/Xantham.TypeScript.Wire/Session.generated.fs` from the wire schema that the
 * `typescript` npm package ships at `dist/api/proto.generated.d.ts`.
 *
 * 123 of the schema's 142 methods take a parameter record whose first two fields are
 * `snapshot: number` and `project: string`. Those two identify *which* program is being asked,
 * not what is being asked of it, and they are constant across every call a caller makes against
 * one program - so at the call site they are pure repetition. `Session<'T>` binds them once and
 * re-exposes the methods without them, the way `Node<'Tag>` binds a source file and an index.
 *
 * ## Why this is a second generator rather than a branch of `tools/proto-gen/generate.mjs`
 *
 * The premise here is a *property of the schema*, not a guarantee it makes: nothing stops the
 * compiler from renaming `project`, from making it optional, or from splitting the pair. If that
 * happens, the `Proto*.generated.fs` layers are still correct and must keep building - only this
 * convenience layer becomes wrong. Keeping the two generators apart means a schema change of that
 * kind shrinks `Session` and leaves everything underneath it untouched.
 *
 * That isolation is the point, so it is bought honestly: this file re-parses the schema with its
 * own copy of the type mapping rather than importing proto-gen's. `tools/proto-gen/generate.mjs`
 * is not modified by this generator's existence, and the check is that the three
 * `Proto*.generated.fs` files come back byte-identical after a full regeneration. The same trade,
 * for the same reason, is recorded in `tools/tsc-ast/fsharp.mjs:4-6`.
 *
 * ## Degrade, never fail
 *
 * A method the mapper cannot render, or one whose params record has stopped carrying the pair, is
 * skipped and reported rather than fatal. The run fails only if there is nothing left to emit at
 * all. A schema drift therefore costs members, not the build.
 *
 * The parser is a TypeScript 5.x install resolved from `<parser-dir>`, for the reason
 * `tools/proto-gen/generate.mjs` explains: TypeScript 7's package exposes only `version` to
 * `require`, so it cannot parse its own schema.
 *
 *   node tools/session-gen/generate.mjs <typescript-pkg-dir> <parser-dir> <out/Session.generated.fs>
 */
import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";
import { Lines } from "../tsc-ast/fsharp.mjs";

const [, , tsPkgDir, parserDir, outFile] = process.argv;
if (!tsPkgDir || !parserDir || !outFile) {
  console.error("usage: generate.mjs <typescript-pkg-dir> <parser-dir> <out/Session.generated.fs>");
  process.exit(2);
}

const ts = (() => {
  const require = createRequire(path.resolve(parserDir, "noop.js"));
  try {
    return require("typescript-5");
  } catch (e) {
    if (e.code !== "MODULE_NOT_FOUND") throw e;
    console.error(
      `cannot resolve the \`typescript-5\` parser from ${parserDir} - run \`npm install\` there. ` +
        "It is the aliased TypeScript 5.x compiler API; the 7.x package cannot parse.",
    );
    process.exit(2);
  }
})();

const DTS = path.join(tsPkgDir, "dist/api/proto.generated.d.ts");
const schemaVersion = JSON.parse(fs.readFileSync(path.join(tsPkgDir, "package.json"), "utf8")).version;
const src = ts.createSourceFile(DTS, fs.readFileSync(DTS, "utf8"), ts.ScriptTarget.Latest, true);

/** Anything skipped, printed at the end. Not fatal: see the header. */
const skipped = [];

// ── F# naming, mirroring `tools/proto-gen/generate.mjs` ───────────────────
// The emitted members have to line up with the records in `Proto.generated.fs` field for field,
// so the escaping and casing rules are transcribed from there rather than reinvented.

const pascal = s => s[0].toUpperCase() + s.slice(1);
const camel = s => s[0].toLowerCase() + s.slice(1);

/** Guards pascal-cased record fields. Broader than the language's reserved set, as proto-gen's is. */
const KEYWORDS = new Set(["type", "module", "end", "done", "function", "match", "with", "to", "or",
  "and", "not", "new", "val", "let", "in", "open", "use", "base", "default", "global", "inline",
  "internal", "public", "private", "void", "file", "fixed", "process", "component", "const"]);
const fsIdent = n => (KEYWORDS.has(n.toLowerCase()) ? "``" + n + "``" : n);

/** Guards camel-cased argument names. Narrower: `file` is a fine argument name. */
const FS_KEYWORDS = new Set(["abstract", "and", "as", "assert", "base", "begin", "class", "const",
  "default", "delegate", "do", "done", "downcast", "downto", "elif", "else", "end", "exception",
  "extern", "false", "finally", "fixed", "for", "fun", "function", "global", "if", "in", "inherit",
  "inline", "interface", "internal", "lazy", "let", "match", "member", "module", "mutable",
  "namespace", "new", "not", "null", "of", "open", "or", "override", "private", "public", "rec",
  "return", "select", "static", "struct", "then", "to", "true", "try", "type", "upcast", "use",
  "val", "void", "when", "while", "with", "yield", "atomic", "break", "checked", "component",
  "constraint", "constructor", "continue", "eager", "event", "external", "functor", "include",
  "method", "mixin", "object", "parallel", "process", "protected", "pure", "sealed", "tailcall",
  "trait", "virtual", "volatile"]);
const TICKS = "``";
const fsParam = n => (FS_KEYWORDS.has(n) ? TICKS + n + TICKS : n);

// ── type mapping, mirroring `tools/proto-gen/generate.mjs` ────────────────

const PRIM = {
  StringKeyword: "string", NumberKeyword: "int", BooleanKeyword: "bool",
  VoidKeyword: "unit", UnknownKeyword: "JsonNode", AnyKeyword: "JsonNode",
  ObjectKeyword: "JsonObject", NullKeyword: "JsonNode", UndefinedKeyword: "unit",
};

/**
 * Maps a TS type node to `{ fs, nullable }`. Unlike proto-gen's, an unmapped type is recorded in
 * `skipped` by the caller rather than pushed onto a fatal problem list - the record it belongs to
 * is still emitted correctly by proto-gen, and only this layer's member for it is lost.
 */
function mapType(node, ctx) {
  switch (node.kind) {
    case ts.SyntaxKind.ParenthesizedType:
      return mapType(node.type, ctx);
    case ts.SyntaxKind.ArrayType:
      return { fs: `${mapType(node.elementType, ctx).fs}[]`, nullable: false };
    case ts.SyntaxKind.TypeOperator:
      if (node.operator === ts.SyntaxKind.ReadonlyKeyword) return mapType(node.type, ctx);
      break;
    case ts.SyntaxKind.LiteralType:
      if (node.literal.kind === ts.SyntaxKind.NullKeyword) return { fs: "JsonNode", nullable: true };
      if (ts.isStringLiteral(node.literal)) return { fs: "string", nullable: false, literal: node.literal.text };
      break;
    case ts.SyntaxKind.UnionType: {
      const arms = node.types.filter(t =>
        t.kind !== ts.SyntaxKind.UndefinedKeyword &&
        t.kind !== ts.SyntaxKind.NullKeyword &&
        !(t.kind === ts.SyntaxKind.LiteralType && t.literal.kind === ts.SyntaxKind.NullKeyword));
      const nullable = arms.length !== node.types.length;
      if (arms.length === 0) return { fs: "JsonNode", nullable: true };
      if (arms.length === 1) {
        const r = mapType(arms[0], ctx);
        return { ...r, nullable: nullable || r.nullable };
      }
      const mapped = arms.map(a => mapType(a, ctx));
      if (mapped.every(m => m.literal !== undefined)) {
        return { fs: "string", nullable, literals: mapped.map(m => m.literal) };
      }
      // The one structural union in the schema is DocumentIdentifier.
      if (ctx.endsWith("#docid")) return { fs: "DocumentIdentifier", nullable };
      return { fs: "JsonNode", nullable: true, unmapped: node.getText().slice(0, 70) };
    }
    case ts.SyntaxKind.TypeLiteral:
      return { fs: "JsonObject", nullable: false };
    case ts.SyntaxKind.TypeReference: {
      const name = node.typeName.getText();
      if (name === "Record") return { fs: "JsonObject", nullable: false };
      if (name === "Array" || name === "ReadonlyArray")
        return { fs: `${mapType(node.typeArguments[0], ctx).fs}[]`, nullable: false };
      if (name === "Path" || name === "__String") return { fs: "string", nullable: false };
      return { fs: name, nullable: false, ref: name };
    }
  }
  const prim = PRIM[ts.SyntaxKind[node.kind]];
  if (prim) return { fs: prim, nullable: prim === "JsonNode" };
  return { fs: "JsonNode", nullable: true, unmapped: node.getText().slice(0, 70) };
}

/**
 * Fields the schema types as a bare `number` that are really a compiler enum. Transcribed from
 * `tools/proto-gen/generate.mjs`, because the member argument types have to agree with the record
 * fields they are assigned to. A key that stops matching is reported, not fatal.
 */
const FIELD_ENUMS = {
  "SymbolResponse.flags": "SymbolFlags",
  "SymbolResponse.checkFlags": "CheckFlags",
  "TypeResponse.flags": "TypeFlags",
  "TypeResponse.objectFlags": "ObjectFlags",
  "TypeResponse.elementFlags": "ElementFlags",
  "SignatureResponse.flags": "SignatureFlags",
  "TypePredicateResponse.kind": "TypePredicateKind",
  "GetSignaturesOfTypeParams.kind": "SignatureKind",
  "ResolveNameParams.meaning": "SymbolFlags",
  "GetSymbolsInScopeParams.meaning": "SymbolFlags",
  "SignatureToSignatureDeclarationParams.kind": "SyntaxKind",
  "SignatureToSignatureDeclarationParams.flags": "NodeBuilderFlags",
};

const retypedFields = new Set();
function retype(mapped, key) {
  const enumName = FIELD_ENUMS[key];
  if (!enumName) return mapped;
  retypedFields.add(key);
  if (mapped.fs === "int") return { ...mapped, fs: enumName };
  if (mapped.fs === "int[]") return { ...mapped, fs: `${enumName}[]` };
  skipped.push(`${key}: listed in FIELD_ENUMS as ${enumName}, but the schema types it '${mapped.fs}'`);
  return mapped;
}

/** Doc comment lines, reflowed as F# `///` comments. */
function docLines(node) {
  const full = node.getFullText();
  const m = full.match(/\/\*\*([\s\S]*?)\*\//);
  if (!m) return [];
  return m[1].split("\n")
    .map(l => l.replace(/^\s*\*\s?/, "").replace(/\s+$/, ""))
    .map(l => l.replace(/\{@link ([^}]*)\}/g, "$1"))
    .reduce((acc, l) => {
      if (l === "" && (acc.length === 0 || acc[acc.length - 1] === "")) return acc;
      acc.push(l);
      return acc;
    }, [])
    .filter((l, i, a) => !(l === "" && i === a.length - 1));
}

// ── read the schema ───────────────────────────────────────────────────────

const interfaces = [];
for (const st of src.statements) {
  if (ts.isInterfaceDeclaration(st)) interfaces.push(st);
}

// Both sides of a batch entry are schema-untyped UTF-8 JSON in `Proto.generated.fs`, spliced into
// a larger document. There is no sensible flat argument for one, so a record carrying one keeps
// only its record form - which a session, whose whole job is to elide two fields of that record,
// cannot offer. Such methods are skipped.
const rawJsonFields = new Set(["BatchRequest.params", "BatchResponse.result"]);

/** name -> [{ wire, ident, fs, optional, bare, raw }], matching the emitted record exactly. */
const recordFields = new Map();
for (const decl of interfaces) {
  const name = decl.name.text;
  if (name === "APIMethodInfo") continue;
  const fields = [];
  recordFields.set(name, fields);
  for (const m of decl.members.filter(ts.isPropertySignature)) {
    const wire = m.name.getText().replace(/^["']|["']$/g, "");
    const isDocId = m.type.getText().includes("DocumentIdentifier");
    const mapped = retype(mapType(m.type, `${name}.${wire}${isDocId ? "#docid" : ""}`), `${name}.${wire}`);
    const optional = !!m.questionToken || mapped.nullable;
    if (rawJsonFields.has(`${name}.${wire}`)) {
      fields.push({ wire, ident: fsIdent(pascal(wire)), fs: "byte[]", optional, bare: false, raw: true });
      continue;
    }
    const bare = mapped.fs === "JsonNode" || mapped.fs === "JsonObject";
    fields.push({
      wire, ident: fsIdent(pascal(wire)), fs: mapped.fs, optional, bare, raw: false,
      unmapped: mapped.unmapped,
    });
  }
}

for (const key of Object.keys(FIELD_ENUMS)) {
  if (!retypedFields.has(key)) skipped.push(`FIELD_ENUMS: ${key} matched no field in the schema`);
}

const info = interfaces.find(d => d.name.text === "APIMethodInfo");
if (!info) {
  console.error("no APIMethodInfo in the schema - nothing to generate");
  process.exit(1);
}
const methods = info.members.filter(ts.isPropertySignature).map(m => {
  const wire = m.name.getText().replace(/^["']|["']$/g, "");
  const [p] = m.type.typeArguments;
  return { wire, params: mapType(p, `${wire}.params`), doc: docLines(m) };
});

// ── partition ─────────────────────────────────────────────────────────────
// The whole premise, recomputed from the schema every run rather than pinned to a list: a method
// belongs on the session when its params record identifies a program the way the session does.

const SNAPSHOT = { ident: "Snapshot", fs: "int" };
const PROJECT = { ident: "Project", fs: "string" };

const identifies = (fields, { ident, fs }) =>
  fields.some(f => f.ident === ident && f.fs === fs && !f.optional);

/** Methods reduced to their non-identifying fields, ready to emit. */
const sessionMethods = [];
/** Methods that identify nothing, which hang off `.Sessionless`. */
const sessionlessMethods = [];

for (const method of methods) {
  const { wire, params, doc } = method;
  const noParams = params.fs === "JsonNode" || params.fs === "unit";
  if (noParams) {
    sessionlessMethods.push({ wire, doc, params: null, fields: [] });
    continue;
  }

  const fields = recordFields.get(params.fs);
  if (!fields) {
    skipped.push(`${wire}: no record for its parameter type '${params.fs}'`);
    continue;
  }
  const unmapped = fields.find(f => f.unmapped);
  if (unmapped) {
    skipped.push(`${wire}: ${params.fs}.${unmapped.wire} is an unmapped type '${unmapped.unmapped}'`);
    continue;
  }

  const hasSnapshot = identifies(fields, SNAPSHOT);
  const hasProject = identifies(fields, PROJECT);

  if (!hasSnapshot && !hasProject) {
    if (fields.some(f => f.raw)) {
      skipped.push(`${wire}: ${params.fs} carries raw JSON, which has no flat argument form`);
      continue;
    }
    sessionlessMethods.push({ wire, doc, params: params.fs, fields });
    continue;
  }

  if (fields.some(f => f.raw)) {
    skipped.push(`${wire}: ${params.fs} carries raw JSON, which has no flat argument form`);
    continue;
  }

  // What is left once the session has supplied what it identifies.
  const rest = fields.filter(f =>
    !(hasSnapshot && f.ident === SNAPSHOT.ident) && !(hasProject && f.ident === PROJECT.ident));
  sessionMethods.push({ wire, doc, params: params.fs, fields, rest, hasSnapshot, hasProject });
}

if (sessionMethods.length === 0) {
  console.error(
    "no method in the schema takes a snapshot and a project - the session layer has no premise left.\n" +
    skipped.map(s => `  ${s}`).join("\n"));
  process.exit(1);
}

// ── emit ──────────────────────────────────────────────────────────────────

/** Doc lines. Blank lines survive as a bare `///`, so paragraph breaks reach the emitted file. */
function doc(out, text) {
  for (const line of String(text).split(/\r?\n/)) out.w(line.trim() ? `/// ${line.trim()}` : "///");
  return out;
}

/** The argument list and record assignments for one method, in proto-gen's order and form. */
function signature(method, { bind }) {
  const source = bind ? method.rest : method.fields;
  const ordered = [...source.filter(f => !f.optional), ...source.filter(f => f.optional)];
  const args = ordered.map(f =>
    f.optional
      ? `[<Struct>] ?${fsParam(camel(f.wire))}: ${f.fs}`
      : `${fsParam(camel(f.wire))}: ${f.fs}`);
  const assignments = ordered.map(f => {
    const arg = fsParam(camel(f.wire));
    // A bare JsonNode field has no voption wrapper to fill; absent is null there.
    return f.optional && f.bare
      ? `${f.ident} = (match ${arg} with ValueSome value -> value | ValueNone -> null)`
      : `${f.ident} = ${arg}`;
  });
  if (bind) {
    if (method.hasProject) assignments.unshift(`${PROJECT.ident} = this.Project`);
    if (method.hasSnapshot) assignments.unshift(`${SNAPSHOT.ident} = this.Snapshot`);
  }
  return { args, assignments };
}

/**
 * The member block for one surface. Emitted for both transports from the same method table so
 * the synchronous and asynchronous surfaces cannot drift, the way `extensions()` in
 * `tools/proto-gen/generate.mjs` does for the channel and the mailbox.
 */
function members(out, className, receiverType, apiModule, list, { bind, summary }) {
  doc(out, summary);
  out.w("[<Extension>]");
  out.w(`type ${className} =`);
  out.blank();
  out.indent(out => {
    for (const method of list) {
      const fn = fsIdent(camel(method.wire));
      for (const d of method.doc) out.w(`/// ${d}`);
      out.w("[<Extension>]");

      if (method.params === null) {
        out.w(`static member ${fn}(this: ${receiverType}) =`);
        out.indent(out => out.w(`${apiModule}.${fn} this.Transport`));
        out.blank();
        continue;
      }

      const { args, assignments } = signature(method, { bind });
      out.w(`static member ${fn}(this: ${receiverType}${args.length ? ", " + args.join(", ") : ""}) =`);
      out.indent(out => {
        // The record is bound and annotated rather than written inline. Several parameter records
        // share a field set - `Snapshot`, `Project` and `File` alone identify a dozen - and F#
        // resolves an unannotated literal to the last one declared, silently. The annotation has
        // to sit on the record and not on the call, so it needs a binding of its own.
        out.w(`let parameters: ${method.params} =`);
        out.indent(out => out.w(`{ ${assignments.join("\n  ")} }`));
        out.blank();
        out.w(`${apiModule}.${fn} this.Transport parameters`);
      });
      out.blank();
    }
  });
  out.blank();
}

const out = new Lines();
out.w("namespace Xantham.TypeScript.Wire");
out.blank();
out.w("// <auto-generated>");
out.w("//   Generated by tools/session-gen/generate.mjs from the wire schema shipped in");
out.w(`//   typescript@${schemaVersion} at dist/api/proto.generated.d.ts.`);
out.w("//   Do not edit by hand - re-run the generator when the compiler is upgraded.");
out.w("// </auto-generated>");
out.blank();
out.w("open System.Runtime.CompilerServices");
out.w("open System.Text.Json.Nodes");
out.w("open Xantham.TypeScript.Wire.Proto");
out.blank();

doc(out, 
  "The methods that identify nothing - they open a snapshot rather than read one - reached\n" +
  "through `session.Sessionless` so that they stay explorable without diluting the session's\n" +
  "own surface with calls that ignore it.");
out.w("type SessionlessApi<'T> = { Transport: 'T }");
out.blank();

doc(out, 
  "A snapshot and a project, bound once.\n" +
  "\n" +
  "Of the wire's methods, most take `snapshot` and `project` as their first two arguments. They\n" +
  "say which program is being asked, not what is being asked of it, and they do not vary across\n" +
  "the calls a caller makes against one program - so `Session` holds them and the members below\n" +
  "take only what is left. It is the same move `Node<'Tag>` makes for a source file and an index.\n" +
  "\n" +
  "The type parameter is the transport. `Session<TscChannel>` answers synchronously and\n" +
  "`Session<TscMailbox>` in `Async`; the member names and argument lists are identical, so the\n" +
  "two differ only in what they return. Build one with `channel.Session(...)` or\n" +
  "`mailbox.Session(...)`.\n" +
  "\n" +
  "Handles mean something only within the program that produced them, so a session is also the\n" +
  "scope in which a symbol or type id is valid. A response that names a different project -\n" +
  "`SymbolResponse.Project`, which the schema documents as the default for follow-up lookups -\n" +
  "is followed with `ForSymbol`, not by passing a project argument.");
out.w("type Session<'T> =");
out.indent(out => {
  out.w("{ Transport: 'T");
  out.w("  Snapshot: int");
  out.w("  Project: string }");
  out.blank();
  doc(out, "The methods that need neither the snapshot nor the project.");
  out.w("member this.Sessionless: SessionlessApi<'T> = { Transport = this.Transport }");
  out.blank();
  doc(out, "The same session against another project of the same snapshot.");
  out.w("member this.WithProject(project: string) = { this with Project = project }");
  out.blank();
  doc(out, 
    "The same session against another snapshot - what `Sessionless.updateSnapshot` hands back\n" +
    "after a file changes.");
  out.w("member this.WithSnapshot(snapshot: int) = { this with Snapshot = snapshot }");
  out.blank();
  doc(out, 
    "Retargets to the project a symbol was first observed in. Follow-up lookups on a symbol can\n" +
    "vary by project, and the response carries the one to use.");
  out.w("member this.ForSymbol(symbol: SymbolResponse) = { this with Project = symbol.Project }");
});
out.blank();

members(out, "SessionExtensions", "Session<TscChannel>", "Api", sessionMethods, {
  bind: true,
  summary:
    "`Api` as members of a session, with the snapshot and project it holds supplied on the\n" +
    "caller's behalf: `session.getSymbolAtPosition(file, position)` rather than\n" +
    "`channel.getSymbolAtPosition(snapshot, project, file, position)`.\n" +
    "\n" +
    "Fields the schema marks optional are `[<Struct>]` optional arguments, so they arrive as the\n" +
    "`voption` the record field already holds and pass straight through. F# requires optional\n" +
    "arguments to come last, so where a required field follows an optional one the argument order\n" +
    "is not the record's own; named arguments sidestep that, and are worth using regardless, since\n" +
    "several methods take three or more arguments of the same type.",
});

members(out, "SessionlessExtensions", "SessionlessApi<TscChannel>", "Api", sessionlessMethods, {
  bind: false,
  summary: "The snapshot-free half of `Api`, reached through `session.Sessionless`.",
});

members(out, "AsyncSessionExtensions", "Session<TscMailbox>", "AsyncApi", sessionMethods, {
  bind: true,
  summary:
    "The mirror of `SessionExtensions` over `TscMailbox`: same names, same arguments, each result\n" +
    "wrapped in `Async`. Calls that overlap in time are collected into a single round trip.",
});

// `batchRequests` has no async counterpart - the mailbox is the batcher, and a batch nested
// inside one would return a result the layer cannot attribute to a caller. Matching `AsyncApi`,
// which omits it for the same reason.
const asyncSessionless = sessionlessMethods.filter(m => m.wire !== "batchRequests");
members(out, "AsyncSessionlessExtensions", "SessionlessApi<TscMailbox>", "AsyncApi", asyncSessionless, {
  bind: false,
  summary:
    "The snapshot-free half of `AsyncApi`. `batchRequests` is absent for the reason `AsyncApi`\n" +
    "omits it: the mailbox is already the batcher.",
});

doc(out, 
  "Ways to get a session. The snapshot and project always come from a response that already\n" +
  "pairs them, so the overloads below take those responses rather than leaving a caller to\n" +
  "unpack the pair and risk crossing a snapshot with another snapshot's project.");
out.w("[<Extension>]");
out.w("type SessionConstructors =");
out.blank();
out.indent(out => {
  for (const [receiver, transport] of [["channel", "TscChannel"], ["mailbox", "TscMailbox"]]) {
    doc(out, `A session on \`${receiver}\` over a snapshot and project named directly.`);
    out.w("[<Extension>]");
    out.w(`static member Session(this: ${transport}, snapshot: int, project: string) =`);
    out.indent(out => out.w(`{ Transport = this; Snapshot = snapshot; Project = project }`));
    out.blank();

    doc(out, 
      "A session on what `createProgram` handed back. Raises when the response carries no\n" +
      "project, which is the compiler saying the root files resolved to no program at all.");
    out.w("[<Extension>]");
    out.w(`static member Session(this: ${transport}, program: CreateProgramResponse) =`);
    out.indent(out => {
      out.w("match program.Project with");
      out.w("| ValueSome project -> { Transport = this; Snapshot = program.Snapshot; Project = project.Id }");
      out.w("| ValueNone -> failwith \"createProgram returned no project, so there is nothing to open a session on\"");
    });
    out.blank();

    doc(out, 
      "A session on one project of a snapshot, named by id. A snapshot can hold several, so the\n" +
      "project is not inferred.");
    out.w("[<Extension>]");
    out.w(`static member Session(this: ${transport}, snapshot: UpdateSnapshotResponse, project: string) =`);
    out.indent(out => out.w("{ Transport = this; Snapshot = snapshot.Snapshot; Project = project }"));
    out.blank();

    doc(out, 
      "A session on a snapshot's only project. Raises when the snapshot holds none or several,\n" +
      "so a caller opening one project never has to index into the array.");
    out.w("[<Extension>]");
    out.w(`static member Session(this: ${transport}, snapshot: UpdateSnapshotResponse) =`);
    out.indent(out => {
      out.w("match snapshot.Projects with");
      out.w("| [| project |] -> { Transport = this; Snapshot = snapshot.Snapshot; Project = project.Id }");
      out.w("| projects ->");
      out.indent(out => out.w(
        "failwithf \"the snapshot holds %d projects, so name the one to open a session on\" projects.Length"));
    });
    out.blank();
  }
});

fs.mkdirSync(path.dirname(outFile), { recursive: true });
fs.writeFileSync(outFile, out.render(), "utf8");

const sessionMembers = sessionMethods.length;
const sessionlessMembers = sessionlessMethods.length;
console.log(
  `session  ${sessionMembers} session members, ${sessionlessMembers} sessionless, ` +
  `over 2 transports, from ${methods.length} methods`);
console.log(`out      ${outFile}`);
if (skipped.length) {
  console.log(`skipped  ${skipped.length} - the layers below are unaffected:`);
  for (const s of skipped) console.log(`  ${s}`);
}
