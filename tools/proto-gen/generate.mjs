/**
 * Generates `src/Xantham.TypeScript.Wire/Proto.generated.fs` and `ProtoApi.generated.fs`
 * from the wire schema that the
 * `typescript` npm package ships at `dist/api/proto.generated.d.ts`.
 *
 * The schema is parsed with the TypeScript compiler API rather than with regexes,
 * and anything the mapper does not recognise is collected as a problem and fails
 * the run - a silently dropped field would surface much later as a null result.
 *
 * TypeScript 7 no longer ships the JS compiler API (`require("typescript")` there
 * exposes only `version`), so the parser is a separate TypeScript 5.x install.
 *
 *   node tools/proto-gen/generate.mjs <typescript-pkg-dir> <parser-dir> <out.generated.fs>
 */
import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";

const [, , tsPkgDir, parserDir, outFile] = process.argv;
if (!tsPkgDir || !parserDir || !outFile) {
  console.error("usage: generate.mjs <typescript-pkg-dir> <parser-dir> <out.generated.fs>");
  process.exit(2);
}
const ts = createRequire(path.resolve(parserDir, "noop.js"))("typescript");

const DTS = path.join(tsPkgDir, "dist/api/proto.generated.d.ts");
const ENUM_DIR = path.join(tsPkgDir, "dist/enums");
const schemaVersion = JSON.parse(fs.readFileSync(path.join(tsPkgDir, "package.json"), "utf8")).version;
const src = ts.createSourceFile(DTS, fs.readFileSync(DTS, "utf8"), ts.ScriptTarget.Latest, true);

const problems = [];
const pascal = s => s[0].toUpperCase() + s.slice(1);

// F# keywords that would collide if they survived pascal-casing.
const KEYWORDS = new Set(["type", "module", "end", "done", "function", "match", "with", "to", "or",
  "and", "not", "new", "val", "let", "in", "open", "use", "base", "default", "global", "inline",
  "internal", "public", "private", "void", "file", "fixed", "process", "component", "const"]);
const fsIdent = n => (KEYWORDS.has(n.toLowerCase()) ? "``" + n + "``" : n);

const PRIM = {
  StringKeyword: "string", NumberKeyword: "int", BooleanKeyword: "bool",
  VoidKeyword: "unit", UnknownKeyword: "JsonNode", AnyKeyword: "JsonNode",
  ObjectKeyword: "JsonObject", NullKeyword: "JsonNode", UndefinedKeyword: "unit",
};

/** Maps a TS type node to { fs, nullable }. `nullable` means the wire may omit it or send null. */
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
      // The one structural union in the schema is DocumentIdentifier, hand-written below.
      if (ctx.endsWith("#docid")) return { fs: "DocumentIdentifier", nullable };
      problems.push(`${ctx}: unsupported union '${node.getText().slice(0, 70)}'`);
      return { fs: "JsonNode", nullable: true };
    }
    case ts.SyntaxKind.TypeLiteral:
      return { fs: "JsonObject", nullable: false, anonymous: node.getText().slice(0, 60) };
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
  problems.push(`${ctx}: unmapped type '${node.getText().slice(0, 70)}' (${ts.SyntaxKind[node.kind]})`);
  return { fs: "JsonNode", nullable: true };
}

/**
 * Fields the schema types as a bare `number` that are really one of the compiler's flag or kind
 * enums, keyed by "<interface>.<wire field>".
 *
 * The schema cannot say so - the Go side serialises every enum as an integer - but upstream's own
 * typed wrapper does, in `dist/api/sync/api.d.ts` and `dist/api/async/types.d.ts`. Each entry
 * below is transcribed from a declaration there; the register in `docs/wire-hand-written.md`
 * names the declaration for each one.
 *
 * Explicit rather than inferred from the field name: `TypeToTypeNodeParams.flags` is a
 * counter-example that a name rule would get wrong, since `typeToTypeNode` and `typeToString`
 * share that one parameter record and upstream types the argument `NodeBuilderFlags` for the
 * first and `TypeFormatFlags` for the second. It stays `int`.
 *
 * The named types are all top-level types in the `Xantham.TypeScript.Wire` namespace, from
 * `Enums.generated.fs` and `Ast.generated.fs`, both of which compile before this file.
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

/**
 * Applies `FIELD_ENUMS` to a mapped field type. Only `int` and `int[]` are retyped: anything else
 * under a listed key means the schema changed shape beneath the table, which is a problem rather
 * than something to retype anyway.
 */
const retypedFields = new Set();
function retype(mapped, key) {
  const enumName = FIELD_ENUMS[key];
  if (!enumName) return mapped;
  retypedFields.add(key);
  if (mapped.fs === "int") return { ...mapped, fs: enumName };
  if (mapped.fs === "int[]") return { ...mapped, fs: `${enumName}[]` };
  problems.push(`${key}: listed in FIELD_ENUMS as ${enumName}, but the schema types it '${mapped.fs}'`);
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

// ── collect declarations ──────────────────────────────────────────────────
const interfaces = [];
const aliases = new Map();
for (const st of src.statements) {
  if (ts.isInterfaceDeclaration(st)) interfaces.push(st);
  else if (ts.isTypeAliasDeclaration(st)) aliases.set(st.name.text, st);
}

// ── enums referenced by the schema ────────────────────────────────────────
const enumImports = new Set();
for (const st of src.statements) {
  if (ts.isImportDeclaration(st) && st.moduleSpecifier.text.startsWith("#enums/")) {
    for (const e of st.importClause?.namedBindings?.elements ?? []) enumImports.add(e.name.text);
  }
}
function readEnum(name) {
  const file = path.join(ENUM_DIR, name[0].toLowerCase() + name.slice(1) + ".js");
  if (!fs.existsSync(file)) {
    problems.push(`enum ${name}: no file at ${file}`);
    return null;
  }
  const text = fs.readFileSync(file, "utf8");
  const re = new RegExp(`${name}\\["([A-Za-z0-9_]+)"\\]\\s*=\\s*(-?\\d+)`, "g");
  const members = [];
  let m;
  while ((m = re.exec(text))) {
    members.push([m[1], m[2]]);
  }
  if (!members.length) problems.push(`enum ${name}: no members parsed`);
  return members;
}

// ── emit ──────────────────────────────────────────────────────────────────
const L = [];
const w = (s = "") => L.push(s);
const emitDoc = (node, indent) => { for (const d of docLines(node)) w(`${indent}/// ${d}`); };

w("namespace Xantham.TypeScript.Wire");
w();
w("// <auto-generated>");
w("//   Generated by tools/proto-gen/generate.mjs from the wire schema shipped in");
w(`//   typescript@${schemaVersion} at dist/api/proto.generated.d.ts.`);
w("//   Do not edit by hand - re-run the generator when the compiler is upgraded.");
w("// </auto-generated>");
w();
w("open System.Text.Json.Nodes");
w("open System.Text.Json.Serialization");
w();

w("/// Enums the wire schema refers to, transcribed from the compiler's own `dist/enums`.");
w("/// Aliases that repeat a value are kept as declared (`ScriptTarget.Latest` beside `ESNext`);");
w("/// F# permits duplicate enum cases, and dropping one would diverge from the compiler's own names.");
w("module ProtoEnums =");
w();
for (const name of [...enumImports].sort()) {
  const members = readEnum(name);
  if (!members) continue;
  w(`    type ${name} =`);
  for (const [k, v] of members) w(`        | ${fsIdent(k)} = ${v}`);
  w();
}

w("/// The schema is not ordered by dependency - `UpdateSnapshotParams` refers to");
w("/// `APIFileChanges`, which is declared far below it - so the module is recursive rather");
w("/// than reordered here. Reordering would diverge from the schema and break the next diff.");
w("module rec Proto =");
w();
w("    open ProtoEnums");
w();

const docIdAlias = aliases.get("DocumentIdentifier");
if (docIdAlias) emitDoc(docIdAlias, "    ");
w("    /// The schema types this as `string | { uri: string }` - either a path or a document URI -");
w("    /// so it is a union here rather than a record with two optional halves. It needs");
w("    /// `DocumentIdentifierConverter` to serialise, which `ProtoJson.options` registers; the");
w("    /// converter cannot be attached here as an attribute because it is declared against this");
w("    /// type and would close the dependency loop.");
w("    type DocumentIdentifier =");
w("        /// A file path, written as a bare JSON string.");
w("        | FileName of fileName: string");
w("        /// A document URI, written as `{ \"uri\": ... }`.");
w("        | Uri of uri: string");
w();

// Schema-untyped payloads that are only ever spliced into, or lifted out of, a larger document.
// Emitting these as raw UTF-8 JSON rather than a JsonNode DOM lets callers reuse
// ProtoJson.serialize and ProtoJson.deserialize unchanged; see RawJson.fs. Both sides of a batch
// entry qualify: the shape of each depends on the sibling method string, which the schema does
// not relate to them. Keyed by "<interface>.<wire field>".
const rawJsonFields = new Set(["BatchRequest.params", "BatchResponse.result"]);

const recordNames = [];
// name -> [{ wire, ident, fs, optional, bare, raw }], kept so the async extensions below can
// rebuild each parameter record from a flat argument list.
const recordFields = new Map();

// A record whose every field is optional gets a `Default` - the empty record, which serialises
// to `{}` because each field carries `WhenWritingDefault`. Without it a caller has to write out
// every field by hand to set one of them, and `CompilerOptions` alone has 110. It is
// `static member val`, so the record is allocated once rather than per read.
// A field's absent form follows how it is emitted above: `voption` for the ordinary case, and
// `null` for the ones typed as a bare `JsonNode`/`JsonObject` or as raw UTF-8 JSON, which are
// nullable reference types rather than value options.
// A field's absent form follows how it is emitted above, and a required field of a defaultable
// record type stands in its own `Default` - which is what makes `CreateProgramOptions`, whose
// `compilerOptions` the schema requires, defaultable in turn.
const absent = field =>
  field.optional ? (field.bare || field.raw ? "null" : "ValueNone") : `${field.fs}.Default`;
// Least fixpoint: a record is defaultable when every field is optional or is itself a
// defaultable record. Starting from nothing and only ever adding means a cycle - two records
// each requiring the other - is never admitted, which is right, since no finite value satisfies
// it.
const defaultable = new Set();
// name -> index in `L` of that record's closing brace.
const recordClose = new Map();
function resolveDefaultable() {
  for (let changed = true; changed; ) {
    changed = false;
    for (const [name, fields] of recordFields) {
      if (defaultable.has(name) || fields.length === 0) continue;
      if (fields.every(f => f.optional || defaultable.has(f.fs))) {
        defaultable.add(name);
        changed = true;
      }
    }
  }
}
// The record is built behind a `Lazy` rather than by `static member val`, and not for laziness:
// static fields of a file initialise in declaration order, the module is in schema order, and a
// record whose default names another's - `CreateProgramOptions` at the top naming
// `CompilerOptions.Default` 300 lines below - would otherwise capture that field before it was
// assigned and hold a silent null. Deferring the body to first read puts it after the whole
// file's initialiser, and the value is still built once.
function defaultLines(name, fields) {
  return [
    `        static member val private DefaultValue = lazy ({`,
    ...fields.map(f => `            ${f.ident} = ${absent(f)}`),
    `        }: ${name})`,
    "",
    "        /// The record with nothing set: every field is either optional or a default of its",
    "        /// own, so this serialises to `{}`. Copy-update it to fill in the fields you mean -",
    `        /// \`{ ${name}.Default with ... }\` - rather than writing all ${fields.length} out.`,
    `        static member Default: ${name} = ${name}.DefaultValue.Value`,
    "",
  ];
}
for (const decl of interfaces) {
  const name = decl.name.text;
  if (name === "APIMethodInfo") continue; // emitted below as the method table
  recordNames.push(name);
  emitDoc(decl, "    ");
  const members = decl.members.filter(ts.isPropertySignature);
  const fields = [];
  recordFields.set(name, fields);
  if (members.length === 0) {
    w(`    type ${name} =`);
    w("        { Unused: unit voption }");
    w();
    w(`        static member Default: ${name} = { Unused = ValueNone }`);
    w();
    defaultable.add(name);
    continue;
  }
  w(`    type ${name} = {`);
  members.forEach((m, i) => {
    const wire = m.name.getText().replace(/^["']|["']$/g, "");
    const isDocId = m.type.getText().includes("DocumentIdentifier");
    const mapped = retype(mapType(m.type, `${name}.${wire}${isDocId ? "#docid" : ""}`), `${name}.${wire}`);
    const optional = !!m.questionToken || mapped.nullable;
    for (const d of docLines(m)) w(`        /// ${d}`);
    if (mapped.literals) w(`        /// One of: ${mapped.literals.map(s => "`" + s + "`").join(", ")}`);
    w(`        [<JsonPropertyName "${wire}">]`);
    if (optional) w("        [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]");
    if (rawJsonFields.has(`${name}.${wire}`)) {
      w("        [<JsonConverter(typeof<RawJsonConverter>)>]");
      w(`        ${fsIdent(pascal(wire))}: byte[]`);
      fields.push({ wire, ident: fsIdent(pascal(wire)), fs: "byte[]", optional, bare: false, raw: true });
      if (i < members.length - 1) w();
      return;
    }
    const bare = mapped.fs === "JsonNode" || mapped.fs === "JsonObject";
    fields.push({ wire, ident: fsIdent(pascal(wire)), fs: mapped.fs, optional, bare, raw: false });
    w(`        ${fsIdent(pascal(wire))}: ${optional && !bare ? `${mapped.fs} voption` : mapped.fs}`);
    if (i < members.length - 1) w();
  });
  recordClose.set(name, L.length);
  w("    }");
  w();
}

// Now that every record's fields are known, work out which are defaultable and splice each
// `Default` back into its own type, reopening the record's closing brace with `with`.
resolveDefaultable();
for (const [name, at] of [...recordClose].sort((a, b) => b[1] - a[1])) {
  if (!defaultable.has(name)) continue;
  L[at] = "    } with";
  L.splice(at + 2, 0, ...defaultLines(name, recordFields.get(name)));
}

// A key that matches nothing is a table entry the schema has moved out from under, and it would
// otherwise go unnoticed as a field quietly staying an `int`.
for (const key of Object.keys(FIELD_ENUMS)) {
  if (!retypedFields.has(key)) problems.push(`FIELD_ENUMS: ${key} matches no field in the schema`);
}

const info = interfaces.find(d => d.name.text === "APIMethodInfo");
const methods = info.members.filter(ts.isPropertySignature).map(m => {
  const wire = m.name.getText().replace(/^["']|["']$/g, "");
  const [p, r] = m.type.typeArguments;
  return {
    wire,
    params: mapType(p, `${wire}.params`),
    result: mapType(r, `${wire}.result`),
    doc: docLines(m),
  };
});

w("    /// Every method the server exposes. The comment on each records the parameter and result");
w("    /// types the schema assigns it; `voption` marks a result the schema permits to be null.");
w("    [<RequireQualifiedAccess>]");
w("    module Method =");
w();
w("        /// The methods whose result is binary rather than JSON - the AST blob.");
w("        ///");
w("        /// The transport returns those bytes raw for a request sent on its own, but a batch");
w("        /// response is JSON and cannot carry them, so inside a batch the same result arrives");
w("        /// as a base64 string instead. `TscMailbox` needs to know which methods that applies");
w("        /// to in order to hand callers the same bytes either way.");
w("        let binaryResultMethods =");
w("            System.Collections.Generic.HashSet<string> [");
for (const { wire, result } of methods) {
  if (result.fs === "SourceFileResponse") w(`                "${wire}"`);
}
w("            ]");
w();
for (const { wire, params, result, doc } of methods) {
  for (const d of doc) w(`        /// ${d}`);
  const p = params.fs === "JsonNode" ? "unit" : params.fs;
  const r = `${result.fs}${result.nullable ? " voption" : ""}`;
  w(`        /// \`${p}\` -> \`${r}\``);
  w("        [<Literal>]");
  w(`        let ${fsIdent(pascal(wire))} = "${wire}"`);
  w();
}

fs.mkdirSync(path.dirname(outFile), { recursive: true });
fs.writeFileSync(outFile, L.join("\n").replace(/\n{3,}/g, "\n\n") + "\n", "utf8");

// ── typed call functions ──────────────────────────────────────────────────
// Emitted separately because they sit on top of ProtoJson, which in turn needs the
// types above: types -> converters -> calls is the only order that compiles.
const A = [];
const a = (s = "") => A.push(s);
a("namespace Xantham.TypeScript.Wire");
a();
a("// <auto-generated>");
a("//   Generated by tools/proto-gen/generate.mjs from the wire schema shipped in");
a(`//   typescript@${schemaVersion} at dist/api/proto.generated.d.ts.`);
a("//   Do not edit by hand - re-run the generator when the compiler is upgraded.");
a("// </auto-generated>");
a();
a("open System.Text.Json.Nodes");
a("open Xantham.TypeScript.Wire.Proto");
a();
a("/// One function per server method, each taking the channel and the method's parameter record.");
a("///");
a("/// Naming follows the wire: `getSymbolAtPosition` is `Api.getSymbolAtPosition`. Results the");
a("/// schema permits to be null are `voption`; the rest raise if the server sends nothing, since");
a("/// a missing field on a request is otherwise indistinguishable from a legitimate empty answer.");
a("[<RequireQualifiedAccess>]");
a("module Api =");
a();

const camel = s => s[0].toLowerCase() + s.slice(1);

// F# reserved words, for escaping camel-cased argument names. Deliberately narrower than
// KEYWORDS above, which also guards pascal-cased record fields and errs on the side of escaping:
// `file` is a fine argument name and reads badly in backticks.
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

/// The extension-member block, emitted for both surfaces from the same method table so the
/// synchronous and asynchronous members cannot drift. Returns the number of methods that got a
/// flattened overload as well as the record one.
function extensions(emit, typeName, receiver, apiModule, list) {
  emit(`/// \`${apiModule}\` as members, so the ${receiver} is not threaded through every call:`);
  emit(`/// \`${receiver}.getSymbolAtPosition parameters\` rather than \`${apiModule}.getSymbolAtPosition ${receiver}\`.`);
  emit("///");
  emit("/// Each method that takes a parameter record gets a second, inlined overload accepting that");
  emit("/// record's fields directly and building it on the caller's behalf. Fields the schema marks");
  emit("/// optional are `[<Struct>]` optional arguments, so they arrive as the `voption` the record");
  emit("/// field already holds and pass straight through. F# requires optional arguments to come last,");
  emit("/// so where a required field follows an optional one the argument order is not the record's");
  emit("/// own. Named arguments sidestep that, and are worth using here regardless: several methods");
  emit("/// take four or more arguments of the same type.");
  emit("[<AutoOpen>]");
  emit(`module ${typeName}Extensions =`);
  emit();
  emit(`    type ${typeName} with`);
  emit();

  let flattened = 0;
  for (const { wire, params, doc } of list) {
    const noParams = params.fs === "JsonNode" || params.fs === "unit";
    const fn = fsIdent(camel(wire));
    for (const d of doc) emit(`        /// ${d}`);
    if (noParams) {
      emit(`        member this.${fn}() = ${apiModule}.${fn} this`);
      emit();
      continue;
    }
    emit(`        member this.${fn}(parameters: ${params.fs}) = ${apiModule}.${fn} this parameters`);
    emit();

    const fields = recordFields.get(params.fs);
    // Nothing to spread, and a raw JSON field has no sensible flat argument - both cases keep the
    // record overload on its own.
    if (!fields || fields.length === 0 || fields.some(f => f.raw)) continue;
    flattened++;

    const ordered = [...fields.filter(f => !f.optional), ...fields.filter(f => f.optional)];
    // [<Struct>] makes an optional argument voption rather than option, which is what the record
    // field already is - so for all but the bare JsonNode fields the argument goes straight in with
    // no conversion at all.
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

    emit(`        /// The fields of ${params.fs}, spread. Builds the record and calls the overload above.`);
    emit(`        member inline this.${fn}(${args.join(", ")}) =`);
    emit(`            this.${fn}(`);
    emit(`                { ${assignments.join("\n                  ")} }`);
    emit(`                : ${params.fs}`);
    emit("            )");
    emit();
  }
  return flattened;
}

let binaryCount = 0, unitParamCount = 0;
for (const { wire, params, result, doc } of methods) {
  for (const d of doc) a(`    /// ${d}`);
  const noParams = params.fs === "JsonNode" || params.fs === "unit";
  const isAst = result.fs === "SourceFileResponse";
  const isVoid = result.fs === "unit";
  if (isAst) binaryCount++;
  if (noParams) unitParamCount++;

  if (isAst) {
    a("    /// Returns the raw binary AST, not the schema's `SourceFileResponse` JSON envelope -");
    a("    /// see `ProtoJson.requestAst`.");
  }
  const fn = fsIdent(camel(wire));
  if (noParams) {
    if (isVoid) {
      a(`    let ${fn} (channel: TscChannel) =`);
      a(`        channel.Request(Method.${pascal(wire)}, "null") |> ignore`);
    } else {
      a(`    let ${fn} (channel: TscChannel) : ${result.fs}${result.nullable ? " voption" : ""} =`);
      const call = `ProtoJson.requestNoParams<${result.fs}> channel Method.${pascal(wire)}`;
      a(result.nullable
        ? `        ${call}`
        : `        match ${call} with\n        | ValueSome result -> result\n        | ValueNone -> failwith "${wire} returned no result, but the schema declares one"`);
    }
  } else if (isAst) {
    a(`    let ${fn} (channel: TscChannel) (parameters: ${params.fs}) =`);
    a(`        ProtoJson.requestAst channel Method.${pascal(wire)} parameters`);
  } else if (isVoid) {
    a(`    let ${fn} (channel: TscChannel) (parameters: ${params.fs}) =`);
    a(`        ProtoJson.requestUnit channel Method.${pascal(wire)} parameters`);
  } else {
    const helper = result.nullable ? "requestOption" : "request";
    a(`    let ${fn} (channel: TscChannel) (parameters: ${params.fs}) : ${result.fs}${result.nullable ? " voption" : ""} =`);
    a(`        ProtoJson.${helper}<${params.fs}, ${result.fs}> channel Method.${pascal(wire)} parameters`);
  }
  a();
}

const apiFlattened = extensions(a, "TscChannel", "channel", "Api", methods);

const apiFile = path.join(path.dirname(outFile), "ProtoApi.generated.fs");
fs.writeFileSync(apiFile, A.join("\n").replace(/\n{3,}/g, "\n\n") + "\n", "utf8");
console.log(`api      ${apiFile} (${methods.length} functions, ${binaryCount} binary, ${unitParamCount} without parameters, ${apiFlattened} with a flattened overload)`);

// ── async surface over the mailbox ────────────────────────────────────────
// The mirror of Api, over TscMailbox rather than TscChannel, emitted from the same method table
// so the two cannot drift. Extension members come with it, so the mailbox does not have to be
// threaded through every call.


const M = [];
const m = (s = "") => M.push(s);
m("namespace Xantham.TypeScript.Wire");
m();
m("// <auto-generated>");
m("//   Generated by tools/proto-gen/generate.mjs from the wire schema shipped in");
m(`//   typescript@${schemaVersion} at dist/api/proto.generated.d.ts.`);
m("//   Do not edit by hand - re-run the generator when the compiler is upgraded.");
m("// </auto-generated>");
m();
m("open System.Text.Json.Nodes");
m("open Xantham.TypeScript.Wire.Proto");
m();
m("/// One function per server method, each taking the mailbox and the method's parameter record.");
m("///");
m("/// The mirror of `Api` over `TscMailbox` rather than `TscChannel`: same names, same parameter");
m("/// and result types, each wrapped in `Async`. Calls that overlap in time are collected into a");
m("/// single round trip, so concurrency here is worth taking even though the channel underneath");
m("/// admits one request at a time.");
m("///");
m("/// `batchRequests` has no counterpart: the mailbox is the batcher, and a batch nested inside");
m("/// one would return a result this layer cannot attribute to a caller.");
m("[<RequireQualifiedAccess>]");
m("module AsyncApi =");
m();

const asyncMethods = methods.filter(x => x.wire !== "batchRequests");

for (const { wire, params, result, doc } of asyncMethods) {
  for (const d of doc) m(`    /// ${d}`);
  const noParams = params.fs === "JsonNode" || params.fs === "unit";
  const isAst = result.fs === "SourceFileResponse";
  const isVoid = result.fs === "unit";
  const fn = fsIdent(camel(wire));
  const method = `Method.${pascal(wire)}`;

  if (isAst) {
    m("    /// Returns the decoded binary AST, not the schema's `SourceFileResponse` JSON envelope.");
    m("    /// The mailbox normalises the two encodings the transport uses for it, so this is the");
    m("    /// same result whether the request travelled alone or inside a batch.");
  }
  if (noParams) {
    if (isVoid) {
      m(`    let ${fn} (mailbox: TscMailbox) : Async<unit> =`);
      m(`        mailbox.RequestUnitNoParams ${method}`);
    } else {
      const member = result.nullable ? "RequestNoParamsOption" : "RequestNoParams";
      m(`    let ${fn} (mailbox: TscMailbox) : Async<${result.fs}${result.nullable ? " voption" : ""}> =`);
      m(`        mailbox.${member}<${result.fs}> ${method}`);
    }
  } else if (isAst) {
    m(`    let ${fn} (mailbox: TscMailbox) (parameters: ${params.fs}) : Async<Ast.SourceFile voption> =`);
    m(`        mailbox.RequestAst(${method}, parameters)`);
  } else if (isVoid) {
    m(`    let ${fn} (mailbox: TscMailbox) (parameters: ${params.fs}) : Async<unit> =`);
    m(`        mailbox.RequestUnit(${method}, parameters)`);
  } else {
    const member = result.nullable ? "RequestOption" : "Request";
    m(`    let ${fn} (mailbox: TscMailbox) (parameters: ${params.fs}) : Async<${result.fs}${result.nullable ? " voption" : ""}> =`);
    m(`        mailbox.${member}<${params.fs}, ${result.fs}>(${method}, parameters)`);
  }
  m();
}

const flattened = extensions(m, "TscMailbox", "mailbox", "AsyncApi", asyncMethods);

const asyncFile = path.join(path.dirname(outFile), "ProtoAsync.generated.fs");
fs.writeFileSync(asyncFile, M.join("\n").replace(/\n{3,}/g, "\n\n") + "\n", "utf8");
console.log(`async    ${asyncFile} (${asyncMethods.length} functions, ${flattened} with a flattened overload)`);

console.log(`schema   typescript@${schemaVersion}`);
console.log(`records  ${recordNames.length}`);
console.log(`methods  ${methods.length}`);
console.log(`enums    ${enumImports.size}`);
console.log(`out      ${outFile}`);
if (problems.length) {
  console.error(`\nPROBLEMS (${problems.length}):`);
  for (const p of problems) console.error("  " + p);
  process.exit(1);
}
console.log("no problems");
