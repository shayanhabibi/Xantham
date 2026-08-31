/**
 * Reads the vendored `ast.json` through the vendored `schema.ts` - the same `SchemaAPI` the
 * upstream Go and TS generators are written against - and emits the SyntaxKind surface as F#.
 *
 * Emits the kind surface (the enum, the section markers, the kind guards) and the child-slot
 * surface (per node type, the ordered slots that a node's `Children` mask is a bitmap over).
 *
 * `upstream/tools/scripts/tsc/generate-ts-ast.ts` is the reference for the guard families and
 * `generate-go-ast.ts` for the numbering rule.
 *
 * Two files, because the second one needs `Ast` from `Library.fs` and `Library.fs` needs the
 * first:
 *   - `Ast.generated.fs`       kinds and slot numbers, no dependencies
 *   - `AstNode.generated.fs`   named child accessors, over `Ast`
 *
 *   node tools/tsc-ast/generate-ast.mts <out-dir>
 */
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { api, type NodeType } from "./upstream/tools/scripts/tsc/schema.ts";
import { fsIdent, header, Lines, needsEscape, xml } from "./fsharp.mjs";
import { ANCHORS, kindValues } from "./kinds.mts";
import { readEnums, render, syntaxKindOracle, type EnumType } from "./enums.mts";
import { sourceFileRecord } from "./record.mts";

const here = path.dirname(fileURLToPath(import.meta.url));

const [, , outDir] = process.argv;
if (!outDir) {
  console.error("usage: generate-ast.mts <out-dir>");
  process.exit(2);
}

/** Anything the emitter could not map faithfully; a non-empty list fails the run. */
const problems: string[] = [];

api.validate();

// ────────────────────────────────────────────────────────────────────────────
// Model
// ────────────────────────────────────────────────────────────────────────────

const elements = api.kindElements();
const values = kindValues();

const markers = api.kindMarkers().map(marker => {
  const target = api.resolveKindMarkerValue(marker.name);
  if (!values.has(target)) problems.push(`marker ${marker.name} resolves to unknown kind ${target}`);
  return { name: marker.name, target };
});

const guards = api.kindGuards();

/** The one layout here that `ast.json` does not describe; see `record.mts`. */
const record = sourceFileRecord(problems);

/**
 * A node's encoded child members, in slot order.
 *
 * The filter is `generate-encoder.ts`'s `isEncodedChild` and the order is its `childProps`
 * (`generate-encoder.ts:285`), which is what `getNodeChildMask` shifts against
 * (`generate-encoder.ts:427-444`) - bit `i` is `childProps[i]`. Dropping `noTS`/`noGo` members
 * is not cosmetic: seven function-like nodes declare a `FullSignature` child that the encoder
 * skips, and it sits *mid-list*, so filtering on `isChild()` alone would shift every slot after
 * it.
 */
function childSlots(node: NodeType) {
  return node.members.filter(member => member.isChild() && !member.noTS && !member.noGo);
}

// ── The data word's commonData bits ─────────────────────────────────────────
//
// Ported from `generate-encoder.ts`, which is the only statement of this layout. Every
// predicate below mirrors one there by name; the citations are the point of the exercise,
// since a filter that drifts by one member shifts every bit after it and nothing in the blob
// would say so.

/** `generate-encoder.ts:125-129`. */
function resolveUnion(type: any): any {
  if (type.kind === "union") return type;
  if (type.kind === "alias") return resolveUnion(type.resolved);
  return undefined;
}

/** `generate-encoder.ts:117-122`. */
function isSyntaxKindUnion(member: MemberInfo) {
  const resolved = resolveUnion(member.declaredType);
  return !!resolved && resolved.kind === "union" && resolved.types.every((t: any) => t.kind === "kind");
}

/** `generate-encoder.ts:1075-1079`. */
function unionKinds(member: MemberInfo): { name: string }[] {
  const resolved = resolveUnion(member.declaredType);
  if (!resolved || resolved.kind !== "union") return [];
  return resolved.types.filter((t: any) => t.kind === "kind");
}

/** `generate-encoder.ts:132-138`. Optional unions spend an extra index on "absent". */
function unionBitWidth(member: MemberInfo) {
  const count = unionKinds(member).length + (member.optional ? 1 : 0);
  return Math.ceil(Math.log2(count));
}

/**
 * `generate-encoder.ts:86-93`. NodeFlags members are already in the node's own `flags` word
 * and are not repeated in commonData - so they are skipped, and skipping them is what keeps
 * the bit positions aligned.
 */
function isNodeFlagsMember(member: MemberInfo) {
  const declared: any = member.declaredType;
  return (declared.kind === "primitive" && declared.name === "NodeFlags") ||
    (!!member.bitmask && String(member.bitmask).startsWith("NodeFlags"));
}

/** Non-child, non-kind-parameter members, in declaration order. `generate-encoder.ts:284`. */
function dataMembers(node: NodeType) {
  return node.members.filter(member =>
    !member.noTS && !member.noGo && !member.isChild() && !member.isKindParam());
}

/** `generate-encoder.ts:147-159`. */
function needsHandWrittenCommonData(node: NodeType) {
  if (node.handWritten) return false;
  return dataMembers(node).some(member => {
    const declared: any = member.declaredType;
    if (declared.kind === "primitive" && (declared.name === "bool" || declared.name === "string")) return false;
    if (isSyntaxKindUnion(member) || isNodeFlagsMember(member)) return false;
    return true;
  });
}

/** `generate-encoder.ts:183-198`. */
function dataTypeOf(node: NodeType): "string" | "children" | "extended" {
  if (node.handWritten) return "extended";
  const strings = dataMembers(node).filter(member => {
    const declared: any = member.declaredType;
    return declared.kind === "primitive" && declared.name === "string";
  });
  if (strings.length > 1) return "extended";
  if (strings.length === 1) return needsHandWrittenCommonData(node) ? "extended" : "string";
  return "children";
}

/**
 * The commonData bit layout: bools one bit each in member order, then the SyntaxKind unions.
 * `generate-encoder.ts:318-331`, with positions relative to bit 24.
 */
function commonDataLayout(node: NodeType) {
  if (needsHandWrittenCommonData(node)) return [];
  const members = dataMembers(node);
  const bools = members.filter(member => {
    const declared: any = member.declaredType;
    return declared.kind === "primitive" && declared.name === "bool";
  });
  const unions = members.filter(member => isSyntaxKindUnion(member));

  const layout: { member: MemberInfo; bit: number; width: number }[] = [];
  let bit = 0;
  for (const member of bools) layout.push({ member, bit: bit++, width: 1 });
  for (const member of unions) {
    const width = unionBitWidth(member);
    layout.push({ member, bit, width });
    bit += width;
  }
  return layout;
}

// Only six bits are available (24-29; 30-31 are the data type). A wider layout would be
// truncated silently.
for (const node of api.nodes()) {
  const layout = commonDataLayout(node);
  const width = layout.reduce((total, entry) => total + entry.width, 0);
  if (width > 6) {
    problems.push(`node ${node.name} wants ${width} commonData bits, more than the 6 available`);
  }
}

// Nodes whose commonData bits are written by a Go function we do not vendor. The literals are
// on this list too but are excluded: their data word is an extended-data offset, and their text
// and flags come out of that record instead (see `Ast.text`). That leaves one node, for which we
// emit nothing - and if a second ever appears, that silence stops being harmless.
const HAND_WRITTEN_COMMON_DATA = ["SyntheticExpression"];
const handWrittenCommonData = api.nodes()
  .filter(node => needsHandWrittenCommonData(node) && dataTypeOf(node) !== "extended")
  .map(node => node.name);
if (handWrittenCommonData.join() !== HAND_WRITTEN_COMMON_DATA.join()) {
  problems.push(
    `nodes with hand-written commonData are now [${handWrittenCommonData}], expected ` +
      `[${HAND_WRITTEN_COMMON_DATA}] - their bits are not derivable from the schema`,
  );
}

// ── Node aliases ────────────────────────────────────────────────────────────
//
// `ast.json` names 73 unions of node types - `Expression`, `Statement`, `TypeNode` - and they
// are the declared type of most typed members. The generator emits guards for the 34 *kind*
// aliases only, so until now nothing could ask "is this node an Expression?" without spelling
// out fifty-five kinds. These resolve each node alias to the set of kinds its members can have.

/** Concrete nodes that transitively extend `base`, by base key. */
const nodesUnderBase = new Map<string, NodeType[]>();
{
  const inherits = (node: NodeType, target: string): boolean =>
    node.name === target || node.extends.some(base => inherits(base, target));

  for (const base of api.bases()) {
    nodesUnderBase.set(base.key, api.nodes().filter(node => inherits(node, base.key)));
  }
}

/**
 * `Token` and `KeywordExpression` are generic over their kind, and the schema names each
 * instantiation - `QuestionToken` is `Token<SyntaxKind.QuestionToken>`, `ThisExpression` is
 * `KeywordExpression<SyntaxKind.ThisKeyword>`. The type argument is what makes them distinct;
 * resolving one to its node and taking that node's kinds would hand back every token there is.
 */
const instantiationKinds = new Map<string, string[]>();
for (const node of api.nodes()) {
  for (const { name, typeArg } of node.instantiationAliases) {
    instantiationKinds.set(
      name,
      api.hasKindAlias(typeArg)
        ? api.expandKindAliasMembers(typeArg).map(kind => kind.name)
        : [typeArg],
    );
  }
}

/**
 * Narrowings that exist only in upstream's hand-written `ast.ts` and so have no schema entry.
 * They are narrower than any kind can express - a `JsxTagNamePropertyAccess` is a
 * `PropertyAccessExpression` whose expression is itself a tag name - so a kind guard admits the
 * whole kind and says so. Listed explicitly, because a new one appearing should be a decision
 * rather than a silent widening.
 */
const HAND_WRITTEN_NARROWINGS: Record<string, string[]> = {
  JsxTagNamePropertyAccess: ["PropertyAccessExpression"],
};

/**
 * The kinds a value of `type` can have.
 *
 * The schema's own resolver already turns kind aliases and instantiation aliases into node
 * types when asked for `resolveAs: "node"`, so the cases here are the ones it leaves: a node
 * type (concrete or a base), a union, and an alias wrapping either. An unresolvable type is a
 * problem rather than an empty set - a silently empty guard would answer `false` forever.
 */
function kindsOfType(type: any, context: string): string[] {
  switch (type.kind) {
    case "node": {
      const node: NodeType = type;
      if (node.isConcrete) return node.allKinds().map(kind => kind.name);
      const under = nodesUnderBase.get(node.name);
      if (!under) {
        problems.push(`${context}: ${node.name} is neither a concrete node nor a known base`);
        return [];
      }
      return under.flatMap(member => member.allKinds().map(kind => kind.name));
    }
    case "alias": {
      const instantiated = instantiationKinds.get(type.name);
      if (instantiated) return instantiated;
      return kindsOfType(type.resolved, context);
    }
    case "union":
      return type.types.flatMap((member: any) => kindsOfType(member, context));
    case "kind":
      return [type.name];
    case "primitive":
      // The schema falls back to a primitive for any name it cannot resolve, which here means
      // either a bare syntax kind (`ObjectBindingPattern` is a kind, not a node - `BindingPattern`
      // owns it) or a narrowing hand-written in upstream's `ast.ts`
      // (`generate-ts-ast.ts:455-460`).
      if (values.has(type.name)) return [type.name];
      if (type.name in HAND_WRITTEN_NARROWINGS) return HAND_WRITTEN_NARROWINGS[type.name];
      problems.push(`${context}: ${type.name} is neither a kind nor a known hand-written type`);
      return [];
    default:
      problems.push(`${context}: cannot resolve ${type.kind} to a set of kinds`);
      return [];
  }
}

/**
 * Each node alias, as a guard: the kinds it names directly, plus the nested aliases it defers
 * to. Nesting is kept rather than flattened for the same reason the kind guards keep it - the
 * emitted code then has the shape of the schema, and `isExpression` reads as a list of the
 * things an expression is.
 */
const nodeAliasGuards = api.nodeAliases().map(alias => {
  const nested: string[] = [];
  const direct: string[] = [];

  if (alias.isBaseAlias) {
    direct.push(...kindsOfType(alias.resolved, `node alias ${alias.name}`));
  } else {
    for (const [index, member] of alias.unionMemberNames.entries()) {
      if (member in api.schema.nodes.aliases) {
        nested.push(member);
        continue;
      }
      direct.push(...kindsOfType(alias.unionMemberTypes[index], `node alias ${alias.name}.${member}`));
    }
  }

  return { name: alias.name, guardName: `is${alias.name}`, nested, direct: [...new Set(direct)] };
});

/** Every kind a node alias admits, nested aliases included. Used by the checks below. */
const aliasKinds = new Map<string, Set<string>>();
{
  const byName = new Map(nodeAliasGuards.map(guard => [guard.name, guard]));
  const resolve = (name: string, stack: string[]): Set<string> => {
    const cached = aliasKinds.get(name);
    if (cached) return cached;
    if (stack.includes(name)) {
      problems.push(`node aliases form a cycle: ${[...stack, name].join(" -> ")}`);
      return new Set();
    }
    const guard = byName.get(name)!;
    const kinds = new Set(guard.direct);
    for (const member of guard.nested) {
      for (const kind of resolve(member, [...stack, name])) kinds.add(kind);
    }
    aliasKinds.set(name, kinds);
    return kinds;
  };
  for (const guard of nodeAliasGuards) resolve(guard.name, []);
}

for (const guard of nodeAliasGuards) {
  const kinds = aliasKinds.get(guard.name)!;
  if (kinds.size === 0) problems.push(`node alias ${guard.name} admits no kinds`);
  for (const kind of guard.direct) {
    if (!values.has(kind)) problems.push(`node alias ${guard.name} names unknown kind ${kind}`);
  }
  if (guards.some(kindGuard => kindGuard.guardName === guard.guardName)) {
    problems.push(`node alias guard ${guard.guardName} collides with a kind alias guard`);
  }
}

/** Nested-first ordering, since F# resolves names top to bottom. */
function sortNodeAliasGuards() {
  const byName = new Map(nodeAliasGuards.map(guard => [guard.name, guard]));
  const emitted = new Set<string>();
  const ordered: typeof nodeAliasGuards = [];

  const visit = (guard: (typeof nodeAliasGuards)[number]) => {
    if (emitted.has(guard.name)) return;
    emitted.add(guard.name);
    for (const member of guard.nested) {
      const nested = byName.get(member);
      if (nested) visit(nested);
    }
    ordered.push(guard);
  };

  for (const guard of nodeAliasGuards) visit(guard);
  return ordered;
}

// ── The typed layer's tags ──────────────────────────────────────────────────
//
// A tag is an interface that nothing implements: it exists to be the type argument of
// `Node<'Tag>`, so that a `Node<Identifier>` and a `Node<Statement>` are different types over
// the same two fields. Tags inherit each other exactly when one's kinds are a subset of the
// other's, which is what makes `'Tag :> Expression` mean "this node is an expression" - the
// same claim the alias guards above test at runtime.

/** Every tag, with the kinds a node carrying it can have. */
const tags = new Map<string, { kinds: Set<string>; order: number; sort: "node" | "alias" | "token" | "any" }>();

/** The tag every other tag inherits, and the type of a member the schema does not narrow. */
const ANY_TAG = "AnyNode";

{
  let order = 0;
  tags.set(ANY_TAG, { kinds: new Set(values.keys()), order: order++, sort: "any" });
  for (const guard of nodeAliasGuards) {
    tags.set(guard.name, { kinds: aliasKinds.get(guard.name)!, order: order++, sort: "alias" });
  }
  for (const [name, kinds] of instantiationKinds) {
    tags.set(name, { kinds: new Set(kinds), order: order++, sort: "token" });
  }
  for (const node of api.nodes()) {
    if (tags.has(node.name)) {
      problems.push(`tag ${node.name} is claimed by both a node definition and an alias`);
      continue;
    }
    tags.set(node.name, { kinds: new Set(node.allKinds().map(kind => kind.name)), order: order++, sort: "node" });
  }
}

const isSubset = (inner: Set<string>, outer: Set<string>) => [...inner].every(kind => outer.has(kind));

/**
 * The tags a tag can widen to, before reduction.
 *
 * Four groups of aliases have identical kind sets - `BlockOrExpression` and `ConciseBody` are
 * the same set of kinds under two names - and each would inherit the other. The tie is broken
 * by declaration order, so the first-declared is the supertype and widening runs towards it;
 * the two are interchangeable anyway, since nothing at runtime can tell them apart.
 */
function supertagsOf(name: string) {
  const tag = tags.get(name)!;
  return [...tags]
    .filter(([other, candidate]) =>
      other !== name &&
      (candidate.sort === "alias" || candidate.sort === "any") &&
      isSubset(tag.kinds, candidate.kinds) &&
      (candidate.kinds.size !== tag.kinds.size || candidate.order < tag.order))
    .map(([other]) => other);
}

const supertags = new Map([...tags.keys()].map(name => [name, supertagsOf(name)]));

/** Direct supertags only: an inherited one is dropped, since F# gets it transitively. */
const inherits = new Map(
  [...supertags].map(([name, supers]) => [
    name,
    supers.filter(candidate => !supers.some(other => other !== candidate && supertags.get(other)!.includes(candidate))),
  ]),
);

/**
 * Declaration order for the tags. A supertag always has at least as many kinds as its subtags,
 * and ties were broken by declaration order above, so this is a topological order and F#'s
 * strict top-to-bottom name resolution is satisfied without a recursive type group.
 */
const tagOrder = [...tags.keys()].sort((left, right) => {
  const a = tags.get(left)!;
  const b = tags.get(right)!;
  return b.kinds.size - a.kinds.size || a.order - b.order;
});

/**
 * The tag for a member's declared type, or `AnyNode` where the schema does not narrow it.
 *
 * The fallback covers the eight members typed as an inline union and the six typed as a bare
 * `Node`; both are genuinely "some node" in the schema, so widening them is not a loss.
 */
function tagForType(type: any, context: string): string {
  switch (type.kind) {
    case "node":
      if (tags.has(type.name)) return type.name;
      // `Node` is the schema's own "any node", and resolves as a node type rather than as a
      // primitive; six members are declared that way.
      if (type.name === "Node") return ANY_TAG;
      // A base, e.g. `TypeNodeBase`. The alias over that base is the tag, if one exists.
      for (const alias of api.nodeAliases()) {
        if (alias.baseKey === type.name) return alias.name;
      }
      problems.push(`${context}: base ${type.name} has no alias to use as a tag`);
      return ANY_TAG;
    case "alias":
      if (tags.has(type.name)) return type.name;
      return tagForType(type.resolved, context);
    case "union":
      return ANY_TAG;
    case "primitive":
      if (tags.has(type.name)) return type.name;
      if (type.name === "Node") return ANY_TAG;
      if (type.name in HAND_WRITTEN_NARROWINGS) {
        const kinds = HAND_WRITTEN_NARROWINGS[type.name];
        return kinds.length === 1 && tags.has(kinds[0]) ? kinds[0] : ANY_TAG;
      }
      problems.push(`${context}: ${type.name} has no tag`);
      return ANY_TAG;
    default:
      problems.push(`${context}: cannot give ${type.kind} a tag`);
      return ANY_TAG;
  }
}

/** Nodes that own at least one slot, and so get a module in `Slot`. */
const slotNodes = api.nodes().filter(node => childSlots(node).length > 0);

/** Nodes with anything to read - a child slot or a commonData bit - and so an `AstNode` module. */
const accessorNodes = api.nodes().filter(node =>
  childSlots(node).length > 0 || commonDataLayout(node).length > 0);

/**
 * Nodes that carry text - through the data word or through an extended record. `Ast.text`
 * covers both, and these are the kinds it answers for, so these are the nodes whose typed
 * module gets a `text`. Several of them have no slots and no commonData at all, `Identifier`
 * among them, so they have no `AstNode` module either.
 */
const textNodes = new Set(
  api.nodes()
    .filter(node => !node.handWritten && dataTypeOf(node) !== "children")
    .map(node => node.name),
);

/** Kinds whose extended record carries a raw text and a template flags word. */
const TEMPLATE_FRAGMENT_KINDS = ["TemplateHead", "TemplateMiddle", "TemplateTail"];

/** Kinds whose extended record carries `TokenFlags` - `Ast.tokenFlags`. */
const LITERAL_FLAG_KINDS = ["StringLiteral", "NumericLiteral", "BigIntLiteral", "RegularExpressionLiteral"];

/** Nodes with a typed module: anything with a slot, a packed member, or text. */
const typedNodes = api.nodes().filter(node =>
  childSlots(node).length > 0 || commonDataLayout(node).length > 0 || textNodes.has(node.name));

// The mask is 8 bits wide (`Ast.ChildMask`, `Library.fs`), so a wider node would silently lose
// its last slots.
for (const node of api.nodes()) {
  const slots = childSlots(node);
  if (slots.length > 8) {
    problems.push(`node ${node.name} declares ${slots.length} child slots, more than the 8-bit mask holds`);
  }
}

/**
 * Slot layouts are per node, but the blob only carries a kind, and 29 kinds are claimed by more
 * than one node (`TrueKeyword` is both a `Token` and a `KeywordExpression`). That is only safe
 * because the claimants agree on their slots; upstream relies on the same thing when it writes
 * one `childProperties` row per kind (`generate-encoder.ts:1031-1040`).
 */
const layoutForKind = new Map<string, { node: NodeType; slots: string[] }>();
for (const node of slotNodes) {
  const slots = childSlots(node).map(member => api.uncapitalize(member.name));
  for (const kind of node.allKinds()) {
    const existing = layoutForKind.get(kind.name);
    if (existing && existing.slots.join() !== slots.join()) {
      problems.push(
        `kind ${kind.name} has two child layouts: ${existing.node.name} says ` +
          `[${existing.slots}] and ${node.name} says [${slots}]`,
      );
      continue;
    }
    if (!existing) layoutForKind.set(kind.name, { node, slots });
  }
}

/** The guard function name for a nested kind alias. */
function guardNameFor(aliasName: string) {
  const guard = guards.find(candidate => candidate.aliasName === aliasName);
  if (!guard) {
    problems.push(`no guard generated for nested alias ${aliasName}`);
    return `is${aliasName}`;
  }
  return fsIdent(guard.guardName);
}

/**
 * A guard whose alias nests another alias calls that alias's guard, so it has to be emitted
 * after the ones it calls - F# resolves names strictly top to bottom.
 */
function sortGuards() {
  const byAlias = new Map(guards.map(guard => [guard.aliasName, guard]));
  const emitted = new Set<string>();
  const ordered: typeof guards = [];

  const visit = (guard: (typeof guards)[number], stack: string[]) => {
    if (emitted.has(guard.aliasName)) return;
    if (stack.includes(guard.aliasName)) {
      problems.push(`kind guards form a cycle: ${[...stack, guard.aliasName].join(" -> ")}`);
      return;
    }
    if (guard.type === "enumerated") {
      for (const member of guard.members) {
        const nested = byAlias.get(member);
        if (nested) visit(nested, [...stack, guard.aliasName]);
      }
    }
    if (emitted.has(guard.aliasName)) return;
    emitted.add(guard.aliasName);
    ordered.push(guard);
  };

  for (const guard of guards) visit(guard, []);
  return ordered;
}

/** Escapes an identifier, and reports it: a backticked name is a change to the public surface. */
function ident(name: string, context: string) {
  if (needsEscape(name)) {
    problems.push(`${context}: ${name} collides with an F# keyword and was escaped`);
  }
  return fsIdent(name);
}

// ────────────────────────────────────────────────────────────────────────────
// Emission
// ────────────────────────────────────────────────────────────────────────────

const lock = JSON.parse(fs.readFileSync(path.join(here, "upstream.lock.json"), "utf8"));
const out = new Lines();

out.w(...header({
  namespace: "Xantham.TypeScript.Wire",
  generator: "tools/tsc-ast/generate-ast.mts",
  repo: lock.repo,
  ref: lock.ref,
  source: "tools/scripts/tsc/ast.json and tsc/internal/api/encoder/encoder.go",
}));
out.blank();

// The slot numbers and record offsets below carry their roles, so that `Ast` cannot pass a
// byte offset where a slot belongs. `Measures.fs` is the first file in the project.
out.w("open Xantham.TypeScript.Wire.Measures");
out.blank();

// Section comments are carried through so that a diff of this file lines up with a diff of the
// schema it came from.
out.doc("A node's kind, as encoded in the `kind` word of a binary AST blob.");
out.doc("");
out.doc("Values are positional in the schema, so they move whenever the compiler inserts a");
out.doc("kind. They are not the values the JavaScript compiler API uses.");
out.doc("");
out.doc("The backing type is `uint32` so that the enum maps onto the blob's `kind` word");
out.doc("without a conversion, and so that `Ast.KindNodeList` (`0xFFFFFFFF`) is representable.");
out.w("type SyntaxKind =");
out.indent(w => {
  for (const element of elements) {
    if (!element.name) {
      w.blank();
      w.w(`// ${element.comment}`);
      continue;
    }
    const trailing = element.comment ? ` // ${element.comment}` : "";
    w.w(`| ${ident(element.name, "kind element")} = ${values.get(element.name)}u${trailing}`);
  }
});
out.blank();

out.w("[<RequireQualifiedAccess>]");
out.w("module AstKind =");
out.blank();
out.indent(w => {
  w.doc("How many kinds the schema declares. The compiler's `KindCount`.");
  w.w("[<Literal>]");
  w.w(`let Count = ${values.size}`);
  w.blank();

  w.doc("Section boundaries the schema declares. The range guards below are written in terms");
  w.doc("of these rather than in terms of literal ordinals.");
  w.w("module Marker =");
  w.blank();
  w.indent(m => {
    for (const marker of markers) {
      m.w("[<Literal>]");
      m.w(`let ${ident(marker.name, "kind marker")} = SyntaxKind.${fsIdent(marker.target)}`);
      m.blank();
    }
  });

  for (const guard of sortGuards()) {
    w.doc(`True when the kind is one of the schema's \`${guard.aliasName}\`.`);
    const name = ident(guard.guardName, "kind guard");

    if (guard.type === "range") {
      w.w(`let inline ${name} (kind: SyntaxKind) =`);
      w.indent(g =>
        g.w(`kind >= Marker.${fsIdent(guard.first)} && kind <= Marker.${fsIdent(guard.last)}`)
      );
    } else {
      // Members that are themselves aliases delegate to that alias's guard rather than being
      // flattened, so the emitted code keeps the schema's structure.
      const nested = guard.members.filter(member => api.hasKindAlias(member));
      const direct = guard.members.filter(member => !api.hasKindAlias(member));
      for (const member of direct) {
        if (!values.has(member)) {
          problems.push(`guard ${guard.guardName} names unknown kind ${member}`);
        }
      }

      w.w(`let ${name} (kind: SyntaxKind) =`);
      w.indent(g => {
        if (direct.length) {
          g.w("match kind with");
          const cases = direct.map(member => `SyntaxKind.${fsIdent(member)}`);
          // Long alternation lists wrap; four to a line keeps them readable.
          for (let index = 0; index < cases.length; index += 4) {
            g.w(`| ${cases.slice(index, index + 4).join(" | ")}`);
          }
          g.w("    -> true");
          g.w(nested.length ? "| _ ->" : "| _ -> false");
        }
        if (nested.length) {
          const call = nested.map(member => `${guardNameFor(member)} kind`).join(" || ");
          g.w(direct.length ? `    ${call}` : call);
        }
      });
    }
    w.blank();
  }

  w.doc("Guards for the schema's *node* aliases - the unions `ast.json` declares over node")
  w.doc("types, as opposed to the kind aliases above. A member declared as an `Expression`")
  w.doc("holds any of these kinds, and these are what the typed layer's views are written on.")
  w.blank()

  for (const guard of sortNodeAliasGuards()) {
    w.doc(`True when the kind is one of the schema's \`${guard.name}\` nodes.`);
    const name = ident(guard.guardName, "node alias guard");

    w.w(`let ${name} (kind: SyntaxKind) =`);
    w.indent(g => {
      const cases = guard.direct.map(kind => `SyntaxKind.${fsIdent(kind)}`);
      if (cases.length) {
        g.w("match kind with");
        for (let index = 0; index < cases.length; index += 4) {
          g.w(`| ${cases.slice(index, index + 4).join(" | ")}`);
        }
        g.w("    -> true");
        g.w(guard.nested.length ? "| _ ->" : "| _ -> false");
      }
      if (guard.nested.length) {
        const call = guard.nested.map(member => `is${fsIdent(member)} kind`).join(" || ");
        g.w(cases.length ? `    ${call}` : call);
      }
    });
    w.blank();
  }

  const textKinds = (want: "string" | "extended") =>
    api.nodes()
      .filter(node => dataTypeOf(node) === want && !node.handWritten)
      .flatMap(node => node.allKinds().map(kind => kind.name));

  // `SourceFile` is an extended-data node too, but its record is hand-written in the compiler's
  // own client and is not described anywhere we vendor, so it is not a text kind here.
  const emitKindSet = (name: string, doc: string[], kinds: string[]) => {
    for (const line of doc) w.doc(line);
    w.w(`let ${name} (kind: SyntaxKind) =`);
    w.indent(k => {
      k.w("match kind with");
      const cases = kinds.map(kind => `SyntaxKind.${fsIdent(kind)}`);
      for (let index = 0; index < cases.length; index += 4) {
        k.w(`| ${cases.slice(index, index + 4).join(" | ")}`);
      }
      k.w("    -> true");
      k.w("| _ -> false");
    });
    w.blank();
  };

  emitKindSet("hasStringText", [
    "True when the node's data word is an index into the string table, so its text is read",
    "straight from there. `Ast.text` handles both this and `hasExtendedText`.",
  ], textKinds("string"));

  emitKindSet("hasExtendedText", [
    "True when the node's text lives in its extended-data record rather than in the data word.",
    "",
    "`SourceFile` also carries extended data, but its record is hand-written in the compiler's",
    "client rather than described by the schema, so it is not included.",
  ], textKinds("extended"));

  w.doc("The kind's schema name. A table rather than `ToString()`, which reflects.");
  w.w("let name (kind: SyntaxKind) =");
  w.indent(n => {
    n.w("match kind with");
    for (const kind of values.keys()) n.w(`| SyntaxKind.${fsIdent(kind)} -> "${kind}"`);
    n.w('| kind -> $"SyntaxKind(%d{int kind})"');
  });
});

out.blank();

out.doc("A node's child slots: the bit positions of its `Children` mask, and the `order`");
out.doc("argument to `Ast.childAtOrder`.");
out.doc("");
out.doc("Slots are declared per node type rather than per kind because that is how the schema");
out.doc("declares them, and a node type is what you have in hand when you write the call.");
out.w("[<RequireQualifiedAccess>]");
out.w("module internal Slot =");
out.blank();
out.indent(w => {
  for (const node of slotNodes) {
    const slots = childSlots(node);
    const kinds = node.allKinds();

    w.doc(
      kinds.length === 1
        ? `Slots of \`SyntaxKind.${kinds[0].name}\`.`
        : `Slots of \`${node.name}\`, which is ${kinds.length} kinds (\`${kinds[0].name}\` and others).`,
    );
    w.w("[<RequireQualifiedAccess>]");
    w.w(`module ${ident(node.name, "node")} =`);
    w.blank();
    w.indent(n => {
      slots.forEach((member, order) => {
        const optional = member.optional ? " Optional." : "";
        const list = member.listKind ? ` A ${member.listKind === "ModifierList" ? "modifier list" : "node list"}: the child is a list node whose own children are the elements.` : "";
        n.doc(`Slot ${order}.${optional}${list}`);
        n.w("[<Literal>]");
        n.w(`let ${ident(api.capitalize(member.name), `${node.name} slot`)} = ${order}<astSlot>`);
        n.blank();
      });

      n.doc("The slot names in slot order, matching the encoder's `childProperties`.");
      n.w(`let Names = [| ${slots.map(member => `"${api.uncapitalize(member.name)}"`).join("; ")} |]`);
    });
    w.blank();
  }

  w.doc("The kind's child slot names, in slot order; empty when the kind has no children.");
  w.doc("");
  w.doc("The F# mirror of the encoder's `childProperties` table, so `Array.findIndex` over it");
  w.doc("gives the same `order` the TypeScript client's `getNamedChild` uses.");
  w.w("let names (kind: SyntaxKind) =");
  w.indent(n => {
    n.w("match kind with");
    // One case per kind, but the arrays are shared with the node modules rather than
    // reallocated, so this stays a lookup rather than a constructor.
    for (const node of slotNodes) {
      const kinds = node.allKinds().filter(kind => layoutForKind.get(kind.name)?.node === node);
      if (!kinds.length) continue;
      const cases = kinds.map(kind => `SyntaxKind.${fsIdent(kind.name)}`);
      for (let index = 0; index < cases.length; index += 4) {
        n.w(`| ${cases.slice(index, index + 4).join(" | ")}`);
      }
      n.w(`    -> ${ident(node.name, "node")}.Names`);
    }
    n.w("| _ -> Array.empty");
  });
});

// ────────────────────────────────────────────────────────────────────────────
// Emission: the SourceFile extended-data record, into Ast.generated.fs
// ────────────────────────────────────────────────────────────────────────────

out.blank();
out.doc("Byte offsets into the `SourceFile` node's extended-data record.");
out.doc("");
out.doc("Unlike everything else here, this layout is not in `ast.json`. The record is written by");
out.doc("a hand-written Go function and read by hand-written TypeScript, so the only statement of");
out.doc("its shape is the format documentation in `encoder.go`, which the generator parses. Every");
out.doc("field is a `uint32`; `Ast` in `Library.fs` is what gives them meaning.");
out.w("[<RequireQualifiedAccess>]");
out.w("module SourceFileRecord =");
out.blank();
out.indent(w => {
  w.doc("The record's total size in bytes.");
  w.w("[<Literal>]");
  w.w(`let Size = ${record.size}`);
  w.blank();

  w.doc("The value a structured-data offset or string index carries when the field is absent.");
  w.w("[<Literal>]");
  w.w(`let Absent = ${record.noStructuredData}u`);

  for (const field of record.fields) {
    w.blank();
    // The table's own wording, verbatim - it is the authority, and paraphrasing it here would
    // hide an upstream change that the parser deliberately fails on.
    w.doc(`${field.doc}.`);
    w.w("[<Literal>]");
    w.w(`let ${field.name} = ${field.offset}<byteOffset>`);
  }
});

// ────────────────────────────────────────────────────────────────────────────
// Emission: AstNode.generated.fs
// ────────────────────────────────────────────────────────────────────────────

const nodesOut = new Lines();

nodesOut.w(...header({
  namespace: "Xantham.TypeScript.Wire",
  generator: "tools/tsc-ast/generate-ast.mts",
  repo: lock.repo,
  ref: lock.ref,
  source: "tools/scripts/tsc/ast.json",
}));
nodesOut.blank();

nodesOut.doc("Named child access, one module per node type.");
nodesOut.doc("");
nodesOut.doc("These are `Ast.childAtOrder` with the slot filled in, so they are as unchecked as it");
nodesOut.doc("is: reading `IfStatement.thenStatement` off a node that is not an `IfStatement` returns");
nodesOut.doc("whatever sits in slot 1 of whatever the node actually is. Call `is` first when the kind");
nodesOut.doc("is not already known.");
nodesOut.w("[<RequireQualifiedAccess>]");
nodesOut.w("module internal AstNode =");
nodesOut.blank();
nodesOut.indent(w => {
  for (const node of accessorNodes) {
    const slots = childSlots(node);
    const kinds = node.allKinds();
    const slotModule = `Slot.${ident(node.name, "node")}`;

    w.doc(slots.length ? `Children of \`${node.name}\`.` : `Data of \`${node.name}\`, which has no children.`);
    w.w("[<RequireQualifiedAccess>]");
    w.w(`module ${ident(node.name, "node")} =`);
    w.blank();
    w.indent(n => {
      n.doc(`True when \`node\` is a \`${node.name}\`.`);
      n.w("let is (file: Ast.SourceFile) (node: int) =");
      n.indent(g => {
        if (kinds.length === 1) {
          g.w(`Ast.kind file node = SyntaxKind.${fsIdent(kinds[0].name)}`);
          return;
        }
        g.w("match Ast.kind file node with");
        const cases = kinds.map(kind => `SyntaxKind.${fsIdent(kind.name)}`);
        for (let index = 0; index < cases.length; index += 4) {
          g.w(`| ${cases.slice(index, index + 4).join(" | ")}`);
        }
        g.w("    -> true");
        g.w("| _ -> false");
      });
      n.blank();

      slots.forEach((member, order) => {
        const slot = `${slotModule}.${ident(api.capitalize(member.name), `${node.name} slot`)}`;
        const name = fsIdent(api.uncapitalize(member.name));
        const optional = member.optional ? " The schema marks it optional." : "";

        if (member.listKind) {
          const listName = fsIdent(api.uncapitalize(member.name) + "List");
          n.doc(`The \`${api.uncapitalize(member.name)}\` list node itself.${optional}`);
          n.w(`let ${listName} (file: Ast.SourceFile) (node: int) =`);
          n.indent(g => g.w(`Ast.childAtOrder file node ${slot}`));
          n.blank();
          n.doc(`The elements of \`${api.uncapitalize(member.name)}\`, empty when the slot is absent.`);
          n.w(`let ${name} (file: Ast.SourceFile) (node: int) =`);
          n.indent(g => {
            g.w(`match ${listName} file node with`);
            g.w("| ValueSome list -> Ast.children file list");
            g.w("| ValueNone -> Seq.empty");
          });
        } else {
          n.doc(`Slot ${order}, \`${api.uncapitalize(member.name)}\`.${optional}`);
          n.w(`let ${name} (file: Ast.SourceFile) (node: int) =`);
          n.indent(g => g.w(`Ast.childAtOrder file node ${slot}`));
        }
        n.blank();
      });

      for (const { member, bit, width } of commonDataLayout(node)) {
        const name = fsIdent(api.uncapitalize(member.name));

        if (!isSyntaxKindUnion(member)) {
          n.doc(`\`${api.uncapitalize(member.name)}\`, commonData bit ${bit}.`);
          n.w(`let ${name} (file: Ast.SourceFile) (node: int) =`);
          n.indent(g => g.w(`Ast.commonData file node &&& ${1 << bit}u <> 0u`));
          n.blank();
          continue;
        }

        // Index-encoded: `generate-encoder.ts:489-510`. Optional spends index 0 on "absent",
        // otherwise index 0 is the first value and is what an unset field decodes to.
        const kinds = unionKinds(member);
        const mask = (1 << width) - 1;
        n.doc(`\`${api.uncapitalize(member.name)}\`, commonData bits ${bit}-${bit + width - 1}.`);
        n.w(`let ${name} (file: Ast.SourceFile) (node: int) =`);
        n.indent(g => {
          const bits = bit === 0 ? "Ast.commonData file node" : `(Ast.commonData file node >>> ${bit})`;
          g.w(`match ${bits} &&& ${mask}u with`);
          kinds.forEach((kind, index) => {
            const value = member.optional ? index + 1 : index;
            g.w(`| ${value}u -> ValueSome SyntaxKind.${fsIdent(kind.name)}`);
          });
          g.w("| _ -> ValueNone");
        });
        n.blank();
      }
    });
    w.blank();
  }
});

// ────────────────────────────────────────────────────────────────────────────
// Self-checks
// ────────────────────────────────────────────────────────────────────────────

// The numbering rule is the whole reason this file can be trusted, and nothing downstream would
// announce it changing: 307 and 79 are hard-coded in the live tests today.
for (const [kind, expected] of ANCHORS) {
  const actual = values.get(kind);
  if (actual !== expected) {
    problems.push(`SyntaxKind.${kind} came out as ${actual}, expected ${expected}`);
  }
}

// And now it does not have to be trusted. `syntaxKind.enum.ts` is upstream's own published
// enum, generated from `kind_generated.go`, vendored beside the flag enums. Every kind and
// every marker is checked against it, which turns the four anchors above from the evidence
// into a fast smoke test.
{
  const oracle = syntaxKindOracle();
  for (const [kind, ours] of values) {
    if (!oracle.has(kind)) problems.push(`SyntaxKind.${kind} is not in the published enum`);
    else if (oracle.get(kind) !== ours) {
      problems.push(`SyntaxKind.${kind} came out as ${ours}, upstream publishes ${oracle.get(kind)}`);
    }
  }
  for (const marker of markers) {
    const ours = values.get(marker.target);
    if (oracle.has(marker.name) && oracle.get(marker.name) !== ours) {
      problems.push(`Marker.${marker.name} came out as ${ours}, upstream publishes ${oracle.get(marker.name)}`);
    }
  }
  const unchecked = [...oracle.keys()].filter(name => !values.has(name) && !markers.some(m => m.name === name));
  if (unchecked.length !== 1 || unchecked[0] !== "Count") {
    problems.push(`published SyntaxKind has entries that are neither a kind nor a marker: ${unchecked.join(", ")}`);
  }
}

// The slot order has the same property, and `generate-encoder.ts:1819-1824` writes this exact
// layout out longhand in a worked example - the one place upstream states it in prose.
const METHOD_DECLARATION_SLOTS =
  "modifiers,asteriskToken,name,postfixToken,typeParameters,parameters,type,body";
const methodDeclaration = layoutForKind.get("MethodDeclaration");
if (methodDeclaration?.slots.join(",") !== METHOD_DECLARATION_SLOTS) {
  problems.push(
    `MethodDeclaration slots came out as [${methodDeclaration?.slots}], expected ` +
      `[${METHOD_DECLARATION_SLOTS}]`,
  );
}

// The record layout is parsed out of prose, so it gets the same treatment as the numbering
// rule: a value stated here that upstream has moved fails the run rather than shifting reads.
const SOURCE_FILE_RECORD_SIZE = 76;
if (record.size !== SOURCE_FILE_RECORD_SIZE) {
  problems.push(
    `the SourceFile extended-data record came out ${record.size} bytes, expected ` +
      `${SOURCE_FILE_RECORD_SIZE}`,
  );
}

// ────────────────────────────────────────────────────────────────────────────
// Emission: the typed layer
// ────────────────────────────────────────────────────────────────────────────

const typedOut = new Lines();

typedOut.w(...header({
  namespace: "Xantham.TypeScript.Wire",
  generator: "tools/tsc-ast/generate-ast.mts",
  repo: lock.repo,
  ref: lock.ref,
  source: "tools/scripts/tsc/ast.json",
}));
typedOut.blank();

typedOut.doc("A typed view over the binary AST: `Node<'Tag>` instead of a bare `int`.");
typedOut.doc("");
typedOut.doc("Three things are generated here, all from `ast.json`:");
typedOut.doc("");
typedOut.doc("  - **Tags**, one per node type, per node alias and per token instantiation. A tag is");
typedOut.doc("    an interface that nothing ever implements; it exists so that `Node<Identifier>` and");
typedOut.doc("    `Node<Statement>` are different types over the same two fields. Tag inheritance is");
typedOut.doc("    kind-set inclusion, so `'Tag :> Expression` is the compile-time form of the runtime");
typedOut.doc("    claim `AstKind.isExpression`.");
typedOut.doc("  - **Accessors**, one module per node type, returning `Node<'Tag>` at the type the");
typedOut.doc("    schema declares for the slot. Unlike `AstNode`, these cannot be pointed at the");
typedOut.doc("    wrong kind of node, because the argument is typed.");
typedOut.doc("  - **Views**, in `Patterns`, narrowing any node to a tag by testing its kind. They are");
typedOut.doc("    `[<return: Struct>]` partial active patterns rather than a union, so a match costs a");
typedOut.doc("    kind read and a two-word copy and allocates nothing.");
typedOut.doc("");
typedOut.doc("Widening is `<Alias>.ofNode`, one function per alias: F# rejects a constraint whose");
typedOut.doc("right-hand side is a type variable, so a single generic `widen` is not expressible.");
typedOut.blank();

// ── Tags ────────────────────────────────────────────────────────────────────

for (const name of tagOrder) {
  const tag = tags.get(name)!;
  const supers = inherits.get(name)!;

  if (name === ANY_TAG) {
    typedOut.doc("The tag every other tag inherits, and the type of a slot the schema does not narrow -");
    typedOut.doc("a member declared as a bare `Node` or as an inline union. Narrow it with a view.");
  } else if (tag.sort === "alias") {
    typedOut.doc(`The schema's \`${name}\` alias, over ${tag.kinds.size} kind${tag.kinds.size === 1 ? "" : "s"}.`);
  } else if (tag.sort === "token") {
    typedOut.doc(`\`${name}\`, a token type the schema names as an instantiation.`);
  } else {
    typedOut.doc(`The node type \`${name}\`.`);
  }

  typedOut.w(`type ${ident(name, "tag")} =`);
  typedOut.indent(t => {
    t.w("interface");
    t.indent(i => {
      for (const parent of supers) i.w(`inherit ${fsIdent(parent)}`);
    });
    t.w("end");
  });
  typedOut.blank();
}

// ── Node<'Tag> ──────────────────────────────────────────────────────────────

typedOut.doc("A node in a decoded source file, tagged with what the schema says it is.");
typedOut.doc("");
typedOut.doc("A struct over the blob and an index, so it is the same two words a raw `int` walk");
typedOut.doc("carries plus the blob it was already holding. The tag is erased at runtime.");
typedOut.w("[<Struct; NoEquality; NoComparison>]");
typedOut.w("type Node<'Tag when 'Tag :> AnyNode> =");
typedOut.indent(w => {
  w.w("internal { File: Ast.SourceFile; Index: int }");
  w.blank();
  w.doc("The node's kind, read from the blob. Tags are erased, so this is the only thing that");
  w.doc("knows what the node actually is.");
  w.w("member this.Kind = Ast.kind this.File this.Index");
  w.blank();
  w.doc("Start of the node including leading trivia, in UTF-16 code units.");
  w.w("member this.Pos = Ast.pos this.File this.Index");
  w.blank();
  w.doc("End of the node, in UTF-16 code units.");
  w.w("member this.End = Ast.endPos this.File this.Index");
  w.blank();
  w.doc("The node's text, cooked rather than as spelled, for the kinds that carry any.");
  w.w("member this.Text = Ast.text this.File this.Index");
});
typedOut.blank();

typedOut.doc("The escape hatch, and the operations that hold for every node whatever its tag.");
typedOut.w("[<RequireQualifiedAccess>]");
typedOut.w("module Node =");
typedOut.blank();
typedOut.indent(w => {
  w.doc("Tags a raw node index. The index is not checked against the tag - `ofIndex` is the");
  w.doc("door back in from a raw walk, and the caller is asserting what it found.");
  w.w("let ofIndex<'Tag when 'Tag :> AnyNode> (file: Ast.SourceFile) (index: int) : Node<'Tag> =");
  w.indent(g => g.w("{ File = file; Index = index }"));
  w.blank();

  w.doc("The raw node index, for dropping down to `Ast` and `AstNode`.");
  w.w("let index (node: Node<'Tag>) = node.Index");
  w.blank();

  w.doc("The decoded file the node belongs to.");
  w.w("let file (node: Node<'Tag>) = node.File");
  w.blank();

  w.doc("Re-tags a node without checking it. The generated views and widenings are the intended");
  w.doc("callers; it cannot produce an invalid node, only a wrong claim about one.");
  w.w("let retag<'From, 'To when 'From :> AnyNode and 'To :> AnyNode> (node: Node<'From>) : Node<'To> =");
  w.indent(g => g.w("{ File = node.File; Index = node.Index }"));
  w.blank();

  w.doc("The file's root node.");
  w.w("let root (file: Ast.SourceFile) : Node<SourceFile> = { File = file; Index = Ast.Root }");
  w.blank();

  w.doc("True when both refer to the same node of the same file, whatever their tags.");
  w.w("let sameAs (other: Node<'Other>) (node: Node<'Tag>) =");
  w.indent(g => g.w("node.Index = other.Index && obj.ReferenceEquals(node.File.Data, other.File.Data)"));
  w.blank();

  w.doc("The node's parent, or `ValueNone` at the root.");
  w.w("let parent (node: Node<'Tag>) : Node<AnyNode> voption =");
  w.indent(g => {
    g.w("match Ast.parent node.File node.Index with");
    g.w("| 0 -> ValueNone");
    g.w("| index -> ValueSome { File = node.File; Index = index }");
  });
  w.blank();

  w.doc("The node's direct children, in blob order, untagged.");
  w.w("let children (node: Node<'Tag>) : Node<AnyNode> seq =");
  w.indent(g => g.w("Ast.children node.File node.Index |> Seq.map (fun index -> { File = node.File; Index = index })"));
  w.blank();

  w.doc("Every node under this one, untagged. Narrow with a view.");
  w.w("let descendants (node: Node<'Tag>) : Node<AnyNode> seq =");
  w.indent(g => g.w("Ast.descendants node.File node.Index |> Seq.map (fun index -> { File = node.File; Index = index })"));
});
typedOut.blank();

// ── Widening ────────────────────────────────────────────────────────────────

typedOut.doc("Widening, one module per alias. Each is the identity at runtime; the point is that");
typedOut.doc("the constraint only admits tags the schema says belong to the alias.");
typedOut.w("[<AutoOpen>]");
typedOut.w("module Widenings =");
typedOut.blank();
typedOut.indent(w => {
  for (const guard of nodeAliasGuards) {
    w.doc(`Widens any \`${guard.name}\` to \`Node<${guard.name}>\`.`);
    w.w("[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]");
    w.w(`module ${ident(guard.name, "widening module")} =`);
    w.blank();
    w.indent(m => {
      m.w(`let inline ofNode<'Tag when 'Tag :> ${fsIdent(guard.name)}> (node: Node<'Tag>) : Node<${fsIdent(guard.name)}> =`);
      m.indent(g => g.w("Node.retag node"));
    });
    w.blank();
  }
});

// ── Accessors ───────────────────────────────────────────────────────────────

typedOut.doc("Typed child and data access, one module per node type.");
typedOut.doc("");
typedOut.doc("These are the `AstNode` accessors with the argument and the result typed, which is");
typedOut.doc("the whole difference: `AstNode.IfStatement.thenStatement` will read slot 1 of anything");
typedOut.doc("you hand it, and `IfStatement.thenStatement` will not compile unless you hand it one.");
typedOut.w("[<AutoOpen>]");
typedOut.w("module Accessors =");
typedOut.blank();
typedOut.indent(w => {
  for (const node of typedNodes) {
    const slots = childSlots(node);
    const tagName = ident(node.name, "accessor module");

    w.doc(slots.length ? `Children of \`${node.name}\`.` : `Data of \`${node.name}\`, which has no children.`);
    w.w("[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]");
    w.w(`module ${tagName} =`);
    w.blank();
    w.indent(n => {
      for (const member of slots) {
        const raw = `AstNode.${tagName}.${fsIdent(api.uncapitalize(member.name))}`;
        const childTag = fsIdent(tagForType(member.declaredType, `${node.name}.${member.name}`));
        const name = fsIdent(api.uncapitalize(member.name));
        const optional = member.optional ? " The schema marks it optional." : "";

        if (member.listKind) {
          n.doc(`The elements of \`${api.uncapitalize(member.name)}\`, empty when the slot is absent.${optional}`);
          n.w(`let ${name} (node: Node<${tagName}>) : Node<${childTag}> seq =`);
          n.indent(g =>
            g.w(`${raw} node.File node.Index |> Seq.map (Node.ofIndex node.File)`)
          );
          n.blank();
          continue;
        }

        n.doc(`\`${api.uncapitalize(member.name)}\`.${optional}`);
        n.w(`let ${name} (node: Node<${tagName}>) : Node<${childTag}> voption =`);
        n.indent(g => {
          g.w(`match ${raw} node.File node.Index with`);
          g.w("| ValueSome child -> ValueSome(Node.ofIndex node.File child)");
          g.w("| ValueNone -> ValueNone");
        });
        n.blank();
      }

      // Packed members keep their own types; only the node argument changes.
      for (const { member } of commonDataLayout(node)) {
        const name = fsIdent(api.uncapitalize(member.name));
        n.doc(`\`${api.uncapitalize(member.name)}\`, out of the data word's commonData bits.`);
        n.w(`let ${name} (node: Node<${tagName}>) =`);
        n.indent(g => g.w(`AstNode.${tagName}.${name} node.File node.Index`));
        n.blank();
      }

      // Text is not a slot and not a commonData bit - it is the data word itself, or the first
      // word of the extended record - so it has no `AstNode` accessor to wrap. `Ast` decides
      // which of the two applies from the kind; the typed layer only decides who gets to ask.
      const kinds = new Set(node.allKinds().map(kind => kind.name));
      const owns = (names: string[]) => names.some(name => kinds.has(name));

      if (textNodes.has(node.name)) {
        n.doc(`The \`${node.name}\`'s text, cooked rather than as spelled in the source.`);
        n.w(`let text (node: Node<${tagName}>) = Ast.text node.File node.Index`);
        n.blank();
      }

      if (owns(TEMPLATE_FRAGMENT_KINDS)) {
        n.doc("The fragment's raw, unescaped source text.");
        n.w(`let rawText (node: Node<${tagName}>) = Ast.rawText node.File node.Index`);
        n.blank();
        n.doc("The fragment's `TokenFlags`, which sit past its raw text in the record.");
        n.w(`let templateFlags (node: Node<${tagName}>) = Ast.templateFlags node.File node.Index`);
        n.blank();
      }

      if (owns(LITERAL_FLAG_KINDS)) {
        n.doc("The literal's `TokenFlags` - the only way back to how it was spelled.");
        n.w(`let tokenFlags (node: Node<${tagName}>) = Ast.tokenFlags node.File node.Index`);
        n.blank();
      }
    });
    w.blank();
  }
});

// ── Views ───────────────────────────────────────────────────────────────────

typedOut.doc("Views: narrowing a node of any tag to a narrower one, by testing its kind.");
typedOut.doc("");
typedOut.doc("Not auto-opened, because a few hundred active patterns in scope by default would");
typedOut.doc("shadow more than they are worth. `open Xantham.TypeScript.Wire.Patterns` where you");
typedOut.doc("match.");
typedOut.doc("");
typedOut.doc("`[<return: Struct>]` is what makes these free: a total union view would allocate a");
typedOut.doc("`Choice` per match, and these allocate nothing at all.");
typedOut.w("module Patterns =");
typedOut.blank();
typedOut.indent(w => {
  const view = (name: string, test: string, doc: string) => {
    w.doc(doc);
    w.w("[<return: Struct>]");
    w.w(`let inline (|${fsIdent(name)}|_|) (node: Node<'Tag>) : Node<${fsIdent(name)}> voption =`);
    w.indent(g => g.w(`if ${test} then ValueSome(Node.retag node) else ValueNone`));
    w.blank();
  };

  for (const node of api.nodes()) {
    const kinds = node.allKinds();
    const test = kinds.length === 1
      ? `node.Kind = SyntaxKind.${fsIdent(kinds[0].name)}`
      : `(${kinds.map(kind => `node.Kind = SyntaxKind.${fsIdent(kind.name)}`).join(" || ")})`;
    view(node.name, test, `Narrows to \`${node.name}\`.`);
  }

  for (const guard of nodeAliasGuards) {
    view(guard.name, `AstKind.${fsIdent(guard.guardName)} node.Kind`, `Narrows to the \`${guard.name}\` alias.`);
  }
});

// ────────────────────────────────────────────────────────────────────────────
// Enums.generated.fs
// ────────────────────────────────────────────────────────────────────────────

const enums = readEnums();

/**
 * A flag set is one that declares bits, i.e. two or more members spelled `1 << n`. Syntactic
 * rather than numeric on purpose: `SymbolFlags.All` is `(1 << 30) - 1` and
 * `ExportDoesNotSupportDefaultModifier` is a `~`, so neither is a power of two and a rule that
 * looked only at values would call the largest flag set in the file an ordinal enum.
 *
 * Detected rather than listed, so an upstream enum changing character is picked up instead of
 * mislabelled - `SpanMapFeature` is a bitfield whose name does not say so, and `[<Flags>]` on an
 * ordinal enum makes `ToString` invent nonsense like `Standard, JSX`.
 */
const isFlagSet = (type: EnumType) => type.members.filter(member => member.bit).length > 1;

const enumsOut = new Lines();

enumsOut.w(...header({
  namespace: "Xantham.TypeScript.Wire",
  generator: "tools/tsc-ast/generate-ast.mts",
  repo: lock.repo,
  ref: lock.ref,
  source: "packages/typescript/src/enums",
}));
enumsOut.blank();

for (const line of [
  "The compiler's own flag and kind enums.",
  "",
  "These are the words the checker answers with - `SymbolResponse.Flags` is a `SymbolFlags`,",
  "`TypeResponse.Flags` is a `TypeFlags` - and the two `Ast` reads straight off a node,",
  "`NodeFlags` and `TokenFlags`.",
  "",
  "Each enum arrives in two halves. The cases are the bits upstream defines, spelled as it spells",
  "them (`1u <<< 23`, not `8388608u`); the companion module holds the members upstream builds by",
  "combining those bits, as `[<Literal>]` values computed the same way. `SymbolFlags.Property` and",
  "`SymbolFlags.Value` both still resolve, so which half a name landed in does not matter to a",
  "caller, and a literal is still usable as a match pattern.",
  "",
  "The split is forced: an enum case may not name another case of its own enum. A composite that",
  "mixes in a bare integer - `SymbolFlags.All = (1 << 30) - 1` - has no enum type to be written at",
  "and stays a case, carrying upstream's spelling as a comment.",
  "",
  "Duplicate values are kept rather than dropped, and F# permits them: `ObjectFlags` reuses its",
  "high bits across disjoint categories of type, so eight of its bits carry two or three names.",
  "`ToString` reports the first name declared at a value, which is upstream order.",
]) enumsOut.w(line ? `// ${line}` : "//");
enumsOut.blank();

for (const type of enums) {
  enumsOut.doc(`\`${type.name}\`, from \`${type.origin}\` upstream.`);

  if (type.isString) {
    // A string-valued enum is not an enum in F#. It emits as literals, which are still usable in
    // a pattern match - the reason `[<Literal>]` is worth the noise over plain bindings.
    enumsOut.w("[<RequireQualifiedAccess>]", `module ${fsIdent(type.name)} =`);
    enumsOut.indent(w => {
      for (const member of type.members) {
        w.blank();
        w.w("[<Literal>]", `let ${fsIdent(member.name)} = ${JSON.stringify(member.value)}`);
      }
    });
    enumsOut.blank();
    continue;
  }

  // An enum case may not name another case of its own enum, so a composite cannot be spelled as
  // the thing it is. Composites move to a companion module instead, as `[<Literal>]` values
  // computed from the cases - `SymbolFlags.Property` and `SymbolFlags.Variable` both still
  // resolve, so the split is invisible to callers. A composite that mixes a bare integer in has
  // no enum type to be rendered at, and stays a case with its evaluated value.
  const cases = new Set(type.members.filter(member => !member.composite).map(member => member.name));
  const literals = new Set<string>();
  const rendered = new Map<string, string>();

  for (const member of type.members) {
    if (!member.composite || !member.tree) continue;
    const fsharp = render(member.tree, name =>
      literals.has(name) ? fsIdent(name) : `${fsIdent(type.name)}.${fsIdent(name)}`);
    if (fsharp === null) {
      cases.add(member.name);
      continue;
    }
    literals.add(member.name);
    rendered.set(member.name, fsharp);
  }

  if (isFlagSet(type)) enumsOut.w("[<System.Flags>]");
  enumsOut.w(`type ${fsIdent(type.name)} =`);
  enumsOut.indent(w => {
    for (const member of type.members) {
      if (!cases.has(member.name)) continue;
      // Bits keep upstream's shift: `JSDocPublic = (1u <<< 23)` says what `8388608u` does not,
      // and F# folds it to the same constant.
      const shift = member.bit ? /^1 << (\d+)$/.exec(member.expression) : null;
      // Upstream's spelling, for the cases where the emitted value is not it. No `<returns>`:
      // the number is on the very next line.
      if (!shift && member.expression !== String(member.value))
        w.w(`/// <summary><code>${xml(member.expression)}</code></summary>`);
      w.w(`| ${fsIdent(member.name)} = ${shift ? `(1u <<< ${shift[1]})` : `${member.value}u`}`);
    }
  });
  enumsOut.blank();

  if (literals.size === 0) continue;

  enumsOut.doc(
    `Composite \`${type.name}\` values, computed from the cases above rather than written out.`,
    "",
    "They live here because an enum case may not name another case of its own enum. Callers see",
    "no difference - both halves answer to the same prefix - and `[<Literal>]` keeps them usable",
    "as match patterns.");
  enumsOut.w(
    "[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]",
    `module ${fsIdent(type.name)} =`);
  enumsOut.indent(w => {
    for (const member of type.members) {
      if (!literals.has(member.name)) continue;
      w.blank();
      // Upstream's own spelling, which is shorter than the parenthesised form below it, and the
      // value it evaluates to - which the F# expression deliberately does not state, and which
      // `Enums.test.fs` reads back out of the assembly to check the two against each other.
      w.w(`/// <summary><code>${xml(member.expression)}</code></summary>`
        + `<returns><c>${member.value}u</c></returns>`);
      w.w("[<Literal>]", `let ${fsIdent(member.name)} = ${rendered.get(member.name)}`);
    }
  });
  enumsOut.blank();
}

const write = (name: string, lines: Lines) => {
  const file = path.join(outDir, name);
  fs.mkdirSync(path.dirname(path.resolve(file)), { recursive: true });
  fs.writeFileSync(file, lines.render());
  return file;
};

console.log(`out     ${write("Enums.generated.fs", enumsOut)}`);
console.log(`out     ${write("Ast.generated.fs", out)}`);
console.log(`out     ${write("AstNode.generated.fs", nodesOut)}`);
console.log(`out     ${write("Typed.generated.fs", typedOut)}`);

console.log(`kinds   ${values.size}`);
console.log(`enums   ${enums.length} over ${enums.reduce((total, type) => total + type.members.length, 0)} members (${enums.filter(isFlagSet).length} flag sets)`);
console.log(`markers ${markers.length}`);
console.log(`guards  ${guards.length} (${guards.filter(guard => guard.type === "range").length} range)`);
console.log(`aliases ${nodeAliasGuards.length} node aliases over ${[...new Set([...aliasKinds.values()].flatMap(kinds => [...kinds]))].length} distinct kinds`);
console.log(`slots   ${slotNodes.reduce((total, node) => total + childSlots(node).length, 0)} over ${slotNodes.length} nodes`);
console.log(`record  ${record.fields.length} SourceFile fields over ${record.size} bytes`);
console.log(`tags    ${tags.size} over ${[...inherits.values()].reduce((total, supers) => total + supers.length, 0)} inheritances`);
console.log(`data    ${api.nodes().reduce((total, node) => total + commonDataLayout(node).length, 0)} commonData members over ${accessorNodes.length} accessor modules`);

if (problems.length) {
  console.error(`\nPROBLEMS (${problems.length}):`);
  for (const problem of problems) console.error(`  ${problem}`);
  process.exit(1);
}
