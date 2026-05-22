# Xantham.TypeScript

This is the 'harness' for utilising the TypeScript compiler and providing a more focused experience for creating
extensions and exposing other internal methods/functions from the TypeScript compiler.

## Proofs

Several wrappers in this project perform operations the F# type system cannot make safe on its own:

- **`failwith` guards** in the `Source` / `ExternalModule` constructors (e.g. "a source file marked as an
  external module had no symbol"),
- **`.Value` field access** on `option`-typed package.json fields and on Fable DU payloads, and
- the **`Ignore` fall-through** in `XanTagKind.Create` and the `ContainsKey` predicates
  (`IsTopLevelStatementKind`, `IsMemberDeclarationKind`, …).

Each of these is sound only because of an *invariant* that holds for real-world TypeScript declarations. Those
invariants are not assumed — they are **proven** as executable tests in
[`tests/Xantham.TypeScript.Tests/Program.fs`](../../tests/Xantham.TypeScript.Tests/Program.fs), run with:

```bash
npm run test:typescript
```

Every proof carries a **stable ID** (`SF-n`, `XTK-n`). Wrapper XML docs cite these IDs, so when a proof fails you
can jump straight from the broken invariant to the wrapper that relied on it (and vice versa).

### Corpus

Each proof is asserted over a corpus of real, published `.d.ts` packages (one full proof series per file):

`@cloudflare/ai-chat`, `@cloudflare/think`, `@cloudflare/dynamic-workflows`, `@cloudflare/workers-types`,
`@cloudflare/sandbox`, `@cloudflare/shell`, `@cloudflare/puppeteer`, `@cloudflare/voice`, `agents`,
`@types/three`, `solid-js`.

These were chosen to span the constructs that stress the wrappers: ambient modules, global augmentation, deep
namespaces, heavy generics, mapped/conditional types, and mixed script/module files. Adding a fixture that breaks
a proof is the intended way to discover an unsound wrapper path.

### SF · Source File Model

Invariants the TypeScript compiler guarantees about source files, which the `Source` model depends on.

| ID | Invariant | Backs |
|----|-----------|-------|
| SF-1  | The program under test contains source files (the corpus is non-empty). | quantification base for all proofs |
| SF-2  | A source file has a checker symbol **iff** it is an external module. | `ExternalModule.create` symbol guard |
| SF-3  | An external-module symbol resolves and exposes ≥1 module specifier. | `ExternalModule.create` `moduleSpecifierInvariant` (`NonEmptyArray`) |
| SF-4  | A script (non-module) source file has a `locals` map. | `Script` `Locals` |
| SF-5  | A script source file has **no** `exports` map (distinguishes script from module). | `SourceKind` discrimination |
| SF-6  | An external module has an `exports` map. | `ExternalModule.create` `symbolExports` guard |
| SF-7  | An external module also has a `locals` map. | `ExternalModule.create` `sourceFileLocals` |
| SF-8  | An external module resolves a `package.json` (its own or a nearest ancestor's). | `Source.create` `packageJsonFields.Value` |
| SF-9  | That resolved `package.json` exposes a `version`. | `Source.create` version `.Value` |
| SF-10 | A non default-lib script source also resolves a `package.json`. | `Source.create` (script path) |
| SF-11 | That `package.json` is both **named** and **versioned**. | `Source.create` `closestNamedAndVersionedPackageJsonFields.Value` |

### XTK · Wrapper Totality

Invariants that the classifier wrappers are *total* over real input — they never reach their failure / `Ignore` case.

| ID | Invariant | Backs |
|----|-----------|-------|
| XTK-1 | `Source.create` never throws on real input (exercises every SF-* guard end to end). | `Source.create` |
| XTK-2 | Every external-module export classifies as `TypeDeclaration` or `ModulesAndExports`. | `XanTagKind.Create` |
| XTK-3 | Every script top-level statement classifies the same way. | `XanTagKind.Create` |
| XTK-4 | Every class & interface member classifies. | `MemberDeclaration.Create` / `IsMemberDeclarationKind` |
| XTK-5 | Every symbol `escapedName` maps to a `SymbolName` (string or known `InternalSymbolName`). | `SymbolName.Create` |
| XTK-6 | Every symbol-table value declaration classifies (asserts no `Ignore`). | `XanTagKind.Create` |
| XTK-7 | Every top-level statement kind is recognised. | `TopLevelStatements.IsTopLevelStatementKind` |
| XTK-8 | Every exported-symbol declaration kind is recognised. | `TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind` |
| XTK-9 | Every local-symbol declaration kind is recognised. | `TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind` |

> Note: the DU `.Value` accessors (e.g. `XanTagKind.Value`, `TypeNode.Value`) rely on a *separate* invariant —
> Fable 5's internal single-payload DU layout (`fields[0]`) — which is documented inline at each accessor rather
> than proven by the corpus, since it is a property of the Fable compiler, not of the input `.d.ts`.
