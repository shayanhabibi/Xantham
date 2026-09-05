---
category: Generator
audience: managing agent
title: Lane AJ - a real enum member's name sanitises to a legal union case
branch: worktree-gen-wave7-aj
base: 44507724b441c5b965bd20fef87038e8cd9b605d
---

# Lane AJ - a real enum member's name sanitises to a legal union case

## What the brief described vs. what is actually reachable

The brief pointed at lane AF's retained-literal `StringEnum` (`Shape/Overloads.fs`'s
`literalDecl` and `Shape/Spec.fs`'s `literalOverloadSets`/`declarationOf`) as the route a raw
`"@cf/meta"`-style literal reaches an unsanitised union case name through. I built that exact
reproducer (`case: "@cf/meta"` in an overload-distinguishing position) and it is **already
correct**: both the declaration name and the case name run through `Naming.enumCaseOfString`
today, so `KVNamespace.CfMeta` / `Store.CfMeta` compile clean. Nothing there needed a change.

The real, reachable defect is a sibling code path: `Shape/LiteralUnions.fs`'s
`classifyLiteralUnions`, in the branch that names a case after a **real TS enum member's own
symbol name** rather than after its literal value (`m.SymbolName when not (isSyntheticName
symbolName) -> Naming.pascalSegment symbolName`, two arms - the all-integer and the
mixed/string one). TypeScript allows a quoted enum member name
(`enum Kind { "@cf/meta" = "meta" }`), and `Naming.pascalSegment` only splits on `-`/`_`/`.` - it
does not strip `@`, `/`, a space, or prefix a leading digit the way `Naming.enumCaseOfString`
does. Verified with `dotnet run --project src/Xantham.Cli -- generate` against a two-member enum
before any fix: it emitted `` | ``@cf/meta`` `` and `` | ``Beta channel`` `` as case names, which
`dotnet build Xantham.slnx`'s compile gate refuses with FS0883 the same way lane AI's type-name
case did.

## Fix

`Shape/LiteralUnions.fs` gained `sanitisedCaseName owner symbolName`: it runs the member's own
name through `Naming.enumCaseOfString` (already used elsewhere in the same file for a literal's
*value*) instead of bare `Naming.pascalSegment`, and reports `SY005`
(`SynthesizeAnonymous.NameSanitisedForIdentifier`, reused exactly as pre-declared - no new
Findings.fs case) only where that differs from what `Naming.pascalSegment` alone would have
produced. That guard matters: `Naming.pascalSegment` already capitalises and already splits on
dash/underscore/dot, so an ordinary member name (`MY_ENUM`, `one-two`) sanitises identically
either way and must not report a finding it didn't produce before. Both call sites (the
all-integer `FsEnum` arm and the mixed/string `FsStringEnum` arm) were changed the same way -
`Shape/LiteralUnions.fs` was not in the brief's owned-files list (`Shape/Spec.fs` only), but the
defect does not live in `Spec.fs`; `uniqueCaseNames` itself and everything reachable from it in
`Spec.fs` were already sanitising correctly, and I could not construct a reproducer against them.

`uniqueCaseNames` (`Shape/Spec.fs:2231`, untouched) still runs after sanitisation, so two member
names sanitising to the same identifer keep separate cases (verified: `"a-b"` and `"a_b"` both
sanitise to `Ab`, and the second becomes `Ab2` via the same suffixing every other case-name
collision already uses).

## Lab: `tests/fixtures/case-sanitise-lab`

Three real TS enums: `Kind` (`"@cf/meta"`, `"beta channel"`, `"2fa"` - the three illegal-shape
members), `Collide` (`"a-b"` / `"a_b"`, the collision negative, all-integer so it exercises the
`FsEnum` arm), `Plain` (`"one-two"`, the negative that pascalSegment already handled and that
must report no finding). Registered in `Pipeline.test.fs`. Per-pass test in `Shape.test.fs`
exercises `classify-literal-unions` directly on a hand-built model with `SymbolName` set to
`"@cf/meta"` / `"one-two"`.

## Measurements

- `SY005` (`SY.NameSanitisedForIdentifier`) corpus count: **4 before, 7 after** (unchanged:
  `key-sanitise-lab` 3, `nested-name-lab` 1; new: `case-sanitise-lab` 3, for `Kind`'s three
  members - `Collide` and `Plain` correctly report none).
- No large fixture (`@cloudflare/workers-types`, `animejs`, `solid-js`, `type-fest`) carries any
  `SY005` before or after - the defect is real but unexercised in the corpus, same as lane AI's
  type-name equivalent.
- `dotnet fsi build.fsx -- test --update`: 450 Generator tests, 85 Wire tests, run gate 179
  checks, all green.
- `dotnet build Xantham.slnx`: compile gate green, including the new golden.
- `git diff --stat` (real changes only - the rest of the tree shows CRLF-normalisation noise
  from this older base, not content changes; verified with `git -c core.autocrlf=false diff
  --stat`): `Shape/LiteralUnions.fs` +19/-2, `Pipeline.test.fs` +61, `Shape.test.fs` +30, new
  `case-sanitise-lab` fixture + golden (3 files).

## FS0883 reproducer (pre-fix)

```ts
export enum Kind {
  "@cf/meta" = "one",
  "beta channel" = "two",
}
```
produced (pre-fix):
```fsharp
[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]
type Kind =
    | [<CompiledName("one")>] ``@cf/meta``
    | [<CompiledName("two")>] ``Beta channel``
```
which `dotnet build Xantham.slnx` refuses: `error FS0883: Invalid namespace, module, type or
union case name`.

## Nothing unexplained

The brief's named route (`Shape/Spec.fs`'s literal-overload retention) was tested and found
already correct; the actual defect was one file over, in the sibling case-minting path the
brief's compressed description pointed at only approximately. Everything else in the corpus that
moved is either the new lab or CRLF noise pre-existing on this base.

## Commit

`44507724` (base) is unchanged; the fix landed as a single commit on
`worktree-gen-wave7-aj`. Never pushed, never merged into `master`.
