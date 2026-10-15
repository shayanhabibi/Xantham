# Exclusive-Arm Fold and Value-Side Name Fixes — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix six defects found after #158. Each binding below is farther from its TypeScript
source than F# requires:
- a source-named arm of an exclusive fold is referenced but never declared (FS0039);
- the exclusive fold drops call, index and construct signatures without a finding, and folds
  arms that disagree on readonly;
- a merged value side is named by collision suffix (`P54`, `Kind2`) without a finding;
- a function merged with a namespace loses its namespace values, and a callable declaration gets
  a `Create` that TypeScript rejects;
- a `Create` makes required `T | null` members omittable and binds `?x: unit` parameters.

**Architecture:** Six tasks, in this order: C, B, I2, D, F, G. Later tasks rely only on
earlier ones' named interfaces.
- **C** restricts the arms that `shape-interfaces` removes from `DeclNames` to the anonymous
  ones (`Spec.isAnonymousArm`), so a source-named arm stays declared, named by today's
  `name-exports` rule.
- **B** makes `Spec.exclusiveArmShape` decline any union with a callable or constructible arm,
  and fold only over index signatures every arm carries identically. The folded interface
  carries those as `[<EmitIndexer>] Item` (MB004).
- **I2** makes the same fold decline arms that disagree on a shared member's readonly.
- **D** names the value-side interface of
a merged export, which TypeScript spells only as `typeof X`, by its role: `<Stem>Enum`,
`<Stem>Namespace`, or `<Stem>Value` for a variable. This mirrors `<Stem>Constructor`, and a new
`SY007` reports each name. Anonymous shapes that a namespace's values own nest under the source
namespace's module (`Point.Defaults`), not under the minted interface.
- **F** binds a callable value export carrying members as the hybrid object, called through
  `Invoke`, and refuses `Create` on every callable declaration (SP003).
- **G** carries each member's presence (`FsPresence`) into both `Create`s. A required
  `T | null` member is a required parameter whose `None` the Emit writes as `null`, and a
  `?: undefined` member binds no parameter (`SP006`).
- F and G change Core.TS and Node and regenerate them in their own breaking commits.

**Companion plan:** `docs/.ai/plans/2026-10-15-namespace-member-harvest.md` runs **after** this
plan. Its R1 nests every namespace type member under its namespace, and its Task 3 Step 10 re-pins
Task C's assertions to the nested form (`Contested.Right`). Its R6 limits `SY004` to
position-derived nesting, so this plan leaves `SY004`'s message as it is.

**Tech Stack:** F# generator (`src/Xantham.Generator`), Expecto suites, Fable.Core 5.2.0 compile
gate.

**Spec:** the investigation findings, with the controller's rulings recorded per task below:
- C: `docs/.ai/plans/2026-10-15-pre158-findings.md` §C (ruling: Option 1)
- B: `docs/.ai/plans/2026-10-15-pre158-findings.md` §B (ruling: Option 2 for index signatures; Option 1 for call/construct signatures, a planner ruling under the fidelity rule — see Task B)
- I2: `docs/.ai/plans/2026-10-15-pre158-findings.md` §I (I2) (ruling: compare readonly in `classify`)
- D: `docs/.ai/plans/2026-10-15-pre158-findings.md` §D (ruling: role-named value side, new SY finding)
- F: `docs/.ai/plans/2026-10-15-pre158-findings.md` §F (ruling: Option A, plus `Create` refused on callables)
- G: `docs/.ai/plans/2026-10-15-pre158-findings.md` §G (ruling: Option 1, presence on the model)

Standing rule (user): where options differ, choose the one whose binding is closest to the
TypeScript source; a fix's blast radius does not justify a less faithful representation.

## Global Constraints

- Branch from `feat/tagged-arm-158` (PR #160, not yet merged): the work uses
  `Spec.isAnonymousArm` and the DD002/DD003/DT007 machinery from #158. Rebase onto `develop` once
  #160 merges. Suggested branch: `fix/exclusive-fold-value-side`. #160 also lands investigation
  I1 (a symmetric `sameShape` union branch), which Task B builds on. Confirm it is on the base
  before Task B.
- Tests run only through `dotnet fsi build.fsx -- test [--quick] [--update] [--filter <prefix>] [--run-gate]`,
  never `dotnet test` (it stalls on the suites' nested builds). The inner loop is
  `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`; run the unfiltered
  `dotnet fsi build.fsx -- test` before each task's final commit.
- Goldens change only through `--update` (or `--goldens`), never by hand, and every golden diff is
  read before commit (aggregate measures and samples for the large fixtures,
  `.claude/rules/generator-fixtures.md`).
- A new finding code is three edits: the case in `Findings.fs`, a `FindingCodes.table` row taking
  the next unused number under its prefix, and its pin in `Findings.test.fs`. This plan adds two:
  `SY007` (`SY.ValueSideNamed`; `SY001`–`SY006` are taken, none retired) and `SP006`
  (`SP.AbsentOnlyMembersOmitted`; `SP001`–`SP005` taken). Task F adds an SP003 reason, not a code.
- Core.TS and Node change only in F and G. Each is regenerated in its own commit with a
  `feat(bindings)!:` subject and a `BREAKING CHANGE:` footer listing what changed, with counts
  read off the diff. The packages' `CHANGELOG.md` files are generated from commits; do not
  hand-edit them.
- Update the relevant phase record in `docs/.ai/plans/generator-architecture.md` /
  `docs/.ai/plans/generator-type-mapping.md` in the same commit as the behaviour change.
- Doc comments follow `.claude/rules/comments.md`; reviewer-only notes use `//FOR-REVIEW`.
- `src/**/*.fs` stays Fantomas-formatted (`build.fsx` formats unless `--no-format`).
- Generated bindings target Fable 5.x only (`Fable.Core` 5.2.0 compile gate).
- Scratch files under `tests/.scratch/`, never the system temp directory. Shell commands are
  prefixed with `rtk`.
- Never stage `.serena/project.yml`. Stage paths explicitly, never `git add .`.
- Commits use conventional-commit subjects and end with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5
  ```

## Design evidence (gathered while planning, frozen CLI at `91742f98`)

Labs under `tests/.scratch/pre158/plan-fold-value-side/`, generated with
`dotnet tests/.scratch/xantham-cli/xantham.dll generate <lab> -o <out>`:

- `c-lab/` (exclusive-arms-lab plus Task C's pair four): `ContestedHolder` reads
  `abstract right: Right` and `Contested.Left`, `Elsewhere.read` reads `Right -> Contested.Left`,
  and neither `Right` nor `Contested.Left` is declared (FS0039). `pickLeft (left: obj) : obj`
  widens (TR023 2). `c-sim/` (the same arms without `never`, so no fold) shows today's
  `name-exports` naming for these arms: `Contested.Left` is written `module Contested = type Left`
  because a top-level `Left` holds the bare name, and `Contested.Right` is written as a
  top-level `type Right`. Every reference reads `Contested.Left` / `Right`. Task C pins exactly
  that, because it is what the generator writes when this plan lands. The companion plan's R1
  then nests both (`Contested.Right`) and re-pins.
- `b-sim/`: a declined union with a call-signature arm renders `type CallOne = U2<CallOne2, CallOne3>`
  where `CallOne2` carries `[<Emit("$0($1...)")>] abstract Invoke: e: string -> unit` **and** a
  `Create`. A construct-signature arm is named `CtorOneConstructor` (name-constructor-objects,
  SI004), the other arm `CtorOne2`. `CallSameIndexDiff` (same call signature, different index
  signatures) folds today and drops both.
- `d-lab/`: `Shape` (tagged union + namespace) has value side **`Shape4`** while `Shape2`/`Shape3`
  are dropped (DD002 2). This verifies the investigators' inferred claim: a pruned arm keeps
  reserving its number, because `synthesize-anonymous` claims names before
  `drop-orphan-delegates` prunes. Role naming avoids the number entirely, so Task D needs no
  separate change for it. `Point` → `Point2`, `Level` (enum) → `Level2`, `Mode` → `Mode2`
  beside an exported `ModeNamespace`, `Token` (const + type) → `Token2`, `Config` (namespace
  alone) → `Config`. A value side's inline member shapes nest under the suffixed name today
  (`Point2.<Member>`). The redirect point is `into` (`Anonymous.fs:648-649`): it nests a type's
  children under `nestBases[typeId]` ahead of the type's own name. `nestBases`
  (`Anonymous.fs:308`) has one other writer, the tagged-arm case modules (`:729`), which never
  touches an export's value id.

**Tasks C, B, I2 and D leave Core.TS and Node unchanged; F and G change both.** Tasks C, B and
I2 change only exclusive folds, and Core.TS and Node contain none (both `symbols.jsonl` have 0
`TR060`). For Task D, a grep of
`src/Xantham.Fable.Core.TS/Fable.Core.TS.fs` and `src/Xantham.Fable.Node/Node.fs` for value-side
positions (`static member|abstract|val X: [Ns.]X<digits>`) finds only Node's
`constants: Constants2`…`Constants9`. Those are pure `export namespace constants` exports:
`SymbolFlags.Type` excludes `ValueModule`/`NamespaceModule`, so `Resolve.fs:253-256` asks no
declared type, `ExportTypes.Declared` is `None`, and Task D's rule requires it to be `Some`. Across
the goldens, the same grep finds `case-sanitise-lab` (`Kind2`, `Collide2`, `Plain2`),
`literal-member-lab` (`Level2`) and `solid-js` `JSX2`. `JSX2` is also a pure namespace, so this
plan leaves it unchanged. The companion plan changes Core.TS, Node and solid-js on its own account
(its Tasks 4–5, including `JSX2`'s members). Task D still regenerates both bindings and checks
that the diff is empty.

For F, an awk count over the bindings finds 64 Node declarations and 2 Core.TS declarations that
carry both `abstract Invoke` and `static member Create`. The findings count ~76 Node
function+namespace exports (`@types/node` merges 82: fs 42, dns 16, zlib 11, …). For G, the
findings count ~378 Core.TS and ~88 Node required `T | null` fields in Create-bearing
declarations. These are planning measures; each regeneration commit reports the measured diff.

## Review Focus

1. **A source-named arm read from another namespace.** `Elsewhere.read(arm: Contested.Right): Contested.Left`
   must read the declared arms, not `obj` and not an undeclared name. Pinned in Task C
   (`abstract read: arm: Right -> Contested.Left`) and by the compile gate.
2. **Both arms carry an identical call signature but different index signatures.** The fold
   must decline as a whole: `U2`, each arm keeping the shared `Invoke` and its own `Item`. Any
   call signature declines (Task B's ruling), so neither the call nor the indexers can be
   dropped silently. Pinned in Task B (`CallSameIndexDiff`).
3. **A value side whose role name is itself an exported type.** `Mode` beside
   `export interface ModeNamespace` must yield `ModeNamespace2` for the value side and leave the
   interface untouched, with `SY007` naming `ModeNamespace2`. Pinned in Task D.
4. **A required member admitting both `null` and `undefined`, or only `undefined` under `?`.**
   `x: T | undefined` must keep its key present, and `x?: undefined` / `x?: never` must bind no
   parameter rather than `?x: unit`. Pinned in Task G (`Presence.reqUndef`, `Presence.undef`,
   both run-gated), and by the re-pinned `exclusive-arms-lab` arms.
5. **Shapes a namespace's values own.** `Point.defaults: { x: number }` is reached in TypeScript
   as `Point.defaults`, so its shape nests under the source namespace (`Point.Defaults`), not
   under the minted value interface (`PointNamespace.Defaults`) or the old suffix
   (`Point2.Defaults`). A variable's value side has no source namespace, so its shapes nest
   under its own minted name (`TokenValue.Options`). A namespace *type* of the same spelling
   (`Point.Anchor`) keeps it, and the value-owned shape takes the number (`Point.Anchor2`). Pinned
   in Task D. Under this plan that order depends on the walk reaching the namespace type first
   (`pinned` before `anchor`). The companion plan's harvest claims namespace types in
   `name-exports` and removes that dependence. Hand any ordering counter-example to the companion.

## File map

| File | Change | Task |
|---|---|---|
| `src/Xantham.Generator/Shape/Interfaces.fs` | `foldedArmIds` anonymous-only (C); fold carries indexers (B); fold `Create` reads presence (G) | C, B, G |
| `src/Xantham.Generator/Shape/Spec.fs` | `ExclusiveArmsFold` carries `indexes`, callable arms decline, `shapeIndexer` (B); readonly in `classify` (I2); `presenceOf` (G) | B, I2, G |
| `src/Xantham.Generator/Shape/Anonymous.fs` | role-named value side + `SY007`, value-side shapes under the namespace (D); class-only exclusion (F) | D, F |
| `src/Xantham.Generator/Shape/Exports.fs` | method arm only for call-signature-only values | F |
| `src/Xantham.Generator/Shape/ParamObjects.fs` | SP003 `Callable` reason (F); presence, `SP006` (G) | F, G |
| `src/Xantham.Generator/Shape/TaggedUnions.fs`, `Render.fs`, `Model.fs`, `Customization/Contract.fs`, `DeclarationCatalog.fs` | `FsPresence`, normalising Emit | G |
| `src/Xantham.Generator/Findings.fs` | `SY007` (D); `SP006`, DT005 message (G) | D, G |
| `tests/fixtures/exclusive-arms-lab/index.d.ts` | pair four (C); signature unions (B); readonly pairs (I2) | C, B, I2 |
| `tests/fixtures/tagged-arm-lab/index.d.ts` | T1/T2 comment | B |
| `tests/fixtures/value-side-lab/{index.d.ts,package.json}` | new lab | D |
| `tests/fixtures/callable-hybrid-lab/{index.d.ts,index.js}` | function+namespace merges | F |
| `tests/fixtures/fable-workaround-lab/{index.d.ts,index.js}` | `Presence`, `Presented`, `describeKey` | G |
| `tests/Xantham.Generator.Tests/Pipeline.test.fs` | `declarationBodyOf`; per-task lab assertions | C, B, I2, D, F, G |
| `tests/Xantham.Generator.Tests/Findings.test.fs` | `SY007`, `SP006` pins | D, G |
| `tests/Xantham.Generator.Tests/{Render,Shape,ResolvedProjection}.test.fs` | record literals | G |
| `tests/Xantham.Generator.RunGate/{Program.fs,Probes.fs}` | `callableHybrids` via `Invoke` (F); presence checks replacing `:405-418`, Fable probe (G) | F, G |
| `tests/Xantham.Generator.Tests/golden/**` | regenerated | all |
| `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**` | regenerated, breaking commits | F, G |
| `docs/.ai/plans/generator-architecture.md`, `generator-type-mapping.md`, `docs/.ai/fable5-workarounds.md` §1 | phase records; workaround doc (G) | all |

---

### Task C: A source-named arm of an exclusive fold stays declared

**Defect** (`c-claimed-arms/findings.md`): `Interfaces.fs:84-93` removes every arm of every
`ExclusiveArmsFold`, source-named ones included, from the `DeclNames` it hands later passes. The
pass's own declarations and the earlier `shape-callbacks` still read the unfiltered names, so
interface members, class sides, `Create` parameters and callbacks reference `QueryArm`,
`Contested.Left` or `Right`, which nothing declares: **FS0039**. Later passes read the filtered
names and widen to `obj` (TR023). **Ruling: Option 1** — only anonymous arms
(`Spec.isAnonymousArm`, `Spec.fs:330`) leave `DeclNames`. A source-named arm is an author's
declaration and stays, named by today's `name-exports` rule: `Contested.Left` beside a
top-level `Left`, bare `Right` otherwise. No new finding. The pins in Steps 3 and 6 spell
today's names. The companion plan (`2026-10-15-namespace-member-harvest.md`, R1 and its Task 3
Step 10) runs after this one and re-pins them to the nested form (`Contested.Right`). Do not
anticipate that here. `synthesize-anonymous` (`Anonymous.fs:726`) and `drop-orphan-delegates`
(`Orphans.fs:172`, `mintedNames`) already treat only anonymous arms as the fold's own, so this
change brings `shape-interfaces` into line with them.

**Files:**
- Modify: `src/Xantham.Generator/Shape/Interfaces.fs:84-90` (`foldedArmIds`)
- Modify: `tests/fixtures/exclusive-arms-lab/index.d.ts` (header comment; append pair four)
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (new helper after `moduleBodyOf` at `:83-97`; `exclusive-arms-lab` block `:5298-5358`)
- Modify: `docs/.ai/plans/generator-architecture.md` (new section at the end)
- Regenerate: `tests/Xantham.Generator.Tests/golden/exclusive-arms-lab`

**Interfaces:**
- Produces: `Pipeline.test.fs` private helper `declarationBodyOf: string -> string -> string`
  (the lines below a `type Name =` header more indented than it, joined by `\n`). Tasks B and D use it.
- Produces: `exclusive-arms-lab` pair four (`Contested`, `Contested.Left`, `Contested.Right`,
  top-level `Left`, `ContestedHolder`, `ContestedCallback`, `Elsewhere.read`, `contest`, `pickLeft`).
  Task B appends after it and extends the TR060/TR061 list this task sets.

- [ ] **Step 1: Extend the lab**

In `tests/fixtures/exclusive-arms-lab/index.d.ts`, replace the header's second line
`// batch two, item 4. Three pairs pin the fold's three outcomes.` with:

```ts
// batch two, item 4. Pairs one to three pin the fold's three outcomes; pair four pins the fold
// over source-named arms.
```

Append to the end of the file:

```ts

// Pair four: source-named arms in a namespace merged with the union alias. Folds - TR060 - and
// both arms stay declared. A top-level `Left` sits beside `Contested.Left`. Each arm is read
// from an interface, a callback, a function of another namespace and a top-level function.
export type Contested = Contested.Left | Contested.Right;
export namespace Contested {
    export interface Left {
        left: string;
        right?: never;
        tag: number;
    }
    export interface Right {
        left?: never;
        right: string[];
        tag: number;
    }
}
export interface Left {
    other: boolean;
}
export interface ContestedHolder {
    left: Contested.Left;
    right: Contested.Right;
    top: Left;
}
export type ContestedCallback = (left: Contested.Left) => void;
export namespace Elsewhere {
    export function read(arm: Contested.Right): Contested.Left;
}
export function contest(options: Contested, callback: ContestedCallback, holder: ContestedHolder): void;
export function pickLeft(left: Contested.Left): Contested.Right;
```

- [ ] **Step 2: Add the test helper**

In `Pipeline.test.fs`, directly after `moduleBodyOf` (ends `:97`), add:

```fsharp
/// The body of a `type Name =` declaration: every non-blank line below the header more indented
/// than it. Scopes an assertion to one declaration.
let private declarationBodyOf (source: string) (name: string) =
    let lines = source.Replace("\r\n", "\n").Split '\n'
    let header = $"type {name} ="

    match lines |> Array.tryFindIndex (fun line -> line.TrimStart().StartsWith header) with
    | None -> failtest $"no declaration named {name} in the rendered source"
    | Some start ->
        let indent = lines[start].Length - lines[start].TrimStart().Length

        lines
        |> Array.skip (start + 1)
        |> Array.takeWhile (fun line ->
            not (String.IsNullOrWhiteSpace line) && line.Length - line.TrimStart().Length > indent)
        |> String.concat "\n"
```

- [ ] **Step 3: Flip the pins and write the failing test**

In the `fixtureTests "exclusive-arms-lab"` block (`:5298`), replace the lead comment
(`:5299-5307`) and the first two test cases (`:5308-5338`) with:

```fsharp
                // Wave fourteen lane CN, batch two, item 4: the exclusive-arm fold itself.
                // `SeparableOptions`, `AnchoredOptions` and `Contested` fold into one interface
                // named after the union, `never` members off the folded surface rather than left
                // as `unit`; `DisagreeingOptions` is not this construct and is untouched. A
                // source-named arm stays declared beside the fold, with its own `never` members
                // as `unit`, so each "no `unit`" check reads the folded declaration's own body.
                [ testCase "both arms anchoring their own required member fold, TR060" <| fun _ ->
                    let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                    let source = rendered.Files |> List.head |> snd
                    let foldedBlock = declarationBodyOf source "SeparableOptions"

                    Expect.stringContains source "type QueryArm =" "a source-named arm stays declared beside the fold"
                    Expect.stringContains source "type MessagesArm =" "on both sides"
                    Expect.stringContains foldedBlock "abstract query: string option" "query kept optional, not unit"
                    Expect.stringContains foldedBlock "abstract messages: string[] option" "messages kept optional, not unit"

                    Expect.isFalse
                        (foldedBlock.Contains ": unit")
                        "no `never` member survives as a unit placeholder on this fold"

                  testCase "one arm's own required member anchors the other's optional one, TR060" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      let foldedBlock = declarationBodyOf source "AnchoredOptions"

                      Expect.stringContains source "type ImageArm =" "a source-named arm stays declared beside the fold"
                      Expect.stringContains source "type SnapshotArm =" "on both sides"

                      Expect.isFalse
                          (foldedBlock.Contains ": unit")
                          "the anchored pair's own members are real on both sides, so no `unit` survives"

                  testCase "a source-named arm of a fold is declared where it is read" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      let nested = moduleBodyOf source "Contested"
                      let holder = declarationBodyOf source "ContestedHolder"

                      Expect.stringContains
                          (declarationBodyOf source "Contested")
                          "abstract right: string[] option"
                          "the union still folds"

                      Expect.stringContains nested "type Left =" "`Contested.Left` is declared in its namespace's module"
                      Expect.stringContains nested "abstract left: string with get, set" "with its own members"
                      Expect.stringContains source "type Right =" "`Contested.Right` is declared under today's bare spelling (re-pinned by the companion plan)"
                      Expect.stringContains holder "abstract left: Contested.Left with get, set" "an interface member reads the nested arm"
                      Expect.stringContains holder "abstract right: Right with get, set" "and the bare one"

                      Expect.stringContains
                          source
                          "type ContestedCallback = (Contested.Left -> unit)"
                          "a callback reads the nested arm"

                      Expect.stringContains
                          (declarationBodyOf source "Elsewhere")
                          "abstract read: arm: Right -> Contested.Left"
                          "a function of another namespace reads both arms"

                      Expect.stringContains
                          source
                          "static member pickLeft (left: Contested.Left) : Right = jsNative"
                          "a top-level function reads both arms, unwidened"
```

In the third test case (`"a shared member disagreeing in type is not this construct"`), change
the expected list `[ "AnchoredOptions"; "SeparableOptions" ]` to
`[ "AnchoredOptions"; "Contested"; "SeparableOptions" ]`.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL in `exclusive-arms-lab` on `type QueryArm =` (the arm is not declared), and the
new case on `type Right =`.

- [ ] **Step 4: Watch the compile gate fail**

The `--update` run wrote the broken golden. Run: `dotnet build Xantham.slnx`
Expected: FAIL in `Xantham.Generator.CompileGate`, `golden\exclusive-arms-lab\ExclusiveArmsLab.fs`,
with FS0039 on `Right` (and on `Contested.Left`), the references `c-lab/` showed.

- [ ] **Step 5: Remove only anonymous arms**

In `Interfaces.fs`, replace `foldedArmIds` (`:84-90`) with the following. The comment above
`exclusiveUnions` (`:62-66`) is unchanged.

```fsharp
                    // The fold replaces only the arms the checker named: a source-named arm is a
                    // declaration in its own right and stays, nesting under its namespace.
                    let foldedArmIds =
                        exclusiveUnions
                        |> List.collect (fun (_, _, outcome) ->
                            match outcome with
                            | Choice1Of2(_, arms) ->
                                arms
                                |> List.filter (fun a -> isAnonymousArm a.Facts)
                                |> List.map (fun a -> a.Facts.Response.TypeId)
                            | Choice2Of2 _ -> [])
                        |> Set.ofList
```

- [ ] **Step 6: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Then `rtk git status tests/Xantham.Generator.Tests/golden`: only
`exclusive-arms-lab` moves. Read its `.fs` diff in full: `QueryArm`, `MessagesArm`, `ImageArm`,
`SnapshotArm` declared again (their `never` members as `unit option`, as `WidgetArm` has today),
pair four as in Step 3. In `symbols.jsonl`, the 4 `AC001` lines for the named arms are gone.
Any other golden moving is a stop-and-report: `@cloudflare/workers-types` and `tagged-arm-lab`
fold anonymous arms only.

Run: `dotnet build Xantham.slnx` → Expected: success.

- [ ] **Step 7: Record the phase**

Append to `docs/.ai/plans/generator-architecture.md`:

```markdown
## Exclusive-arm fold arms and signatures, value-side names (2026-10-15, #158 follow-up)

`shape-interfaces` removes only an exclusive fold's anonymous arms (`Spec.isAnonymousArm`) from
`DeclNames`. A source-named arm stays declared, under the name `name-exports` gives every
namespace member at this point (`Contested.Left` beside a top-level `Left`, bare `Right`
otherwise). The fold no longer leaves references to an undeclared arm (FS0039), and no longer
widens a parameter typed by one (TR023). `exclusive-arms-lab` declares `QueryArm`, `MessagesArm`,
`ImageArm` and `SnapshotArm` again; no other golden moved. The namespace-member harvest plan
(`2026-10-15-namespace-member-harvest.md`, R1) later nests every namespace member, which re-pins
pair four to `Contested.Right`.
```

- [ ] **Step 8: Full suite and commit the fix**

Run: `dotnet fsi build.fsx -- test` → Expected: PASS.

```
rtk git add src/Xantham.Generator/Shape/Interfaces.fs tests/fixtures/exclusive-arms-lab/index.d.ts tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden/exclusive-arms-lab docs/.ai/plans/generator-architecture.md
rtk git commit -m "fix(generator): keep source-named arms of an exclusive fold declared" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task B: The exclusive fold carries only index signatures every arm shares

**Defect** (`b-fold-signatures/findings.md`): `Spec.exclusiveArmShape` (`Spec.fs:637`) reads
`arm.Members` only, and `foldedDecls` (`Interfaces.fs:406-464`) builds the folded interface from
members alone. Every call, construct and index signature on an arm disappears with no finding.

**Ruling: Option 2 for index signatures; Option 1 for call and construct signatures** (planner
ruling under the standing fidelity rule).
- The fold admits index signatures that every arm carries identically, and carries them once as
  `[<EmitIndexer>] Item` (MB004). An object literal satisfies an index signature, so the fold's
  `Create` overloads stay sound.
- Any call or construct signature on any arm makes the union `NotExclusiveArms`, so it stays `U2`
  and each arm keeps its signatures through the ordinary path. The fold exists for its `Create`
  overloads, the one place exclusivity holds (TR060). TypeScript accepts no object literal for a
  callable type, and Task F refuses `Create` on every callable declaration. A callable fold would
  therefore have no `Create` and would hold exclusivity nowhere, which is farther from the source
  than `U2` over arms that each keep their own members exactly.
- This reverses the controller's earlier "hoist identical call signatures as `Invoke`" half of
  Option 2. Task B therefore extracts no `Invoke` builder.

No new finding. `Anonymous.fs:718` and `Orphans.fs:158` call the same predicate, so arm nesting
and orphan pruning follow without edits. Task B builds on the symmetric `sameShape` that PR #160
lands (investigation I1: mutual inclusion in the union branch, `Spec.fs:497-500`). Rebase onto
#160 before starting.

This task assumes Task C has landed (`foldedArmIds` filters `isAnonymousArm`).

**Files:**
- Modify: `src/Xantham.Generator/Shape/Spec.fs:606-737` (`ExclusiveArmsShape` doc and case; new helpers after `isNeverTyped`; `exclusiveArmShape`)
- Modify: `src/Xantham.Generator/Shape/Spec.fs:3614-3929` (new `shapeIndexer` above the `shapeMembers` doc comment; `indexers` in `shapeMembers` `:3909-3929`)
- Modify: `src/Xantham.Generator/Shape/Interfaces.fs` (`:72-90`; `foldedDecls` `:406-464`)
- Modify: `tests/fixtures/exclusive-arms-lab/index.d.ts` (append), `tests/fixtures/tagged-arm-lab/index.d.ts:99-101` (comment)
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`exclusive-arms-lab` block; `tagged-arm-lab` block `:4568`)
- Modify: `docs/.ai/plans/generator-architecture.md` (Task C's section)

**Interfaces:**
- Consumes: `declarationBodyOf` and the TR060/TR061 list from Task C; symmetric `sameShape` (PR #160).
- Produces:
  ```fsharp
  type internal ExclusiveArmsShape =
      | NotExclusiveArms
      | ExclusiveArmsFold of shared: ResolvedMember list * arms: ExclusiveArm list * indexes: ResolvedIndex list
      | ExclusiveArmsDecline of arms: int
  val internal shapeIndexer: Context -> ShapeModel -> string -> ResolvedIndex -> FsMember * Finding list   // Spec.fs
  ```
  Task I2 edits `classify` inside `exclusiveArmShape`.

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/exclusive-arms-lab/index.d.ts`:

```ts

// Signatures on the arms. The fold carries an index signature only where every arm carries it
// identically (`IndexBoth`). A call or construct signature on any arm, or an index signature
// some arm lacks or spells differently, keeps the union erased, each arm keeping its own.
export type CallOne = { (e: string): void; a: number; b?: never } | { b: string; a?: never };
export type CallBoth = { (e: string): void; a: number; b?: never } | { (e: string): void; b: string; a?: never };
export type CallDiff = { (e: string): void; a: number; b?: never } | { (n: number): boolean; b: string; a?: never };
export type IndexOne = { [k: string]: unknown; a: number; b?: never } | { b: string; a?: never };
export type IndexBoth = { [k: string]: unknown; a: number; b?: never } | { [k: string]: unknown; b: string; a?: never };
export type IndexDiff = { [k: string]: unknown; a: number; b?: never } | { [k: number]: string; b: string; a?: never };
export type IndexReadonlyDiff =
    | { readonly [k: string]: unknown; a: number; b?: never }
    | { [k: string]: unknown; b: string; a?: never };
export type CallSameIndexDiff =
    | { (e: string): void; [k: string]: unknown; a: number; b?: never }
    | { (e: string): void; [k: number]: string; b: string; a?: never };
export type CtorOne = { new (e: string): object; a: number; b?: never } | { b: string; a?: never };
export function signatures(
    callOne: CallOne,
    callBoth: CallBoth,
    callDiff: CallDiff,
    indexOne: IndexOne,
    indexBoth: IndexBoth,
    indexDiff: IndexDiff,
    indexReadonlyDiff: IndexReadonlyDiff,
    callSameIndexDiff: CallSameIndexDiff,
    ctorOne: CtorOne,
): void;
```

- [ ] **Step 2: Write the failing tests**

Append to the `exclusive-arms-lab` test list:

```fsharp
                  testCase "a fold carries the index signature every arm carries identically, MB004" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd
                      let indexBoth = declarationBodyOf source "IndexBoth"

                      Expect.stringContains indexBoth "[<EmitIndexer>]" "a shared index signature is the fold's indexer"
                      Expect.stringContains indexBoth "abstract Item: string -> obj with get, set" "keyed and valued as every arm writes it"
                      Expect.stringContains indexBoth "static member Create (a: float) : IndexBoth = jsNative" "beside one Create per arm"
                      Expect.stringContains indexBoth "static member Create (b: string) : IndexBoth = jsNative" "for both arms"

                      Expect.contains
                          (rendered.Findings |> List.filter (fun f -> f.Key = "MB004") |> List.map _.Symbol)
                          "IndexBoth.[]"
                          "the fold's indexer reports MB004"

                  testCase "a call or construct signature, or an index signature the arms do not share, keeps the union erased" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      for union in
                          [ "CallOne"
                            "CallBoth"
                            "CallDiff"
                            "IndexOne"
                            "IndexDiff"
                            "IndexReadonlyDiff"
                            "CallSameIndexDiff"
                            "CtorOne" ] do
                          Expect.stringContains source $"type {union} = U2<" $"{union} does not fold"

                      Expect.stringContains
                          (declarationBodyOf source "CallOne2")
                          "abstract Invoke: e: string -> unit"
                          "the arm keeps its call signature"

                      for arm in [ "CallBoth2"; "CallBoth3" ] do
                          Expect.stringContains
                              (declarationBodyOf source arm)
                              "abstract Invoke: e: string -> unit"
                              $"{arm} keeps the call signature both arms share"

                      Expect.stringContains
                          (declarationBodyOf source "IndexOne2")
                          "abstract Item: string -> obj with get, set"
                          "the arm keeps its index signature"

                      Expect.stringContains
                          (declarationBodyOf source "CtorOneConstructor")
                          "abstract Create: e: string -> obj"
                          "the arm keeps its construct signature"

                      Expect.stringContains
                          (declarationBodyOf source "CallSameIndexDiff2")
                          "abstract Item: string -> obj with get, set"
                          "each arm keeps its own string indexer"

                      Expect.stringContains
                          (declarationBodyOf source "CallSameIndexDiff3")
                          "abstract Item: float -> string with get, set"
                          "and its own number indexer"
```

The arm names follow today's decline naming (`b-sim/`: `CallOne2`, `CtorOneConstructor` from
`name-constructor-objects`). If an assertion fails on a name alone, read the golden and report
the name rather than changing the lab. The arms' `Create` overloads are not asserted: Task F
removes them from every callable arm.

In the third test case, change the expected TR060/TR061 list to
`[ "AnchoredOptions"; "Contested"; "IndexBoth"; "SeparableOptions" ]`.

In the `tagged-arm-lab` block (`:4568`), add a case to its list:

```fsharp
                  testCase "an arm's lone signature keeps the exclusive union erased" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type T1 = U2<" "T1 carries a call signature"
                      Expect.stringContains source "type T2 = U2<" "T2's index signature is on one arm only"
                      Expect.stringContains source "abstract Invoke: e: T1.Extra -> unit" "the arm reads its namespace member"
                      Expect.stringContains source "abstract Item: string -> U2<" "and T2's arm its indexer"
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on `[<EmitIndexer>]` in `IndexBoth` (the fold drops it), and on `type CallBoth = U2<`.

- [ ] **Step 3: Carry index signatures on the fold's shape; decline on call and construct signatures**

In `Spec.fs`, replace the `ExclusiveArmsShape` doc comment and type (`:614-624`) with:

```fsharp
/// A union of object arms exclusive on some of their members (§4.5, wave fourteen item 4):
/// every arm agrees, by name, type and optionality, on every member it shares with the rest,
/// and every member exclusive to one arm is typed `never` on every other arm that carries it -
/// never silently absent. No arm is callable or constructible, and every arm carries the same
/// index signatures, held once in `indexes`. `ExclusiveArmsFold` carries at least one arm whose
/// own member is required in that arm, giving F# a required-arity fact to resolve `Create`
/// overloads on; `ExclusiveArmsDecline` is the same shape where every arm's own members are
/// optional, so no such fact exists anywhere in the union.
type internal ExclusiveArmsShape =
    | NotExclusiveArms
    | ExclusiveArmsFold of shared: ResolvedMember list * arms: ExclusiveArm list * indexes: ResolvedIndex list
    | ExclusiveArmsDecline of arms: int
```

Directly after `isNeverTyped` (ends `:635`), add:

```fsharp
/// Whether two index signatures read as one `Item`.
let private sameIndex (model: ShapeModel) (left: ResolvedIndex) (right: ResolvedIndex) =
    left.KeyTypeId = right.KeyTypeId
    && left.IsReadonly = right.IsReadonly
    && sameShape model left.ValueTypeId right.ValueTypeId

/// The first arm's index signatures where every arm's pair with them by position.
let private indexesOnEveryArm (model: ShapeModel) (arms: TypeFacts list) : ResolvedIndex list option =
    match arms |> List.map _.IndexInfos with
    | [] -> Some []
    | first :: rest ->
        if
            rest
            |> List.forall (fun other -> other.Length = first.Length && List.forall2 (sameIndex model) first other)
        then
            Some first
        else
            None
```

In `exclusiveArmShape`, directly after `isObjectArm` (`:643-645`), add:

```fsharp
    let indexes = indexesOnEveryArm model arms
```

and replace

```fsharp
    if arms.Length < 2 || not (arms |> List.forall isObjectArm) then
        NotExclusiveArms
    else
```

with

```fsharp
    if arms.Length < 2 || not (arms |> List.forall isObjectArm) then
        NotExclusiveArms
    // The fold's `Create` overloads are where exclusivity holds, and an object literal is
    // neither callable nor constructible.
    elif
        arms
        |> List.exists (fun arm -> not (arm.CallSignatures.IsEmpty && arm.ConstructSignatures.IsEmpty))
        || indexes.IsNone
    then
        NotExclusiveArms
    else
```

and the fold's result (`:737`) `ExclusiveArmsFold(shared, armShapes)` with
`ExclusiveArmsFold(shared, armShapes, indexes.Value)`.

`Anonymous.fs:719` and `Orphans.fs:159` match `ExclusiveArmsFold _`, which still matches the
three-field case; leave them.

- [ ] **Step 4: Extract the indexer builder**

In `Spec.fs`, directly above the `shapeMembers` doc comment (`:3614`), add:

```fsharp
/// An index signature as an `[<EmitIndexer>] Item` member of `self`, reported as MB004.
let internal shapeIndexer
    (ctx: Context)
    (model: ShapeModel)
    (self: string)
    (info: ResolvedIndex)
    : FsMember * Finding list =
    let owner = $"{self}.[]"
    let key, keyFindings = typeRef ctx model (Some self) owner info.KeyTypeId
    let value, valueFindings = typeRef ctx model (Some self) owner info.ValueTypeId

    FsIndexer
        {
            Key = key
            Value = value
            ReadOnly = info.IsReadonly
        },
    keyFindings @ valueFindings @ [ Finding.make owner Members.IndexSignatureAsIndexer ]
```

and in `shapeMembers` replace the `indexers` binding (`:3912-3929`, keep its comment) with:

```fsharp
    let indexers =
        facts.IndexInfos
        |> List.map (fun info ->
            let indexer, indexerFindings = shapeIndexer ctx model self info
            findings <- findings @ indexerFindings
            indexer)
```

The finding order (key, value, MB004) is unchanged, so no golden moves from this step.

- [ ] **Step 5: The fold carries the shared index signatures**

In `Interfaces.fs`:
- `:80` becomes `| ExclusiveArmsFold(shared, arms, indexes) -> Some(typeId, name, Choice1Of2(shared, arms, indexes))`.
- In `foldedArmIds` (Task C's version), `| Choice1Of2(_, arms) ->` becomes `| Choice1Of2(_, arms, _) ->`.
- In `foldedDecls`, `| Choice1Of2(shared, arms) ->` becomes `| Choice1Of2(shared, arms, indexes) ->`.
  After `exclusiveMembers` is bound, add:

  ```fsharp
                                let indexers, indexerFindings =
                                    indexes |> List.map (shapeIndexer ctx model name) |> List.unzip
  ```

  set `Members = sharedMembers @ exclusiveMembers @ indexers` (the ordinary path's order:
  properties, then indexers), and extend the findings expression to:

  ```fsharp
                                (List.concat sharedFindings
                                 @ List.concat exclusiveFindings
                                 @ List.concat overloadFindings
                                 @ List.concat indexerFindings
                                 @ [ Finding.make name (TypeReference.ExclusiveArmsFolded arms.Length) ]))
  ```

`synthesize-paramobjects` skips a declaration with `CreateOverloads` (`ParamObjects.fs:146-153`),
so the fold's indexer does not raise SP003.

- [ ] **Step 6: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. `rtk git status tests/Xantham.Generator.Tests/golden`: `exclusive-arms-lab` and
`tagged-arm-lab` move, nothing else. Read both `.fs` diffs in full:
- `exclusive-arms-lab`: the new unions as asserted; pairs one to four unchanged from Task C.
- `tagged-arm-lab`: `T1`/`T2` become `U2<…>` over two minted arm interfaces each (one carrying
  `Invoke` or `Item`), and TR060 falls by 2. `T1.Extra`/`T2.Idx` stay declared in their namespace
  modules: the existing assertion at `:4762` still holds, with no DD003 on them. The other folds
  (`Choice`, `Pick`) are unchanged.
- `@cloudflare/workers-types`' three folds carry no signatures (`index.d.ts:4033`, `:4932`,
  `:4963`), so it must not move; if it does, stop and report.

Run: `dotnet build Xantham.slnx` → Expected: success.

- [ ] **Step 7: Fixture comment and phase record**

In `tests/fixtures/tagged-arm-lab/index.d.ts:99-101`, replace the comment with:

```ts
// A namespace member merged under an exclusive-shaped union is API, read or not; a top-level
// interface of the same name sits beside it. The call signature (`T1`) and index signature
// (`T2`) that read it sit on one arm only, so neither union folds.
```

Append to Task C's section in `docs/.ai/plans/generator-architecture.md`:

```markdown
`exclusiveArmShape` folds only where no arm carries a call or construct signature and every arm
carries the same index signatures (`Spec.sameShape` on the value, equal key and readonly). The
folded interface carries the shared ones as `[<EmitIndexer>] Item` (MB004) through
`Spec.shapeIndexer`, the builder the ordinary path uses. A callable or constructible arm makes the
union `NotExclusiveArms`: no object literal is callable, so the fold's `Create` overloads, where
its exclusivity holds, would build no value TypeScript accepts. `U2` keeps every arm exact.
`tagged-arm-lab`'s `T1` and `T2` stop folding (TR060 −2); no real fixture moved.
```

- [ ] **Step 8: Full suite and commit**

Run: `dotnet fsi build.fsx -- test` → Expected: PASS.

```
rtk git add src/Xantham.Generator/Shape/Spec.fs src/Xantham.Generator/Shape/Interfaces.fs tests/fixtures/exclusive-arms-lab/index.d.ts tests/fixtures/tagged-arm-lab/index.d.ts tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden/exclusive-arms-lab tests/Xantham.Generator.Tests/golden/tagged-arm-lab docs/.ai/plans/generator-architecture.md
rtk git commit -m "fix(generator): exclusive folds carry shared index signatures and decline callable arms" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task I2: An exclusive fold whose arms disagree on readonly stays erased

**Defect** (`i-158-residuals/findings.md` §I2): `classify` in `exclusiveArmShape` compares a
shared member's shape and optionality, and the head arm supplies `ReadOnly`. In `lab-readonly/`,
`R1` (readonly `id` on the first arm) folds to a get-only `abstract id: string`, and `R2` (readonly
on the second arm) folds to `with get, set`. Both raise TR060: the result depends on arm order.
**Ruling (controller):** `classify` also requires `m.ReadOnly = first.ReadOnly`, so such a union
is `NotExclusiveArms` and stays `U2` over exact arms. No new finding. `memberDeclaredBy`
(`Spec.fs:512-517`) keeps ignoring readonly: a DU case field has no setter, so the tagged fold
loses nothing there (`lab-readonly` `R3`). It gains a doc line saying so.

This task assumes Task B has landed (same function).

**Files:**
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (`classify` inside `exclusiveArmShape`, the `sameShape` forall at `:679`; doc of `memberDeclaredBy` `:511`; `ExclusiveArmsShape` doc)
- Modify: `tests/fixtures/exclusive-arms-lab/index.d.ts` (append), `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`exclusive-arms-lab` block)
- Modify: `docs/.ai/plans/generator-architecture.md` (Task C's section)

**Interfaces:**
- Consumes: Task B's `exclusiveArmShape`. Produces nothing new.

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/exclusive-arms-lab/index.d.ts`:

```ts

// A shared member readonly on one arm only: either fold would declare the head arm's setter
// or its lack for both, so neither folds.
export type ReadonlyFirst =
    | { readonly id: string; a: number; b?: never }
    | { id: string; b: string; a?: never };
export type ReadonlySecond =
    | { id: string; a: number; b?: never }
    | { readonly id: string; b: string; a?: never };
export function readonlyPairs(first: ReadonlyFirst, second: ReadonlySecond): void;
```

- [ ] **Step 2: Write the failing test**

Append to the `exclusive-arms-lab` test list:

```fsharp
                  testCase "a shared member readonly on one arm only keeps the union erased" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      Expect.stringContains source "type ReadonlyFirst = U2<" "the readonly-first pair does not fold"
                      Expect.stringContains source "type ReadonlySecond = U2<" "nor does the readonly-second pair"
```

The TR060/TR061 list from Task B already excludes both unions, and it stays unchanged.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on `type ReadonlyFirst = U2<` (both fold today).

- [ ] **Step 3: Compare readonly**

In `classify`, replace

```fsharp
                    if not (real |> List.forall (fun (_, m) -> sameShape model m.TypeId first.TypeId)) then
```

with

```fsharp
                    if
                        not (
                            real
                            |> List.forall (fun (_, m) ->
                                sameShape model m.TypeId first.TypeId && m.ReadOnly = first.ReadOnly)
                        )
                    then
```

In the `ExclusiveArmsShape` doc comment, change "agrees, by name, type and optionality," to
"agrees, by name, type, optionality and readonly,". Replace the doc comment of `memberDeclaredBy`
(`:511`) with:

```fsharp
/// Whether `arm` declares a member under the name, type and optionality of `m`. Readonly is not
/// compared: a tagged case field has no setter either way.
```

- [ ] **Step 4: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Only `exclusive-arms-lab` moves (the two new unions). Read its diff.

- [ ] **Step 5: Phase record, full suite, commit**

Append to Task C's section in `docs/.ai/plans/generator-architecture.md`:

```markdown
The exclusive fold also requires every arm to agree on a shared member's readonly; otherwise the
union stays `U2`, so the folded member's setter cannot depend on which arm comes first.
```

Run: `dotnet fsi build.fsx -- test` → Expected: PASS.

```
rtk git add src/Xantham.Generator/Shape/Spec.fs tests/fixtures/exclusive-arms-lab/index.d.ts tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden/exclusive-arms-lab docs/.ai/plans/generator-architecture.md
rtk git commit -m "fix(generator): exclusive folds require arms to agree on readonly" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task D: A merged value side is named for its role

**Defect** (`d-merged-alias-name/findings.md`): the export loop of `nameAnonymous`
(`Anonymous.fs:762-770`) walks an export's type side, then its value side, under one root. The
value side's own symbol name is the export's, already taken by the type side, so `claim`
(`Anonymous.fs:352-389`) hands out the first free `{wanted}{i+2}`: `Shape4`, `Point2`, `Level2`.
No finding records it, and numbers reserved by pruned arms leak into the name (Design evidence).
**Ruling (fidelity):** TypeScript has no spelling for the value-side interface; it writes
`typeof Point`. Two things follow.

- **The interface gets a generator-minted role name at the root.** An enum's object is
  `<Stem>Enum`, a namespace merged with a type is `<Stem>Namespace`, and a variable merged with a
  type is `<Stem>Value`. This mirrors `<Stem>Constructor` (`ConstructorObjects.fs:76-82`).
  `claim`'s numeric suffix applies only where the role name is taken, and a new `SY007`
  (Ergonomic) reports each role-named value side.
- **Shapes the namespace's values own nest under the source namespace.** An anonymous shape
  owned by a namespace's *values* (`Point.defaults: { x: number }`) is reached in TypeScript as
  `Point.defaults`. It nests under `module Point` (`Point.Defaults`), not under the minted
  interface. `claim`'s existing collision suffix applies only where `Point.Defaults` is already
  taken. An enum's object owns no anonymous shapes. A variable has no source namespace, so the
  shapes its value owns nest under the variable's own minted name (`TokenValue.<Member>`), the
  same position-derived rule as `Widget.Options`.

**Mechanism, verified against source.** `into` (`Anonymous.fs:648-649`) nests a type's children
under `Map.tryFind typeId nestBases`, falling back to the type's own name. `nestBases` is a
mutable map (`:308`) whose only other writer is the tagged-arm case-module path (`:729`), keyed by
arm ids, never an export's value id. Recording `nestBases[valueId] = root` before walking the
value side therefore redirects every child position (members, index values, signature parameters
and returns) to the namespace path. The value side's own name is unaffected. A child's `claim`
(`:352-389`) raises `SY004` for the dotted name. That is position-derived nesting, which the
companion plan's R6 keeps `SY004` for.

**Scope (planner ruling):** the role name applies when all of these hold:
- the export's symbol is an enum, a variable, or a `ValueModule`, and is neither a class nor a
  function;
- the export has both a declared and a value type;
- the value side would be declared here (`needsName`) and is not a constructor object
  (`isConstructorObject`, which `name-constructor-objects` names `<Stem>Constructor`);
- the bare root is already taken.

Where the bare root is free, the value side keeps the export's own name, its source spelling.
A pure namespace has no declared type and keeps its bare name. A class's static side stays with
`shape-classes`/`name-constructor-objects`. Functions are excluded here and admitted by Task F
(`f-function-merge/findings.md`): a function merged with a namespace is a hybrid value side, and
one merged with a type alone has a call-signature-only value that binds as a method.

**Public API:** renames `case-sanitise-lab` `Kind2`/`Collide2`/`Plain2` → `KindEnum`/`CollideEnum`/`PlainEnum`
and `literal-member-lab` `Level2` → `LevelEnum`, both hand-authored labs. No golden has a
suffixed variable or namespace value side (Design evidence grep). Core.TS and Node are expected
to be unchanged by this task; Step 7 proves it.

**Files:**
- Modify: `src/Xantham.Generator/Findings.fs` (`:197-202` table row; `SynthesizeAnonymous` `:918-952`)
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs:762-770`
- Create: `tests/fixtures/value-side-lab/index.d.ts`, `tests/fixtures/value-side-lab/package.json`
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (new block after `exclusive-arms-lab`'s)
- Modify: `tests/Xantham.Generator.Tests/Findings.test.fs:166` (pin)
- Modify: `docs/.ai/plans/generator-type-mapping.md` §4.14 (`:840-844`), `docs/.ai/plans/generator-architecture.md` (Task C's section)

**Interfaces:**
- Consumes: `declarationBodyOf` (Task C).
- Produces: `SynthesizeAnonymous.ValueSideNamed of typeSide: string * declaredAs: string`
  (`[<Ergonomic>]`, `SY.ValueSideNamed`, `SY007`), raised on `declaredAs`.

- [ ] **Step 1: Create the lab**

`tests/fixtures/value-side-lab/package.json`:

```json
{"name":"value-side-lab","type":"module","types":"index.d.ts","main":"index.js","description":"Hand-authored fixture: the name a merged export's value side is declared under - an enum's object, a namespace merged with a type and a const merged with a type are named for their role, a taken role name takes a number, a namespace's value-owned shapes nest under the namespace, and a namespace alone keeps its name."}
```

`tests/fixtures/value-side-lab/index.d.ts`:

```ts
// Hand-authored fixture: the name a merged export's value side is declared under.

// A tagged union merged with a namespace: its arms are dropped, its value side declared.
export type Shape = { kind: "circle"; radius: number } | { kind: "square"; side: number };
export namespace Shape {
    const version: string;
    function unit(): Shape;
}

// An interface merged with a namespace. The inline shape a namespace value owns nests under
// the namespace. The namespace type `Point.Anchor`, read by `pinned` ahead of `anchor`, keeps
// its spelling, and the value-owned shape `anchor` takes the collision number. The top-level
// `Anchor` makes `Point.Anchor` nest under today's naming as well.
export interface Point { x: number }
export interface Anchor { other: boolean }
export namespace Point {
    function origin(): Point;
    const defaults: { x: number };
    interface Anchor { at: number }
    const pinned: Anchor;
    const anchor: { at: string };
}

// An enum: the type side is the F# enum, the value side the enum object.
export enum Level {
    Low = 0,
    High = 1,
}

// The role name is itself an exported type, so the value side takes the next number.
export type Mode = { fast: boolean };
export namespace Mode {
    const fallback: Mode;
}
export interface ModeNamespace { reserved: string }

// A const merged with a type: no source namespace, so its value-owned shape nests under the
// minted value side.
export type Token = { value: string };
export declare const Token: { parse(text: string): Token; options: { strict: boolean } };

// Negative: a namespace alone keeps its name.
export namespace Config {
    const path: string;
}
```

- [ ] **Step 2: Register the lab with the failing test**

In `Pipeline.test.fs`, directly after the closing `)` of the `fixtureTests "exclusive-arms-lab"`
block, add a sibling:

```fsharp
        yield!
            fixtureTests "value-side-lab" (handFixture "value-side-lab") GeneratorConfig.Default (fun package ->
                [ testCase "a merged export's value side is named for its role, SY007" <| fun _ ->
                      let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                      let source = rendered.Files |> List.head |> snd

                      let messagesFor key =
                          rendered.Findings
                          |> List.filter (fun f -> f.Key = key)
                          |> List.map (fun f -> f.Symbol, f.Message)
                          |> List.sort

                      Expect.stringContains source "static member Shape: ShapeNamespace = jsNative" "a namespace merged with a tagged union"
                      Expect.isFalse (source.Contains "type Shape4") "no number left over from the dropped arms"
                      Expect.stringContains source "static member Point: PointNamespace = jsNative" "a namespace merged with an interface"

                      let pointValue = declarationBodyOf source "PointNamespace"
                      let pointModule = moduleBodyOf source "Point"

                      Expect.stringContains pointValue "abstract defaults: Point.Defaults" "a value-owned shape nests under the source namespace"
                      Expect.isFalse (source.Contains "PointNamespace.") "nothing nests under the minted value interface"
                      Expect.isFalse (source.Contains "Point2") "nor under a collision number"
                      Expect.stringContains pointModule "type Defaults =" "it is declared in the namespace's module"
                      Expect.stringContains pointValue "abstract pinned: Point.Anchor" "the namespace type keeps its spelling"
                      Expect.stringContains pointValue "abstract anchor: Point.Anchor2" "the value-owned shape it collides with takes the number"
                      Expect.stringContains pointModule "type Anchor2 =" "declared beside the namespace type"
                      Expect.stringContains source "static member Level: LevelEnum = jsNative" "an enum's value side"
                      Expect.stringContains source "static member Mode: ModeNamespace2 = jsNative" "a taken role name takes a number"

                      Expect.stringContains
                          (declarationBodyOf source "ModeNamespace")
                          "abstract reserved: string with get, set"
                          "and the exported type holding the role name is untouched"

                      Expect.stringContains source "static member Config: Config = jsNative" "a namespace alone keeps its name"
                      Expect.stringContains source "static member Token: TokenValue = jsNative" "a const merged with a type"

                      Expect.stringContains
                          (declarationBodyOf source "TokenValue")
                          "abstract options: TokenValue.Options"
                          "a variable has no source namespace; its value-owned shape nests under its own name"

                      Expect.equal
                          (messagesFor "SY007")
                          [ "LevelEnum", "Level's value side declared as LevelEnum; Level is the type side"
                            "ModeNamespace2", "Mode's value side declared as ModeNamespace2; Mode is the type side"
                            "PointNamespace", "Point's value side declared as PointNamespace; Point is the type side"
                            "ShapeNamespace", "Shape's value side declared as ShapeNamespace; Shape is the type side"
                            "TokenValue", "Token's value side declared as TokenValue; Token is the type side" ]
                          "each role-named value side is reported, and only those" ])
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on `static member Shape: ShapeNamespace` (renders `Shape4`). If the build fails
on `SY007` not existing, that is Step 3; the test text needs no finding type.

- [ ] **Step 3: The finding**

In `Findings.fs`, after the row `"SY.CallbackDelegateNamed", "SY006"` (`:202`), add:

```fsharp
            "SY.ValueSideNamed", "SY007"
```

In `SynthesizeAnonymous`, after the `CallbackDelegateNamed` case (`:938`), add:

```fsharp

    /// A merged export's value side, which TypeScript spells only as `typeof X`, declared under
    /// its role (`<Stem>Enum`, `<Stem>Namespace`, `<Stem>Value`) beside the type side holding the
    /// export's own name, and numbered only where the role name is taken. `Exports.Shape` reads
    /// `ShapeNamespace`.
    | [<Ergonomic>] ValueSideNamed of typeSide: string * declaredAs: string
```

and to its `Message` match:

```fsharp
            | ValueSideNamed(typeSide, declaredAs) ->
                $"{typeSide}'s value side declared as {declaredAs}; {typeSide} is the type side"
```

In `Findings.test.fs`, after `"SY.CallbackDelegateNamed SY006 exact"` (`:166`), add:

```fsharp
                    "SY.ValueSideNamed SY007 ergonomic"
```

- [ ] **Step 4: Name the value side**

In `Anonymous.fs`, directly above `let fallback = defaultExportName ctx` (`:759`), add:

```fsharp
    /// The role an export's value side is named for beside its type side, and whether the
    /// shapes its values own nest under the source namespace: an enum's object, a namespace or a
    /// variable merged with a type. A class's static side and a function are not value sides
    /// this pass names.
    let valueSideRole (export: HarvestedExport) =
        let flags = export.Symbol.Flags

        if hasAny (SymbolFlags.Class ||| SymbolFlags.Function) flags then
            None
        elif hasAny (SymbolFlags.RegularEnum ||| SymbolFlags.ConstEnum) flags then
            Some("Enum", true)
        elif hasAny SymbolFlags.ValueModule flags then
            Some("Namespace", true)
        elif hasAny (SymbolFlags.FunctionScopedVariable ||| SymbolFlags.BlockScopedVariable) flags then
            Some("Value", false)
        else
            None
```

and replace the export loop's `| Some ids ->` arm (`:767-769`) with:

```fsharp
        | Some ids ->
            ids.Declared |> Option.iter (walk root export.Order)

            match ids.Declared, ids.Value with
            | Some declared, Some value when declared <> value ->
                match valueSideRole export, Map.tryFind value model.Types with
                | Some(role, underNamespace), Some facts when
                    Set.contains root taken && needsName facts && not (isConstructorObject facts)
                    ->
                    let declaredAs = claim None $"{root}{role}" value export.Order

                    findings <-
                        findings
                        @ [ Finding.make declaredAs (SynthesizeAnonymous.ValueSideNamed(root, declaredAs)) ]

                    // TypeScript reaches a namespace value's shape as `Point.defaults`: it
                    // belongs to the namespace's module, not to the minted value interface.
                    if underNamespace then
                        nestBases <- Map.add value root nestBases
                | _ -> ()
            | _ -> ()

            ids.Value |> Option.iter (walk root export.Order)
```

The walk of a value side named here skips `needsName` (`Anonymous.fs:481`, the id is in
`names`). For an enum or namespace, `into` (`Anonymous.fs:648-649`) reads `nestBases` first, so
its children claim `Point.Defaults`. A child whose path is taken (`Point.Anchor`, claimed by the
namespace type that `pinned` reads first) takes `claim`'s number (`Point.Anchor2`). A variable's
children nest under its claimed name (`TokenValue.Options`). `name-exports` runs first
(`Passes.fs:11-12`), so an exported `ModeNamespace` is already in `taken` and `claim` numbers the
value side. `isConstructorObject` is `Spec`'s (used at `Interfaces.fs:283`). Leaving
constructor objects to `name-constructor-objects` keeps lib-style
`declare var X: { new (): X }` merges on `<Stem>Constructor`.

If the `Point.Anchor`/`Point.Anchor2` assertions fail only because the walk reaches `anchor`
before `pinned` (member order), report it rather than reordering the lab. The companion plan's
harvest names `Point.Anchor` in `name-exports`, ahead of every walk, which makes the outcome
independent of member order.

- [ ] **Step 5: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. `rtk git status tests/Xantham.Generator.Tests/golden`: the new `value-side-lab`,
plus `case-sanitise-lab` (`Kind2`/`Collide2`/`Plain2` → `KindEnum`/`CollideEnum`/`PlainEnum`, three
`SY007`) and `literal-member-lab` (`Level2` → `LevelEnum`, one `SY007`). Read all three diffs in
full; their manifests' ergonomic counts grow by the `SY007` symbols. Under this task, `solid-js`
`JSX2` is a pure namespace and must not move. Any other golden moving is a stop-and-report,
including one from a variable merged with a type: the grep found none suffixed.

- [ ] **Step 6: Phase records**

In `docs/.ai/plans/generator-type-mapping.md` §4.14, after the bullet ending
`… two exports of one name with no namespace between them.` (`:840-842`), add:

```markdown
  - **Landed, #158 follow-up D.** TypeScript spells a merged export's value side only as
    `typeof X`. It is named for its role beside the type side holding the export's name: an
    enum's object `<Stem>Enum`, a namespace merged with a type `<Stem>Namespace`, and a variable
    merged with a type `<Stem>Value`. This mirrors a class static side's `<Stem>Constructor`.
    `SY.ValueSideNamed` reports each one, and a taken role name takes a number
    (`ModeNamespace2`). An anonymous shape owned by a namespace's value nests under the
    namespace's module (`Point.Defaults`), the spelling TypeScript reaches it by; it is numbered
    only where that module already holds the name.
```

Append to Task C's section in `docs/.ai/plans/generator-architecture.md`:

```markdown
Where the bare name is taken, `synthesize-anonymous` names an enum's value side `<Stem>Enum`, a
namespace merged with a type `<Stem>Namespace`, and a variable merged with a type `<Stem>Value`
(`SY007`), instead of the first free number. The number had also counted arms that
`drop-orphan-delegates` later prunes (`Shape4` beside a DU whose `Shape2`/`Shape3` were
dropped). The shapes an enum's or namespace's values own nest under the source namespace
(`nestBases[value] = root`): `Point.Defaults`, or `Point.Anchor2` where the namespace type
`Point.Anchor` holds the name. Renamed in the goldens: `case-sanitise-lab` `KindEnum`,
`CollideEnum`, `PlainEnum`; `literal-member-lab` `LevelEnum`. This change leaves Core.TS and Node
unchanged.
```

- [ ] **Step 7: Prove Core.TS and Node unchanged**

```
dotnet fsi build.fsx -- generate --only compiler-lib
dotnet fsi build.fsx -- generate --only node-lib
rtk git status src/Xantham.Fable.Core.TS src/Xantham.Fable.Node
```

Expected: clean. If either moves, the change renames a published public type: stop and report
the renamed declarations. The task then commits the regenerated bindings separately as
`feat(bindings)!: role-named value sides in Core.TS/Node` with a `BREAKING CHANGE:` footer
listing each rename. The packages' `CHANGELOG.md` files are generated from commits, so the
footer is the entry.

- [ ] **Step 8: Full suite and commit**

Run: `dotnet fsi build.fsx -- test` → Expected: PASS. Run: `dotnet build Xantham.slnx` → Expected: success.

```
rtk git add src/Xantham.Generator/Findings.fs src/Xantham.Generator/Shape/Anonymous.fs tests/fixtures/value-side-lab tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/Findings.test.fs tests/Xantham.Generator.Tests/golden/value-side-lab tests/Xantham.Generator.Tests/golden/case-sanitise-lab tests/Xantham.Generator.Tests/golden/literal-member-lab docs/.ai/plans/generator-type-mapping.md docs/.ai/plans/generator-architecture.md
rtk git commit -m "fix(generator): name merged value sides for their role, SY007" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

---

### Task F: A function merged with a namespace binds as the hybrid it is

**Defect** (`f-function-merge/findings.md`): `Shape/Exports.fs:74`
(`| Some facts when not facts.CallSignatures.IsEmpty ->`) emits one method per call signature for
every callable value export. It never reads the value type's `Members`, `IndexInfos` or
`ConstructSignatures` (`Model.fs:1278-1283`).
- A function merged with a namespace that holds values (`function P` + `namespace P { const
  version }`) loses `P.version`/`P.helper` with no finding. The same applies to a callable `const`
  carrying properties (`trigger: Trigger`).
- `nameAnonymous` still declares the hybrid interface (`P`, `Q2`, `H2`), and nothing reads it.
- Node carries ~76 such unread hybrids, among them every `__promisify__`.

**Ruling (controller): Option A.** The method arm applies only where the value type carries call
signatures and nothing else. Every other callable value falls through to the existing value arm:
`static member P: <hybrid> = jsNative`, called through `Invoke` (SI008). Task D's role rule widens
from "neither a class nor a function" to "not a class": `QNamespace` where the stem is taken, and
the bare hybrid name (`Stamp`) where it is free, with SY007. A function with no namespace values
keeps the method form. No new finding.

**In scope (controller): `Create` on a callable declaration is refused.** TypeScript accepts no
object literal for a callable type (`const t: Trigger = { label: "" }` does not type-check), so
`synthesize-paramobjects` refuses with a new SP003 reason. That covers a hybrid whose call
signatures reach `Invoke`, and one whose call signatures are lost because a member is named
`Invoke` (SI001; `Collides`). The SI001 case leaves no `FsInvoke` on the declaration, so the pass
reads callability from the declaration's type facts, not from its members.

**Planner rulings:**
- **`collides` binds as a property.** Its member is named `Invoke`, and the call is the SI001
  loss already reported on `Collides`. Today the method form loses the property instead. The
  property form is the one every other position reading `Collides` already gets. *Cost if wrong:*
  a consumer calling `collides` needs `emitJsExpr`; they could not read `Invoke` before.
- **Breaking.** `@types/node` callers of a function merged with a namespace (`fs.exists`,
  `child_process.exec`, …) re-spell `f x` as `f.Invoke x`, and every callable hybrid loses
  `Create` (Node 64, Core.TS 2, by an awk count over the bindings). Core.TS and Node are
  regenerated in their own commits with `BREAKING CHANGE:` footers.

This task assumes Task D has landed (`valueSideRole`).

**Files:**
- Modify: `src/Xantham.Generator/Shape/Exports.fs:74` (method-arm guard)
- Modify: `src/Xantham.Generator/Shape/Anonymous.fs` (`valueSideRole`, from Task D)
- Modify: `src/Xantham.Generator/Shape/ParamObjects.fs` (`Reason` `:14-34`; `refusal` `:66-95`; the pass `:137-160`)
- Modify: `tests/fixtures/callable-hybrid-lab/{index.d.ts,index.js}`
- Modify: `tests/Xantham.Generator.Tests/Pipeline.test.fs` (`callable-hybrid-lab` block `:1189`)
- Modify: `tests/Xantham.Generator.RunGate/Program.fs:1980-2027` (`callableHybrids`)
- Modify: `docs/.ai/plans/generator-architecture.md` (Task C's section), `docs/.ai/plans/generator-type-mapping.md` §4.4 or §4.13 (function + namespace merging)
- Regenerate: goldens; `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**` (own commits)

**Interfaces:**
- Consumes: Task D's `valueSideRole: HarvestedExport -> (string * bool) option`, `SY007`.
- Produces: `ParamObjects.Reason.Callable` (private literal). No public signature changes.

- [ ] **Step 1: Extend the lab**

Append to `tests/fixtures/callable-hybrid-lab/index.d.ts`:

```ts

// ---------------------------------------------------------------------------
// A function merged with a namespace that holds a value is a hybrid: the export is that object,
// called through Invoke. A function standing alone, or merged with a type alone, stays a method.
// ---------------------------------------------------------------------------

export function stamp(s: string): number;
export namespace stamp {
    const version: string;
}

export interface Merged { m: string }
export function Merged(): Merged;
export namespace Merged {
    const version: string;
}

export function solo(x: string): number;

export type Fn = { value: number };
export function Fn(value: number): Fn;
```

Append to `tests/fixtures/callable-hybrid-lab/index.js`:

```js

export const stamp = Object.assign((s) => s.length, { version: "1.0" });

export function Merged() {
    return { m: "made" };
}
Merged.version = "2.0";

export function solo(x) {
    return x.length;
}

export function Fn(value) {
    return { value };
}
```

- [ ] **Step 2: Write the failing tests**

Append to the `callable-hybrid-lab` test list (`Pipeline.test.fs:1189`):

```fsharp
                    testCase "a callable value carrying members binds whole, called through Invoke" <| fun _ ->
                        let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                        let source = rendered.Files |> List.head |> snd
                        let merged = declarationBodyOf source "MergedNamespace"

                        Expect.stringContains source "static member stamp: Stamp = jsNative" "a function merged with a namespace"
                        Expect.stringContains (declarationBodyOf source "Stamp") "abstract version: string" "keeps its namespace value"
                        Expect.stringContains (declarationBodyOf source "Stamp") "abstract Invoke: s: string -> float" "and its call"
                        Expect.stringContains source "static member Merged: MergedNamespace = jsNative" "the stem the interface holds takes the role name"
                        Expect.stringContains merged "abstract Invoke: unit -> Merged" "the call returns the interface"
                        Expect.stringContains merged "abstract version: string" "beside the namespace value"
                        Expect.isFalse (source.Contains "type Merged2") "no collision number"
                        Expect.stringContains source "static member trigger: Trigger = jsNative" "a callable const binds its declared hybrid"
                        Expect.stringContains source "static member collides: Collides = jsNative" "a hybrid whose call is the SI001 loss binds the same"
                        Expect.stringContains source "static member solo (x: string) : float = jsNative" "a function alone stays a method"
                        Expect.stringContains source "static member Fn (value: float) : Fn = jsNative" "a function merged with a type alone stays a method"

                        Expect.contains
                            (rendered.Findings |> List.filter (fun f -> f.Key = "SY007") |> List.map _.Symbol)
                            "MergedNamespace"
                            "the role name is reported"

                    testCase "a callable declaration has no Create" <| fun _ ->
                        let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                        let source = rendered.Files |> List.head |> snd

                        for name in [ "Trigger"; "Multi"; "Ambiguous"; "Identity"; "Collides"; "Stamp"; "MergedNamespace" ] do
                            Expect.isFalse
                                ((declarationBodyOf source name).Contains "static member Create")
                                $"{name} is callable, which no object literal is"

                            Expect.equal
                                (rendered.Findings
                                 |> List.filter (fun f -> f.Symbol = name && f.Key = "SP003")
                                 |> List.map _.Message)
                                [ "no ParamObject Create synthesized: the declaration is callable, which no object literal is" ]
                                $"{name}'s refusal is reported"

                        Expect.isFalse
                            ((declarationBodyOf source "Boxed<'T>").Contains "static member Create")
                            "a generic hybrid the same"

                        Expect.stringContains
                            (declarationBodyOf source "Widget")
                            "static member Create (handler: Widget.Handler) : Widget = jsNative"
                            "a declaration holding a callable member is not itself callable"
```

`declarationBodyOf source "Boxed<'T>"` matches the header `type Boxed<'T> =`.

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on `static member stamp: Stamp = jsNative` (rendered as a method today).

- [ ] **Step 3: Bind a callable value carrying members as the value**

In `Exports.fs`, replace `| Some facts when not facts.CallSignatures.IsEmpty ->` (`:74`) with:

```fsharp
                                // A function's value is a method only where its call signatures
                                // are all it carries; one merged with a namespace holding values
                                // is a hybrid object, bound whole and called through `Invoke`.
                                | Some facts when
                                    not facts.CallSignatures.IsEmpty
                                    && facts.Members.IsEmpty
                                    && facts.IndexInfos.IsEmpty
                                    && facts.ConstructSignatures.IsEmpty
                                    ->
```

In `Anonymous.fs`'s `valueSideRole` (Task D), replace

```fsharp
        if hasAny (SymbolFlags.Class ||| SymbolFlags.Function) flags then
            None
```

with

```fsharp
        if hasAny SymbolFlags.Class flags then
            None
```

and its doc comment's last sentence with "A class's static side is not a value side this pass
names." A function merged with a namespace now reads `Namespace` (its symbol carries
`ValueModule`). A function merged with a type alone matches no role and keeps today's naming.
Its value type is call signatures alone, so it binds as a method and nothing reads a value-side
declaration.

- [ ] **Step 4: Refuse Create on a callable declaration**

In `ParamObjects.fs`'s `Reason` module, add:

```fsharp
    [<Literal>]
    let Callable = "the declaration is callable, which no object literal is"
```

Give `refusal` a leading `(callable: bool)` parameter, and make its first branch after the
`Entrypoint` check:

```fsharp
    elif callable || decl.Members |> List.exists isInvoke then
        Some Reason.Callable
```

In the pass body, before `let decls =`, add:

```fsharp
                    // A declaration whose type is callable, whether or not its call signatures
                    // reached `Invoke` (SI001 leaves none when a member is named `Invoke`).
                    let callable =
                        model.DeclNames
                        |> Map.toList
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when not facts.CallSignatures.IsEmpty -> Some name
                            | _ -> None)
                        |> Set.ofList
```

and call `refusal (Set.contains decl.Name callable) decl members`.

- [ ] **Step 5: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS. Read every moved golden's `.fs` diff:
- `callable-hybrid-lab`: the export members become properties, and every callable hybrid loses
  `Create`.
- `callable-overloads-lab` (`ledger`), `overload-arity-lab` (`setter`), `type-scope-lab`
  (`tagged`), `solid-js` (`h`): the property form plus the `Create` refusals.
- `exclusive-arms-lab` and `tagged-arm-lab`: callable arms (`CallOne2`, `CallBoth2`/`3`,
  `CallSameIndexDiff2`/`3`, `T1`'s arm) lose `Create`.

Any other golden moving is read and reported in the commit body. If an existing assertion pins an
old `static member <callable> (…)` method form or a callable `Create`, re-pin it to the new form
and list it in the commit body.

- [ ] **Step 6: The run gate calls through Invoke**

In `Program.fs`, replace the doc comment and body of `callableHybrids` (`:1980-2027`) with:

```fsharp
/// §4.4's call side: a hybrid's own call signature reaches Invoke, so `x.Invoke(a)` compiles to
/// `x(a)` rather than a method call named `Invoke`, mirroring `[<EmitConstructor>]` on the
/// construct side. A callable export carrying members is bound as that object, so its call and
/// its members read off the one value; a function standing alone stays a direct static method.
let private callableHybrids () =
    let widget = CallableHybridLab.Exports.widget

    equal
        "a member-position hybrid's Invoke reaches the function it is read off"
        "handled:ping"
        (widget.handler.Invoke "ping")

    equal "and its sibling property still reads off the same object" true widget.handler.enabled

    equal "a named-declaration hybrid's export calls through Invoke" 42.0 (CallableHybridLab.Exports.trigger.Invoke 21.0)
    equal "and reads its property off the same function" "double" CallableHybridLab.Exports.trigger.label
    equal "an overloaded hybrid's Invoke reaches the two-argument arity" 7.0 (CallableHybridLab.Exports.multi.Invoke(3.0, 4.0))
    equal "and the one-argument arity" 4.0 (CallableHybridLab.Exports.multi.Invoke 3.0)

    equal
        "a hybrid whose overloads collided on their parameters calls the one function, under the union of their returns"
        30.0
        (unbox<float> (CallableHybridLab.Exports.ambiguous.Invoke 3.0))

    equal "a call signature's own type parameter reaches through generically" "id" (CallableHybridLab.Exports.identity.Invoke "id")
    equal "a generic interface's call signature reads its own type parameter" 42.0 (CallableHybridLab.Exports.boxedNumber.Invoke())

    equal
        "a hybrid whose member is already named Invoke reads that member; its call is the SI001 loss"
        "not-a-function"
        CallableHybridLab.Exports.collides.Invoke

    equal "a function merged with a namespace calls through Invoke" 3.0 (CallableHybridLab.Exports.stamp.Invoke "abc")
    equal "and reads the namespace's value off the same function" "1.0" CallableHybridLab.Exports.stamp.version
    equal "an interface-merged function calls through Invoke" "made" (CallableHybridLab.Exports.Merged.Invoke()).m
    equal "and reads its namespace value" "2.0" CallableHybridLab.Exports.Merged.version
    equal "a function standing alone stays a method" 5.0 (CallableHybridLab.Exports.solo "hello")
    equal "as does one merged with a type alone" 4.0 (CallableHybridLab.Exports.Fn 4.0).value
```

Run: `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"`
Expected: PASS. If another run-gate call site fails to compile because its export is now a
property, re-spell it `.Invoke` and list it in the commit body.

- [ ] **Step 7: Phase records**

Append to Task C's section in `docs/.ai/plans/generator-architecture.md`:

```markdown
`shape-exports` binds a callable value export as a method only where its value type carries
call signatures alone. A function merged with a namespace holding values, or a callable `const`
carrying properties, binds as that hybrid object (`static member stamp: Stamp`) and is called
through `Invoke`. Its value side is role-named like any merged value side (`MergedNamespace`,
`SY007`; Task D's rule now excludes classes only). `synthesize-paramobjects` refuses `Create` on
any declaration whose type is callable (SP003 "the declaration is callable…"), including one
whose call is the SI001 loss: no object literal is callable. Node: ~76 function+namespace
exports move to the property form, and 64 callable hybrids lose `Create`; Core.TS loses 2.
```

In `docs/.ai/plans/generator-type-mapping.md` §4.13, after the "Declaration merging" bullet, add:

```markdown
- A function merged with a namespace that holds values is one hybrid value: the export binds as
  the object (`Exports.stamp: Stamp`), its call through `Invoke` and its namespace values as
  members (#158 follow-up F). A function with no namespace values stays a static method per
  signature. A callable declaration has no `Create`: TypeScript accepts no object literal for it.
```

- [ ] **Step 8: Full suite and commit the generator**

Run: `dotnet fsi build.fsx -- test` → Expected: PASS. Run: `dotnet build Xantham.slnx` → Expected: success.

```
rtk git add src/Xantham.Generator/Shape/Exports.fs src/Xantham.Generator/Shape/Anonymous.fs src/Xantham.Generator/Shape/ParamObjects.fs tests/fixtures/callable-hybrid-lab tests/Xantham.Generator.Tests/Pipeline.test.fs tests/Xantham.Generator.Tests/golden tests/Xantham.Generator.RunGate/Program.fs docs/.ai/plans/generator-architecture.md docs/.ai/plans/generator-type-mapping.md
rtk git commit -m "fix(generator): bind callable values carrying members as hybrids; refuse Create on callables" -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

- [ ] **Step 9: Regenerate Core.TS, then Node, each as a breaking commit**

```
dotnet fsi build.fsx -- generate --only compiler-lib
rtk git diff --stat src/Xantham.Fable.Core.TS
```

Expected: the 2 callable hybrids lose `Create`; `symbols.jsonl` gains their SP003. Nothing else
moves; if anything else does, stop and report. Then:

```
rtk git add src/Xantham.Fable.Core.TS
rtk git commit -m "feat(bindings)!: Core.TS callable declarations have no Create" -m "BREAKING CHANGE: <list the two declarations from the diff> no longer carry a ParamObject Create; TypeScript accepts no object literal for a callable type." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
dotnet fsi build.fsx -- generate --only node-lib
rtk git diff --stat src/Xantham.Fable.Node
```

Fill the `BREAKING CHANGE:` footer from the diff. It is a list of names, not prose to invent:
- for Core.TS, the two declarations;
- for Node, the export members now bound as properties (count, plus the first ten names), and
  the count of `Create` removals.

Expected for Node: about 76 function+namespace exports become properties, about 64 `Create`s
are removed, and the hybrids declared but unread today are now read. Commit:

```
rtk git add src/Xantham.Fable.Node
rtk git commit -m "feat(bindings)!: Node functions merged with namespaces bind as hybrids" -m "BREAKING CHANGE: <count> function exports merged with a namespace (fs.exists, child_process.exec, ...) are bound as the hybrid object and called through .Invoke; <count> callable declarations no longer carry a ParamObject Create." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

The packages' `CHANGELOG.md` files are generated from commits by the release tooling (their
front matter carries `last_commit_released`). Do not hand-edit them: the footer is the entry.

---

### Task G: A Create writes each member as TypeScript requires it present

**Defect** (`g-null-create/findings.md`): `ResolvedMember.Optional` (`Model.fs:1183`) is
consumed by `optionalRef` and then lost: `FsPropertyMember` (`Model.fs:1452`) and `FsTaggedField`
(`Model.fs:1685`) carry no presence. Both `Create`s infer omittability from the type:
- `ParamObjects.parameterFor` (`ParamObjects.fs:102-112`)
- `Render.caseCreate` (`Render.fs:1073-1091`)
- `TaggedUnions.fs:119-125` (`optionalOrLiteral`)

The `unionRef` hoist (`Spec.fs:2594-2612`) folds `null`, `undefined` and `?` into one `FsOption`.
As a result, required `x: T | null` and `x: T | undefined` become omittable `?x` parameters, which
build objects TypeScript rejects. `x?: undefined` and `x?: never` bind a `?x: unit` parameter. The
run gate pins the bug (`Program.fs:405-418`: `Slot.Create()` describes `"absent"` for
`value: string | null`).

**Ruling (controller): Option 1.**
- A `Presence` on `FsPropertyMember` and `FsTaggedField`, read by both `Create`s and by the
  `Create<Case>` decision:
  - a required `T | null` member stays required, and `None` is written `null` through a
    normalising Emit;
  - a `?: undefined` member binds no parameter and is reported with a new `SP006`;
  - a required `T | undefined` member stays required, with its key present.
- Core.TS (~378 fields) and Node (~88) regenerate in their own commits with `BREAKING CHANGE:`
  footers: `Create` signatures change.

**Planner rulings:**
- **A required `T | null | undefined` member is `UndefinedWhenAbsent`**, so the key is present
  and `None` is `undefined`. Both values satisfy the type, and `undefined` is what Fable's `None`
  already is. *Cost if wrong:* a consumer wanting `null` passes it through `emitJsExpr`.
- **A `?: never` member is `AbsentOnly` too.** The checker collapses it to `undefined` (the
  `isNeverTyped` note, `Spec.fs:625-635`), and TypeScript accepts no value for it. This removes
  the 21 `?x: unit` parameters, including the exclusive-arm-lab arms (`?count: unit`). *Cost if
  wrong:* a consumer can no longer pass `()` there; TypeScript accepted no value either.
- **A tagged case gets `Create<Case>` wherever any field is not plain `Present`.** That is the
  only constructor that writes `null`/present keys: the DU case constructor omits a `None` field's
  key (`Model.fs:1689-1695`'s note). **Residual, not fixed here:** the DU case constructor itself
  still builds an invalid object for a required `T | null` field. Fable controls that emission. It
  is reported in this plan's hand-off, not silently accepted.
- **`DT005`'s message changes** to say what `Create<Case>` writes. That churns every DT005
  `symbols.jsonl` line (message only).

This task is independent of B–F, except that it runs last so that it regenerates Core.TS and Node
once over their final shape.

**Files:**
- Modify: `src/Xantham.Generator/Model.fs` (`FsPresence` above `FsPropertyMember` `:1452`; `FsPropertyMember`; `FsTaggedField` `:1685`; `FsCreateOverload` `:1626-1632`)
- Modify: `src/Xantham.Generator/Shape/Spec.fs` (`presenceOf` after `splitNullish` `:154-160`; property construction `:3840-3848`, `:3877-3885`)
- Modify: `src/Xantham.Generator/Shape/TaggedUnions.fs:80-160`, `src/Xantham.Generator/Shape/Interfaces.fs:379-440`, `src/Xantham.Generator/Shape/ParamObjects.fs:97-230`
- Modify: `src/Xantham.Generator/Render.fs` (`createAttribute` `:563-593`; `caseCreate` `:1073-1102`)
- Modify: `src/Xantham.Generator/Customization/Contract.fs:553-561`, `src/Xantham.Generator/DeclarationCatalog.fs:1149-1157` (record construction)
- Modify: `src/Xantham.Generator/Findings.fs` (`:228-232` table row; `SynthesizeParamObjects` `:1104-1128`; `CaseCreateSynthesized` message `:886-887`)
- Modify: `tests/Xantham.Generator.Tests/{Render,Shape,ResolvedProjection}.test.fs` (record literals), `Findings.test.fs:196` (pin), `Pipeline.test.fs` (`fable-workaround-lab` block `:3938-3970`)
- Modify: `tests/fixtures/fable-workaround-lab/{index.d.ts,index.js}`
- Modify: `tests/Xantham.Generator.RunGate/{Probes.fs,Program.fs}` (`probes` `:726`; `workarounds` `:398-418`)
- Modify: `docs/.ai/fable5-workarounds.md` §1 (`:49-95`), `docs/.ai/plans/generator-type-mapping.md` §4.3, `docs/.ai/plans/generator-architecture.md` (Task C's section)
- Regenerate: goldens; `src/Xantham.Fable.Core.TS/**`, `src/Xantham.Fable.Node/**` (own commits)

**Interfaces:**
- Produces:
  ```fsharp
  type FsPresence = Present | Omittable | AbsentOnly | NullWhenAbsent | UndefinedWhenAbsent   // Model.fs
  // FsPropertyMember and FsTaggedField gain: Presence: FsPresence
  // FsCreateOverload gains: AbsentValues: (string * FsPresence) list
  val internal presenceOf: ShapeModel -> bool -> int<Measure.typeId> -> FsPresence             // Spec.fs
  // SynthesizeParamObjects.AbsentOnlyMembersOmitted of members: string   (SP.AbsentOnlyMembersOmitted, SP006, Exact)
  ```

- [ ] **Step 1: Probe what Fable writes for a required option argument**

Append to `tests/Xantham.Generator.RunGate/Probes.fs`:

```fsharp

// What a ParamObject writes for `None` in a required option parameter, and the normalising Emit
// a required `T | null` member needs if the answer is not `null`.

[<Interface>]
type NullableSlot =
    abstract value: string option with get, set

    [<ParamObject; Emit("$0")>]
    static member Create(value: string option) : NullableSlot = jsNative

    [<ParamObject; Emit("(o => ({ ...o, value: o['value'] ?? null }))($0)")>]
    static member CreateNormalised(value: string option) : NullableSlot = jsNative
```

At the end of `probes ()` in `Program.fs` (`:726`), add:

```fsharp
    let plain = Probes.NullableSlot.Create(value = None)
    let noneValue: string option = None
    let viaValue = Probes.NullableSlot.Create(value = noneValue)

    printfn
        "probe: required None writes key=%b undefined=%b null=%b; through a value key=%b"
        (emitJsExpr plain "\"value\" in $0")
        (emitJsExpr plain "$0.value === undefined")
        (emitJsExpr plain "$0.value === null")
        (emitJsExpr viaValue "\"value\" in $0")

    equal
        "the normalising Emit writes null for a None held in a value"
        true
        (emitJsExpr (Probes.NullableSlot.CreateNormalised(value = noneValue)) "\"value\" in $0 && $0.value === null")

    equal
        "the normalising Emit writes null for None"
        true
        (emitJsExpr (Probes.NullableSlot.CreateNormalised(value = None)) "\"value\" in $0 && $0.value === null")

    equal "and keeps a present value" (Some "a") (Probes.NullableSlot.CreateNormalised(value = Some "a")).value
```

Run: `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"`
Expected: PASS, and one `probe:` line. Record it in the commit body of Step 9, and pick the
variant Step 5 uses:
- **V1** `key=true undefined=true`: Fable writes the key. `UndefinedWhenAbsent` needs no
  normaliser; only `NullWhenAbsent` keys are rewritten.
- **V2** `key=false`: Fable omits the key. Both `NullWhenAbsent` and `UndefinedWhenAbsent` keys
  are rewritten.
- **V3** `null=true`: Fable already writes `null`, so the plain `$0` is correct for null.
  `UndefinedWhenAbsent` would then be wrong. Stop and report; this plan does not cover it.

Spike evidence (`2026-10-15-pre158-findings.md`, Fable spikes S2, Fable 5.17.2 from a DLL): a
literal `None` omits the key (`({})`), and a `None` held in a value writes it as `undefined`
(`({ k: noneVar })`). The expected probe line is therefore `key=false … through a value key=true`,
which selects **V2**. A probe in which either form omits the key selects V2; V1 holds only when
both forms write it. The normalising IIFE wrote `null` for both forms in the spike.

- [ ] **Step 2: Extend the lab**

Append to `tests/fixtures/fable-workaround-lab/index.d.ts`:

```ts

// --- Presence a Create must write (§1). -----------------------------------------------------

export interface Presence {
    value: string | null;
    opt?: string;
    undef?: undefined;
    reqUndef: string | undefined;
}

export type Presented =
    | { kind: "a"; value: string | null; opt?: string; undef?: undefined; reqUndef: string | undefined }
    | { kind: "b"; other: number };

/** `"absent"`, `"null"`, `"undefined"` or `"string"`, read off `key` as JavaScript sees it. */
export declare function describeKey(o: object, key: string): string;

export declare function present(p: Presence, q: Presented): void;
```

Append to `tests/fixtures/fable-workaround-lab/index.js`:

```js

export function describeKey(o, key) {
    if (!(key in o)) return "absent";
    return o[key] === null ? "null" : o[key] === undefined ? "undefined" : "string";
}

export function present() {}
```

- [ ] **Step 3: Write the failing tests**

In the `fable-workaround-lab` block (`Pipeline.test.fs:3944`), add a case to its list:

```fsharp
                      testCase "a Create writes each member as TypeScript requires it present, SP006" <| fun _ ->
                          let rendered = Async.RunSynchronously(Pipeline.generate GeneratorConfig.Default package)
                          let source = rendered.Files |> List.head |> snd
                          let presence = declarationBodyOf source "Presence"

                          Expect.stringContains
                              presence
                              "static member Create (value: string option, reqUndef: string option, ?opt: string) : Presence = jsNative"
                              "required nullable members are required parameters; a `?: undefined` member binds none"

                          Expect.stringContains presence "value: o['value'] ?? null" "None writes null where the type admits null alone"

                          Expect.stringContains
                              (declarationBodyOf source "Slot")
                              "static member Create (value: string option) : Slot = jsNative"
                              "the workaround document's Slot"

                          Expect.stringContains
                              source
                              "static member CreateA (value: string option, reqUndef: string option, ?opt: string) : Presented = jsNative"
                              "a tagged case's Create the same"

                          Expect.stringContains source "(o => ({ kind: 'a', ...o, value: o['value'] ?? null" "with the tag written first"

                          Expect.equal
                              (rendered.Findings
                               |> List.filter (fun f -> f.Key = "SP006")
                               |> List.map (fun f -> f.Symbol, f.Message))
                              [ "Presence", "Create omits undef: a type admitting only undefined binds no parameter" ]
                              "the omission is reported"
```

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: FAIL on the `Presence` Create signature (renders `?value: string, ?undef: unit, ?reqUndef: string`).

- [ ] **Step 4: The model carries presence**

In `Model.fs`, directly above `type FsPropertyMember`, add:

```fsharp
/// How an object TypeScript accepts carries a property.
type FsPresence =
    /// Required, with a value.
    | Present
    /// Written with `?`: the key may be left out.
    | Omittable
    /// Written with `?` under a type admitting only `undefined` (`?: undefined`, `?: never`): the
    /// key is left out, and a `Create` binds no parameter for it.
    | AbsentOnly
    /// Required, under a type admitting `null` and not `undefined`: the key is written, `null`
    /// where the F# value is `None`.
    | NullWhenAbsent
    /// Required, under a type admitting `undefined`: the key is written, `undefined` where the
    /// F# value is `None`.
    | UndefinedWhenAbsent
```

Add `Presence: FsPresence` after `Type: FsTypeRef` in `FsPropertyMember` and in `FsTaggedField`.
In `FsCreateOverload`, after `Literals`, add:

```fsharp
        /// Required keys whose `None` the Emit writes as a value (`NullWhenAbsent`,
        /// `UndefinedWhenAbsent`), in parameter order.
        AbsentValues: (string * FsPresence) list
```

In `Spec.fs`, directly after `splitNullish` (`:154-160`), add:

```fsharp
/// How an object TypeScript accepts carries a member, from its `?` marker and its type.
let internal presenceOf (model: ShapeModel) (optional: bool) (typeId: int<Measure.typeId>) : FsPresence =
    match Map.tryFind typeId model.Types with
    | Some facts when optional && (flag TypeFlags.Never facts || flag TypeFlags.Undefined facts) -> AbsentOnly
    | _ when optional -> Omittable
    | Some facts when flag TypeFlags.Union facts ->
        let absence = absenceAcross model (fst (splitNullish model facts))

        if absence.FromUndefined || absence.FromVoid then UndefinedWhenAbsent
        elif absence.FromNull then NullWhenAbsent
        else Present
    | _ -> Present
```

Set `Presence` at every construction:
- `Spec.fs:3840-3848` (property branch): `Presence = presenceOf model m.Optional effectiveTypeId`.
- `Spec.fs:3877-3885` (symbol-keyed): `Presence = Present`.
- `Interfaces.fs` `buildMember` (`:393-404`): `Presence = (if optional then Omittable else presenceOf model m.Optional m.TypeId)`.
- `TaggedUnions.fs:98-104` (case field): `Presence = presenceOf model m.Optional m.TypeId`.
- `Contract.fs:553-561`: `Presence = (match p.Type with FsOption _ -> Omittable | _ -> Present)`, which keeps today's behaviour for a customization replacement whose `?` the contract does not carry.
- `DeclarationCatalog.fs:1149-1157` (export value as property): `Presence = Present`.
- Test record literals in `Render.test.fs`, `Shape.test.fs`, `ResolvedProjection.test.fs`: `Presence = Omittable` where `Type` is `FsOption _`, `Present` otherwise. Add `AbsentValues = []` to every `FsCreateOverload` record expression (`Render.test.fs:432`, `:576`, `:1208`, `:1231`, `:1282`). Record patterns (`Shape.test.fs:3686`, …) need no change.

`DeclarationCatalog`'s surface (`:1484-1545`) names property fields explicitly and excludes
`CreateOverloads`, so the catalogue API hash is unchanged: no `CatalogCompatibility` bump.

- [ ] **Step 5: Create reads presence**

In `ParamObjects.fs`:
- `parameterFor`'s `FsProperty` arm: `Optional = (p.Presence = Omittable)`.
- In the pass, replace `let bound = members |> List.filter (isLiteralMember >> not)` with:

  ```fsharp
                                    let absentOnly, bound =
                                        members
                                        |> List.filter (isLiteralMember >> not)
                                        |> List.partition (function
                                            | FsProperty p -> p.Presence = AbsentOnly
                                            | _ -> false)

                                    let absentValues =
                                        bound
                                        |> List.choose (function
                                            | FsProperty p when p.Presence = NullWhenAbsent || p.Presence = UndefinedWhenAbsent ->
                                                Some(p.Name, p.Presence)
                                            | _ -> None)
  ```

  Add `AbsentValues = absentValues` to the overload. Append to `findings` when `absentOnly` is non-empty:

  ```fsharp
                                        @ (if absentOnly.IsEmpty then
                                               []
                                           else
                                               [
                                                   Finding.make
                                                       decl.Name
                                                       (SynthesizeParamObjects.AbsentOnlyMembersOmitted(
                                                           absentOnly
                                                           |> List.choose (function
                                                               | FsProperty p -> Some p.Name
                                                               | _ -> None)
                                                           |> String.concat ", "
                                                       ))
                                               ])
  ```

In `Interfaces.fs` `buildParam` (`:379-391`): `Optional = (presenceOf model m.Optional m.TypeId = Omittable)`.
In the fold's overload builder (`:416-439`), filter out shared members whose presence is
`AbsentOnly` before `buildParam`. Set `AbsentValues` from the remaining shared and own members
whose presence is `NullWhenAbsent`/`UndefinedWhenAbsent`, as `(Naming.memberName m.Symbol.Name, presence)`.

In `TaggedUnions.fs`, replace `optionalOrLiteral` (`:119-125`) with:

```fsharp
                                                        // The case constructor omits a `None` field's key, so only
                                                        // `Create<Case>` writes `null`, a present key, or leaves an
                                                        // `?` key out; a literal field it writes itself.
                                                        let needsCreate =
                                                            fields
                                                            |> List.exists (fun field ->
                                                                field.Literal.IsSome || field.Presence <> Present)
```

and rename its two uses (`wantsCreate`, the DT007 payload at `:152`).

In `Render.fs` `caseCreate` (`:1073-1102`): filter fields with `field.Literal.IsNone && field.Presence <> AbsentOnly`;
`Optional = (field.Presence = Omittable)`; and
`AbsentValues = fields |> List.choose (fun f -> if f.Presence = NullWhenAbsent || f.Presence = UndefinedWhenAbsent then Some(f.Name, f.Presence) else None)`
(over the filtered fields).

In `Render.fs` `createAttribute` (`:563-593`), add after `literals`:

```fsharp
    // A required key whose `None` must reach JavaScript as a value: `null` for a type admitting
    // `null` alone; under V2, the present `undefined` key too.
    let absentWrites =
        overload.AbsentValues
        |> List.choose (fun (key, presence) ->
            match presence with
            | NullWhenAbsent -> Some $"{jsKey key}: o[{jsString key}] ?? null"
            | UndefinedWhenAbsent -> Some $"{jsKey key}: o[{jsString key}]"   // V1: None (Fable writes the key)
            | Present
            | Omittable
            | AbsentOnly -> None)
```

Under **V1** (Step 1), the `UndefinedWhenAbsent` arm returns `None` instead. In the bound
branch, replace the `match literals, parameters with` block with the following. The first,
second and last arms are today's arms. The `[], []` arm keeps today's `Emit("$0")` for a `Create`
with no parameters:

```fsharp
        match literals, parameters with
        | [], [] -> "[<ParamObject; Emit(\"$0\")>]"
        | literals, [] ->
            let expression = "({ " + String.concat ", " literals + " })"
            $"[<Emit({stringLit expression})>]"
        | literals, _ when not absentWrites.IsEmpty ->
            // The parameter object is read once, then its absent required keys written.
            let expression =
                "(o => ({ " + String.concat ", " (literals @ [ "...o" ] @ absentWrites) + " }))($0)"

            $"[<ParamObject; Emit({stringLit expression})>]"
        | [], _ -> "[<ParamObject; Emit(\"$0\")>]"
        | literals, _ ->
            let expression = "({ " + String.concat ", " (literals @ [ "...$0" ]) + " })"
            $"[<ParamObject; Emit({stringLit expression})>]"
```

In the positional branch (`else`), replace `let field = $"{stringLit parameter.Name}: ${index}"`
with:

```fsharp
                let field =
                    match List.tryFind (fst >> (=) parameter.Name) overload.AbsentValues with
                    | Some(_, NullWhenAbsent) -> $"{stringLit parameter.Name}: (${index} ?? null)"
                    | _ -> $"{stringLit parameter.Name}: ${index}"
```

A required positional argument is always written, so `UndefinedWhenAbsent` needs nothing there.

In `Findings.fs`:
- after the row `"SP.LiteralMembersWrittenByCreate", "SP005"`, add `"SP.AbsentOnlyMembersOmitted", "SP006"`;
- in `SynthesizeParamObjects`, add after `LiteralMembersWrittenByCreate`:

  ```fsharp
      /// Members whose type admits only `undefined` (`?: undefined`, `?: never`): `Create` binds no
      /// parameter for them and leaves their keys out, the one value TypeScript accepts.
      | [<Exact>] AbsentOnlyMembersOmitted of members: string
  ```

  with message `| AbsentOnlyMembersOmitted members -> $"Create omits {members}: a type admitting only undefined binds no parameter"`;
- change the `CaseCreateSynthesized` message (`:886-887`) to
  `$"case Create '{create}': ? fields omittable, literal fields and absent required values written"`.

In `Findings.test.fs`, after `"SP.LiteralMembersWrittenByCreate SP005 exact"` (`:196`), add
`"SP.AbsentOnlyMembersOmitted SP006 exact"`.

- [ ] **Step 6: Run and pass**

Run: `dotnet fsi build.fsx -- test --quick --update --filter "generator e2e"`
Expected: PASS once the re-pins below are done.

Assertions in other blocks that pin the old `?x` form for a required nullable member, or a
`?x: unit` parameter, fail. Each one is re-pinned to the faithful signature and listed in the
commit body. Known: `exclusive-arms-lab` arms' `?count: unit` / `?label: unit` (`WidgetArm`,
`GadgetArm`), and `exclusive-signature-lab` `?html: unit` / `?url: unit`.

Read the golden diffs by measure for the large rungs (`rtk git diff --stat`, plus a sample of ten
`Create` lines from each of `@cloudflare/workers-types`, `solid-js` and `animejs`). Read the lab
diffs in full. Expected churn, from the findings: ~83 required `T | null` fields in workers, ~70
required `T | undefined`, ~39 solid-js, 7 animejs, and every `DT005` message line. More
tagged cases gain `Create<Case>` (any case with a required nullable or undefined-admitting
field). Count them in the commit body (`DT005` before and after, from `build.fsx -- findings`).

- [ ] **Step 7: Replace the run-gate check that pins the bug**

In `Program.fs` `workarounds`, replace `:405-418` (from `equal "None omits the property rather than writing null"`
through the `asNull` check) with:

```fsharp
    equal
        "a None reaches a `string | null` member as the null it declares"
        "null"
        (FableWorkaroundLab.Exports.describe (FableWorkaroundLab.Slot.Create(value = None)))

    equal "and Some as the string" "string" (FableWorkaroundLab.Exports.describe (FableWorkaroundLab.Slot.Create(value = Some "a")))

    let presence = FableWorkaroundLab.Presence.Create(value = None, reqUndef = None)
    equal "a required nullable member is written null" "null" (FableWorkaroundLab.Exports.describeKey (box presence, "value"))
    equal "a required undefined-admitting member is present" "undefined" (FableWorkaroundLab.Exports.describeKey (box presence, "reqUndef"))
    equal "an omitted `?` member is absent" "absent" (FableWorkaroundLab.Exports.describeKey (box presence, "opt"))
    equal "a `?: undefined` member is absent" "absent" (FableWorkaroundLab.Exports.describeKey (box presence, "undef"))

    let presented = FableWorkaroundLab.Presented.CreateA(value = None, reqUndef = None)
    equal "a tagged case's Create writes its tag" "a" (emitJsExpr presented "$0.kind")
    equal "and its required nullable field as null" "null" (FableWorkaroundLab.Exports.describeKey (box presented, "value"))
    equal "its required undefined-admitting field present" "undefined" (FableWorkaroundLab.Exports.describeKey (box presented, "reqUndef"))
    equal "and leaves its `?` fields out" "absent" (FableWorkaroundLab.Exports.describeKey (box presented, "opt"))
    equal "including `?: undefined`" "absent" (FableWorkaroundLab.Exports.describeKey (box presented, "undef"))
```

The `explicitNull` checks that follow (`:420` on) stay: they cover a member with no `Create`.
Remove the `NullableSlot` probe and its `probes ()` lines from Step 1. The generated
`Slot.Create` now carries the answer.

Run: `dotnet fsi build.fsx -- test --quick --run-gate --filter "generator e2e"`
Expected: PASS.

- [ ] **Step 8: Documentation and phase records**

In `docs/.ai/fable5-workarounds.md` §1:
- In "What the binding emits", replace the `Create` line with `static member Create (value: string option) : Slot = jsNative`,
  and the attribute line above it with the normalising Emit the golden now carries (copy it from
  `tests/Xantham.Generator.Tests/golden/fable-workaround-lab/FableWorkaroundLab.fs`).
- In "What Fable 5 does with it", replace the last sentence ("Writing is one-way…") with:
  "Writing through `Create` is exact: a required `string | null` member takes `value: string option`,
  and `None` reaches JavaScript as `null` through the `Create` Emit."
- In "The workaround", delete the `asNull` lines and the `Slot.Create(?value = asNull)` call. Keep
  the `createObj` literal for a member with no `Create`, and keep the reading-direction example.

In `docs/.ai/plans/generator-type-mapping.md` §4.3, append:

```markdown
- `Create` keeps TypeScript's presence: a required `T | null` member is a required
  `T option` parameter whose `None` the Emit writes as `null`. A required `T | undefined` is
  required, with its key present. `x?: T` is `?x`, and a `?` member whose type admits only
  `undefined` (`?: undefined`, `?: never`) binds no parameter (`SP006`). A tagged case's
  `Create<Case>` follows the same rule, and exists wherever a field is not plainly present: the
  case constructor omits a `None` field's key (#158 follow-up G).
```

Append to Task C's section in `docs/.ai/plans/generator-architecture.md`:

```markdown
`FsPropertyMember` and `FsTaggedField` carry `Presence` (`Spec.presenceOf`, from the `?` marker
and the hoisted absence spellings), so the `Create`s stop inferring omittability from `FsOption`.
A required key whose `None` must be a value is written by a normalising Emit
(`(o => ({ ...o, k: o['k'] ?? null }))($0)`), which evaluates the parameter object once. Fable's
handling of a required `None` argument was probed in the run gate first (<V1 or V2>).
```

- [ ] **Step 9: Full suite and commit the generator**

Run: `dotnet fsi build.fsx -- test` → Expected: PASS. Run: `dotnet build Xantham.slnx` → Expected: success.

```
rtk git add src/Xantham.Generator tests/Xantham.Generator.Tests tests/fixtures/fable-workaround-lab tests/Xantham.Generator.RunGate docs/.ai/fable5-workarounds.md docs/.ai/plans/generator-type-mapping.md docs/.ai/plans/generator-architecture.md
rtk git commit -m "fix(generator): Create writes members with the presence TypeScript requires" -m "Run-gate probe: <the probe line from Step 1>. Re-pinned: <the assertions Step 6 re-pinned>." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

- [ ] **Step 10: Regenerate Core.TS, then Node, each as a breaking commit**

```
dotnet fsi build.fsx -- generate --only compiler-lib
rtk git diff --stat src/Xantham.Fable.Core.TS
rtk git add src/Xantham.Fable.Core.TS
rtk git commit -m "feat(bindings)!: Core.TS Create keeps required nullable members required" -m "BREAKING CHANGE: <count from the diff> Create parameters for required T | null members are now required T option (None writes null); <count> ?x: unit parameters for ?: undefined/never members are removed." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
dotnet fsi build.fsx -- generate --only node-lib
rtk git diff --stat src/Xantham.Fable.Node
rtk git add src/Xantham.Fable.Node
rtk git commit -m "feat(bindings)!: Node Create keeps required nullable members required" -m "BREAKING CHANGE: <count from the diff> Create parameters for required T | null and T | undefined members are now required T option; <count> ?x: unit parameters are removed." -m "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01LySb2Kjdc8qoY9Qoikr2u5"
```

Expected: Core.TS ≈378 changed `Create` parameters, Node ≈88 plus its `T | undefined` ones. The
`<count>` fields are numbers read off the diff (`git diff -U0 | grep -c "static member Create"`
over the changed lines), not estimates. Run `dotnet build Xantham.slnx` after each regeneration;
a consumer compile gate failing on a call site that relied on omission is expected only in
consumer code outside the repository. A failure inside the repository is reported.
