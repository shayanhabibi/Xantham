# Pre-#158 defects: plans so far and open decisions

**Checkpoint, 2026-10-15.** Investigation of the defects older than #158 is finished, and four
plans are written. Nothing has been executed. The work is paused on the user decisions in
§3. Branch `feat/tagged-arm-158` (PR #160, base `develop`, open and unmerged) carries these docs
as local commits `65c63a5f` and `c83ee8e6`, above the pushed PR head `91742f98`. They are not pushed.
Pushing, merging, publishing and opening issues or PRs each need the user's explicit approval.

## 1. Records

- `docs/.ai/plans/2026-10-15-pre158-findings.md` holds the evidence for every plan. It covers
  investigations A–I, each with its repro, root cause, scope, fix options and test plan, plus the
  Fable consumption spikes S1–S3. Labs, outputs and spike projects are in the gitignored
  `tests/.scratch/pre158/<slug>/`, which exists only in the main checkout.
- The #158 ledger is `tests/.scratch/tagged-arm-158-ledger.md` (gitignored).
- Frozen generator CLI at `91742f98`:
  `dotnet tests/.scratch/xantham-cli/xantham.dll generate <dir> -o <out>`.

## 2. Plan of record

The user approved this order. Each plan's header lists the interfaces it consumes from the plans
before it.

| Order | Where | Covers | State |
|---|---|---|---|
| 1 | PR #160 | I1, I3, I4, I6, I7, I8 | Not started. Blocked on decision D1, which may reshape DT004/DT007 in #160 itself. |
| 2 | `plans/2026-10-15-exclusive-fold-and-value-side-fixes.md` (plan 1) | C, B, I2, D, F, G | Written. D2 and D3 are open. |
| 3 | `plans/2026-10-15-namespace-member-harvest.md` (plan 2) | A; H option 2 (R11, `MintedNames` provenance pruning); E's narrowed R3 seed; Task 3b (R12), which writes lib and referenced namespace members under their namespace | Written. API 8 → 9. Core.TS and Node are regenerated as breaking changes. |
| 4 | `plans/2026-10-15-global-augmentation.md` (plan 3) | E Option 1: globals of a module package, and foreign additions written as per-member `[<Emit>]` extensions | Written. API 9 → 10. Node is regenerated as a breaking change. D4 is open. |
| 5 | `plans/2026-10-15-generic-alias-applications.md` (plan 4) | H option 1: an object-alias application is written as an application (`Wrap<'T>`), not a per-position copy | Written. Estimate: 44 copies in the goldens, none in Core.TS or Node. API → 11 only if a release shipped after plan 3. |

### PR #160 fixes (not yet applied)

Each item has its section in the findings record (§I).

- **I1:** make `sameShape`'s union branch mutual inclusion. Land it before plan 1 Task B.
- **I3:** restore a non-empty shipped group to `catalog-recursive-json-lab`.
- **I4:**
  - A shared `caseCarriesArmWhole` predicate: index signatures and symbol-keyed members mean an arm is not carried whole.
  - DD001 versus DD003, decided over the initial readers.
- **I6:** DT007 message wording.
- **I7:** the case constructor of a folded case that builds no arm. D1 supersedes it.
- **I8:** DD002/DD003 doc comments.

I5 (dropped declarations keep their `symbols.jsonl` rows) is a separate follow-up and is not
scheduled.

### Cross-plan facts already reconciled (`c83ee8e6`)

- `ApiVersion` sequence: plan 2 takes it to 9 and plan 3 to 10. Plan 4 takes it to 11, and only if a release shipped in between.
- Plan 3 Task 4 is the API 10 bump. Its first step confirms that plan 2 Task 3b landed.
- Construct signatures are written as instance extension members, never static. Array owners extend `'T[]`. `ResizeArray<'T>` cannot be augmented (FS0964).
- Plan 1 G Step 1 records the spike result: a literal `None` omits the key and a `None` held in a value writes `undefined`. That selects V2. The probe tests both forms.
- Plan 3's corrections to E:
  - The mixed-run `declare var` defect does not reproduce.
  - The `[<Global(name)>]` setter that emits `g(3)` is a Fable defect the generator never triggers.

  Plan 3 Tasks 2 and 3 pin both.

## 3. Open decisions (the user's)

The standing rule applies: *the most faithful option to the TS source, always*. Churn,
back-compat and scope are measures to report, never reasons for a ruling.

### D1 — Tagged unions whose arms share a tag value

**Ruled 2026-10-20: option (a).** The Fable fork adds `CaseTest` (`tools/fable-fork/fork.json`,
`Fable.Core` 5.999.0-xantham.ea2e7f8d7.2 on the Xantham feed), and PR #160 implements it per
`docs/.ai/plans/2026-10-20-shared-tag-case-tests.md`. DT004 retires, DT007 and I6/I7 are
superseded. The record below is the decision as it stood.

**Cases.** Seven lab cases:
- shared-tag-lab: `Terminal`, `OnEvent.Event`;
- tagged-arm-lab: `Task`, `Run`, `Job`, `Report`, `Outcome`.

One real-world case: workers-types `TailStream` `Log`. Core.TS and Node have none.

**Current behaviour.** The 2026-09-06 lane-CB rule (`a0084902`) merges the arms into one case that
carries only the members every arm declares. That loses members, and the case builds an object no
arm accepts. PR #160's DT007, its `Create<Case>` gating and `caseCarriesArm` all patch around this
rule.

**How TypeScript tells the arms apart.** Every existing case can be told apart, by a second literal
discriminant, then `in` presence, then `typeof`.

**Fable facts (spike S1).**
- Today's `TypeScriptTaggedUnion`, given two cases with the same `CompiledName`, compiles with no
  warning but matches wrongly. The later case owns the `case "done"` slot and the first is
  unreachable. This is a live bug.
- An erased union with Emit predicates and downcasts works from a DLL.
- A complete active pattern works only from a package that ships `fable/` sources. Separate
  companion package: works. Single mixed package: fails.

**Options.**
- (a) Upstream Fable gains a per-case test on `TypeScriptTaggedUnion`, giving real DU cases. A Fable-repo session has the handover; see §5.
- (b) A DLL binding with Emit predicates and downcasts, plus a companion source package holding the active patterns.
- (c) An erased `U<n>` of exact arm interfaces, with Emit predicates and no pattern.

**Independent of the choice.** Stop emitting a duplicate-tag DU.

**Effect on PR #160.** The choice decides what happens to DT004, DT007 and I7 there.

### D2 — Plan 1 Task B: call and construct signatures on fold arms

The planner ruled that any call or construct signature on an arm declines the fold to `U2`, and
that only shared index signatures are carried (MB004).

**Conflict.** This reverses the user's earlier Option 2 for call signatures.

**The planner's reason.** Plan 1 F refuses `Create` on callables, because TypeScript accepts no
object literal for them. A callable fold would then have no `Create`, and `Create` is the only
place exclusivity holds.

**Decide.** Confirm or restore Option 2.

### D3 — Plan 1 G: required `T | null` fields

- **The `null` rewrite.** `docs/.ai/fable5-workarounds-user-response.md` records a ruling that the
  null/undefined/absent distinction is the consumer's problem. G's `?? null` normaliser in `Create`
  writes `null` for a required `T | null` field, which cuts against that.
  - G's key-presence half does not conflict: a required field becomes a required parameter and
    its key is always written.
  - Decide whether G keeps the normaliser or only the key-presence fix.
- **The DU case residual.** A tagged case's constructor omits the key of a `None` field. Fable
  controls that emission, and spike S2 confirms it. Options:
  - a Widened finding;
  - decline the DU for such unions;
  - accept it, given that `Create<Case>` writes the key.
- **Planner rulings to confirm (plan 1 G):**
  - A required `T | null | undefined` field is undefined-when-absent.
  - `?: never` counts as absent-only, which removes 21 `?x: unit` parameters.
  - SP006 is raised for interface `Create` only.
  - `CatalogCompatibility` gets no bump.

### D4 — Plan 3 P1: the globals container

**Ruled 2026-10-20: keep `Exports`.** The user: "Shouldnt we have Exports". `module Globals`
holds `[<Erase; Global("globalThis")>] type Exports`, as plan 3 P1 specifies.

Plan 3 keeps the existing `module Globals` + `[<Erase>][<Global("globalThis")>] type Exports`.

- **Why:** the run gate already proves that form, and a root `type Globals` would collide with
  animejs's exported `Globals` interface.
- **Alternative:** a root `type Globals`.
- **Decide:** confirm.

## 4. Unverified

- Every plan's expected strings are reasoned, not observed. Each plan's first task measures or
  spikes before code changes.
- Plan 4's copy count (44) is an estimate. Its Task 1 measures it.
- From spike S1/S2/S3, still untested:
  - constructing the first duplicate-tag case;
  - shape (b)/(d) through a ProjectReference;
  - `fable precompile` as the pattern host;
  - object-expression and SRTP visibility for extensions other than properties.
- `.serena/project.yml` is modified locally and unrelated. Never stage it.

## 5. Fable-repo session

The user started a Claude session in `C:\Users\shaya\RiderProjects\fable` and gave it the handover
`docs/.ai/handovers/fable-shared-tag-upstream.md`. That file covers:
- the shared-tag problem and TypeScript's narrowings;
- the DLL constraints;
- the candidate per-case discriminant attribute on `TypeScriptTaggedUnion`;
- the `[<Erase>]` extension failure;
- the `[<Global(name)>]` setter bug.

It is waiting for the user's instruction. Its outcome feeds D1 option (a).

## 6. Resuming

1. Read this file, then the findings record. Read a plan only once its decision is settled.
2. Take the user's rulings on D1–D4 and record each in the plan it changes, in the ledger form
   `Ruling: <decision> — <why> — <cost if wrong>`.
3. Apply the PR #160 fixes, then execute plans 1–4 in order. Each plan has its own branch suggestion.
4. Repo rules:
   - Test only via `dotnet fsi build.fsx -- test …`.
   - Goldens change only via `--update`, and every diff is read before commit.
   - Prefix shell commands with `rtk`.
   - Put scratch work under `tests/.scratch/`.
   - Follow `comments.md`.
   - A new finding code needs a case, a `FindingCodes.table` row and a `Findings.test.fs` pin.
