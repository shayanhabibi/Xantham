# Shared-tag tagged unions as one case per arm (PR #160, D1 option a)

**Goal:** a tagged union whose arms share a tag value declares one DU case per arm, each carrying
`[<CaseTest>]` from the vendored Fable fork, instead of folding the arms into one case that keeps
only the members they agree on. Lands in PR #160 together with the remaining #160 fixes.

**Spec:** `docs/.ai/handovers/pre158-plans-and-open-decisions.md` §3 D1, the findings record
`docs/.ai/plans/2026-10-15-pre158-findings.md` §I and spike S1, and the fork's `CaseTestAttribute`
(`Fable.Core` 5.999.0-xantham.ea2e7f8d7.2, pinned in `tools/fable-fork/fork.json`).

## Rulings

- Ruling: D1 is option (a), `CaseTest` from the Fable fork. — The user directed the work onto
  the vendored fork ("continue work on feat/tagged-arm-158 using the vendored fable fork"). —
  If wrong: the DU emission reverts to an erased `U<n>` of arm interfaces (option c).
- Ruling: within a tag group, the test is chosen in TypeScript's narrowing order, one strategy
  for the whole group: (1) a second literal discriminant, (2) `in` presence, (3) `typeof`. —
  Matches the order TS narrows in (fable brief §2). — If wrong: a group picks a different but
  still exclusive test; behaviour is unchanged.
- Ruling: a group no strategy separates is not a DU; the union stays an erased union under
  DT003 `TagValueShared`, its message reworded. — An arm the generator cannot tell apart has no
  faithful case. — If wrong: such unions lose pattern matching they could have had.
- Ruling: a union whose arms *all* share one tag value (`Onset`, workers `TailStream` `Log`)
  is a DU when its arms separate. — Same fidelity reason; `TagCollides` keeps only the
  inseparable case. — If wrong: those unions revert to erased unions.
- Ruling: case names keep the existing `uniqueCaseNames` convention: `Done`, `Done2`. — Names
  carry no fidelity; this is the convention every other collision already uses. — If wrong:
  a rename of the second and later cases, no shape change.
- Ruling: DT004 `ArmsMergedOnSharedTag` and DT007 `FoldedCaseOmitsArmMembers` retire (rows kept,
  cases removed). Nothing folds any more. I6 and I7 are superseded. New DT008
  `ArmsSeparatedByCaseTest(tag, value)`, Exact, names each separated group.
- Ruling: a generic tagged alias whose cases carry a `CaseTest` does not match an instance arm
  by tag value (`armsCorrespond` requires every case test empty). — Correspondence by value is
  ambiguous inside a group. — If wrong: such an application renders as a per-position copy, the
  pre-existing H behaviour.

- Ruling (during execution): a required boolean-literal field of a tagged case is a literal
  field, written by `Create<Case>`. — `ok: true` / `ok: false` is what separates `Task`'s arms;
  as a plain `bool` parameter, `CreateDone(ok = false)` builds the other arm. — If wrong: the
  case takes `ok` as a parameter again. Interface `Create` keeps string and number literals only;
  extending it is a follow-up with corpus-wide churn.
- Ruling (during execution): DT007 is deleted outright rather than retired. — It never reached
  `develop`, so no manifest carries its code.

## Follow-ups found during execution

- A union exported as `U` mints arm names `U2`, `U3`, …, which shadow `Fable.Core`'s erased
  unions in the same file: `type U = U3<U2, U3, U4>` reads the arm `U3`, not `Fable.Core.U3`.
  The lab spells the I1 reproducer `Mirror` instead; the naming hazard stands for any package
  that exports `U`.
- Interface `Create` writes no boolean literal (above).

## The test forms

`FsCaseTest` is structural; Render writes the JavaScript with `jsString` and member access
`$0.k` for an identifier name, `$0["k"]` otherwise.

| Strategy | Applies when | Arm test |
|---|---|---|
| Literal | member `k` on every arm of the group, typed one string/number/boolean literal each, values pairwise distinct; at most one arm optional | required: `$0.k === v`; optional: `($0.k === v \|\| $0.k === undefined)` |
| Presence | every arm but at most one has a required member no other group arm declares | own: `'m' in $0`; the one without: `!('m1' in $0) && …` over one own member of each other arm |
| Typeof | member `k` required on every arm, each type in one `typeof` class (string, number, boolean, bigint, symbol, function, object without null), classes distinct | `typeof $0.k === "string"` |

Expected per lab case: `Terminal`, `Run`, `Report` presence; `Task` literal `ok`; `Job`,
`Outcome` presence with one negated arm; `Onset` literal `truncated`, its optional arm
`($0.truncated === false || $0.truncated === undefined)`.

## Tasks

1. **I1 `sameShape` mutual inclusion** (`Spec.fs:479`): the union branch requires inclusion
   both ways and drops the length check. Lab: I1's U/V and W/Y pairs added to
   `tagged-arm-lab`.
2. **Findings**: add DT008, retire DT004/DT007 cases (rows stay), reword DT003; pins in
   `Findings.test.fs`.
3. **Model + Spec**: `FsCaseTest` and `FsTaggedCase.CaseTest`; `Discriminated` carries one
   entry per arm with its test; `sharedTagTests`; drop `foldedArm`, `caseConstructsArm`;
   `taggedCaseNames` maps each arm to its own case; `isTaggedDeclaration` and `TagCollides`
   follow the rulings.
4. **TaggedUnions.fs**: one case per arm, `CaseTest` set, `Create<Case>` without the
   constructibility gate, DT008 per separated group.
5. **Render**: `[<CompiledName(..); CaseTest(..)>]` on a case with a test.
6. **Orphans (I4)**: `caseCarriesArmWhole` — every non-tag member a case field, `IndexInfos`
   empty, no symbol-keyed member — decides whether an arm interface drops; DD001 versus DD003
   is decided over the initial readers. I8 doc comments on DD002/DD003.
7. **I3**: restore a non-empty shipped group in `catalog-recursive-json-lab` (`Payload` +
   `unwrap`, per §I3).
8. **Labs and goldens**: `shared-tag-lab` and `tagged-arm-lab` comments rewritten for the new
   behaviour, I4's `F`/`G` added; goldens via `--update`; run gate checks matching `Terminal`,
   `Task`, `Job` and `Onset` arms from JS objects.
9. **Records**: `generator-type-mapping.md` §4.5(2) and the architecture phase record; the
   handover marks D1 ruled.

Gate: `dotnet fsi build.fsx -- test --run-gate`, unfiltered, before the PR push.
