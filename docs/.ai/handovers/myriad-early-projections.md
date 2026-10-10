# Handoff: typed Agents/Pi operations, 2026-10-09

**Scope:** carry before-Shape value contracts through generated SDK method calls, with
explicit shared input ownership and an executable CloudEdge Agents/Pi integration.
This increment supersedes MP-2 below for the selected instance methods; raw SDK surfaces
outside those selections retain their reported losses.

**Heads:** both primary checkouts use `fix/myriad-early-projections`. Xantham increment base
`0cab4e95992d519a05b3cc43ef9e3c13dd2656df`; CloudEdge base
`3bf08618837910e433b779cf6c0396fc900a1080`. Shayan owns integration into develop.
All current implementation, builds, tests and mutations ran in the primary checkouts.

**Gates (E):** `tests/.scratch/strict-operation-audit/` contains the source binding,
full acceptance log and mutation receipts. The user explicitly forbids Bozzetto for
F#/Fable work, so suites ran directly, serializing shared build dependencies.

```text
full acceptance (182.614s): generator 1233 passed / 1 ignored; Wire 99 passed / 1 ignored; 0 failures
Fable RunGate: 525 checks passed; Partas JSX and runtime gate passed
projection focused: 30 passed, 0 ignored, 0 failed (write and check)
core source mutants: 5 compiled/killed; operation guard mutants: 3 compiled/killed
runtime output mutants: 4 compiled/killed; recursive layout disabled: FS0039 as expected
acceptance binding: all 15,673 source/config/compiler/fixture input hashes unchanged
```

Reproduce Xantham acceptance: `dotnet fsi build.fsx -- test --run-gate`.
CloudEdge's `docs/sdk-build.md#agents-pi-operations` documents its pinned
source-backed generation, static consumer and local workerd gate. The new profile is
selected explicitly; it does not substitute an older published CLI.

**Binding:** `acceptance.before.json` / `acceptance.after.json` bind source, configuration,
installed compiler and fixture declarations. The core mutation receipt predates three
formatting changes and the export-placement fix; `core-mutants/verify-binding.py` reconstructs
both prior source hashes exactly. Final composed acceptance covers the current files.
Runtime and operation-guard mutations changed actual primary files sequentially and restored
their bytes. Restore/setup failures and a zero-test filter attempt are excluded from evidence.

**Rule ledger (E):** permanent controls are in `ResolvedProjection.test.fs`,
`Projection.test.fs`, `Pipeline.test.fs` and RunGate `Projections.fs`.

| Rule / owner | Positive control | Negative control | Discriminating mutation |
| --- | --- | --- | --- |
| Complete source facts / Semantics | string, tagged records, arrays, optional fields | unresolved/empty/recursive arms | swallow incomplete union constituents |
| Source identity / Resolve and Semantics | same facts retain occurrence identity | defining declaration content changes fingerprint | omit imported source closure |
| Authoritative arrays / Semantics | globally augmented built-in Array | local Array namesake | disable checker array predicate |
| Declared keys / Resolve and Semantics | ordinary and escaped record keys | escaped operation metadata, computed/symbol keys | bypass declared operation key agreement |
| Selected-field construction / Semantics | optional selected field and optional siblings | required selected field or sibling | admit required selected field |
| Receiver ownership / Pipeline | actual Session receiver | same method on incompatible OtherSession | bypass final qualified receiver check |
| Shared contract / Myriad Operations | two methods consume one owner's Value | mismatched resolved shapes | bypass shared-shape agreement |
| Exact JS boundary / Myriad and RunGate | text/image payloads and distinct presence | null string rejected before SDK call | wrong tag, omitted→present undefined, unsafe own-property write, accept null string |
| Recursive groups / Pipeline and Render | two-package cycle compiles in one namespace | separate-file cycle fails; invalid namespace refuses | disable recursive grouping → FS0039 |
| Export ownership / Pipeline | colliding dependency type and public subpath | entry exports cannot move to dependency owner | disable explicit entry-export placement |

**Shape matrix:** primitive strings/numbers/booleans, string literals, arrays, data records,
value unions and optional record fields are admitted. Required literal tags are supplied by
encoders. `None` omits a property; `Some Undefined` writes it; `Null` is distinct. The source
matrix refuses overloaded/generic/rest calls, generic/recursive/indexed/callable payloads,
computed/symbol keys and untracked ImportType/TypeQuery references. Invalid F# inputs,
independent input DUs and image records missing MIME type fail compilation. The receiver
counterexample has the same method name and a different parameter type.

**Measurements:** the expanded lab has 7 Exact / 7 Ergonomic / 4 Widened / 0 Escape.
Its old finite declarations and codec goldens retain their original mappings. Operation
projections add their own CU006 boundary findings; they do not suppress raw losses.
The full corpus passes without changes outside the expanded projection lab and new recursive lab. Real CloudEdge measurements are recorded with its acceptance ledger.

**Growth:** `git diff --shortstat 0cab4e9..HEAD`: 42 files changed, 2943 insertions(+), 77 deletions(-), Xantham only. CloudEdge records its separate growth and new-file owners in its SDK build handoff.

| New files | Owner and one-line reason |
| --- | --- |
| `src/Xantham.Generator.Myriad/Operations.fs` | Existing Myriad adapter: emit recursive input contracts and typed calls from the authenticated early snapshot, separate from finite literal codecs. |
| `golden/projection-operations/ProjectionOperations.{Input,Thinking,Queue,Fields,Shared,SharedNext}.fs` under Generator.Tests | Existing golden/RunGate owner: pin independent and shared contracts, actual calls and property-presence behavior. |
| `tests/fixtures/recursive-groups-lab/node_modules/recursive-groups-lab/{index.d.ts,api.d.ts,package.json,xantham.json}` | Fixture owner: minimal entry side of a declaration cycle and colliding public subpath. |
| `tests/fixtures/recursive-groups-lab/node_modules/recursive-peer-lab/{index.d.ts,package.json}` | Same fixture owner: dependency side of the cycle and colliding type. |
| `golden/recursive-groups-lab/{groups/RecursiveGroupsLab.fs,manifest.json,symbols.jsonl}` under Generator.Tests | Existing Pipeline golden owner: compile and measure the recursive representation and export placement. |

No new resolver, identity authority or parsing of widened F# output was introduced. Resolve,
Semantics, Contract, Pipeline and the existing namespace renderer retain their respective facts.

**Context sources:** schema was fresh/latest with matching snapshot/current ID
`0cbe94d87d33e4cd8d2f3f0437fa2b9860db04e798b93d6efa5d56ca09edcb97`.
Xantham, CloudEdge and Myriad are absent, so bounded direct reads and uncommitted diffs were
permitted; indexed find→pgq→sources did not apply. LAN workers assisted bounded first-pass
reading. F# semantic MCP tools were unavailable; compiler and runtime gates supply the evidence.
One independent reviewer checked this composed increment and its bound receipts.

**Residuals:** OP-1 unsupported calls/shapes fail with `projection/unsupported-operation` or
`projection/unsupported-shape`; missing facts use `projection/incomplete-operation` or
`projection/incomplete-shape`. Support is bounded, not a general TS-to-F# type equivalence.
OP-2 unselected SDK operations/results keep their raw mappings; this delivers the four
PiHarness/PiSession submit/prompt input contracts and typed factory context. The closed owner
ships reached PiAI/PiDurable/Chord types, not standalone PiAI/PiDurable value-export libraries.
OP-3 property values/presence are modeled, without claiming `exactOptionalPropertyTypes` write
semantics. OP-4 overlapping values cannot recover their declaration of origin; sharing requires
explicit `createShared` coordination. Earlier CORETS-1 audit residual remains separate.

**Owner decisions needed:** none. **Review:** no remaining Xantham findings; the independent reviewer verified all 12 mutation bindings. CloudEdge final integration receipts are reviewed in its handoff.
Retire this handover on merge after retaining the architecture and footgun records.

---

## Accepted predecessor: early Myriad union projections, 2026-10-09

**Scope:** opt-in companion generation between Resolve and Shape, preserving selected union
membership and distinct null/undefined values with normal F# DUs, codecs and active patterns.

**Heads:** Xantham `fix/myriad-early-projections`, based on rebased `develop`
`45be2390733cd09856372480adffccf16725c3c8` (includes audit fixes via #113).
Shayan owns integration into develop. No package publishing or develop merge performed.

**Gates (E):** final ledger below; dependency inputs frozen in
`tests/.scratch/myriad-acceptance/dependencies.before.json`. The user explicitly excluded
Bozzetto from F#/Fable work, so these suites ran directly on this machine without Bozzetto leases.

```text
full acceptance (185.163s): generator 1216 passed / 2 ignored; Wire 99 passed / 1 ignored; 0 failures
Fable RunGate: 507 passed; Partas JSX and runtime gate passed
worktree cache-location test: 1/1 passed in a focused follow-up (explained extra ignore)
projection focused: 20 passed, 0 ignored, 0 failed (write and check)
projection contract: 10 passed; 6 compiled mutants killed
projection guards: 8 passed; 9 compiled mutants killed
projection Fable runtime: 39 passed; loose-null mutant compiled and failed the null/undefined assertion
additional emitter naming/escaping runtime matrix: 20 passed
release unit checks: 12 passed; ShipIt integration contract passed
Myriad package: dotnet pack -c Release --no-build --no-restore succeeded
```

Reproduce acceptance: `dotnet fsi build.fsx -- test --run-gate`.
Focused loop: `dotnet fsi build.fsx -- test --quick --filter projection`.
Release checks: `node --test .github/scripts/release-versions.test.cjs .github/scripts/release-bot.test.cjs`
and `node tools/verify-release.mjs`. The runnable `--union` example and consumer API are documented
in `site/content/xantham-cli/guide/customization.md`.

The extra generator ignore was `generator cli.tsc version reports the compiler installed in the cache`: it uses `Tsc.locateAt` on the worktree itself, bypassing the normal compiler borrowing. A temporary link to the identical main compiler install enabled a focused 1/1 pass; the link was removed. The other generator ignore is the existing Linux `solid-js` path-casing guard; Wire retains its baseline ignore. All nine dependency-input hashes were unchanged.

**Binding:** source/gate receipts are under `tests/.scratch/myriad-acceptance/`;
contract mutations under `early-projections-contract/mutations-phantom/`, guard mutations under
`projection-guard-mutations/`, runtime/emitter evidence under `myriad-active-pattern/` (all beneath
`tests/.scratch`). These are copied closures, not mutations of the working source. Earlier harness
setup failures and the invalid single-case-DU negative test are excluded from accepted evidence.

**Rule ledger (E).** Each control below is a permanent test except the extra emitter edge matrix.

| Rule / owner | Positive control | Negative control | Discriminating mutation |
| --- | --- | --- | --- |
| Original resolved arms / Semantics | exact mixed union | missing, unresolved, cyclic, empty arms | drop unresolved constituents |
| Per-declaration genericity / Resolve | plain alias sharing phantom's checker ID | phantom and used generics | bypass declaration parameters |
| Named source identity / Semantics | EqualA and EqualB keep different identities | equal sets cannot collapse named owners | constant identity |
| Source authentication / Semantics | stable repeat output | source content changes without arm changes | omit source file hash |
| Snapshot seals / Contract and Apply | current token and plan | foreign token, cached plan replay | bypass each nonce check and runner guard |
| Extension identity / Apply | registered early and late phases | duplicate IDs, empty version | bypass each check |
| Output claims / Apply | separate module and file | traversal, occupied file/type, empty exports, reserved witness file | bypass each refusal |
| Compiler boundary / Pipeline and Compile | complete valid output and advertised types | invalid payload type, missing advertised type; destination sentinel survives | skip compiler validation |
| Membership and absence / Myriad and RunGate | literal/number/null/undefined and active patterns | unknown strings and other JS kinds | strict null equality changed to loose equality |
| Raw catalog policy / Pipeline and Provenance | raw files/catalog exactly match ordinary generation | projection policy cannot change their bytes | direct equality controls |
| F# nominal distinction / compiler | explicit encode/decode conversion | EqualA supplied as EqualB fails FS0001 | direct negative compile control |

**Shape matrix:** string literal, number, null, undefined, same-set aliases and overlapping sets pass;
unknown JS kinds reject at decode. Boolean/numeric literals, broad strings, generics (including
phantom), object arms, void and incomplete resolution reject explicitly. Escaped/control/Unicode
strings and DU/Fable member-name collisions pass. Generated types remain additive companions.

**Measurements:** existing golden files and finding counts remain unchanged. New raw lab counts:
4 Exact, 0 Ergonomic, 2 Widened, 0 Escape. Five generated companions retain raw findings and add
five `CU006` Escape findings for the arbitrary-source extension boundary. No catalog version bump.

**Growth:** `git diff --shortstat 45be239..HEAD`: 44 files changed, 2200 insertions(+), 80 deletions(-), Xantham only.

| New files | Owner and one-line reason |
| --- | --- |
| `src/Xantham.Generator.Myriad/Xantham.Generator.Myriad.fsproj`, `LiteralUnions.fs`, `README.md` | Myriad adapter: isolate the optional published dependency and its emitter from the core generator. |
| `src/Xantham.Generator.Myriad/CHANGELOG.md` | Existing ShipIt release convention: version ownership for the new package. |
| `tests/Xantham.Generator.Tests/ResolvedProjection.test.fs` | Customization contract: isolate the before-Shape source/identity matrix. |
| `tests/Xantham.Generator.Tests/Projection.test.fs` | Pipeline customization: end-to-end validated projection and refusal controls. |
| `tests/Xantham.Generator.RunGate/Projections.fs` | Existing RunGate: execute generated codecs, patterns and the explicit raw echo bridge. |
| `tests/fixtures/projection-lab/index.d.ts`, `index.js`, `package.json` | Fixture owner: minimal declaration, actual echo runtime and package resolution contract. |
| `tests/Xantham.Generator.Tests/golden/projection-lab/ProjectionLab.fs`, `manifest.json`, `symbols.jsonl` | Existing Pipeline golden owner: pin ordinary raw output and retained losses. |
| `tests/Xantham.Generator.Tests/golden/projection-lab-companions/ProjectionViews.Choice.fs`, `ProjectionViews.EqualA.fs`, `ProjectionViews.EqualB.fs`, `ProjectionViews.OverlapC.fs`, `ProjectionViews.Weird.fs` | Myriad golden owner: pin actual selected projections, compiled by the existing gates. |
| `docs/.ai/handovers/myriad-early-projections.md` | Increment handoff: retain review and validation evidence until merge. |

Existing owners extended: Resolve's cached declaration reader; Customization contract, semantics,
apply and provenance; Pipeline runners and writer; release lists; the existing runnable example.
No parallel TypeScript resolver, catalog identity authority or widened-output parser was added.

**Context sources:** retained fresh/latest retrieval snapshot
`0cbe94d87d33e4cd8d2f3f0437fa2b9860db04e798b93d6efa5d56ca09edcb97`
matched `current_snapshot_id`; Xantham and Myriad were absent from its tracked repositories.
Thus bounded direct reads applied; uncommitted edits were reviewed through diffs. The indexed
find → pgq → sources sequence did not apply to these repositories. Local Myriad API inspection
used clean revision `bf274d16bec0d88eadf98e212933f03b6a59297c`, followed by actual published
Myriad.Core 1.1.0 builds. F# semantic MCP tools were unavailable; compiler and runtime evidence
supplied semantic checks. One reviewer checked the increment without rerunning the baseline;
root reviewed the reviewer's small release-registration edits.

**Residuals:** MP-1 unsupported arms/generics return `projection/unsupported-union`; unresolved
facts return `projection/incomplete-union`. MP-2 raw generated signatures retain widening: callers
explicitly encode/decode at JS boundaries; automatic function/property wrappers are future work.
MP-3 overlapping primitive values carry no declaration-of-origin information; preserve an outer
application DU if needed. MP-4 absent property reads and explicit undefined need separate presence
inspection. MP-5 codecs execute under Fable, not as .NET runtime operations. The earlier CORETS-1
catalog drift audit residual is unchanged by this increment.

**Owner decisions needed:** none. **Review:** no remaining findings.
Retire this handover on merge after retaining the already-updated architecture and footgun records.
