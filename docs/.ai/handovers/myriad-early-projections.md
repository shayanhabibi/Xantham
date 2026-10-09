# Handoff: early Myriad union projections, 2026-10-09

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
