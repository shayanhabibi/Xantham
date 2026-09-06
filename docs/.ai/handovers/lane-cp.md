feasible

# Lane CP — overloaded `ParamObject` statics, hand-proven

## The question

Yes. Fable accepts a single type carrying two `[<ParamObject; Emit("$0")>]` `Create` statics
separated only by which parameters are required, and each overload's emitted object literal
holds exactly the arguments passed at its own call site.

Proven over two hand-written types in `tests/Xantham.Generator.RunGate/Probes.fs` (probes 17-18),
exercised from `paramObjectOverloads ()` in `tests/Xantham.Generator.RunGate/Program.fs`:

- `SearchOptions` — `AiSearchSearchRequest`'s shape, both arms carrying a required member of
  their own (`query` vs `messages`).
  - `Create(query = "hello", shared = 1.0)` → `{"query":"hello","shared":1}`
  - `Create(messages = [|"a";"b"|], shared = 2.0)` → `{"messages":["a","b"],"shared":2}`
    (no `query` key on either side)
- `ContainerLikeOptions` — `Container.Start.Options`/`Options2`'s shape, one arm required
  (`image`), the other's distinguisher (`snapshot`) optional in source.
  - `Create(enableInternet = true, image = "img")` → `{"enableInternet":true,"image":"img"}`
  - `Create(enableInternet = true, snapshot = "snap")` → `{"enableInternet":true,"snapshot":"snap"}`
  - `Create(enableInternet = true)` (both distinguishers omitted) → `{"enableInternet":true}`,
    resolving unambiguously to the all-optional arm

## The optional-member question

**Yes**, F# separates the overloads even when one arm's distinguishing member is optional in the
TypeScript source. `ContainerLikeOptions` above is exactly `Container.Start.Options2`'s case —
its `Create` overload holds no required member beyond the shared `enableInternet` — and it both
compiles and resolves correctly at every call site tried, including the all-shared-only call.
Overload resolution is not ambiguous here because the *other* arm (`Options`, mirrored by the
`image`-required overload) still carries a required member the all-optional arm lacks, so the
compiler always has a required-arity or argument-type fact to resolve on. `TR061`
(`ExclusiveArmsNotFoldable`) is not needed for this pair; wave thirteen's caution does not hold
for the real `Container.Start.Options`/`Options2` shape.

## Lab fixture

`tests/fixtures/paramobject-overload-lab/index.d.ts` — an exclusive-arm union
(`QueryOptions`/`MessagesOptions` via `?: never`), registered in `Pipeline.test.fs`. It pins
*today's* unfolded baseline (each arm mints its own interface and its own `Create`) as the
fixture lane CN's fold will later diff against; the Fable capability itself is proven in the
run-gate probes above, not through the pipeline (no fold pass exists to generate it).

## Counts

- Run gate: 309 → 314 checks (+5, all new).
- Generator tests: 490 → 493 (+3: golden match, determinism, one assertion testCase).
- Wire tests: 90 passed, 1 skipped — unchanged.
- Tiers, corpus-wide: exact 535→537 (+2), ergonomic 1603→1605 (+2), widened 797 (+0), escape 200
  (+0) — the entire movement is `paramobject-overload-lab`'s own (exact 2, ergonomic 2, widened
  0, escape 0; `MB003` 4, `TR032` 2, `SP001` 2), not a corpus-wide effect.
- `dotnet fsi build.fsx -- test`: green, exit 0.

## `git diff --stat`

```
tests/Xantham.Generator.RunGate/Probes.fs      | 36 +++++++++++++++++++++
tests/Xantham.Generator.RunGate/Program.fs     | 43 ++++++++++++++++++++++++++
tests/Xantham.Generator.Tests/Pipeline.test.fs | 35 +++++++++++++++++++++
3 files changed, 114 insertions(+)
```

Plus two untracked new directories: `tests/fixtures/paramobject-overload-lab/` and
`tests/Xantham.Generator.Tests/golden/paramobject-overload-lab/`.

## Branch and commit

Branch `worktree-gen-wave14-cp`, forked at `8d3a4fa`. Commit follows this file in the same
commit (see log).

## Nothing unexplained.
