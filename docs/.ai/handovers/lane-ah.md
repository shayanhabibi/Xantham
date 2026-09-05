# Lane AH — run gate's wave-six probes become goldens (item 8)

Wave seven. Branch `worktree-gen-wave7-ah`, based on `worktree-generator-wave-seven` at
`f708e348ca9a2c434ac1f3b8e7b6641f8e1e964b`. Final commit `9e1ad4d`.

## What changed

`Probes.fs` held three hand-written mirrors of forms wave six proposed. Lanes AA and AD have since
landed both proposals as real goldens (`hook-interface-lab`, `nested-name-lab`), already linked
into the run gate's `.fsproj`, but `Program.fs` was still exercising the hand-written mirrors
instead of the generated types, and two of the mirrored assertions (`NestedNameLab.Widget.Create` /
`NestedNameLab.Widget.Options.Create`) were never invoked by any check at all.

- **Probes 1 & 2 (optional hook as opt-in interface)** — fully retargeted. Added a `HandledBoth`
  subclass (implements both `HookInterfaceLab.Station.IFetchHandler` and `IAlarmHandler`) so the
  "member name unmangled, once per interface" and "second interface dispatches" checks have a
  two-interface instance to read, same as `Probes.HookedBench` gave them. All 13 checks that used
  `Probes.HookedBench`/`Probes.BareBench`/`Probes.IFetchHandler`/`Probes.IAlarmHandler` now run
  against `HookInterfaceLab` types inside `optionalHooks ()`. Those four Probes declarations are
  deleted from `Probes.fs`.
- **Probe 3, the object-literal half (nested inline shape naming)** — retargeted onto
  `NestedNameLab.Widget` / `Widget.Options`, which the golden already declared but `Program.fs`
  never constructed. Added to `nestedNames ()`.
- **Probe 3, the remaining half — stays hand-written.** Two forms neither golden carries: a nested
  inline shape holding a field of its *own owner's* type (a back-reference under `module rec`,
  distinct from `nested-name-lab`'s cross-references between sibling nested types), and an import
  bound from inside a nested module rather than the file's top level (`nested-name-lab`'s two
  imports are both on top-level `Exports`). `Probes.fs` keeps `Widget`/`Widget.Options`/
  `Widget.Options.Retry`/`Widget.Exports` for these two checks only; its header now says why.

## Run gate check count

**160 before, 160 after** (baseline confirmed at HEAD before any edit). Intermediate state after
retargeting had 156 — four probe-1/2 checks were dropped as literal duplicates of pre-existing
`optionalHooks` checks (already true of `Handled`/`Unhandled`, just different instance labels) —
restored to 160 by re-adding them against a fresh `Unhandled "plain"` instance and the two-interface
`HandledBoth`, since the strict instruction is the raw count, not just distinct behaviour.

## Lines removed from Probes.fs

Net -40 lines (83 → 43): deleted `IFetchHandler`, `IAlarmHandler`, `HookedBench`, `BareBench`, and
their doc comments; rewrote the header to point at `optionalHooks`/`nestedNames` for the retargeted
checks and explain what's left.

## Gates

- `dotnet build Xantham.slnx` — not re-run separately; `dotnet fsi build.fsx -- test` covers the
  same projects and was green.
- `dotnet fsi build.fsx -- test` — green: 419 generator tests, 85 wire tests, run gate 160 checks.

## Nothing unexplained

Working tree showed every other `src/`/`tests/` file as modified under `git status`, but each was
CRLF-normalization noise only (`git diff --stat` empty, warning-only) — not touched by this lane,
not staged, not committed. Only `Probes.fs` and `Program.fs` are in the commit.
