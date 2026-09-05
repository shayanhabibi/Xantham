---
category: Generator
audience: managing agent
title: Lane AL - build.fsx exit code and fantomas line endings
branch: worktree-gen-wave8-al
---

# Lane AL — items 9 and 10

Two defects in the repository's own tooling. Both are fixed; both are proven by running them.

## Item 9 — `build.fsx` exited 0 when its pipeline failed

**The defect survived the Partas.Build alpha 3 upgrade.** The worklist asked for this to be
established before the fix was priced, and it was, by running the identical failing scenario
against the pre-fix script.

`rootCommand`'s result was discarded. It already computed the right code; nothing consumed it.
The fix wraps the `rootCommand` expression in `exit`, which also confirms the return type is
`int` — had it been `unit`, the script would not compile.

Measured, with a deliberate parse error appended to `src/Xantham.Fable.Core/Brand.fs` so the
`format` stage fails:

| Scenario | Exit code |
| --- | --- |
| Pre-fix script, failing pipeline | 0 |
| Post-fix script, failing pipeline | 1 |
| Post-fix script, passing command (`--help`) | 0 |

Both directions matter. A fix proven only on the failing case is indistinguishable from an
unconditional `exit 1`.

The banner text is unchanged: a failing run still prints `Error: Pipeline is failed because the
result is not indicating as successful`, and the stage-timing table still names the failed stage.
What changed is that the process now agrees with the banner. The advice given to every lane in
waves six and seven — read the summary line, not the exit code — is retired.

## Item 10 — fantomas rewrote every F# file's line endings

`.gitattributes` carries `* text=auto eol=lf`, so the working tree holds LF. `.editorconfig`
declared only `indent_size`, so fantomas fell back to `Environment.NewLine` and wrote CRLF on
Windows. Every formatted file then read as modified with a byte-empty content diff under
`git diff --ignore-cr-at-eol`.

The cost was not cosmetic. `git status` could not show a lane's footprint, and `git merge`
refuses to run against an unclean tree — so wave eight's integration was blocked before it
started. The integration branch's own worktree was holding 47 such files when the wave opened.

`end_of_line = lf` in `.editorconfig` settles it, and the glob widens to `[*.{fs,fsi,fsx}]`
because fantomas formats all three. Fantomas honours EditorConfig's `end_of_line`; that is
established empirically by the acceptance test rather than from documentation.

**Acceptance test:** from a clean tree, run the full `dotnet fsi build.fsx -- test`, then
`git status --short`. It reports zero modified files.

## Provenance

Items 9 and 10 were dispatched to a lane that produced both edits and then stalled twice waiting
on a background build without committing, so the managing agent committed the lane's working tree
and carried out the verification. The edits are the lane's; the measurements above are the
manager's.

There is a symmetry worth recording: an agent that cannot trust an exit code falls back to
polling a long build and loses its turn to the poll. That is the recurring cost item 9 removes,
and this lane paid it while fixing it.
