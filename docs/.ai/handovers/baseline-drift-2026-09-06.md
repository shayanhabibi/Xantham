Baseline drift correction on `agents/cloudflare-sdk-bindings`, 2026-09-06.

An isolated, allowlisted `git archive` of `22c0cb2ee7bec6003de516af8e8737414608f8bf`
reproduced all three failures seen in the full gate. It used compiler
`7.1.0-dev.20260902.1`, the tracked lab inputs, and a read-only copy of installed
`solid-js` 1.9.15. The baseline build had zero warnings/errors; each exact test filter
ran one test, failed once, and skipped none.

| Baseline test | Reproduced result | Correction |
| --- | --- | --- |
| Intersection operands | Expected 3 `volume` declarations; actual 2 | Assert both inherited bases and the two operand declarations. |
| Inherited members | Expected 3 `cloneNode` declarations; actual 4 | Assert the root plus three narrowings and all four return types. |
| Solid golden | First difference at line 666; 336 positional differences, identical to the current failure | Regenerate through the existing harness. |

Solid regeneration changes declaration order only: complete type/module blocks and
all 70 export-member blocks retain their content, including documentation and attributes.
The 192 symbol reports retain their tiers and findings; four source paths also correct
`types/render/suspense.d.ts` to the installed filename `types/render/Suspense.d.ts`.
The aggregate manifest is byte-identical: exact **19**, ergonomic **93**, widened **62**,
escape **18**; all **979** findings and every finding-code count are unchanged.
No finding codes were added. The golden diff is two files, 54 insertions/54 deletions.
`Shape/Ordering.fs` compares source-file casing verbatim, so compiler/API canonical
casing may affect order across hosts. This refresh establishes the observed casing and
ordering drift on these pinned inputs; platform-independent ordering remains unproven.
This slice changes no ordering or path-normalization logic.

The Release harness, using `--no-build --no-restore` and the same compiler override,
passed the exact Solid golden filter with `XANTHAM_UPDATE_GOLDEN=1` (**1/1**), then
`--filter 'generator e2e.solid-js'` without update (**2/2**, golden and determinism).
Neither invocation skipped tests. The final composed gate passed 600 generator tests,
90 wire tests and 323 JavaScript runtime checks. Its one wire executable-layout skip is intentional
under the explicit compiler override; the generator suite skipped none.

[Durable evidence and exact commands](/home/hhh/.local/state/clef-dimensional-rescue/checkpoints/2026-09-06-xantham-fsharp-cloudedge/baseline-evidence/baseline-evidence.json)
record the baseline SHA, archive allowlist, compiler/input hashes, pins, and all three failures.
[Refresh measurements](/home/hhh/.local/state/clef-dimensional-rescue/checkpoints/2026-09-06-xantham-fsharp-cloudedge/baseline-evidence/solid-golden-ordering-comparison.json)
record the declaration comparison, file-case corrections, and before/after finding counts;
the same directory holds the baseline and refresh logs. No commits were created by this lane.
