# CloudEdge Shape regression reductions — 2026-09-06

Work is uncommitted on `agents/cloudflare-sdk-bindings`, as directed by the managing agent.
No remote operations, generated consumer handpatches, or full corpus gates were performed.

## Generic constraint closure

Smallest declaration reproducer:

```typescript
export interface Box<T> { value: T; }
export function accept<T, E extends Box<T>>(env: E): void;
```

`Shape.Spec.shapeSignature` retained type parameters named by value arguments and return
types only. It therefore emitted `accept<'E when 'E :> Box<'T>>`, leaving `'T` unbound.
Sandbox and SandboxPreview encounter the same pattern in `proxyToSandbox`.

The fix closes that live set transitively over the constraints of live parameters, then
partitions the original parameter list to retain declaration order. Constraints belonging to
unused parameters do not retain their dependency components. No findings were added.

`constraint-closure-lab` covers an export, an instance method, a transitive chain, and an
unused chain. Three Shape unit cases independently pin the direct, transitive, and dead cases.
The existing Any/Unknown guard in `Shape/FreeTypeParams.fs` was left untouched.

The pre-fix generated fixture is preserved at `/tmp/xantham-constraint-closure-baseline`.
Compiling it produced three FS0039 errors, at the method and two export sites:
`/tmp/xantham-constraint-baseline-compile.log`. With the fix, `TP006` findings fall from 6 to
2; the two retained findings are the deliberately unused chain. Symbol tiers change from
0 exact / 1 ergonomic / 4 widened / 0 escape to 2 / 2 / 1 / 0.

## Same-named outer inheritance

Smallest declaration reproducer:

```typescript
export interface Message { text: string; }
export interface View { message: Message & { renderedId?: string }; }
```

Shape correctly retains the canonical edge `View.Message -> Message`. The original renderer
wrote the latter as bare `inherit Message` inside `module View; type Message`, which F#
resolved to the newly declared type itself and rejected with FS0954. The managing agent owns
the Render fix. `inherited-name-lab` requires `inherit InheritedNameLab.Message`, preserving
the base declaration's identity rather than suppressing the edge.

## Validation

- Test-project Debug build: zero errors or warnings.
- Signature constraint closure: 3/3; existing Shape passes: 99/99.
- Each new lab: golden comparison and fresh-session determinism, 2/2.
- Live contract assertions for retained constraints and qualified inheritance: both passed.
- Both generated lab files compiled together against Fable.Core 5.2.0: zero errors or
  warnings. Project: `/tmp/xantham-shape-regressions-gate/ShapeRegressions.fsproj`;
  log: `/tmp/xantham-shape-regressions-compile.log`.
- `git diff --check` passed.

Live runs used the already installed native compiler at
`/tmp/clef-cloudflare-xantham-20260906/node_modules/@typescript/typescript-linux-x64/lib/tsc`
via `XANTHAM_TSGO_EXE`, with `XANTHAM_REQUIRE_TSC=1`. Xantham's default native package lookup
was unavailable; runs selecting zero tests during initial path discovery were not counted.

Both fixtures are registered after `constraint-arg-lab` in `Pipeline.test.fs`; their goldens
were generated through the registered tests. The composed solution build and full pipeline
subsequently passed: 658 generator tests, 90 wire tests and 327 Fable runtime checks, with one
default-executable-location check skipped under the explicit compiler override. CloudEdge
regeneration and full-library acceptance remain separate work.
