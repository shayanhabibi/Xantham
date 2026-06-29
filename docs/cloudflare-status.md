# Cloudflare workers-types generation — status & remaining-work map

**As of 2026-06-28.** Target: `@cloudflare/workers-types` (latest) generated F#, compiled
against `Fable.Core` + `Xantham.Fable.Core`. Error count is the Phase-B compile-error total.

## Trajectory

The surface went from **not compiling at all** (≈102 syntax errors → fully parsing) to
**~2,868 semantic errors**, with every catastrophic *correctness* defect resolved.

| Milestone | Errors |
|:----------|-------:|
| Syntax dam broke (output first parses) | 9,880 |
| After the structural/semantic fixes below | 3,204 |
| + lib.es map (#4) | 3,054 |
| + inherit-non-interface (drop `inherit exn/obj`) | 2,972 |
| + named `Array<T>` → `ResizeArray` (staged, uncommitted) | 2,868 |

## Committed fixes (correctness-critical)

- **string-collapse**: `string` no longer renders as `D1SessionBookmark` (was 2,094×).
- type-parameter constraints (`when 'T :> X`); `EmitProperty` not `CompiledName` on abstract
  members (FS0755, was 6,498); `Array`/`T[]` → `ResizeArray`; emit `U10+` erased unions;
  inherit type-arg fix; **coordinated alias self-reference fix** (IR self-refs 332 → 0);
  lib.es map (`Error→exn`, `PromiseLike→Promise`, iterators→`seq`, …); drop non-interface
  heritage bases.

## Staged (uncommitted, verified, ready to commit)

- `Render.fs`: named lib `Array<T>` → `ResizeArray<'T>` via a `typescript`-source stdlib
  substitution (safety-gated by the substitution map). 185 tests pass, no crash. −104 errors.

## Remaining ~2,868 errors — classified

| Class | ~count | What it is | Tractability |
|:------|-------:|:-----------|:-------------|
| **`Response`/`Request`/`RequestInit`/`WebSocket`/`QueuingStrategy` undefined** | ~340 | workers-types' OWN globals (source `typescript`) dropped to ref-only by the source-ignore gate | **FIXED (Fix A, staged)** — globals now emitted at top level; see path-system-pass-findings.md. Payoff masked by B + dup-members below. |
| **`Type`/`Format`/`ToolCalls`/`Role`/`FunctionCall`/… undefined** | large share of FS0039 | hoisted nested types deduped by `ResolvedType`; references named per-property dangle | **root cause PINNED, not surgical** — TypeStore + render cache both keyed by ResolvedType; heterogeneous sharing (pop≡length) makes redirect wrong (+1046). Needs encoder-side distinct keys. See path-system-pass-findings.md. |
| **duplicate members from declaration merging** | ~60+ | `Response` etc. emit `url`/`type`/`status` 2× (merged lib + workers decls) → FS0035x/FS0663 | new — surfaced by Fix A; dedup members by name at render |
| **FS0033 typed arrays** (`Uint8Array<X>`, `ArrayBufferView<X>`, `Float32Array<X>`) | ~130 | generic in TS, non-generic in Fable.Core.JS — needs the spurious arg dropped | attempted: dropping the arg traded ~300 FS0033 for ~140 FS0039 (the element type is load-bearing somewhere) + `%A` debug spam → reverted. Not yet clean. |
| **FS0953 cyclic aliases** | 170 | `type X = U2<X,X>` residual; splits encoder-corrupt (`Array<spurious>` bodies) + anonymous-union limitation | Step-2, deferred-documented (see alias-self-reference-followup.md) |
| FS0663/0883/0698/0660/0035 | ~230 | bogus typar constraints / structural-construct issues from the above families | mostly downstream of the path/alias work |

## The strategic read

**Clean, isolated wins are thinning.** The two largest remaining levers — the
`typescript`-source global placement (~340) and the dropped hoisted nested types (#3) —
both live in the **path / module-resolution subsystem** (transient→anchored path
localisation, `TypeStore` dedup keyed by `ResolvedType`). Four focused attempts on that
subsystem each produced double-grafts or crashes; it resists surgical fixes and likely
needs a dedicated, deeper effort (possibly with an encoder-side component, since some
alias/array bodies are genuinely corrupt in the IR).

**Recommendation:** the remaining high-value work is a **dedicated path-system pass**
(global placement + nested-type hoisting), treated as its own project with proper
architectural design rather than incremental surgical attempts. The typed-array drop-arg
is a smaller separate item once the element-type dependency is understood. Everything
catastrophic is already fixed and committed; what remains is well-formedness polish
concentrated in one subsystem.

Detailed root-cause notes: `docs/alias-self-reference-followup.md`.
