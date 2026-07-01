# Toolchain: Fantomas / Fabulous.AST version constraints (breadcrumb)

**Status:** known blocker. Do not re-investigate from scratch — the paths below were
exhaustively verified (2026-06-30) and are dead ends.

## The pin

The generator (`src/Xantham.Generator`) formats its output via **Fabulous.AST 2.0.0-pre06**,
which pulls **Fantomas.Core 7.0.1 transitively**. Fantomas is *not* a direct reference.

```
Fabulous.AST 2.0.0-pre06  ->  Fantomas.Core [7.0.1]   (exact/bracketed pin)
```

## Why you cannot simply upgrade Fantomas

There is a real formatting bug in **Fantomas 7.0.1**: a multi-line generic type used as a
tupled abstract-method parameter (e.g. `workflowName: U2<...> * ...`) can be laid out so the
closing `>` lands at an offside-invalid column, and `<deprecated>`/`<br/>` doc-comment blocks in a
large `module rec` trip F#'s XML-doc / offside parser (FS0058 / FS0010 / FS0550 cascades). The
upstream fix is **Fantomas 8.0.0-alpha-003** (issue [#3043]: *"multiline type inside a signature
gets extra indentation to avoid compiler error"*).

**That fix is unreachable from this project. Verified:**

| Attempt | Outcome |
| --- | --- |
| Bump Fabulous.AST to **pre07 / pre08** | Both *also* hard-pin `Fantomas.Core [7.0.1]` — no Fabulous.AST release ships against Fantomas 8. pre08 additionally requires `FSharp.Core >= 10.1.300`, which this project cannot resolve (a transitive constraint caps it at 10.0.101) → `TypeInitializationException: Fabulous.AST.TypeLongIdent` at runtime. |
| Override **`Fantomas.Core` -> 8.0.0-alpha-***  directly, keep Fabulous.AST pre06 | `MissingMethodException: BindingNode..ctor` at runtime — Fabulous.AST is compiled against the exact Fantomas 7.0.1 SyntaxOak ABI; the `BindingNode` constructor signature changed in Fantomas 8. ABI-locked. |
| Raise Fantomas `MaxLineLength` (via `CodeFormatter.FormatOakAsync` with a custom `FormatConfig`) | **Cosmetic only** — collapses the multi-line layout but the net error count is unchanged (committed 34/strict-off-10 == MaxLineLength=1024 34/strict-off-10). Reverted. |
| Remove the inserted `<br/>` doc-line joins | **Catastrophic** — `<br/>` is load-bearing for multi-line doc structure; removing it merges doc lines and explodes to ~3200 errors. |

## What the residual errors actually are

Against a faithful FS-error harness the fresh `agents@0.17.1` surface shows **34 errors** under
default (F# 8+) settings, **10** under `--strict-indentation-` / `LangVersion=7.0`:

- **~24** are **strict-indentation** false-positives (FS0058): valid F# 7, flagged only under
  F# 8+'s default `--strict-indentation`. Purely a Fantomas-7-layout / F#-version artifact.
- **~10** are genuine parser cascades (FS0010 / FS0550) localized to ~3 `<deprecated>`/`<br/>`
  doc-comment regions (JSONSchema and siblings). They **do not reduce to any isolable construct** —
  ~15 faithful minimal repros of every suspected construct compile clean; the errors exist only in
  the full ~13 000-line `module rec`. The rest of the surface type-checks.

## When to revisit

Revisit **only** when a Fabulous.AST release ships built against **Fantomas ≥ 8.0.0** (check its
`.nuspec` `Fantomas.Core` dependency range — it must *not* be `[7.0.1]`). At that point:

1. Bump `Fabulous.AST` in `src/Xantham.Generator/Xantham.Generator.fsproj`.
2. Expect a small API migration — pre08 already renamed `AbstractMemberModifiers` ->
   `MemberDefnModifiers` (one line in `Generator/TypeRender.Render.fs`, `Documentation.renderForAbstractMember`).
3. Ensure `FSharp.Core` can resolve to whatever the new Fabulous.AST requires (pre08 wanted
   `>= 10.1.300`) — otherwise you get `TypeInitializationException` at runtime.
4. Re-run the FS-error harness; the strict-indentation + doc-comment cascades should clear.

[#3043]: https://github.com/fsprojects/fantomas/issues/3043
