---
category: Generator
audience: managing agent
title: Lane AN - a bare `x: null` already raises a finding; TR057 left unraised
branch: worktree-gen-wave8-an
base: 9a97b95222a732e72dd7a76a6f494e5c454c3087
---

# Lane AN — wave eight, item 5

## The question

Item 5 asked whether a bare `x: null` type — not a union, so it never reaches `unionRef` and
widens straight to `obj` — should carry its own absence finding (`TR.BareNullToObj`, pre-declared
as `TR057`, tier widened, no payload), or whether the site is already covered by some other
finding, making a second case duplication.

## What actually fires at the site

`absence-alphabet-lab`'s `Absence.onlyNull: null` member falls through the entire flag chain in
`typeRefOnPath` (`Shape/Spec.fs`): it is not `Boolean`, `Union`, `EnumLiteral`, a literal kind,
`String`/`Number`/`BigInt`, `NonPrimitive`, `UniqueESSymbol`, `ESSymbol`, and — critically — the
`Void || Undefined || Never` branch does not include `Null`. It lands in the final `else`, which
raises `TR.TypeFlagsNotMapped` (`TR014`, tier widened) and widens to `obj`.

Confirmed directly from the regenerated golden's `symbols.jsonl`:

```
{"name":"TR.TypeFlagsNotMapped","key":"TR014","pass":"shape-interfaces","tier":"widened",
 "symbol":"Absence.onlyNull","fields":{"flags":"Null"},
 "message":"type flags Null not mapped yet; widened to obj"}
```

`TR033` (`OnlyNullUndefinedToUnit`) does stay silent here, exactly as the lab's comment already
said — `onlyNull` is not a union, so `TR033`'s union-only precondition never applies. But `TR014`
is not silent: it already records the widening, and its `flags` field literally says `"Null"`,
which is exactly the fact `TR057` would exist to carry.

## Population

`dotnet fsi build.fsx -- findings --key TR014` over the whole corpus (`@cloudflare/workers-types`,
`animejs`, `type-fest`, `solid-js`, and every lab) shows exactly **one** `TR014` hit anywhere:
`absence-alphabet-lab` itself. Every other fixture, including the three large npm packages, has
zero. A bare, non-union `null` type is not a shape real `.d.ts` corpora produce — it is exactly
the degenerate/mistake case wave seven's finding predicted when it pinned the negative.

## Decision: leave `TR057` unraised

Raising `TR057` at this site would fire two findings (`TR014` and `TR057`) for one member, and
`TR057`'s proposed message ("a bare null type widened to obj; absence is not carried") states a
fact `TR014`'s existing `flags: "Null"` field already carries. That is the duplication the brief
warned against, not a gap. Current behaviour is correct: the site is covered, and the population
data (1 hit total, entirely inside the lab) gives no reason to add a narrower, redundant case for
zero real-world benefit.

`TR057` (`TR.BareNullToObj`) in `Findings.fs` is left declared but never raised, exactly the
"dead row" outcome the brief priced in.

## Changes made

- `tests/fixtures/absence-alphabet-lab/index.d.ts`: extended the `onlyNull` doc comment to name
  `TR014` as the finding that actually fires at the site, so a future reader does not read "TR033
  stays silent" as "nothing fires here."
- `tests/Xantham.Generator.Tests/golden/absence-alphabet-lab/AbsenceAlphabetLab.fs`: regenerated
  golden reflecting the comment change (`--update`).
- No changes to `Findings.fs`, `Shape/Spec.fs`, or any type-reference mapping — behaviour is
  unchanged by design.

## Measurements

- `absence-alphabet-lab` suite: 5/5 passed (`--filter "absence"`, `--update`).
- Full gated run (`dotnet fsi build.fsx -- test`): **460** generator tests passed, **90** wire
  tests passed (1 skipped), run gate **230** checks passed — all match the stated baselines
  exactly.
- `TR057` count: **0** (never raised).
- `TR014` count: **1** total across the entire corpus, all inside `absence-alphabet-lab`
  (`Absence.onlyNull`); zero elsewhere.
- Corpus tier totals unaffected (comment-only change): no shift in exact/ergonomic/widened/escape
  counts anywhere.
- `git diff --ignore-cr-at-eol --stat` after the full run showed ~47 files with fantomas
  line-ending-only churn; all reverted with `git checkout --` before committing, leaving only the
  two real files above.
