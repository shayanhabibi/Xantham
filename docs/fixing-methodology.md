# Fixing methodology — diagnose inside-out (deepest stratum first)

Xantham is a staged pipeline:

```
TypeScript → Fable (encoder) → IR JSON → Decoder → Generator → render (Fantomas)
             └ owns type IDENTITY      └ resolution   └ naming/paths/render
```

**When a defect appears in the generated F#, trace it to the EARLIEST stage that could be
wrong, and fix it there — before reaching for a decoder or generator patch.**

## Why inside-out

A defect born upstream (the encoder loading the wrong libs, binding a reference to the wrong
symbol, minting a duplicate identity) propagates through every downstream stage *as if it
were real data*. Each stage then grows a compensating patch — a substitution map here, a
redirect there, a placement special-case elsewhere. Those patches are **boundary patches**:
they multiply, they never fully converge (you keep finding "one more case"), and they bury
the real cause under layers of workaround. Fixing the originating stratum dissolves the whole
symptom class at once and almost always reaches further than any boundary patch could.

## The canonical lesson — lib.dom (2026-06-29)

~193 dangling references to `AbortSignal`, `Headers`, `ReadableStream`, `MessageEvent`,
`Crypto`, plus undefined `Iterable`/`HTMLCollectionOf`/`Attr`, looked like a
generator/decoder problem: duplicate "twins" of each type, references resolving to an
unemitted copy, inconsistent module placement. The tempting fixes were all on the outer
boundary — a generator substitution map, then a decoder twin-redirect keyed by name.

The actual root was **one line in the encoder**: `src/Xantham.Fable/Types/Reader.fs`
`commonCompilerOptions` set `target = Latest` but never set `lib`. With no `lib`, TypeScript
defaults to including `lib.dom.d.ts` — but the Cloudflare Workers runtime has **no DOM**. So
lib.dom's `interface AbortSignal` etc. were loaded as phantom globals, collided with
workers-types' own `class AbortSignal`, and references bound to the DOM twin (which is never
emitted). Setting `lib = ["lib.esnext.d.ts"]` (matching Cloudflare's own `lib: ["esnext"]`)
removed the phantoms at the source:

- IR: **19,624 → 7,385 types**; libEs exports **11,102 → 642**.
- Twins collapsed to the single workers declaration; `Request.signal` now binds the workers
  `AbortSignal`.
- Generated-output errors: **2,668 → 2,396 (−272)** — the largest single drop — with no crash,
  and it obviated the boundary patches that were in flight.

## The checklist (apply before any decoder/generator fix)

1. **Inspect the IR first** (`cf-staged.json` / a fresh extraction). Is the type identity,
   reference binding, or member set already wrong *in the IR*? If so, the fix is upstream.
2. **Check the encoder's TS-program config** (`Reader.fs` compiler options) and symbol
   resolution (`Reading/TypeReference.fs`, duplicate winner-selection in `Read.fs`). Phantom
   libs, wrong `target`/`lib`/`moduleResolution`, and mis-bound symbols originate here.
3. Only when the IR is provably correct does the defect belong to the **decoder** (resolution
   of a correct IR) or the **generator** (naming, paths, rendering of a correct graph).
4. Reaching for string manipulation anywhere but the final renderer is a smell. Reaching for
   a name-keyed redirect/substitution to paper over a duplicate is a sign the duplicate
   should not exist — look upstream.

This ordering sits on top of the layer-ownership rule (fix at the owning stage): inside-out
tells you *which stage owns it*, by forcing you to find where the defect is actually born.
