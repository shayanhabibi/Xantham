# TypeScript's utility types, and which of them Fable can help with

Counts are corpus-wide over 55 fixtures at `62bd44e` (wave fourteen, batch one composed), from
`dotnet fsi build.fsx -- findings` and from the committed `symbols.jsonl`. Pins are `Fable.Core`
**5.2.0**, the `fable` tool 5.0.0, `fable-library-js` 5.0.0.

**Answer inline, in the `(ANSWER)` blocks, as you did for `fable5-workarounds.md`.**

Companion documents: `fable-binding-gaps.md` is names `Fable.Core` does not ship;
`fable5-workarounds.md` is Fable's compilation model losing something. This one is TypeScript's
type-level operators, and it exists because the answer differs sharply between them — **most of the
mass here is not Fable's to fix, and section 0 says which, so that the items you take to Fable are
the ones Fable can actually close.**

---

## 0. Read this before raising anything

The corpus uses utility types heavily. Applied to a **concrete** operand, almost all of them are
already fine:

| Utility type | Uses in the four npm fixtures |
| --- | ---: |
| `Record` | 299 |
| `Pick` | 104 |
| `Exclude` | 70 |
| `Required` | 63 |
| `Partial` | 42 |
| `Omit` | 38 |
| `NonNullable` | 37 |
| `Extract` | 23 |
| `Readonly` | 22 |
| `ReturnType` | 15 |
| `Parameters` | 13 |
| `NoInfer` | 13 |
| `Uppercase` / `Lowercase` / `Capitalize` | 12 / 10 / 6 |
| `InstanceType` / `Awaited` | 6 / 6 |

**The checker resolves a mapped or conditional type over a concrete operand before the generator
sees it.** `Pick<Options, "a" | "b">` arrives as an ordinary object type with two members, and the
binding emits an interface with two members. No finding fires, nothing widens, and there is nothing
to ask Fable for. The same holds for `Partial`, `Required`, `Readonly`, `Omit`, `Exclude`,
`Extract`, `NonNullable`, and the string-manipulation family.

**What loses is a utility type applied to a still-generic parameter.** TypeScript defers the
computation until the parameter is known; F# has no deferral, so there is nothing to emit. That is
section 4, it is 382 findings, it is the single largest block in this document, and **it is not
actionable with Fable** — no name Fable ships and no change to its compilation model closes it.

So the order to work in is: **sections 1 and 2 are real Fable asks. Section 3 is a question for
you. Section 4 is a decline and is here so it stops being re-raised.**

---

## 1. A bound name that takes fewer type parameters than TypeScript gives it

**70 findings**, key `TR024` (`TR.LibExtraTypeArgumentsDropped`), graded ergonomic. Every one is a
name `Fable.Core` *does* ship — at a lower arity than the current lib declares, so the extra
arguments are dropped and the element type is lost.

This is the crispest Fable ask in the document: the names exist, only their generic parameters are
missing.

| TypeScript | given | F# destination | takes | sites |
| --- | ---: | --- | ---: | ---: |
| `ArrayBufferView<TArrayBuffer>` | 1 | `JS.ArrayBufferView` | 0 | **63** |
| `DataView<TArrayBuffer>` | 1 | `JS.DataView` | 0 | 2 |
| `AsyncIterable<T, TReturn, TNext>` | 3 | `JS.AsyncIterable<'T>` | 1 | 2 |
| `AsyncIterator<T, TReturn, TNext>` | 3 | `JS.AsyncIterator<'T>` | 1 | 1 |
| `AsyncIterableIterator<T, TReturn, TNext>` | 3 | `JS.AsyncIterable<'T>` | 1 | 1 |
| `ProgressEvent<T>` | 1 | `Browser.Types.ProgressEvent` | 0 | 1 |

`ArrayBufferView` is 63 of the 70 and is almost the whole item. TypeScript made it generic over its
backing buffer (`ArrayBufferView<TArrayBuffer extends ArrayBufferLike = ArrayBufferLike>`), which
is what lets a declaration say "a view onto a `SharedArrayBuffer`" rather than onto any buffer.
`JS.ArrayBufferView` is non-generic, so every one of those 63 sites loses which buffer it views.

The `AsyncIterable` family is the same shape for a different reason: TypeScript's iteration
protocol types took two further parameters (`TReturn`, `TNext`) and Fable's carry one.

### What the binding emits

```fsharp
abstract buffer: JS.ArrayBufferView with get, set
```

where TypeScript declared `ArrayBufferView<SharedArrayBuffer>`.

[//]: # (ANSWER)
FSharp allows two types to exist with disparate typar arity. Therefore we should implement the other arities as
utilities for keeping typescript information, and just lose the extra typars where they serve no purpose.

The extra arities must be implemented in the same way that is discussed IN THE NEXT TWO ANSWERS.

[//]: # (ANSWER END)

---

## 2. Names the lib declares that `Fable.Core` does not ship at all

**136 findings**, key `TR023` (`TR.NotAmongGeneratedDeclarations`), graded widened. Each is a name a
package references and the binding has nowhere to send, so it reads `obj`.

`Iterable`, `IterableIterator` and `BigUint64Array` are already entries 1 and 3 of
`fable-binding-gaps.md`; they are repeated here only for the count. **The rest of this list has not
been recorded anywhere before.**

Grouped by what they are:

**Iteration protocol — 29 sites.** `IterableIterator` 20, `Iterable` 9. Already
`fable-binding-gaps.md` entry 1.

**Typed arrays — 5 sites.** `BigUint64Array` 3 (already entry 3), `Float16Array` 2. The families are
otherwise complete, which is what makes the holes conspicuous.

**DOM geometry — 20 sites.** `DOMMatrix` 6, `DOMPoint` 4, `DOMRect` 3, `DOMPointInit` 2,
`DOMRectReadOnly`, `DOMRectList`, `DOMQuad`, `DOMPointReadOnly`, `DOMMatrixReadOnly` 1 each.

**DOM events and listeners — 14 sites.** `EventListenerObject` 4, `EventListener` 4,
`EventListenerOptions` 3, `SecurityPolicyViolationEvent`, `FormDataEvent`,
`OnErrorEventHandlerNonNull` 1 each.

**Web Animations — 13 sites.** `Animation` 9, `KeyframeAnimationOptions`, `Keyframe`,
`PropertyIndexedKeyframes`, `GetAnimationsOptions` 1 each.

**DOM collections and traversal — 15 sites.** `HTMLCollectionOf` 10, `ParentNode`, `Range`,
`HTMLSlotElement`, `CustomElementRegistry`, `GetRootNodeOptions` 1 each.

**Element options records — 10 sites.** `ScrollToOptions` 3, `FullscreenOptions`, `FocusOptions`,
`CheckVisibilityOptions`, `PointerLockOptions`, `SVGBoundingBoxOptions`, `GetHTMLOptions`,
`ErrorOptions` 1 each.

**File System Access — 3 sites.** `FileSystemHandle`, `FileSystemFileHandle`,
`FileSystemDirectoryHandle`.

**CSS Typed OM — 2 sites.** `StylePropertyMap`, `StylePropertyMapReadOnly`.

**WebGPU — 2 sites.** `GPUCompilationMessage`, `GPUCompilationInfo`.

**Media — 2 sites.** `VideoFrame`, `AudioData`.

**Miscellaneous — 12 sites.** `ResizeObserver` 3, `Response` 2, `RegExp` 2, `ImageBitmap`,
`CryptoKey`, `String`, `Number`, `Boolean` 1 each. The three boxed primitives and `RegExp` are
probably a generator question rather than a Fable one and are flagged as such below.

The groups above sum to 127; with the nine below that are ours rather than yours, the total is the
136 that `TR023` reports.

### Two of these are ours, not yours

`Module` 8 and `Env` 1 are `@cloudflare/workers-types`'s own names reached across a group boundary.
They are a generator gap and are **not** a Fable ask. Listed for completeness so the 136 reconciles.

Likewise `String`, `Number`, `Boolean` and `RegExp` are boxed-primitive and built-in references that
almost certainly want mapping to `string`, `float`, `bool` and `System.Text.RegularExpressions` in
the generator rather than binding in `Fable.Core`. Also not a Fable ask.

The **Browser DOM families above are the real question**: most are declared in the
`Fable.Browser.*` package family rather than in `Fable.Core`, and the compile gate already
references several of those packages. So the ask may be "which `Fable.Browser.*` package should the
generator's lib table point at" rather than "please add these names".

[//]: # (ANSWER)
SEE NEXT ANSWER.

We should be creating our own Fable.Browser implementation. Where we descend into recursion and lose information
compared to the original implementation, we will simply copy the original implementations code/binding.

This has been asked for MULTIPLE TIMES. And is consistently deferred.

I don't care if these would ship with every library that asks for them. That wouldn't be the case if
what we require from the agents was done. ONE corpus that generates the library bindings that we will publish
and ALL other user generations will bind on.

It will ALWAYS be published and handled from this repository.

It will ALWAYS maintain the same naming scheme as upstream Fable.Browser (SEE NEXT ANSWER).

[//]: # (ANSWER END)

---

## 3. `Record` — we ship our own, and it is the most-used utility type in the corpus

`Record<K, V>` is used **299 times** across the four npm fixtures, more than any other utility type.

Wave thirteen's lane CH made an object whose whole content is one index signature reference
`Xantham.Fable.Core.Record<'Key,'Value>` rather than mint an interface for it. That closed
`MB004` from 140 to 63 and now carries **221 sites** under `TR059`
(`TR.IndexSignatureAsRecord`, ergonomic). It works, and it is gated at the run gate.

The point for you is that **this repository declares the type, in `src/Xantham.Fable.Core/Record.fs`,
because `Fable.Core` ships nothing for it.** Every generated binding therefore depends on a support
assembly of ours for its single commonest construct.

Two questions, and they are yours rather than Fable's to answer first:

1. Should `Record`/`ReadonlyRecord` be raised upstream so a binding can depend on `Fable.Core`
   alone?
2. If not, is a support assembly the intended distribution shape for generated bindings
   permanently?

[//]: # (ANSWER)
($REF1)
EXPLICITLY: Always assume it is located at Fable.Core.
It is NOT YET shipped upstream, because it requires THIS to be published and require
the upstream changes. We are acting as the proof for the changes. Therefor, for the moment, WE MUST
publish our own version in the Xantham.Fable.Core  project.

Something that all agents have mishandled, is that the `Brand` and `Record` implementations landed in
a separate file to the `Xantham.Fable.Core/Library.fs` file. The `Brand` and `Record` implementations are
subsequently under the namespace `namespace Xantham.Fable.Core`. This is INCORRECT.

We MUST shadow into `Fable.Core` or `Fable.Core.JsInterop`.

This is done in `Xantham.Fable.Core/Library.fs` using `module [<AutoOpen>] Fable.Core.JsInterop.XanthamExtensions`.
THAT is the shape to follow.

THIS ALSO APPLIES TO BROWSER/LIB IMPLEMENTATIONS.

[//]: # (ANSWER END)

---

## 4. Utility types over a generic parameter — not a Fable item, recorded so it stops recurring

**382 findings** across two keys, and the largest block in this document. **Nothing Fable ships or
changes closes any of it.**

| Key | Name | Count | Where |
| --- | --- | ---: | --- |
| `SA002` | `SA.PhantomComputation` | 197 | `type-fest` 170, `@cloudflare` 11, `solid-js` 7, labs 9 |
| `TR045` | `TR.ConditionalTypeDeferred` | 185 | `type-fest` 154, `solid-js` 20, `@cloudflare` 3, rest 8 |

`type-fest` holds 324 of the 382, which is unsurprising: it is a library *of* utility types, and
its whole content is type-level computation over parameters that are generic by construction.
`type-fest`'s 202 widened symbols are this and nothing else.

TypeScript defers `T extends U ? A : B` until `T` is known and resolves it at each instantiation.
**F# has no deferred type**, so there is no form to emit and no name that would help. The generator
emits an erased phantom, which casts are the only use of.

`solid-js`'s block is one idiom repeated: `NoInfer<T> = [T][T extends any ? 0 : never]`, used 13
times in source and reaching 20 `TR045` sites plus about 19 of its 22 `TR020`
(`TR.IndexedAccessNoForm`) sites, because the index is itself a deferred conditional.

Waves twelve, thirteen and fourteen each declined this independently. **It is here to be declined
once in writing rather than re-priced every wave.** If you disagree, the thing to challenge is the
premise that F# cannot defer — not any Fable capability.

[//]: # (ANSWER — only if you disagree with the decline)

Added to our Xantham.Fable.Core/Library.fs.
I've forcefully done this for Measures, Record and for NoInfer since the implementation instructions
have been screwed up multiple times. NoInfer we should just be dropping
to its typar instead of `obj`. Other utilities we will have to just drop if we can't find better
implementations that are close.

[//]: # (ANSWER END)

---

## 5. Erased-union arity above nine

**6 findings**, key `TR036` (`TR.UnionTooWide`), graded widened.

`Fable.Core` ships `U2`–`U9`. Wave thirteen raised the generator's cap from 4 to 9 to match, which
took the count from 72 to 9 and now to 6. The remainder sit at arity 10, 11 and 14 and widen to
`obj`.

Wave thirteen considered generating `U10`–`U100` into this repository's support assembly and
withdrew it on cost: 5,005 union cases and 5,005 `op_ErasedCast` overloads, against which **every
`!^` cast in every consumer's code would resolve**. The recorded decision is that the ceiling is
what `Fable.Core` ships.

The question for you is only whether `Fable.Core` intends to ship above `U9`. If not, this is
closed and six sites widen forever, which is an acceptable answer.

If it ever is attempted, wave thirteen recorded the proof obligation: an erased union that is not
actually erased fails silently at runtime, so a `U10` must be proven at the run gate beside a `U9`
control before the other ninety are generated.

[//]: # (ANSWER)
Whenever a shipped file/package creates a union type with arity greater than 9, it must be generated
inline at the footer of the file.

IE. If a package uses arity 12, then a U12 implementation is shipped at the bottom of the file.

Don't price this, just do it. I don't need you to test the (!^) semantics or whatever. If it doesn't work,
it's cheap for me to tell you to get rid of it. It's expensive to have the team cycling on it.

This is NON-NEGOTIABLE.

[//]: # (ANSWER END)

---

## How to work through this

Sections 1 and 2 are the Fable conversation and together they are 206 findings. Section 1 is the
better-shaped ask — six names that exist and need generic parameters, 63 of them one name. Section
2 is larger but mostly resolves to "which `Fable.Browser.*` package should the lib table point at",
which may need no change from Fable at all.

Section 3 is a distribution question about `Record` and it is yours before it is Fable's. Section 5
is a yes-or-no. **Section 4 is 382 findings and needs nothing from anyone** — it is the mapping
working as designed.
