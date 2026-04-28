(**
---
title: Names and Casing
category: Xantham.Decoder
categoryindex: 2
index: 3
---

# `Name` — modified-source-preserving identifiers

The decoder threads a `Name` value type through anywhere an identifier is
exposed. `Name` carries either:

* `Source of original` — the identifier as it appeared in the source.
* `Modified of original * modified` — both the original and a transformed
  form.

This dual representation lets generators rename or normalize identifiers
without losing the original spelling — which is often required for output
attributes (`[<CompiledName "...">]`), error messages, or round-tripping.

## Creating names
*)

(*** condition: prepare ***)
#I "../../src/Xantham.Decoder/bin/Debug/net10.0"
#r "Xantham.Decoder.dll"
open Xantham.Decoder
(** *)

let n1 = Name.create "fooBar", Name.create "type" // backtick-normalized if needed
let n2 = Name.Create "fooBar", Name.Create "type" // raw, no normalization
let n3 = Name.Create("type", "``type``")
let n4 = Name.createModified "type" "``type``"
(*** hide ***)
{| n1 = n1; n2 = n2; n3 = n3; n4 = n4 |}
(*** include-it ***)

(**
`Name.create` runs the active normalization (default: F# backtick quoting via
`NormalizeIdentifierBackticks`). Use the static `Create` if you need the raw
form.

## Reading the value back

Two accessors:

* `name.ValueOrSource` — always the original string.
* `name.ValueOrModified` — the modified form if any, otherwise the original.
*)

let original = n3.ValueOrSource     // "type"
let display  = n3.ValueOrModified   // "``type``"

(*** hide ***)
{| original = original; display = display |}
(*** include-it ***)

(**
## Mapping over names

`Name` exposes three map functions that differ in *which* string they
transform and how the result is wrapped:

* `Name.map f` — applies `f` to the modified value (or original if
  unmodified). If the result equals the original, the name collapses back to
  `Source`.
* `Name.mapSource f` — always applies `f` to the original. Returns
  `Modified` if the result differs.
* `Name.mapModified f` — applies `f` only to the modified part of an already-
  modified name; otherwise no-op.

## Pre-baked normalisations

Several common transforms are provided:

| Function | Behaviour |
|----------|-----------|
| `Name.normalize` | Apply the active normalization (default backticks). |
| `Name.pascalCase` / `sourcePascalCase` | PascalCase the modified / source value. |
| `Name.camelCase` / `sourceCamelCase` | camelCase the modified / source value. |
| `Name.capitalize` / `sourceCapitalize` | Uppercase first character only. |
| `Name.normalizeForType` | Alias for `pascalCase`. |
| `Name.normalizeForParameter` | Alias for `camelCase`. |
| `Name.normalizeForProperty` / `Method` / `EnumCase` | Alias for the appropriate casing. |
| `Name.normalizeForTypeParameter` | PascalCase + leading single quote. |
| `Name.mapToModuleName` | PascalCase + leading `I`, used for module-as-interface naming. |

Each `for…` helper has a `source…` variant that operates on the original
string regardless of any prior modification.

## Customising normalization

The active normalization is global mutable state and can be swapped at
process startup via `Name.Normalization.setNormalizeSetting`:
*)

open Xantham.Decoder.Name.Normalization

setNormalizeSetting (SafeCustom (fun s -> s.Replace("$", "_")))
Name.create "$foo"
(*** include-it ***)

(**
Three modes:

* `Backticks` (default) — F# `NormalizeIdentifierBackticks` only.
* `SafeCustom f` — `f` runs *before* backticks; cannot break F# parser.
* `Custom f` — `f` replaces backticks entirely. Risky; only use if you know
  what your output language requires.

## Casing as a unit of measure

When you want the *type* of a `Name` to encode its casing, the decoder offers
units of measure under `Case`:

* `Case.pascal`
* `Case.camel`
* `Case.modulename`
* `Case.typar`

These are tags only — they perform no transformation by themselves. Combine
them with the strongly-typed sub-modules to get both the cast and the
transformation:
*)

let typeName : Name<Case.pascal> = Name.Pascal.create "foo_bar"
let propName : Name<Case.camel>  = Name.Camel.create  "FooBar"
let modName  : Name<Case.modulename> = Name.Module.create "foo"   // → IFoo
let typar    : Name<Case.typar>  = Name.Typar.create  "T"          // → 'T

(*** hide ***)
[
  (nameof typeName, unbox typeName)
  (nameof propName, unbox propName)
  (nameof modName, unbox modName)
  (nameof typar, unbox typar)
]
|> List.iter (fun x -> x ||> printfn "%s = %A")
(*** include-output ***)

(**
The four sub-modules (`Name.Pascal`, `Name.Camel`, `Name.Module`,
`Name.Typar`) each provide:

* `fromName` — convert from an untyped `Name`, applying the casing.
* `create` — convert from a `string`, applying the casing.
* `fromCase` — re-cast from another casing, re-applying the new transform.

For runtime dispatch over an unknown casing, use the `CasedName` DU:
*)

let any : CasedName = CasedName.Pascal typeName
let underlying : Name = any.Value

(**
## Working with cased names generically

`Name.Case` (the inner module) has measure-aware versions of the common
accessors so you don't have to drop measures manually:
*)

let v1 = Name.Case.valueOrModified typeName
let v2 = Name.Case.map (fun s -> s + "Suffix") propName
let isMod = Name.Case.isModified modName

(**
## Forcing measures

`Case` is a module that provides the unsafe primitives for working with cased
names and powers the strongly typed sub-modules `Name.Pascal`, `Name.Camel`,
`Name.Module`, and `Name.Typar`.

It **DOES NOT** perform any transformation on the underlying `Name`, and so
should be avoided in general use.

One scenario this is useful for is when you have a strongly typed field
such as a field representing a member name which is typed as
`Name<Case.camel>`, but you need to render a special meaning name such
 as `Invoke` or `Item`.
*)

// Trying to create a camel cased member named "Invoke"
Name.Camel.create "Invoke" // Name<Case.camel>
|> printfn "❌ %A"

Name.Pascal.create "Invoke" // Name<Case.pascal>
|> Name.Camel.fromCase // Name<Case.camel>
|> printfn "❌ %A"

Name.create "Invoke" // Name
|> Case.addCamelMeasure // Name<Case.camel>
|> printfn "✔️ %A"
(*** include-output ***)
