module Xantham.Generator.Tests.Tests.LibEsSubstitution

open Expecto
open Xantham.Generator.Generator

// Guards the single source of truth for TS-stdlib (lib.es / lib.dom) -> F#/Fable name
// substitution. Each mapped name MUST resolve to its faithful Fable equivalent, and
// non-stdlib names MUST be left untouched (None) so the surface's own types are never
// rewritten. A regression that drops/changes an entry here is caught immediately.

let private mappings = [
    // name,                expected substitution
    "Array",                Some "ResizeArray"
    "Error",                Some "exn"
    "PromiseLike",          Some "Promise"
    "IterableIterator",     Some "seq"
    "Iterator",             Some "seq"
    "AsyncIterableIterator", Some "seq"
    "Iterable",             Some "seq"
    "ReadonlyArray",        Some "System.Collections.Generic.IReadOnlyList"
    "ConcatArray",          Some "System.Collections.Generic.IReadOnlyList"
    "Function",             Some "obj"
    // lib.es/lib.dom globals Fable does not define -> erased to seq / obj (left unmapped they render
    // as a dangling bare name that Fable's transpiler rejects, e.g. `RegExp` is not defined).
    "StringIterator",       Some "seq"
    "RegExpStringIterator", Some "seq"
    "Generator",            Some "seq"
    "AsyncGenerator",       Some "seq"
    "RegExp",               Some "obj"
    "ReadonlyMap",          Some "obj"
    "PropertyKey",          Some "obj"
    "TemplateStringsArray", Some "obj"
]

// Names that are the surface's OWN types (or otherwise not stdlib) — must NOT be rewritten,
// or a `typescript`-sourced workers global like Response would be silently replaced.
let private untouched = [
    "Response"; "Request"; "WebSocket"; "ReadableStream"; "Headers"
    "MyCustomInterface"; "AbortSignal"; ""
    // lib.dom types: no longer in scope (encoder restricts lib to esnext), so they were
    // removed from the map and must NOT be substituted.
    "HTMLCollectionOf"; "Attr"
]

[<Tests>]
let tests =
    testList "LibEsSubstitution" [
        testList "mapped stdlib names" [
            for name, expected in mappings ->
                testCase $"{name} -> {expected}" <| fun _ ->
                    LibEsSubstitution.substitute name
                    |> Flip.Expect.equal $"substitute '{name}'" expected
        ]
        testList "non-stdlib names are untouched" [
            for name in untouched ->
                testCase $"'{name}' -> None" <| fun _ ->
                    LibEsSubstitution.substitute name
                    |> Flip.Expect.equal $"substitute '{name}' should be None" None
        ]
    ]
