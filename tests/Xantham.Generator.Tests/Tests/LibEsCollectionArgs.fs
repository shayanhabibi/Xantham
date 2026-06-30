module Xantham.Generator.Tests.Tests.LibEsCollectionArgs

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Mocking.ArenaInterner.ResolvedType

// ── Bare generic lib.es collection reference must carry its element argument ─────
//
// ROOT CAUSE (IR, surfaced in the generator's lib.es substitution):
//   `Array<'T>`'s OWN self-returning methods (`with`/`toSpliced`/`toReversed`/...) record
//   their return type in the IR as a BARE reference to the `Array` Interface — the implicit
//   self-application `Array<'T>` is lost upstream, so NO surrounding `TypeReference` carries
//   the element arg. The lib.es substitution interceptor (LibEsSubstitution.prelude) maps
//   `Array` -> `ResizeArray`; before the fix it built a NAME-ONLY `ResizeArray` atom, dropping
//   the argument and emitting bare `ResizeArray` -> FS0033 "The type 'ResizeArray<_>' expects
//   1 type argument(s) but is given 0" (~76 occurrences in the real cf regen).
//
// FIX (LibEsSubstitution.prelude): when the substituted target is a GENERIC F#/Fable collection
//   (ResizeArray / seq / IReadOnlyList) and the substituted Interface declares type parameters,
//   recover the args from the Interface's OWN declared type params -> `ResizeArray<'T>`. A real
//   `TypeReference{Array,[elem]}` still renders `ResizeArray<elem>` because the args-carrying arm
//   (RenderScope.Prelude.fs, `applyArgsToCollectionHead`) collapses the placeholder and applies
//   the real args (no double-wrap `ResizeArray<'T><elem>`).

// A fresh context per call carrying the REAL lib.es substitution interceptor (the single source
// of truth shared with the generator). Per-call isolation mirrors the generator's single-threaded
// prerender and avoids the shared mutable PreludeRenders cache racing under Expecto's parallelism.
let private libEsContext () =
    GeneratorContext.EmptyWithCustomisation (fun customiser ->
        { customiser with
            Customisation.Interceptors.ResolvedTypePrelude = fun _ -> LibEsSubstitution.prelude })

let private testRender (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender (libEsContext ()) ref
    |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef expectedTypeText

// The lib.es `Array<'T>` interface: IsLibEs, one declared type parameter `'T`.
let private libEsArray =
    Interface.create "Array"
    |> Interface.esLib
    |> Interface.withTypeParameters [ TypeParameter.create "T" ]
    |> Interface.wrap

[<Tests>]
let tests =
    testList "lib.es generic collection arg recovery" [
        // THE FIX: a BARE reference to the generic lib.es `Array` interface (no surrounding
        // TypeReference, no args) must render `ResizeArray<'T>` — recovering the element arg from
        // the interface's OWN declared type parameter — NOT bare `ResizeArray` (FS0033).
        testCase "bare generic lib.es Array reference renders ResizeArray<'T> not bare" <| fun _ ->
            libEsArray
            |> testRender "ResizeArray<'T>"
            ||> Flip.Expect.equal "bare Array interface ref must carry its element arg as ResizeArray<'T>"

        // NO DOUBLE-WRAP: a real `Array<string>` application (TypeReference carrying the element
        // arg) must render `ResizeArray<string>` — the args-carrying arm collapses the substituted
        // head's placeholder `'T` and applies the real arg, NOT `ResizeArray<'T><string>`.
        testCase "Array<string> application renders ResizeArray<string> (no double-wrap)" <| fun _ ->
            libEsArray
            |> TypeReference.create
            |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
            |> TypeReference.wrap
            |> testRender "ResizeArray<string>"
            ||> Flip.Expect.equal "Array<string> must render ResizeArray<string> with the real element arg"
    ]
