module Xantham.Generator.Tests.Tests.TypeRefRenderEncoderInvariant

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Mocking.ArenaInterner.ResolvedType
open Xantham.Generator.Generator
open Xantham.Generator.Types

let ctx = GeneratorContext.Empty

// TypeScript declaration merging can produce a TypeReference whose
// argument count doesn't match the inner type's declared type parameter
// count — e.g. a class `extends Body<X>` where the resolved Body
// declaration has no type params (this shape really occurs in lib.dom +
// merged extensions, surfaced when generating bindings for `agents` and
// `@cloudflare/workers-types`). The generator previously raised
// EncoderInvariantViolation here, halting the entire run. The current
// behaviour is to log a warning and degrade gracefully — render the
// inner type without the extra arguments — which produces valid F#.

[<Tests>]
let encoderInvariantTests =
    testList "TypeReference arity mismatch" [
        testCase "asymmetric args and params no longer throws (degrades to inner type)" <| fun _ ->
            let interface2Params =
                Interface.create "TestInterface"
                |> Interface.withTypeParameters [
                    TypeParameter.create "T"
                    TypeParameter.create "U"
                ]
                |> Interface.wrap

            let typeRefWith3Args =
                TypeReference.create interface2Params
                |> TypeReference.withTypeArguments [
                    TypeParameter.create "A" |> TypeParameter.wrap
                    TypeParameter.create "B" |> TypeParameter.wrap
                    TypeParameter.create "C" |> TypeParameter.wrap
                ]
                |> TypeReference.wrap

            let prerender () =
                let scope = RenderScopeStore.create ()
                prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeRefWith3Args)
                |> ignore

            // Previously: Expect.throwsT<EncoderInvariantViolation> ...
            // Now: must not throw at all.
            prerender ()

        testCase "matching args and params still renders normally" <| fun _ ->
            let interface2Params =
                Interface.create "TestInterface"
                |> Interface.withTypeParameters [
                    TypeParameter.create "T"
                    TypeParameter.create "U"
                ]
                |> Interface.wrap

            let typeRefWith2Args =
                TypeReference.create interface2Params
                |> TypeReference.withTypeArguments [
                    TypeParameter.create "A" |> TypeParameter.wrap
                    TypeParameter.create "B" |> TypeParameter.wrap
                ]
                |> TypeReference.wrap

            let prerender () =
                let scope = RenderScopeStore.create ()
                prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeRefWith2Args)
                |> ignore
            prerender ()

        // Specific repro: lib.dom Body has 0 type params; class heritage
        // applies 1. This was the exact shape that crashed agents.
        testCase "0-param Interface with 1 type argument degrades to inner type" <| fun _ ->
            let body =
                Interface.create "Body"
                |> Interface.wrap
            let typeRef =
                TypeReference.create body
                |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
                |> TypeReference.wrap
            let scope = RenderScopeStore.create ()
            prerender ctx scope (LazyContainer.CreateTypeKeyDummy<ResolvedType> typeRef)
            |> ignore
    ]
