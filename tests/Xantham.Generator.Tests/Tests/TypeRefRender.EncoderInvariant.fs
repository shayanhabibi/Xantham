module Xantham.Generator.Tests.Tests.TypeRefRenderEncoderInvariant

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Mocking.ArenaInterner.ResolvedType
open Xantham.Generator.Generator
open Xantham.Generator.Types

let ctx = GeneratorContext.Empty

[<Tests>]
let encoderInvariantTests =
    testList "EncoderInvariantViolation" [
        testCase "throws when TypeReference has asymmetric args and params" <| fun _ ->
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

            Expect.throwsT<EncoderInvariantViolation> prerender "Should throw EncoderInvariantViolation"

        testCase "succeeds when TypeReference args match declared params" <| fun _ ->
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
            Expect.isOk (
                try
                prerender()
                Ok()
                with
                | _ -> Error()
                ) "Should not throw EncoderInvariantViolation" 
    ]
