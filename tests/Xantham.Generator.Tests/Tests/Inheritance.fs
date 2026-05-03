module Xantham.Generator.Tests.Tests.Inheritance

open Expecto
open Mocking.ArenaInterner.ResolvedType
open Xantham


let private testRender = TypeRefRender.testRender

let interfaceTypeParameters =
    [
        let defaultForT = primitive TypeKindPrimitive.String
        TypeParameter.create "T"
        |> TypeParameter.withDefault defaultForT
        TypeParameter.create "U"
    ]

let testInterface =
    Interface.create "TestInterface"
    |> Interface.withTypeParameters interfaceTypeParameters
    |> Interface.wrap

let testTypeReference =
    TypeReference.create testInterface
    |> TypeReference.withTypeArguments [
        TypeParameter.create "T"
        |> TypeParameter.wrap
    ]
    |> TypeReference.wrap

[<Tests>]
let inheritanceTests =
    testList "Inheritance" [
        testCase "interface with typars" <| fun _ ->
            testInterface
            |> testRender "Global.TestInterface<string, _>"
            ||> Flip.Expect.equal ""
        testCase "typeref with typars, interface with typars" <| fun _ ->
            testTypeReference
            |> testRender "TestInterface<'T, 'U>"
            ||> Flip.Expect.equal ""
    ]