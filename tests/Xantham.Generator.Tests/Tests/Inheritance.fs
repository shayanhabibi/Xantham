module Xantham.Generator.Tests.Tests.Inheritance

open Expecto
open Fabulous.AST
open Mocking.ArenaInterner.ResolvedType
open Xantham
open Xantham.Generator
open Xantham.Generator.Types


let private testRender = TypeRefRender.testRender

let interfaceTypeParameters =
    [
        let defaultForT = primitive TypeKindPrimitive.String
        TypeParameter.create "U"
        TypeParameter.create "T"
        |> TypeParameter.withDefault defaultForT
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
            |> testRender "Global.TestInterface<_, string>"
            ||> Flip.Expect.equal ""
        testCase "typeref with typars, interface with typars" <| fun _ ->
            testTypeReference
            |> testRender "TestInterface<'T, string>"
            ||> Flip.Expect.equal ""
    ]