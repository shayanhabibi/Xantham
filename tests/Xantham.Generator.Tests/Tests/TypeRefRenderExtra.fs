module Xantham.Generator.Tests.Tests.TypeRefRenderExtra

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.TypeRefRender
open Mocking.ArenaInterner.ResolvedType

let private testRender = Xantham.Generator.Tests.Tests.TypeRefRender.testRender

let nestedArrayTests = testList "Nested Arrays" [
    testCase "Array of Array of primitive" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> Array.create
        |> Array.create
        |> testRender "ResizeArray<ResizeArray<string>>"
        ||> Flip.Expect.equal ""
    testCase "Array of Array of Array" <| fun _ ->
        primitive TypeKindPrimitive.Integer
        |> Array.create
        |> Array.create
        |> Array.create
        |> testRender "ResizeArray<ResizeArray<ResizeArray<int>>>"
        ||> Flip.Expect.equal ""
    testCase "Array of union" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
        ]
        |> Union.create
        |> Array.create
        |> testRender "ResizeArray<U2<string, int>>"
        ||> Flip.Expect.equal ""
    testCase "Array of nullable primitive" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Null
        ]
        |> Union.create
        |> Array.create
        |> testRender "ResizeArray<option<string>>"
        ||> Flip.Expect.equal ""
]

let extraTupleTests = testList "Extra Tuples" [
    testCase "Tuple with all optional" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            |> Tuple.createElement
            |> Tuple.Element.optional
            primitive TypeKindPrimitive.Integer
            |> Tuple.createElement
            |> Tuple.Element.optional
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "option<string> * option<int>"
        ||> Flip.Expect.equal ""
    testCase "Tuple with mixed optional positions" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            |> Tuple.createElement
            primitive TypeKindPrimitive.Integer
            |> Tuple.createElement
            |> Tuple.Element.optional
            primitive TypeKindPrimitive.Boolean
            |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "string * option<int> * bool"
        ||> Flip.Expect.equal ""
    testCase "Tuple of arrays" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            |> Array.create
            |> Tuple.createElement
            primitive TypeKindPrimitive.Integer
            |> Array.create
            |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "ResizeArray<string> * ResizeArray<int>"
        ||> Flip.Expect.equal ""
    testCase "Tuple of unions" <| fun _ ->
        [
            Union.create [
                primitive TypeKindPrimitive.String
                primitive TypeKindPrimitive.Integer
            ]
            |> Tuple.createElement
            Union.create [
                primitive TypeKindPrimitive.Boolean
                primitive TypeKindPrimitive.Number
            ]
            |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "U2<string, int> * U2<bool, float>"
        ||> Flip.Expect.equal ""
]

let extraTypeReferenceTests = testList "Extra TypeReferences" [
    testCase "TypeReference with Array argument" <| fun _ ->
        primitive TypeKindPrimitive.NonPrimitive
        |> TypeReference.create
        |> TypeReference.withTypeArguments [
            primitive TypeKindPrimitive.String
            |> Array.create
        ]
        |> TypeReference.wrap
        |> testRender "obj<ResizeArray<string>>"
        ||> Flip.Expect.equal ""
    testCase "TypeReference with tuple argument" <| fun _ ->
        primitive TypeKindPrimitive.NonPrimitive
        |> TypeReference.create
        |> TypeReference.withTypeArguments [
            [
                primitive TypeKindPrimitive.String |> Tuple.createElement
                primitive TypeKindPrimitive.Integer |> Tuple.createElement
            ]
            |> Tuple.create
            |> Tuple.wrap
        ]
        |> TypeReference.wrap
        |> testRender "obj<string * int>"
        ||> Flip.Expect.equal ""
]

let extraUnionTests = testList "Extra Unions" [
    testCase "Union of arrays" <| fun _ ->
        [
            primitive TypeKindPrimitive.String |> Array.create
            primitive TypeKindPrimitive.Integer |> Array.create
        ]
        |> Union.create
        |> testRender "U2<ResizeArray<string>, ResizeArray<int>>"
        ||> Flip.Expect.equal ""
    testCase "Union of tuples" <| fun _ ->
        [
            [
                primitive TypeKindPrimitive.String |> Tuple.createElement
                primitive TypeKindPrimitive.Integer |> Tuple.createElement
            ] |> Tuple.create |> Tuple.wrap
            [
                primitive TypeKindPrimitive.Boolean |> Tuple.createElement
                primitive TypeKindPrimitive.Number |> Tuple.createElement
            ] |> Tuple.create |> Tuple.wrap
        ]
        |> Union.create
        |> testRender "U2<string * int, bool * float>"
        ||> Flip.Expect.equal ""
    testCase "Single non-nullable primitive union collapses" <| fun _ ->
        [ primitive TypeKindPrimitive.String ]
        |> Union.create
        |> testRender "string"
        ||> Flip.Expect.equal ""
    testCase "Single nullable primitive union becomes option" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Null
        ]
        |> Union.create
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "BigInt within a union" <| fun _ ->
        [
            primitive TypeKindPrimitive.BigInt
            primitive TypeKindPrimitive.String
        ]
        |> Union.create
        |> testRender "U2<bigint, string>"
        ||> Flip.Expect.equal ""
]

let extraCallSignatureTests = testList "Extra Call Signatures" [
    testCase "returns array" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> Array.create
        |> CallSignature.create
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "unit -> ResizeArray<string>"
        ||> Flip.Expect.equal ""
    testCase "returns union" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
        ]
        |> Union.create
        |> CallSignature.create
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "unit -> U2<string, int>"
        ||> Flip.Expect.equal ""
    testCase "takes array returns array" <| fun _ ->
        primitive TypeKindPrimitive.Integer
        |> Array.create
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.String
            |> Array.create
            |> Parameter.create "items"
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "ResizeArray<string> -> ResizeArray<int>"
        ||> Flip.Expect.equal ""
]

[<Tests>]
let tests = testList "TypeRef Extra" [
    nestedArrayTests
    extraTupleTests
    extraTypeReferenceTests
    extraUnionTests
    extraCallSignatureTests
]
