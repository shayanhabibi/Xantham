module Xantham.Generator.Tests.Tests.TypeRefRender

open Expecto
open FSharp.SignalsDotnet
open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.TypeRefRender
open Mocking.ArenaInterner.ResolvedType

let ctx = GeneratorContext.Empty
let tryGet (ctx: GeneratorContext) resolvedType =
    ctx.PreludeRenders
    |> DictionarySignal.tryGet resolvedType

type TypeRefExpectation<'T> = 'T * string
    
let testTypeRef (expectedTypeText: string) (ref: TypeRefRender) =
    $"let _: %s{expectedTypeText} = JS.undefined",
    Ast.Oak() {
        Ast.AnonymousModule() {
            Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref)
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()
let testRender (expectedTypeText: string) (ref: ResolvedType) =
    let render = TestHelper.prerender ctx ref
    render.Value
    |> testTypeRef expectedTypeText
// let testAnchoredRender (anchorPosition: AnchorPath) (expectedTypeText: string) (ref: ResolvedType) =
//     TestHelper.prerender ctx ref
//     |> TypeRefRender.anchor anchorPosition
//     |> testTypeRef expectedTypeText
// let testAnchoredRelativeRender (relativePosition: AnchorPath) (anchorPosition: AnchorPath) (expectedTypeText: string) (ref: ResolvedType) =
//     TestHelper.prerender ctx ref
//     |> TypeRefRender.anchor anchorPosition
//     |> TypeRefRender.localisePaths relativePosition
//     |> testTypeRef expectedTypeText
    
let primitives = [
    TypeKindPrimitive.Any, "option<obj>"
    TypeKindPrimitive.Unknown, "option<obj>"
    TypeKindPrimitive.Never, "unit"
    TypeKindPrimitive.Void, "unit"
    TypeKindPrimitive.Undefined, "unit"
    TypeKindPrimitive.Null, "unit"
    TypeKindPrimitive.String, "string"
    TypeKindPrimitive.Integer, "int"
    TypeKindPrimitive.Number, "float"
    TypeKindPrimitive.Boolean, "bool"
    TypeKindPrimitive.BigInt, "bigint"
    TypeKindPrimitive.ESSymbol, "obj"
    TypeKindPrimitive.NonPrimitive, "obj"
]
    
let primitiveTests =
     primitives
     |> List.map (fun (primitive, expectedTypeText) ->
         testCase $"Primitive %A{primitive}" <| fun _ ->
             ResolvedType.Primitive primitive
             |> testRender expectedTypeText
             ||> Flip.Expect.equal ""
         )
     |> testList "Primitives"

let primitiveTuples =
    let clamp (i: int) = System.Int32.Clamp(i, 0, 12)
    let random = System.Random(0)
    let gen() = random.Next(0, 12)
    let sample (indexes: int list) =
        indexes
        |> List.map (clamp >> List.item >> funApply primitives)
    [ for _ in [0..20] do [ for _ in [ 0 .. 4 ] do gen() ] ]
    |> List.map (sample >> fun typeCombos ->
        let label = typeCombos |> List.map (fst >> _.ToString()) |> String.concat ", "
        testCase label <| fun _ ->
            let types, expectedTexts =
                typeCombos
                |> List.unzip
            types
            |> List.map (ResolvedType.Primitive >> Tuple.createElement)
            |> Tuple.create
            |> Tuple.wrap
            |> testRender (expectedTexts |> String.concat " * ")
            ||> Flip.Expect.equal ""
        )
    |> testList "Primitive tuples"

let primitiveArrays =
    primitives
    |> List.map (fun (primitive, expectedTypeText) ->
        let label =
            $"Primitive %A{primitive} Array = Array<%s{expectedTypeText}>"
        testCase label <| fun _ ->
            ResolvedType.Primitive primitive
            |> Array.create
            |> testRender $"Array<%s{expectedTypeText}>"
            ||> Flip.Expect.equal ""
        )
    |> testList "Primitive arrays"

let erasedUnionTests = testList "Erased Union" [
    testCase "primitive" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
        ]
        |> Union.create
        |> testRender "U2<string, int>"
        ||> Flip.Expect.equal ""
    testCase "simplifies to optional" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Undefined
        ]
        |> Union.create
        |> testRender "option<string>"
        ||> Flip.Expect.equal ""
    testCase "inner optional" <| fun _ ->
        [
            Union.create [
                primitive TypeKindPrimitive.String
                primitive TypeKindPrimitive.Undefined
            ]
            primitive TypeKindPrimitive.Integer
        ]
        |> Union.create
        // Original:
        // |> testRender "U2<option<string>, int>"
        // This is deemed less correct; nullability is not specific to a union case, it should be lifted.
        |> testRender "option<U2<string, int>>"
        ||> Flip.Expect.equal ""
    // testCase "literals" <| fun _ ->
    //     [
    //         Literal.create "Foo"
    //         |> Literal.wrap
    //         Literal.create "Bar"
    //         |> Literal.wrap
    //     ]
    //     |> Union.create
    //     |> testAnchoredRender (
    //         ModulePath.init "Foo"
    //         |> TypePath.create "Bar"
    //         |> MemberPath.createOnType "baz"
    //         |> ParameterPath.create "para"
    //         |> AnchorPath.Parameter
    //         ) "Foo.Bar.Baz.Para"
    //     ||> Flip.Expect.equal ""
    // testCase "literal relative" <| fun _ ->
    //     let path =
    //         ModulePath.init "Foo"
    //         |> TypePath.create "Bar"
    //         |> MemberPath.createOnType "baz"
    //         |> ParameterPath.create "para"
    //         |> AnchorPath.Parameter
    //     [
    //         Literal.create "Foo"
    //         |> Literal.wrap
    //         Literal.create "Bar"
    //         |> Literal.wrap
    //     ]
    //     |> Union.create
    //     |> testAnchoredRelativeRender (
    //         ModulePath.createFromList [ "Foo"; "Bar"; "Baz" ]
    //         |> TypePath.create "Local"
    //         |> AnchorPath.Type
    //         ) path "Para"
    //     ||> Flip.Expect.equal ""
]

let typeReferenceTests = testList "TypeReference" [
    testCase "Primitive type reference" <| fun _ ->
        primitive TypeKindPrimitive.Integer
        |> TypeReference.create
        |> TypeReference.wrap
        |> testRender "int"
        ||> Flip.Expect.equal ""
    testCase "preference resolved type" <| fun _ ->
        primitive TypeKindPrimitive.Integer
        |> TypeReference.create
        |> TypeReference.withResolvedType (primitive TypeKindPrimitive.String)
        |> TypeReference.wrap
        |> testRender "string"
        ||> Flip.Expect.equal "Expect to resolve to string instead of int"
    testCase "with single primitive type argument" <| fun _ ->
        primitive TypeKindPrimitive.NonPrimitive
        |> TypeReference.create
        |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
        |> TypeReference.wrap
        |> testRender "obj<string>"
        ||> Flip.Expect.equal ""
    testCase "option base type with primitive type argument" <| fun _ ->
        primitive TypeKindPrimitive.Any
        |> TypeReference.create
        |> TypeReference.withTypeArguments [ primitive TypeKindPrimitive.String ]
        |> TypeReference.wrap
        |> testRender "option<obj<string>>"
        ||> Flip.Expect.equal ""
    testCase "with multiple primitive type arguments" <| fun _ ->
        primitive TypeKindPrimitive.NonPrimitive
        |> TypeReference.create
        |> TypeReference.withTypeArguments [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
        ]
        |> TypeReference.wrap
        |> testRender "obj<string, int>"
        ||> Flip.Expect.equal ""
]

let unionTests = testList "Unions" [
    testCase "primitives with stacking intents" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            primitive TypeKindPrimitive.Integer
            primitive TypeKindPrimitive.Any
            primitive TypeKindPrimitive.NonPrimitive
            primitive TypeKindPrimitive.Undefined
        ]
        |> Union.create
        |> testRender "option<U3<string, int, obj>>"
        ||> Flip.Expect.equal ""
    testCase "all nullables are unified to a single obj option" <| fun _ ->
        [
            primitive TypeKindPrimitive.Null
            primitive TypeKindPrimitive.Undefined
            primitive TypeKindPrimitive.Unknown
            primitive TypeKindPrimitive.Any
            primitive TypeKindPrimitive.Never
            primitive TypeKindPrimitive.Void
        ]
        |> Union.create
        |> testRender "option<obj>"
        ||> Flip.Expect.equal ""
    testCase "2 layer nested unions" <| fun _ ->
        [
           primitive TypeKindPrimitive.String
           Union.create [
               primitive TypeKindPrimitive.Integer
               primitive TypeKindPrimitive.Boolean
           ]
           primitive TypeKindPrimitive.Number
        ]
        |> Union.create
        |> testRender "U4<string, int, bool, float>"
        ||> Flip.Expect.equal ""
    testCase "2 layer nested unions with top nullability" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            Union.create [
                primitive TypeKindPrimitive.Integer
                primitive TypeKindPrimitive.Boolean
            ]
            primitive TypeKindPrimitive.Number
            primitive TypeKindPrimitive.Null
        ]
        |> Union.create
        |> testRender "option<U4<string, int, bool, float>>"
        ||> Flip.Expect.equal ""
    testCase "2 layer nested unions with nested nullability" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            Union.create [
                primitive TypeKindPrimitive.Integer
                primitive TypeKindPrimitive.Boolean
                primitive TypeKindPrimitive.Null
            ]
            primitive TypeKindPrimitive.Number
        ]
        |> Union.create
        |> testRender "option<U4<string, int, bool, float>>"
        ||> Flip.Expect.equal ""
    testCase "2 layer nested unions with top and nested nullability" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            Union.create [
                primitive TypeKindPrimitive.Integer
                primitive TypeKindPrimitive.Boolean
                primitive TypeKindPrimitive.Null
            ]
            primitive TypeKindPrimitive.Number
            primitive TypeKindPrimitive.Void
        ]
        |> Union.create
        |> testRender "option<U4<string, int, bool, float>>"
        ||> Flip.Expect.equal ""
    testCase "2 layer nested unions with stacking intents" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            Union.create [
                primitive TypeKindPrimitive.Integer
                primitive TypeKindPrimitive.Any
            ]
            primitive TypeKindPrimitive.Unknown
        ]
        |> Union.create
        |> testRender "option<U3<string, int, obj>>"
        ||> Flip.Expect.equal ""
    testCase "3 layer nested unions" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            Union.create [
                primitive TypeKindPrimitive.Integer
                Union.create [
                    primitive TypeKindPrimitive.Boolean
                    primitive TypeKindPrimitive.Null
                ]
            ]
        ]
        |> Union.create
        |> testRender "option<U3<string, int, bool>>"
        ||> Flip.Expect.equal ""
]

let callSignatureTests = testList "Call Signatures" [
    testCase "noop" <| fun _ ->
        primitive TypeKindPrimitive.Void
        |> CallSignature.create
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "unit -> unit"
        ||> Flip.Expect.equal ""
    testCase "primitive return type" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "unit -> string"
        ||> Flip.Expect.equal ""
    testCase "primitive parameter and return type" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Integer
            |> Parameter.create "para" 
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "int -> string"
        ||> Flip.Expect.equal ""
    testCase "primitive parameter and return type with optional" <| fun _ ->
        primitive TypeKindPrimitive.Any
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Unknown
            |> Parameter.create "para" 
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "option<obj> -> option<obj>"
        ||> Flip.Expect.equal ""
    testCase "multiple parameters" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Integer
            |> Parameter.create "intParameter" 
            primitive TypeKindPrimitive.Boolean
            |> Parameter.create "boolParameter" 
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "int -> bool -> string"
        ||> Flip.Expect.equal ""
    testCase "nested call signature" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> CallSignature.create
        |> CallSignature.withParameters [
            primitive TypeKindPrimitive.Integer
            |> Parameter.create "intParameter"
            (
                primitive TypeKindPrimitive.Integer
                |> CallSignature.create
                |> CallSignature.withParameters [
                    primitive TypeKindPrimitive.Boolean
                    |> Parameter.create "boolParameter"
                ]
                |> List.singleton
                |> CallSignature.wrap
                |> TypeLiteral.addMember
                |> funApply TypeLiteral.empty
                |> TypeLiteral.wrap
            )
            |> Parameter.create "callSignatureParameter"
        ]
        |> List.singleton
        |> CallSignature.wrap
        |> TypeLiteral.addMember
        |> funApply TypeLiteral.empty
        |> TypeLiteral.wrap
        |> testRender "int -> (bool -> int) -> string"
        ||> Flip.Expect.equal " \
The intent behind a signature such as `int -> bool -> int -> string` is inherently different from
`int -> (bool -> int) -> string`. The latter prescribes a function that takes a function as an argument."
]

let contextPersistanceTests = testList "Context memoization" [
    testCase "Unseen primitive" <| fun _ ->
        let newRef = primitive TypeKindPrimitive.String
        let getRef () = tryGet ctx newRef
        getRef()
        |> Flip.Expect.isNone "Should not have seen primitive"
        TestHelper.prerender ctx newRef
        |> ignore
        getRef()
        |> Flip.Expect.isSome "Should have seen primitive"
    testCase "Nested seen type memoization" <| fun _ ->
        let nestedType =
            primitive TypeKindPrimitive.String
            |> TypeReference.create
            |> TypeReference.wrap
        tryGet ctx nestedType
        |> Flip.Expect.isNone "Should not have seen wrapper type"
        match nestedType with
        | ResolvedType.TypeReference { Type = Resolve resolvedType } ->
            TestHelper.prerender ctx resolvedType |> ignore
            tryGet ctx resolvedType
            |> Flip.Expect.isSome "Should have seen resolved nested type"
        | _ -> failwith "Expected resolved reference type"
    
]

[<Tests>]
let tests = testList "TypeRef" [
    contextPersistanceTests
    primitiveTests
    primitiveTuples
    primitiveArrays
    erasedUnionTests
    typeReferenceTests
    unionTests
    callSignatureTests
    testCase "Simple tuple with optional element" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            |> Tuple.createElement
            primitive TypeKindPrimitive.Integer
            |> Tuple.createElement
            primitive TypeKindPrimitive.Boolean
            |> Tuple.createElement
            |> Tuple.Element.optional
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> testRender "string * int * option<bool>"
        ||> Flip.Expect.equal "" 
    testCase "Simple interface" <| fun _ ->
        Interface.create "Bar"
        |> Interface.withPath [ "Foo" ]
        |> Interface.wrap
        |> testRender "Foo.Bar"
        ||> Flip.Expect.equal ""
    testCase "Primitive Array" <| fun _ ->
        primitive TypeKindPrimitive.String
        |> Array.create
        |> testRender "Array<string>"
        ||> Flip.Expect.equal "" 
    testCase "Tuple array" <| fun _ ->
        [
            primitive TypeKindPrimitive.String
            |> Tuple.createElement
            primitive TypeKindPrimitive.Integer
            |> Tuple.createElement
        ]
        |> Tuple.create
        |> Tuple.wrap
        |> Array.create
        |> testRender "Array<string * int>"
        ||> Flip.Expect.equal "" 
       
]