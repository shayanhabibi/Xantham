module Xantham.Generator.Tests.Tests.Name

open Expecto
open Xantham.Decoder

// Shorthand helpers matching the DU constructors
let private modified original modified_ = Name.Modified(original, modified_)
let private modifiedCase<[<Measure>] 'U> original modified: Name<'U> = Name.Modified(original, modified) |> Case.addMeasure
let private source original = Name.Source original
let private sourceCase<[<Measure>] 'U> original: Name<'U> = Name.Source original |> Case.addMeasure

[<Tests>]
let caseTests =
    testList "Name.Case" [
        let inline (==>) a b = a, modifiedCase a b
        let inline (<=>) a b = a, sourceCase b
        testTheory "Case.pascal" [
            "``pascal_case``" ==> "PascalCase"
            "pascal_case" ==> "PascalCase"
            "pascal-case" ==> "PascalCase"
            "pascalCase" ==> "PascalCase"
            "PASCAL_CASE" <=> "PASCAL_CASE" // upper snake case is preserved
            "_pascal_case" ==> "_pascalCase"
        ] <| fun (original, expected) ->
            Name.Pascal.create original |> Flip.Expect.equal "" expected
        testTheory "Case.camel" [
            "``camel_case``" ==> "camelCase"
            "camel_case" ==> "camelCase"
            "camel-case" ==> "camelCase"
            "camelCase" <=> "camelCase"
            "CAMEL_CASE" <=> "CAMEL_CASE" // upper snake case is preserved
            "_camel_case" ==> "_camelCase"
            "_Camel_Case" ==> "_camelCase"
        ] <| fun (original, expected) ->
            Name.Camel.create original |> Flip.Expect.equal "" expected
        testTheory "Case.module" [
            "``module_name``" ==> "ModuleName"
            "ModuleName" ==> "IModuleName"
            "module_name" ==> "IModuleName"
            "MODULE_NAME" ==> "IMODULE_NAME" // upper snake case is preserved
            "module-name" ==> "IModuleName"
            "_moduleName" ==> "IModuleName"
        ] <| fun (original, expected) ->
            Name.Module.create original |> Flip.Expect.equal "" expected
        testTheory "Case.typar" [
            "``typar``" ==> "'typar"
            "Typar" ==> "'Typar"
            "t" ==> "'t"
            "_T" ==> "'_T"
        ] <| fun (original, expected) ->
            Name.Typar.create original |> Flip.Expect.equal "" expected
    ]

[<Tests>]
let sourceCaseTests =
    testList "Name.Case.sourceMap" [
        testTheory "Case.pascal" [
            let inline (==>) a b = Name.Pascal.fromCase a, b 
            modifiedCase "ab" "ab" ==> modifiedCase "ab" "Ab"
            modifiedCase "ba" "ab" ==> modifiedCase "ba" "Ba"
        ] <| fun (a, b) ->
            (a,b)
            ||> Flip.Expect.equal ""
        testTheory "Case.camel" [
            let inline (==>) a b = Name.Camel.fromCase a, b 
            modifiedCase "Bb" "Aa" ==> modifiedCase "Bb" "bb"
            modifiedCase "a" "a" ==> sourceCase "a"
            modifiedCase "A" "a" ==> sourceCase "A" // counts as UPPER_SNAKE_CASE
        ] <| fun (actual, expected) ->
            Flip.Expect.equal "" expected actual
    ]