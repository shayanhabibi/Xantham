/// The shape tier's nano-pass payoff: each pass exercised on a hand-built model, asserted on
/// the output model and its findings. No wire, no fixtures.
module Xantham.Generator.Tests.ShapeTests

open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

[<Tests>]
let typeRefTests =
    testList "shape typeRef" [
        testCase "primitives map to F# primitives without findings" <| fun _ ->
            let model = Build.shapeModel Build.primitives

            for typeId, expected in [ 1, FsString; 2, FsFloat; 3, FsBool; 4, FsUnit ] do
                let reference, findings = Shape.typeRef model "x" typeId
                Expect.equal reference expected $"type {typeId}"
                Expect.isEmpty findings $"type {typeId} findings"

        testCase "a union with undefined hoists to option with an ergonomic finding" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef model "x" 10

            Expect.equal reference (FsOption FsString) "string | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "one ergonomic finding"

        testCase "a union of null and undefined alone maps to unit, widened" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 5; 6 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef model "x" 10

            Expect.equal reference FsUnit "null | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a union of several non-null members widens to obj" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 2; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef model "x" 10

            Expect.equal reference (FsOption FsObj) "string | number | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened, but the hoist survives"

        testCase "an aliased object type references its generated declaration" <| fun _ ->
            let aliased =
                Build.facts { Build.typeResponse 20 TypeFlags.Object with AliasSymbol = ValueSome 100 }

            let model =
                { Build.shapeModel [ aliased ] with DeclNames = Map.ofList [ 100, "Options" ] }

            Expect.equal (Shape.typeRef model "x" 20) (FsNamed "Options", []) "alias reference"

        testCase "an external object type widens to obj and the finding names it" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with SymbolName = Some "RegExp" }

            let model = Build.shapeModel [ external ]
            let reference, findings = Shape.typeRef model "x" 21

            Expect.equal reference FsObj "widened"

            match findings with
            | [ finding ] ->
                Expect.equal finding.Tier Widened "tier"
                Expect.stringContains finding.Message "RegExp" "the message says what was widened"
            | findings -> failtest $"expected one finding, got %A{findings}"

        testCase "a type id absent from the table is an escape, not an exception" <| fun _ ->
            let reference, findings = Shape.typeRef (Build.shapeModel []) "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Escape ] "escape"

        testCase "a deliberately-not-followed type reports its reason" <| fun _ ->
            let model =
                { Build.shapeModel [] with NotFollowed = Map.ofList [ 99, "beyond the depth cutoff (12)" ] }

            let reference, findings = Shape.typeRef model "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened, not escaped"
            Expect.stringContains (findings.Head.Message) "depth cutoff" "the reason is carried"
    ]

/// The Options-and-ansiRegex shape of the ansi-regex fixture, built by hand: an aliased
/// object type with one readonly boolean member, and a default-exported function taking it
/// optionally and returning an external type.
let private ansiRegexShaped () =
    let optionsSymbol = Build.symbol 100 "Options" SymbolFlags.TypeAlias
    let functionSymbol = Build.symbol 200 "ansiRegex" SymbolFlags.Function

    let optionsType =
        { Build.facts { Build.typeResponse 20 TypeFlags.Object with AliasSymbol = ValueSome 100 } with
            Members =
                [ { Build.resolvedMember (Build.symbol 101 "onlyFirst" SymbolFlags.Property) 3 with
                      ReadOnly = true } ] }

    let regExpType =
        { Build.facts (Build.typeResponse 21 TypeFlags.Object) with SymbolName = Some "RegExp" }

    let functionType =
        { Build.facts (Build.typeResponse 30 TypeFlags.Object) with
            CallSignatures =
                [ { Parameters =
                      [ { Build.resolvedMember (Build.symbol 201 "options" SymbolFlags.FunctionScopedVariable) 20 with
                            Optional = true } ]
                    ReturnTypeId = 21 } ] }

    { Build.shapeModel ([ optionsType; regExpType; functionType ] @ Build.primitives) with
        Harvest =
            { Exports =
                [ Build.export "Options" optionsSymbol
                  Build.export "default" functionSymbol ] }
        ExportTypes =
            Map.ofList
                [ 100, { Declared = Some 20; Value = None }
                  200, { Declared = None; Value = Some 30 } ] }

[<Tests>]
let shapePassTests =
    testList "shape passes" [
        testCase "name-exports names type-like exports only" <| fun _ ->
            let model, findings = Build.runPass Shape.nameExports (ansiRegexShaped ())

            Expect.isEmpty findings "no findings"
            Expect.equal model.DeclNames (Map.ofList [ 100, "Options" ]) "the alias, not the function"

        testCase "shape-interfaces shapes the plain object alias" <| fun _ ->
            let named, _ = Build.runPass Shape.nameExports (ansiRegexShaped ())
            let model, findings = Build.runPass Shape.shapeInterfaces named

            Expect.isEmpty findings "nothing widened"

            match model.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Options" "name"

                match decl.Members with
                | [ m ] ->
                    Expect.equal m.Name "onlyFirst" "member name"
                    Expect.equal m.Type FsBool "member type"
                    Expect.isTrue m.ReadOnly "readonly survives"
                | members -> failtest $"expected one member, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-exports binds the default export under its declared name" <| fun _ ->
            let named, _ = Build.runPass Shape.nameExports (ansiRegexShaped ())
            let model, findings = Build.runPass Shape.shapeExports named

            match model.Decls with
            | [ FsExports [ m ] ] ->
                Expect.equal m.Name "ansiRegex" "named after the declaring symbol, not 'default'"
                Expect.equal m.Binding ImportDefault "bound as the default import"
                Expect.equal m.Return FsObj "RegExp widened"

                match m.Parameters with
                | [ p ] ->
                    Expect.isTrue p.Optional "optional"
                    Expect.equal p.Type (FsOption(FsNamed "Options")) "optional alias parameter"
                | parameters -> failtest $"expected one parameter, got %A{parameters}"

                Expect.equal
                    (findings |> List.map (fun f -> f.Tier, f.Symbol))
                    [ Ergonomic, "ansiRegex(options)"; Widened, "ansiRegex()" ]
                    "the hoist and the widening are both findings"
            | decls -> failtest $"expected the Exports group, got %A{decls}"

        testCase "order-declarations puts interfaces in source order, Exports last" <| fun _ ->
            let interface' name order =
                FsInterface
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = Some order
                      Members = [] }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsExports []
                          interface' "B" { File = "b.ts"; NodeIndex = 5 }
                          interface' "A" { File = "a.ts"; NodeIndex = 9 } ] }

            let ordered, _ = Build.runPass Shape.orderDeclarations model

            let names =
                ordered.Decls
                |> List.map (function
                    | FsInterface decl -> decl.Name
                    | FsExports _ -> "<exports>")

            Expect.equal names [ "A"; "B"; "<exports>" ] "file order first, Exports last"

        testCase "audit-coverage reports an export nothing represented" <| fun _ ->
            let model =
                { Build.shapeModel [] with
                    Harvest = { Exports = [ Build.export "Gone" (Build.symbol 300 "Gone" SymbolFlags.TypeAlias) ] } }

            let _, findings = Build.runPass Shape.auditCoverage model

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Escape, "Gone" ]
                "the dropped export is a finding, not silence"

        testCase "the pipeline fold stamps findings with the pass that made them" <| fun _ ->
            let degrading: Pass<int> =
                { Name = "always-degrades"
                  Run = fun _ n -> async { return Degraded(n + 1, [ Finding.make Widened "x" "because" ]) } }

            let model, findings =
                Async.RunSynchronously(Pipeline.runTier Build.context [ degrading ] 0)

            Expect.equal model 1 "advanced"
            Expect.equal (findings |> List.map _.Pass) [ "always-degrades" ] "stamped"
    ]
