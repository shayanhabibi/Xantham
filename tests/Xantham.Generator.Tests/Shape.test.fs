/// The shape tier's nano-pass payoff: each pass exercised on a hand-built model, asserted on
/// the output model and its findings. No wire, no fixtures.
module Xantham.Generator.Tests.ShapeTests

open System.Text.Json.Nodes
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator

/// A string-literal type carrying its payload.
let private stringLiteral (id: int) (text: string) =
    Build.facts
        { Build.typeResponse id TypeFlags.StringLiteral with
            Value = JsonValue.Create text }

/// A numeric-literal type carrying its payload.
let private numberLiteral (id: int) (value: float) =
    Build.facts
        { Build.typeResponse id TypeFlags.NumberLiteral with
            Value = JsonValue.Create value }

/// A tuple type over the given component ids, with one element flag each.
let private tuple (id: int) (components: int list) (flags: ElementFlags list) =
    { Build.facts
        { Build.typeResponse id TypeFlags.Object with
            IsTupleType = ValueSome true } with
        TypeArguments = components
        TupleElements = flags }

/// A type parameter type, named by its own symbol the way the resolve tier records it.
let private typeParam (id: int) (name: string) =
    { Build.facts (Build.typeResponse id TypeFlags.TypeParameter) with SymbolName = Some name }

/// A generic declaration: its own target, holding its parameters as its arguments.
let private genericDecl (id: int) (parameters: int list) (members: ResolvedMember list) =
    { Build.facts
        { Build.typeResponse id TypeFlags.Object with
            Target = ValueSome id
            TypeParameters = ValueSome(List.toArray parameters) } with
        TypeArguments = parameters
        Members = members }

[<Tests>]
let typeRefTests =
    testList "shape typeRef" [
        testCase "primitives map to F# primitives without findings" <| fun _ ->
            let model = Build.shapeModel Build.primitives

            for typeId, expected in [ 1, FsString; 2, FsFloat; 3, FsBool; 4, FsUnit ] do
                let reference, findings = Shape.typeRef Build.context model None "x" typeId
                Expect.equal reference expected $"type {typeId}"
                Expect.isEmpty findings $"type {typeId} findings"

        testCase "a union with undefined hoists to option with an ergonomic finding" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption FsString) "string | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "one ergonomic finding"

        testCase "a union of null and undefined alone maps to unit, widened" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 5; 6 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference FsUnit "null | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a union of several non-null members is erased (D4)" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 2; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption(FsErasedUnion [ FsString; FsFloat ])) "string | number | undefined"

            Expect.equal
                (findings |> List.map _.Tier)
                [ Ergonomic ]
                "the hoist is reported; the erased union is not a widening"

        testCase "a union wider than the erased arity still widens to obj" <| fun _ ->
            // Five distinct arms: `U5` exists in Fable, but past four the consumer is doing
            // runtime tests the type no longer helps them write.
            let named id name =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with SymbolName = Some name }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with
                    UnionMembers = [ 1; 2; 3; 20; 21; 22 ] }

            let model =
                Build.shapeModel (union :: named 20 "A" :: named 21 "B" :: named 22 "C" :: Build.primitives)

            let model =
                { model with DeclNames = [ 20, "A"; 21, "B"; 22, "C" ] |> Map.ofList }

            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "six members, five arms"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a fixed tuple maps to an F# tuple (D7)" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1; 2 ] [ ElementFlags.Required; ElementFlags.Required ] :: Build.primitives)

            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsTuple [ FsString; FsFloat ]) "[string, number]"
            Expect.isEmpty findings "Fable compiles both to the same JS array"

        testCase "an optional tail element arrives already hoisted to option" <| fun _ ->
            // The checker hands `[number, number?]` over as `number` and `number | undefined`,
            // so D1's hoist does the work and D7 imposes nothing of its own.
            let optionalTail =
                { Build.facts (Build.typeResponse 11 TypeFlags.Union) with UnionMembers = [ 2; 5 ] }

            let model =
                Build.shapeModel (
                    tuple 10 [ 2; 11 ] [ ElementFlags.Required; ElementFlags.Optional ]
                    :: optionalTail
                    :: Build.primitives
                )

            let reference, _ = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsTuple [ FsFloat; FsOption FsFloat ]) "[number, number?]"

        testCase "a rest element leaves no tuple form, so it widens to an array" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1; 2 ] [ ElementFlags.Required; ElementFlags.Rest ] :: Build.primitives)

            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsArray FsObj) "components disagree, so the element is obj"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a one-element tuple has no F# form either" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1 ] [ ElementFlags.Required ] :: Build.primitives)

            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsArray FsString) "widened to its element"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a type parameter in scope names its variable (§4.9)" <| fun _ ->
            let model =
                { Build.shapeModel (typeParam 20 "T" :: Build.primitives) with
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Shape.typeRef Build.context model None "x" 20

            Expect.equal reference (FsTypeVar "T") "'T"
            Expect.isEmpty findings "a bound variable costs nothing"

        testCase "a type parameter of some other declaration is not in scope" <| fun _ ->
            let model = Build.shapeModel (typeParam 20 "T" :: Build.primitives)

            let reference, findings = Shape.typeRef Build.context model None "x" 20

            Expect.equal reference FsObj "nothing here binds T"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "an instantiation of a declared generic is written as an application" <| fun _ ->
            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with Target = ValueSome 30 } with
                    TypeArguments = [ 1 ] }

            let model =
                { Build.shapeModel (genericDecl 30 [ 20 ] [] :: instantiation :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ] }

            let reference, findings = Shape.typeRef Build.context model None "x" 31

            Expect.equal reference (FsApp("Box", [ FsString ])) "Box<string>, not the expansion"
            Expect.isEmpty findings "an application is exact"

        testCase "a generic declaration named at a reference re-applies its parameters" <| fun _ ->
            // `map(next: T): Box<T>` refers to the declaration itself; F# has no bare `Box`.
            let model =
                { Build.shapeModel (genericDecl 30 [ 20 ] [] :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ]
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Shape.typeRef Build.context model None "x" 30

            Expect.equal reference (FsApp("Box", [ FsTypeVar "T" ])) "Box<'T>"
            Expect.isEmpty findings "exact"

        testCase "a named literal union references its declaration, hoist intact" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 8; 5 ] }

            let model =
                { Build.shapeModel (union :: stringLiteral 7 "ms" :: stringLiteral 8 "s" :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "TimeUnit" ] }

            let reference, findings = Shape.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption(FsNamed "TimeUnit")) "the classified union's name"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "only the hoist"

        testCase "an array of a generated declaration reads as an F# array" <| fun _ ->
            let array =
                { Build.facts (Build.typeResponse 11 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "Array"
                    TypeArguments = [ 20 ] }

            let element = Build.facts (Build.typeResponse 20 TypeFlags.Object)

            let model =
                { Build.shapeModel [ array; element ] with DeclNames = Map.ofList [ 20, "Timer" ] }

            Expect.equal
                (Shape.typeRef Build.context model None "x" 11)
                (FsArray(FsNamed "Timer"), [])
                "Array<Timer> -> Timer[], whatever the lib group's disposition"

        testCase "an anonymous callback reads as a delegate (D5)" <| fun _ ->
            let callback =
                { Build.facts (Build.typeResponse 12 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 300 "value" SymbolFlags.FunctionScopedVariable) 1 ]
                              4 ] }

            let model = Build.shapeModel (callback :: Build.primitives)

            Expect.equal
                (Shape.typeRef Build.context model None "x" 12)
                (FsDelegate([ FsString ], FsUnit), [])
                "(value: string) => void -> Action<string>"

        testCase "a polymorphic this return reads as the declaring type" <| fun _ ->
            let thisType =
                Build.facts
                    { Build.typeResponse 13 TypeFlags.TypeParameter with IsThisType = ValueSome true }

            let model = Build.shapeModel [ thisType ]
            let reference, findings = Shape.typeRef Build.context model (Some "Timer") "Timer.play()" 13

            Expect.equal reference (FsNamed "Timer") "chainable"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "ergonomic, not silent"

        testCase "an aliased object type references its generated declaration" <| fun _ ->
            let aliased = Build.facts (Build.typeResponse 20 TypeFlags.Object)

            let model =
                { Build.shapeModel [ aliased ] with DeclNames = Map.ofList [ 20, "Options" ] }

            Expect.equal (Shape.typeRef Build.context model None "x" 20) (FsNamed "Options", []) "alias reference"

        testCase "an external object type widens to obj and the finding names it" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with SymbolName = Some "RegExp" }

            let model = Build.shapeModel [ external ]
            let reference, findings = Shape.typeRef Build.context model None "x" 21

            Expect.equal reference FsObj "widened"

            match findings with
            | [ finding ] ->
                Expect.equal finding.Tier Widened "tier"
                Expect.stringContains finding.Message "RegExp" "the message says what was widened"
            | findings -> failtest $"expected one finding, got %A{findings}"

        testCase "a type id absent from the table is an escape, not an exception" <| fun _ ->
            let reference, findings = Shape.typeRef Build.context (Build.shapeModel []) None "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Escape ] "escape"

        testCase "a deliberately-not-followed type reports its reason" <| fun _ ->
            let model =
                { Build.shapeModel [] with NotFollowed = Map.ofList [ 99, "beyond the depth cutoff (12)" ] }

            let reference, findings = Shape.typeRef Build.context model None "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened, not escaped"
            Expect.stringContains findings.Head.Message "depth cutoff" "the reason is carried"

        testCase "a referenced group's type templates into its module, exact, no finding" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "RegExp" }

            let context =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups = Map.ofList [ "typescript/lib", Reference ] } }

            Expect.equal
                (Shape.typeRef context (Build.shapeModel [ external ]) None "x" 21)
                (FsNamed "TypeScript.Lib.RegExp", [])
                "the O7 template"

        testCase "an anonymous type in a referenced group still widens, with a finding" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 22 TypeFlags.Object) with Origin = Dependency "left-pad" }

            let context =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups = Map.ofList [ "left-pad", Reference ] } }

            let reference, findings = Shape.typeRef context (Build.shapeModel [ external ]) None "x" 22

            Expect.equal reference FsObj "nothing to template with"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "reported, not silent"
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
                [ Build.signature
                      [ { Build.resolvedMember (Build.symbol 201 "options" SymbolFlags.FunctionScopedVariable) 20 with
                            Optional = true } ]
                      21 ] }

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
        testCase "name-exports names type-like exports by their declared type id" <| fun _ ->
            let model, findings = Build.runPass Shape.nameExports (ansiRegexShaped ())

            Expect.isEmpty findings "no findings"
            Expect.equal model.DeclNames (Map.ofList [ 20, "Options" ]) "the alias's type, not the function"

        testCase "shape-interfaces shapes the plain object alias" <| fun _ ->
            let named, _ = Build.runPass Shape.nameExports (ansiRegexShaped ())
            let model, findings = Build.runPass Shape.shapeInterfaces named

            Expect.isEmpty findings "nothing widened"

            match model.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Options" "name"

                match decl.Members with
                | [ FsProperty m ] ->
                    Expect.equal m.Name "onlyFirst" "member name"
                    Expect.equal m.Type FsBool "member type"
                    Expect.isTrue m.ReadOnly "readonly survives"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces declares a type whose only shape is an index signature" <| fun _ ->
            // `interface Bag { [key: string]: number }` has no properties at all, so before
            // §4.10's signatures were read it looked empty and abbreviated to obj.
            let bagSymbol = Build.symbol 100 "Bag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 2
                            IsReadonly = false } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "Bag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass Shape.nameExports model
            let shaped, _ = Build.runPass Shape.shapeInterfaces named

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Bag" "the index signature is shape enough to declare"

                match decl.Members with
                | [ FsIndexer indexer ] ->
                    Expect.equal indexer.Key FsString "key type"
                    Expect.equal indexer.Value FsFloat "value type"
                    Expect.isFalse indexer.ReadOnly "a writable signature keeps its setter"
                | members -> failtest $"expected one indexer, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces drops the setter for a readonly index signature" <| fun _ ->
            let bagSymbol = Build.symbol 100 "FrozenBag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 1
                            IsReadonly = true } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "FrozenBag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass Shape.nameExports model
            let shaped, _ = Build.runPass Shape.shapeInterfaces named

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsIndexer indexer ] -> Expect.isTrue indexer.ReadOnly "readonly survives to the emission"
                | members -> failtest $"expected one indexer, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "synthesize-paramobjects declines a type carrying an index signature" <| fun _ ->
            // An index signature has no name to bind a Create parameter to, so the type is
            // not plain data however many named members sit beside it.
            let bagSymbol = Build.symbol 100 "Bag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 101 "label" SymbolFlags.Property) 1 ]
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 2
                            IsReadonly = false } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "Bag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass Shape.nameExports model
            let shaped, _ = Build.runPass Shape.shapeInterfaces named
            let withCreate, _ = Build.runPass Shape.synthesizeParamObjects shaped

            match withCreate.Decls with
            | [ FsInterface decl ] -> Expect.isEmpty decl.CreateOverloads "no Create for an indexed type"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-exports binds the default export under its declared name" <| fun _ ->
            let named, _ = Build.runPass Shape.nameExports (ansiRegexShaped ())
            let shaped, findings = Build.runPass Shape.shapeExports named
            let model, _ = Build.runPass Shape.orderDeclarations shaped

            match model.Decls with
            | [ FsExports [ m ] ] ->
                Expect.equal m.Name "ansiRegex" "named after the declaring symbol, not 'default'"
                Expect.equal m.Binding ImportDefault "bound as the default import"

                match m.Body with
                | ExportFunction([ p ], FsObj) ->
                    Expect.isTrue p.Optional "optional"
                    Expect.equal p.Type (FsOption(FsNamed "Options")) "optional alias parameter"
                | body -> failtest $"expected a function with one parameter returning obj, got %A{body}"

                Expect.equal
                    (findings |> List.map (fun f -> f.Tier, f.Symbol))
                    [ Ergonomic, "ansiRegex(options)"; Widened, "ansiRegex()" ]
                    "the hoist and the widening are both findings"
            | decls -> failtest $"expected the Exports group, got %A{decls}"

        testCase "synthesize-anonymous names a parameter-position object literal by its path" <| fun _ ->
            let anonymous =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 401 "speed" SymbolFlags.Property) 2 ] }

            let makeType =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 402 "options" SymbolFlags.FunctionScopedVariable) 40 ]
                              4 ] }

            let model =
                { Build.shapeModel (anonymous :: makeType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "make" (Build.symbol 400 "make" SymbolFlags.Function) ] }
                    ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 41 } ] }

            let named, _ = Build.runPass Shape.synthesizeAnonymous model

            Expect.equal (Map.tryFind 40 named.DeclNames) (Some "MakeOptions") "path-derived name"
            Expect.equal (Map.tryFind 41 named.DeclNames) None "the callable itself stays inline"

        testCase "synthesize-anonymous prefers a non-exported type's own name" <| fun _ ->
            let internal' =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Globals"
                    Members = [ Build.resolvedMember (Build.symbol 401 "speed" SymbolFlags.Property) 2 ] }

            let model =
                { Build.shapeModel (internal' :: Build.primitives) with
                    Harvest =
                        { Exports = [ Build.export "globals" (Build.symbol 400 "globals" SymbolFlags.BlockScopedVariable) ] }
                    ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 40 } ] }

            let named, _ = Build.runPass Shape.synthesizeAnonymous model

            Expect.equal (Map.tryFind 40 named.DeclNames) (Some "Globals") "its own name, not the path"

        testCase "classify-literal-unions makes a StringEnum with CompiledName per case" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 8 ] }

            let model =
                { Build.shapeModel [ union; stringLiteral 7 "ms"; stringLiteral 8 "s" ] with
                    DeclNames = Map.ofList [ 10, "TimeUnit" ] }

            let shaped, findings = Build.runPass Shape.classifyLiteralUnions model

            Expect.isEmpty findings "exact"

            match shaped.Decls with
            | [ FsStringEnum decl ] ->
                Expect.equal decl.Name "TimeUnit" "name"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName))
                    [ "Ms", Some "ms"; "S", Some "s" ]
                    "PascalCased cases carrying their literals"
            | decls -> failtest $"expected one StringEnum, got %A{decls}"

        testCase "classify-literal-unions keeps mixed unions in one StringEnum via CompiledValue (D12)" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 9 ] }

            let model =
                { Build.shapeModel [ union; stringLiteral 7 "auto"; numberLiteral 9 1.5 ] with
                    DeclNames = Map.ofList [ 10, "Speed" ] }

            let shaped, findings = Build.runPass Shape.classifyLiteralUnions model

            Expect.equal (findings |> List.map _.Tier) [ Exact ] "D12 is exact, and says so"

            match shaped.Decls with
            | [ FsStringEnum decl ] ->
                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName, c.CompiledValue))
                    [ "Auto", Some "auto", None; "N1_5", None, Some(LitNumber 1.5) ]
                    "the numeric case carries CompiledValue"
            | decls -> failtest $"expected one StringEnum, got %A{decls}"

        testCase "classify-literal-unions makes an F# enum from an all-integer union" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 9 ] }

            let model =
                { Build.shapeModel [ union; numberLiteral 7 0.0; numberLiteral 9 1.0 ] with
                    DeclNames = Map.ofList [ 10, "Flag" ] }

            let shaped, _ = Build.runPass Shape.classifyLiteralUnions model

            match shaped.Decls with
            | [ FsEnum decl ] -> Expect.equal decl.Cases [ "N0", 0; "N1", 1 ] "integer cases"
            | decls -> failtest $"expected one enum, got %A{decls}"

        testCase "shape-callbacks abbreviates a named callback to its delegate" <| fun _ ->
            let timer = Build.facts (Build.typeResponse 60 TypeFlags.Object)

            let callback =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 500 "timer" SymbolFlags.FunctionScopedVariable) 60 ]
                              4 ] }

            let model =
                { Build.shapeModel (callback :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 50, "TimerCallback"; 60, "Timer" ] }

            let shaped, findings = Build.runPass Shape.shapeCallbacks model

            Expect.isEmpty findings "exact"

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal decl.Name "TimerCallback" "name"
                Expect.equal decl.Target (FsDelegate([ FsNamed "Timer" ], FsUnit)) "Action<Timer>"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

        testCase "shape-interfaces emits methods with overloads, this-returns chained" <| fun _ ->
            let methodType =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature [] 71
                          Build.signature
                              [ Build.resolvedMember (Build.symbol 700 "speed" SymbolFlags.FunctionScopedVariable) 2 ]
                              71 ] }

            let thisType =
                Build.facts
                    { Build.typeResponse 71 TypeFlags.TypeParameter with IsThisType = ValueSome true }

            let timer =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 601 "play" SymbolFlags.Method) 70 ] }

            let model =
                { Build.shapeModel (methodType :: thisType :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Timer" ] }

            let shaped, findings = Build.runPass Shape.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsMethod first; FsMethod second ] ->
                    Expect.equal first.Parameters [] "first overload takes nothing"
                    Expect.equal first.Return (FsNamed "Timer") "this reads as Timer"
                    Expect.equal (second.Parameters |> List.map _.Name) [ "speed" ] "second overload's parameter"
                | members -> failtest $"expected two method overloads, got %A{members}"

                Expect.equal
                    (findings |> List.map _.Tier |> List.distinct)
                    [ Ergonomic ]
                    "this-chaining is the only finding"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces binds a declaration's parameters for its members (§4.9)" <| fun _ ->
            let box =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "value" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (box :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ] }

            let shaped, _ = Build.runPass Shape.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Box<'T>"

                match decl.Members with
                | [ FsProperty property ] -> Expect.equal property.Type (FsTypeVar "T") "the member names the variable"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "a constraint naming a generated type survives" <| fun _ ->
            let bounded = { typeParam 20 "T" with Constraint = Some 60 }
            let timer = Build.facts (Build.typeResponse 60 TypeFlags.Object)

            let holder =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "held" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (holder :: bounded :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Timer" ] }

            let shaped, findings = Build.runPass Shape.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = Some(FsNamed "Timer") } ] "'T :> Timer"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.isEmpty
                (findings |> List.filter (fun finding -> finding.Message.Contains "constraint"))
                "a bound F# can state costs nothing"

        testCase "a constraint with no F# form is dropped with a finding" <| fun _ ->
            // `K extends string`: an F# subtype constraint cannot name a primitive, and the
            // nearest approximation would reject code TypeScript accepts.
            let bounded = { typeParam 20 "K" with Constraint = Some 1 }

            let keyed =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "key" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (keyed :: bounded :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Keyed" ] }

            let shaped, findings = Build.runPass Shape.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "K"; Constraint = None } ] "the variable stays, the bound goes"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "a dropped bound is ergonomic, not widening"

        testCase "shape-callbacks binds the parameters the alias carries" <| fun _ ->
            // `type Mapper<T> = (input: T) => T` leaves the function type parameterless; the
            // variable is only reachable through the alias's arguments.
            let mapper =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    AliasTypeArguments = [ 20 ]
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 500 "input" SymbolFlags.FunctionScopedVariable) 20 ]
                              20 ] }

            let model =
                { Build.shapeModel (mapper :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 50, "Mapper" ] }

            let shaped, findings = Build.runPass Shape.shapeCallbacks model

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Mapper<'T>"
                Expect.equal decl.Target (FsDelegate([ FsTypeVar "T" ], FsTypeVar "T")) "Func<'T, 'T>"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

            Expect.isEmpty findings "a generic alias is exact"

        testCase "shape-classes emits a constructor member per construct signature" <| fun _ ->
            let instance =
                { Build.facts (Build.typeResponse 80 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 801 "progress" SymbolFlags.Property) 2 ] }

            let static' =
                { Build.facts (Build.typeResponse 81 TypeFlags.Object) with
                    ConstructSignatures =
                        [ Build.signature
                              [ { Build.resolvedMember (Build.symbol 802 "options" SymbolFlags.FunctionScopedVariable) 2 with
                                    Optional = true } ]
                              80 ] }

            let model =
                { Build.shapeModel (instance :: static' :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export "Timer" (Build.symbol 800 "Timer" (SymbolFlags.Class ||| SymbolFlags.Value)) ] }
                    ExportTypes = Map.ofList [ 800, { Declared = Some 80; Value = Some 81 } ]
                    DeclNames = Map.ofList [ 80, "Timer" ] }

            let shaped, findings = Build.runPass Shape.shapeClasses model

            Expect.isEmpty (findings |> List.filter (fun f -> f.Tier = Escape)) "no drops"

            match shaped.ExportMembers with
            | [ (0, m) ] ->
                Expect.equal m.Name "Timer" "constructor member name"

                match m.Body with
                | ExportConstructor([ p ], FsNamed "Timer") -> Expect.isTrue p.Optional "optional ctor parameter"
                | body -> failtest $"expected a constructor returning Timer, got %A{body}"
            | members -> failtest $"expected one constructor member, got %A{members}"

        testCase "synthesize-paramobjects gives plain-data interfaces a Create overload (D3)" <| fun _ ->
            let decl =
                FsInterface
                    { Name = "Options"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members =
                        [ FsProperty
                              { Name = "delay"
                                Docs = ""
                                Tags = []
                                ReadOnly = false
                                Type = FsOption FsFloat }
                          FsProperty
                              { Name = "target"
                                Docs = ""
                                Tags = []
                                ReadOnly = false
                                Type = FsString } ]
                      CreateOverloads = [] }

            let model = { Build.shapeModel [] with Decls = [ decl ] }
            let shaped, findings = Build.runPass Shape.synthesizeParamObjects model

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "reported"

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.CreateOverloads with
                | [ [ target; delay ] ] ->
                    Expect.equal (target.Name, target.Optional) ("target", false) "required first"
                    Expect.equal (delay.Name, delay.Optional) ("delay", true) "optional after"
                | overloads -> failtest $"expected one two-parameter overload, got %A{overloads}"
            | decls -> failtest $"expected the interface back, got %A{decls}"

        testCase "a methodful interface gets no Create overload" <| fun _ ->
            let decl =
                FsInterface
                    { Name = "Timer"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members =
                        [ FsMethod
                              { Name = "play"
                                Docs = ""
                                Tags = []
                                Parameters = []
                                Return = FsUnit } ]
                      CreateOverloads = [] }

            let model = { Build.shapeModel [] with Decls = [ decl ] }
            let shaped, findings = Build.runPass Shape.synthesizeParamObjects model

            Expect.isEmpty findings "nothing to report"

            match shaped.Decls with
            | [ FsInterface decl ] -> Expect.isEmpty decl.CreateOverloads "not plain data"
            | decls -> failtest $"expected the interface back, got %A{decls}"

        testCase "dedupe-overloads sees through abbreviations and drops the twin" <| fun _ ->
            let parameter name reference =
                { Name = name
                  Optional = false
                  Rest = false
                  Type = reference }

            let method' name parameters =
                FsMethod
                    { Name = name
                      Docs = ""
                      Tags = []
                      Parameters = parameters
                      Return = FsUnit }

            let abbrev name =
                FsAbbrev
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Target = FsObj }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ abbrev "DOMTargets"
                          abbrev "JSTargets"
                          FsInterface
                              { Name = "Scope"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                  [ method' "add" [ parameter "targets" (FsNamed "DOMTargets") ]
                                    method' "add" [ parameter "targets" (FsNamed "JSTargets") ]
                                    method' "add" [ parameter "targets" FsString ] ]
                                CreateOverloads = [] } ] }

            let deduped, findings = Build.runPass Shape.dedupeOverloads model

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Widened, "Scope.add" ]
                "the twin is a finding"

            match deduped.Decls |> List.pick (function FsInterface d -> Some d | _ -> None) with
            | decl ->
                Expect.equal
                    (decl.Members
                     |> List.map (function
                         | FsMethod m -> m.Parameters.Head.Type
                         | FsProperty p -> p.Type))
                    [ FsNamed "DOMTargets"; FsString ]
                    "first of the obj pair survives; the string overload is distinct"

        testCase "detect-tagged-unions reads the arms' fields, not the arm types" <| fun _ ->
            // The arm properties become the case fields, because that is what Fable's erasure
            // writes: `Circle(radius = 2.0)` -> `{ kind: "circle", radius: 2 }`. The tag itself
            // is not a field - Fable writes it from the compiled name.
            let arm id tag extra =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol (id * 10) "kind" SymbolFlags.Property) tag
                          Build.resolvedMember (Build.symbol (id * 10 + 1) extra SymbolFlags.Property) 2 ] }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 20; 21 ] }

            let model =
                { Build.shapeModel (
                      union
                      :: arm 20 7 "radius"
                      :: arm 21 8 "width"
                      :: stringLiteral 7 "circle"
                      :: stringLiteral 8 "round-rect"
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Shape" ] }

            let shaped, findings = Build.runPass Shape.detectTaggedUnions model

            match shaped.Decls |> List.pick (function FsTaggedUnion d -> Some d | _ -> None) with
            | decl ->
                Expect.equal decl.Tag "kind" "the discriminant the checker proved"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName))
                    [ "Circle", Some "circle"; "RoundRect", Some "round-rect" ]
                    "case names derive from the tag values, which keep a CompiledName"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Fields |> List.map (fun f -> f.Name, f.Type)))
                    [ [ "radius", FsFloat ]; [ "width", FsFloat ] ]
                    "the tag is not a field; everything else is"

            Expect.equal (findings |> List.map _.Tier) [ Exact ] "a tagged union costs no fidelity"

        testCase "detect-tagged-unions leaves an arm that is not plain data alone" <| fun _ ->
            let method' id =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    CallSignatures = [ Build.signature [] 4 ] }

            let arm id tag =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol (id * 10) "kind" SymbolFlags.Property) tag
                          Build.resolvedMember (Build.symbol (id * 10 + 1) "run" SymbolFlags.Method) 30 ] }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 20; 21 ] }

            let model =
                { Build.shapeModel (
                      union
                      :: method' 30
                      :: arm 20 7
                      :: arm 21 8
                      :: stringLiteral 7 "a"
                      :: stringLiteral 8 "b"
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Shape" ] }

            let shaped, findings = Build.runPass Shape.detectTaggedUnions model

            Expect.isEmpty
                (shaped.Decls |> List.choose (function FsTaggedUnion d -> Some d | _ -> None))
                "a method has no case-field form, so the erased union stands"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "the missed match is reported"

        testCase "shape-aliases twin unions chain to the smallest id, never cycle" <| fun _ ->
            // Two declared unions over the same member set: only the smaller id is canonical.
            // The larger abbreviates to it; the smaller resolves structurally. An A <-> B
            // abbreviation cycle here sends fsc into non-termination once a generic
            // instantiation references it, so the chain must strictly decrease.
            let twin id =
                { Build.facts (Build.typeResponse id TypeFlags.Union) with
                    UnionMembers = [ 1; 2 ] }

            let model =
                { Build.shapeModel (twin 10 :: twin 11 :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "ScrollThresholdValue"; 11, "TimelinePosition" ] }

            let shaped, findings = Build.runPass Shape.shapeAliases model

            let targets =
                shaped.Decls
                |> List.choose (function
                    | FsAbbrev d -> Some(d.Name, d.Target)
                    | _ -> None)

            Expect.equal
                targets
                [ "ScrollThresholdValue", FsErasedUnion [ FsString; FsFloat ]
                  "TimelinePosition", FsNamed "ScrollThresholdValue" ]
                "the smaller twin resolves structurally, the larger references it"

            Expect.equal (findings |> List.map _.Tier) [] "an erased union costs no fidelity"

        testCase "order-declarations puts declarations in source order, Exports last" <| fun _ ->
            let interface' name order =
                FsInterface
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = Some order
                      TypeParameters = []
                      Inherits = []
                      Members = []
                      CreateOverloads = [] }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ interface' "B" { File = "b.ts"; NodeIndex = 5 }
                          interface' "A" { File = "a.ts"; NodeIndex = 9 } ]
                    ExportMembers =
                        [ 0,
                          { Name = "make"
                            Docs = ""
                            Tags = []
                            Binding = ImportNamed "make"
                            Body = ExportValue FsFloat } ] }

            let ordered, _ = Build.runPass Shape.orderDeclarations model

            let names =
                ordered.Decls
                |> List.map (function
                    | FsInterface decl -> decl.Name
                    | FsExports _ -> "<exports>"
                    | decl -> failtest $"unexpected decl %A{decl}")

            Expect.equal names [ "A"; "B"; "<exports>" ] "file order first, Exports last"
            Expect.isEmpty ordered.ExportMembers "consumed into the Exports decl"

        testCase "repair-arity drops an alias whose target lost its parameters, and widens its uses" <| fun _ ->
            // `type Params<'P> = obj` is FS0035, and every reference to it has to go with it.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsAbbrev
                              { Name = "Params"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = [ { Name = "P"; Constraint = None } ]
                                Target = FsObj }
                          FsInterface
                              { Name = "Context"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                    [ FsProperty
                                          { Name = "params"
                                            Docs = ""
                                            Tags = []
                                            ReadOnly = true
                                            Type = FsApp("Params", [ FsString ]) } ]
                                CreateOverloads = [] } ] }

            let repaired, findings = Build.runPass Shape.repairArity model

            match repaired.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Context" "the alias is gone, its user stays"

                match decl.Members with
                | [ FsProperty p ] -> Expect.equal p.Type FsObj "the reference widened"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected the interface alone, got %A{decls}"

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Widened, "Params"; Widened, "Context" ]
                "the drop and the widening are both findings"

        testCase "repair-arity widens a generic named without its arguments" <| fun _ ->
            // FS0033: `PagesFunctionContext` takes three arguments and this position has none.
            let generic =
                FsInterface
                    { Name = "Ctx"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = [ { Name = "Env"; Constraint = None } ]
                      Inherits = []
                      Members = []
                      CreateOverloads = [] }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ generic
                          FsAbbrev
                              { Name = "Handler"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Target = FsDelegate([ FsNamed "Ctx" ], FsNamed "Other") } ] }

            let repaired, findings = Build.runPass Shape.repairArity model

            match repaired.Decls with
            | [ _; FsAbbrev decl ] ->
                Expect.equal
                    decl.Target
                    (FsDelegate([ FsObj ], FsNamed "Other"))
                    "the generic widened; a name this run does not declare is left alone (O7)"
            | decls -> failtest $"expected the interface and the alias, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier) [ Widened ] "one widening, reported"

        testCase "repair-arity demotes a settable property that holds no value" <| fun _ ->
            // FS0252: `[__BRAND]: never` shapes to `unit`, which cannot be a setter's type.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsInterface
                              { Name = "Branded"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                    [ FsProperty
                                          { Name = "__BRAND"
                                            Docs = ""
                                            Tags = []
                                            ReadOnly = false
                                            Type = FsUnit } ]
                                CreateOverloads = [] } ] }

            let repaired, findings = Build.runPass Shape.repairArity model

            match repaired.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsProperty p ] -> Expect.isTrue p.ReadOnly "the setter is gone, the member reads"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected the interface, got %A{decls}"

            Expect.equal (findings |> List.map (fun f -> f.Tier, f.Symbol)) [ Ergonomic, "Branded" ] "reported"

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
