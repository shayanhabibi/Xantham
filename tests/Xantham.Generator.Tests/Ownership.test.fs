module Xantham.Generator.Tests.OwnershipTests

open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator
open Xantham.Generator.Tests.Build

let private entryOrder = Some { File = "/packages/ownership/index.d.ts"; NodeIndex = 1 }
let private coreOrder = Some { File = "/compiler/lib.es5.d.ts"; NodeIndex = 1 }

let private abbreviation name order target =
    FsAbbrev
        { Name = name
          Docs = ""
          Tags = []
          Order = order
          TypeParameters = []
          Target = target }

let private placement expected exportedNames names declarations facts =
    let ctx =
        { context with
            PackageDir = "/packages/ownership"
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib", Ship ] } }

    let exported =
        declarations
        |> List.choose (function
            | FsAbbrev decl when List.contains decl.Name exportedNames ->
                names
                |> List.tryPick (fun (typeId, name) ->
                    if name <> decl.Name then None
                    else
                        let sym =
                            { symbol (typeId + 100) name SymbolFlags.TypeAlias with
                                Declarations = decl.Order |> Option.map (fun order -> [| $"{order.NodeIndex}.TypeAliasDeclaration.{order.File}" |]) |> ValueOption.ofOption }
                        Some(typeId, { export name sym with Order = decl.Order }))
            | _ -> None)

    let model =
        { shapeModel facts with
            Harvest = { HarvestModel.Empty with Exports = exported |> List.map snd }
            ExportTypes = exported |> List.map (fun (typeId, export) -> export.Symbol.Id, { Declared = Some typeId; Value = None }) |> Map.ofList
            DeclNames = Map.ofList names
            Decls = declarations }

    let actual =
        Pipeline.groupModules ctx model
        |> List.collect (fun group ->
            group.Decls
            |> List.choose (fun decl -> Render.declName decl |> Option.map (fun name -> name, group.IsEntry)))
        |> Map.ofList

    Map.ofList expected, actual

let private sharedName expected definingFile =
    let ctx =
        { context with
            PackageDir = "/packages/ownership"
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib", Ship; "shared-types", Ship ] } }

    let definingOrder = Some { File = definingFile; NodeIndex = 2 }
    let definition =
        { symbol 100 "SharedType" SymbolFlags.Interface with
            Declarations = ValueSome [| $"2.InterfaceDeclaration.{definingFile}" |] }
    let alias =
        { symbol 101 "LocalType" SymbolFlags.TypeAlias with
            Declarations = ValueSome [| "1.TypeAliasDeclaration./packages/ownership/index.d.ts" |] }
    let shared =
        facts { typeResponse 10 TypeFlags.Object with Symbol = ValueSome definition.Id }
    let model =
        { shapeModel [ shared ] with
            Harvest =
                { HarvestModel.Empty with
                    Exports =
                        [ { export "LocalType" alias with Order = entryOrder }
                          { export "SharedType" definition with Order = definingOrder } ] }
            ExportTypes =
                Map.ofList
                    [ 100, { Declared = Some 10; Value = None }
                      101, { Declared = Some 10; Value = None } ] }

    let named, _ =
        Pipeline.runTier ctx [ Shape.ExportNames.nameExports ] model |> Async.RunSynchronously

    expected, (Map.find 10 named.DeclNames, Map.find 10 named.DeclOrders)

let private hookPlacement expected origin file =
    let ctx =
        { context with
            PackageDir = "/packages/ownership"
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib", Ship; "shared-sdk", Ship ] } }
    let iface name =
        FsInterface
            { Name = name; Docs = ""; Tags = []; Order = None; TypeParameters = []
              Inherits = []; Members = []; Entrypoint = None; CreateOverloads = []; Statics = [] }
    let station =
        { facts (typeResponse 10 TypeFlags.Object) with
            Origin = origin; SymbolName = Some "Station"; DeclFile = Some file }
    let model =
        { shapeModel [ station ] with
            DeclNames = Map.ofList [ 10, "Station" ]
            Decls =
                [ iface "Station"
                  iface "Station.IFetchHandler"
                  abbreviation "Station.LocalAlias" entryOrder (FsNamed "Station") ] }
    let actual =
        Pipeline.groupModules ctx model
        |> List.collect (fun group ->
            group.Decls
            |> List.choose (fun decl -> Render.declName decl |> Option.map (fun name -> name, group.Module)))
        |> Map.ofList

    Map.ofList [ "Station", expected; "Station.IFetchHandler", expected; "Station.LocalAlias", "TestPkg" ], actual

[<Tests>]
let tests =
    testList "compiler-lib ownership" [
        let inline (==>) file (name, order) = file, (name, order)

        let coreType id name =
            { facts (typeResponse id TypeFlags.Object) with
                Origin = CompilerLib
                SymbolName = Some name
                DeclFile = Some "/compiler/lib.es5.d.ts" }

        testCase "entry aliases retain their declaration owner" <| fun _ ->
            placement
                [ "Items", true; "Brief", true; "Store", true; "Store.Update.Value", true ]
                [ "Items"; "Brief"; "Store" ]
                [ 10, "Items"; 11, "Brief"; 12, "Store.Update.Value"; 13, "Store" ]
                [ abbreviation "Items" entryOrder (FsNamed "Item")
                  abbreviation "Brief" entryOrder (FsNamed "Item")
                  abbreviation "Store.Update.Value" entryOrder (FsNamed "Item")
                  abbreviation "Store" entryOrder FsObj ]
                [ coreType 10 "Array"; coreType 11 "Omit"; coreType 12 "Partial"
                  { facts (typeResponse 13 TypeFlags.Object) with Origin = EntryPackage } ]
            ||> Flip.Expect.equal "library constructors do not own their entry instantiations"

        testCase "an entry alias does not move its shared constructor" <| fun _ ->
            placement
                [ "Date", false; "LocalDate", true ]
                [ "Date" ]
                [ 10, "Date" ]
                [ abbreviation "Date" coreOrder FsObj
                  abbreviation "LocalDate" entryOrder (FsNamed "Date") ]
                [ coreType 10 "Date" ]
            ||> Flip.Expect.equal "two declaration owners can share one checker type id"

        testCase "the global environment belongs to the current program" <| fun _ ->
            placement
                [ "GlobalThis", true ]
                []
                [ 10, "GlobalThis" ]
                [ abbreviation "GlobalThis" None (FsNamed "Item") ]
                [ { coreType 10 "globalThis" with DeclFile = None } ]
            ||> Flip.Expect.equal "program globals must not make reusable core depend on entry declarations"

        testCase "transitive core declarations retain ownership when reached through an entry alias" <| fun _ ->
            placement
                [ "DateTimeFormatOptions", false; "DateTimeFormatOptions.LocaleMatcher", false ]
                []
                [ 10, "DateTimeFormatOptions"; 11, "DateTimeFormatOptions.LocaleMatcher" ]
                [ abbreviation "DateTimeFormatOptions" entryOrder FsObj
                  abbreviation "DateTimeFormatOptions.LocaleMatcher" entryOrder FsString ]
                [ coreType 10 "DateTimeFormatOptions"
                  facts (typeResponse 11 TypeFlags.Union) ]
            ||> Flip.Expect.equal "traversal order is not declaration ownership"

        testTheory "shared identity retains its defining group" [
            "/compiler/lib.es5.d.ts"
                ==> ("SharedType", Some { File = "/compiler/lib.es5.d.ts"; NodeIndex = 2 })
            "/packages/ownership/node_modules/shared-types/index.d.ts"
                ==> ("SharedType", Some { File = "/packages/ownership/node_modules/shared-types/index.d.ts"; NodeIndex = 2 })
            "/packages/ownership/definitions.d.ts"
                ==> ("LocalType", entryOrder) // Same-package aliases retain their existing harvest-order policy.
        ] <| fun (file, expected) ->
            sharedName expected file
            ||> Flip.Expect.equal "the defining export owns cross-package identity"

        testCase "a synthesized hook without a type-table name follows its shipped owner" <| fun _ ->
            hookPlacement "SharedSdk" (Dependency "shared-sdk") "/packages/ownership/node_modules/shared-sdk/index.d.ts"
            ||> Flip.Expect.equal "a nested secondary alias still belongs to its own entry declaration"

        testCase "a synthesized hook follows its owner's compiler-library family" <| fun _ ->
            hookPlacement "TypeScript.Lib.Dom" CompilerLib "/compiler/lib.dom.d.ts"
            ||> Flip.Expect.equal "the missing hook type id must not move it to the Es family"
    ]
