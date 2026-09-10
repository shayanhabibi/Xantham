module Xantham.Generator.Tests.OwnershipTests

open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator
open Xantham.Generator.Tests.Build

let private entryOrder = Some { File = "/packages/ownership/index.d.ts" * Measure.uom<Measure.declFile>; NodeIndex = 1<Measure.nodeId> }
let private coreOrder = Some { File = "/compiler/lib.es5.d.ts" * Measure.uom<Measure.declFile>; NodeIndex = 1<Measure.nodeId> }

let private abbreviation name order target =
    FsAbbrev
        { Value = None; Name = name
          Docs = ""
          Tags = []
          Order = order
          TypeParameters = []
          Target = target }

let private placement expected exportedNames names declarations facts =
    let ctx =
        { context with
            PackageDir = "/packages/ownership" * Measure.uom<Measure.dirPath>
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib" * Measure.uom<Measure.npmDependency>, Ship ] } }

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
            ExportTypes = exported |> List.map (fun (typeId, export) -> export.Symbol.Id * Measure.uom<Measure.symbolId>, { Declared = Some (typeId * Measure.uom<Measure.typeId>); Value = None }) |> Map.ofList
            DeclNames = names |> List.map (fun (id, name) -> id * Measure.uom<Measure.typeId>, name) |> Map.ofList
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
            PackageDir = "/packages/ownership" * Measure.uom<Measure.dirPath>
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib" * Measure.uom<Measure.npmDependency>, Ship; "shared-types" * Measure.uom<Measure.npmDependency>, Ship ] } }

    let definingOrder = Some { File = definingFile; NodeIndex = 2<Measure.nodeId> }
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
                    [ 100<Measure.symbolId>, { Declared = Some 10<Measure.typeId>; Value = None }
                      101<Measure.symbolId>, { Declared = Some 10<Measure.typeId>; Value = None } ] }

    let named, _ =
        Pipeline.runTier ctx [ Shape.ExportNames.nameExports ] model |> Async.RunSynchronously

    expected, (Map.find 10<Measure.typeId> named.DeclNames, Map.find 10<Measure.typeId> named.DeclOrders)

let private hookPlacement expected origin file compilerLib =
    let ctx =
        { context with
            PackageDir = "/packages/ownership" * Measure.uom<Measure.dirPath>
            Config =
                { GeneratorConfig.Default with
                    Groups = Map.ofList [ "typescript/lib" * Measure.uom<Measure.npmDependency>, Ship; "shared-sdk" * Measure.uom<Measure.npmDependency>, Ship ]
                    CompilerLib = compilerLib } }
    let iface name =
        FsInterface
            { Name = name; Docs = ""; Tags = []; Order = None; TypeParameters = []
              Inherits = []; Members = []; Entrypoint = None; CreateOverloads = []; Statics = [] }
    let station =
        { facts (typeResponse 10 TypeFlags.Object) with
            Origin = origin; SymbolName = Some ("Station" * Measure.uom<Measure.symbolName>); DeclFile = Some file }
    let model =
        { shapeModel [ station ] with
            DeclNames = Map.ofList [ 10<Measure.typeId>, "Station" ]
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
                DeclFile = Some ("/compiler/lib.es5.d.ts" * Measure.uom<Measure.declFile>) }

        testCase "entry aliases retain their declaration owner" <| fun _ ->
            placement
                [ "Items", true; "Brief", true; "Store", true; "Store.Update.Value", true ]
                [ "Items"; "Brief"; "Store" ]
                [ 10, "Items"; 11, "Brief"; 12, "Store.Update.Value"; 13, "Store" ]
                [ abbreviation "Items" entryOrder (FsNamed "Item")
                  abbreviation "Brief" entryOrder (FsNamed "Item")
                  abbreviation "Store.Update.Value" entryOrder (FsNamed "Item")
                  abbreviation "Store" entryOrder FsObj ]
                [ coreType 10 ("Array" * Measure.uom<Measure.symbolName>); coreType 11 ("Omit" * Measure.uom<Measure.symbolName>); coreType 12 ("Partial" * Measure.uom<Measure.symbolName>)
                  { facts (typeResponse 13 TypeFlags.Object) with Origin = EntryPackage } ]
            ||> Flip.Expect.equal "library constructors do not own their entry instantiations"

        testCase "an entry alias does not move its shared constructor" <| fun _ ->
            placement
                [ "Date", false; "LocalDate", true ]
                [ "Date" ]
                [ 10, "Date" ]
                [ abbreviation "Date" coreOrder FsObj
                  abbreviation "LocalDate" entryOrder (FsNamed "Date") ]
                [ coreType 10 ("Date" * Measure.uom<Measure.symbolName>) ]
            ||> Flip.Expect.equal "two declaration owners can share one checker type id"

        testCase "the global environment belongs to the current program" <| fun _ ->
            placement
                [ "GlobalThis", true ]
                []
                [ 10, "GlobalThis" ]
                [ abbreviation "GlobalThis" None (FsNamed "Item") ]
                [ { coreType 10 ("globalThis" * Measure.uom<Measure.symbolName>) with DeclFile = None } ]
            ||> Flip.Expect.equal "program globals must not make reusable core depend on entry declarations"

        testCase "transitive core declarations retain ownership when reached through an entry alias" <| fun _ ->
            placement
                [ "DateTimeFormatOptions", false; "DateTimeFormatOptions.LocaleMatcher", false ]
                []
                [ 10, "DateTimeFormatOptions"; 11, "DateTimeFormatOptions.LocaleMatcher" ]
                [ abbreviation "DateTimeFormatOptions" entryOrder FsObj
                  abbreviation "DateTimeFormatOptions.LocaleMatcher" entryOrder FsString ]
                [ coreType 10 ("DateTimeFormatOptions" * Measure.uom<Measure.symbolName>)
                  facts (typeResponse 11 TypeFlags.Union) ]
            ||> Flip.Expect.equal "traversal order is not declaration ownership"

        testTheory "shared identity retains its defining group" [
                "/compiler/lib.es5.d.ts"
                ==> ("SharedType", Some { File = ("/compiler/lib.es5.d.ts" * Measure.uom<Measure.declFile>); NodeIndex = 2<Measure.nodeId> })
                "/packages/ownership/node_modules/shared-types/index.d.ts"
                ==> ("SharedType", Some { File = ("/packages/ownership/node_modules/shared-types/index.d.ts" * Measure.uom<Measure.declFile>); NodeIndex = 2<Measure.nodeId> })
                "/packages/ownership/definitions.d.ts"
                ==> ("LocalType", entryOrder) // Same-package aliases retain their existing harvest-order policy.
        ] <| fun (file, expected) ->
            sharedName expected (file * Measure.uom<Measure.declFile>)
            ||> Flip.Expect.equal "the defining export owns cross-package identity"

        testCase "a synthesized hook without a type-table name follows its shipped owner" <| fun _ ->
            hookPlacement "SharedSdk" (Dependency ("shared-sdk" * Measure.uom<Measure.npmDependency>)) ("/packages/ownership/node_modules/shared-sdk/index.d.ts" * Measure.uom<Measure.declFile>) CompilerLibConfig.Default
            ||> Flip.Expect.equal "a nested secondary alias still belongs to its own entry declaration"

        testCase "a synthesized hook follows its owner's compiler-library family" <| fun _ ->
            hookPlacement "TypeScript.Lib.Dom" CompilerLib ("/compiler/lib.dom.d.ts" * Measure.uom<Measure.declFile>) CompilerLibConfig.Default
            ||> Flip.Expect.equal "the missing hook type id must not move it to the Es family"

        testTheory "configured compiler-library families own their declarations" [
            "/compiler/lib.es5.d.ts", "Fable.Core.TS.Ecma"
            "/compiler/lib.dom.d.ts", "Fable.Core.TS.Browser"
        ] <| fun (file, expected) ->
            let compilerLib =
                { CompilerLibConfig.Default with
                    ModuleName = Some "Fable.Core.TS"
                    EsModuleName = Some "Ecma"
                    DomModuleName = Some "Browser" }

            hookPlacement expected CompilerLib (file * Measure.uom<Measure.declFile>) compilerLib
            ||> Flip.Expect.equal "ownership uses the configured family's fully qualified module"
    ]

[<Tests>]
let exportOrderTests =
    testList "portable export source order" [
        let expected = [ "entryFirst"; "entryTie"; "entryLast"; "dependency"; "compiler"; "missing" ]
        let inline (==>) compilerRoot names = compilerRoot, names
        testTheory "compiler installation position preserves logical source order" [
            "/a-toolchain" ==> expected
            "/z-toolchain" ==> expected
        ] <| fun (compilerRoot, expected) ->
            let packageDir = "/packages/entry"
            let ordered name file index =
                { export name (symbol index name SymbolFlags.Variable) with
                    Order = Some { File = file * Measure.uom<Measure.declFile>; NodeIndex = index * Measure.uom<Measure.nodeId> } }
            let model =
                { HarvestModel.Empty with
                    Exports =
                        [ ordered "compiler" (compilerRoot + "/node_modules/@typescript/typescript-linux-x64/lib/lib.es5.d.ts") 1
                          ordered "entryTie" (packageDir + "/index.d.ts") 2
                          ordered "dependency" (packageDir + "/node_modules/@scope/dependency/index.d.ts") 1
                          ordered "entryLast" (packageDir + "/z.d.ts") 1
                          export "missing" (symbol 100 "missing" SymbolFlags.Variable)
                          ordered "entryFirst" (packageDir + "/index.d.ts") 2 ] }
            let ctx = { context with PackageDir = packageDir * Measure.uom<Measure.dirPath> }
            let actual, _ = Pipeline.runTier ctx [ Harvest.orderExports ] model |> Async.RunSynchronously
            actual.Exports |> List.map _.ExportName
            |> Flip.Expect.equal "entry files precede dependency and compiler files; source/name ties remain stable" expected

            let declarations =
                { shapeModel [] with
                    Decls = model.Exports |> List.map (fun export -> abbreviation export.ExportName export.Order FsString) }
            let shaped, _ =
                Pipeline.runTier ctx [ Shape.Ordering.orderDeclarations ] declarations |> Async.RunSynchronously
            shaped.Decls |> List.choose Render.declName
            |> Flip.Expect.equal "emitted declarations retain the same logical source order" expected
    ]
