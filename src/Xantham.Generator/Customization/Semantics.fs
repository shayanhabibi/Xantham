module internal Xantham.Generator.Customization.Semantics

open System
open System.Text.Json
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator
open Xantham.Generator.Measure

let private packageName (ctx: Context) = function
    | CompilerLib -> "typescript/lib"
    | Dependency name -> name / uom<_>
    | _ -> ctx.PackageName / uom<_>

let private normalizedHandle (ctx: Context) (handle: string<declHandle>) =
    match (handle / uom<declHandle>).Split([|'.'|], 3) with
    | [| index; kind; file |] ->
        let _, owner, path = Grouping.sourceOrderKey ctx.PackageDir (file * uom<filePath>)
        String.concat ":" [owner; path; kind; index]
    | _ -> invalidOp "customization/missing-source-metadata: malformed declaration handle"

let project (ctx: Context) (shape: ShapeModel) (findings: Finding list) =
    async {
        let mutable table = shape.Types
        let mutable symbols = Map.empty
        let mutable diagnostics = []
        let mutable pending = table |> Map.toList |> List.map fst
        let mutable visited = Set.empty

        while not (List.isEmpty pending) do
            let typeId = List.head pending
            pending <- List.tail pending
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited
                let facts = table[typeId]
                let! actual = ctx.Session.getSymbolOfType (typeId / uom<_>)
                let! symbol =
                    match actual with
                    | ValueSome _ -> async.Return actual
                    | ValueNone -> ctx.Session.getAliasSymbolOfType (typeId / uom<_>)
                match symbol with
                | ValueSome symbol when not (symbol.Name.StartsWith "__") ->
                    symbols <- Map.add typeId symbol symbols
                    let handles = symbol.DeclarationHandles |> ValueOption.defaultValue [||] |> Array.toList
                    let origin = Grouping.classify ctx.PackageDir (ValueSome symbol)
                    let facts =
                        { facts with
                            SymbolName = Some symbol.SymbolName
                            SymbolParent = symbol.ParentSymbolId |> ValueOption.toOption
                            Origin = origin
                            Declarations = handles }
                    let! resolved, discovered =
                        if facts.Response.Flags.HasFlag TypeFlags.Object && not (symbol.Flags.HasFlag SymbolFlags.TypeParameter) then
                            Resolve.customizationFacts ctx facts
                        else async.Return(facts, [])
                    table <- Map.add typeId resolved table
                    for response in discovered do
                        if not (Map.containsKey response.TypeId table) then
                            table <- Map.add response.TypeId (TypeFacts.shallow response) table
                        if List.contains response.TypeId resolved.BaseTypes then pending <- pending @ [response.TypeId]
                | _ -> ()

        let projectionShape = { shape with Types = table }
        let declaredPath (symbol: SymbolResponse) =
            let parent =
                symbol.ParentSymbolId |> ValueOption.toOption
                |> Option.bind (fun id -> Map.tryFind id shape.Harvest.Namespaces)
                |> Option.map (fun name -> (name / uom<symbolName>).Split '.' |> Array.toList)
                |> Option.defaultValue []
            parent @ [symbol.Name]

        let declarationIds =
            symbols |> Map.toList |> List.choose (fun (id, symbol) ->
                let handles = table[id].Declarations
                if List.isEmpty handles then
                    diagnostics <- diagnostics @ [Diagnostic.create "customization/missing-source-metadata" $"Source metadata is unavailable for {symbol.Name}" (Some symbol.Name)]
                    None
                else
                    let declaration = handles |> List.map (normalizedHandle ctx) |> List.distinct |> List.sort |> JsonSerializer.Serialize
                    Some(id, declaration)) |> Map.ofList

        let keyOf id =
            let declaration = declarationIds[id]
            let facts = table[id]
            let arguments = facts.TypeArguments |> List.map (fun arg -> Shape.Spec.typeRef ctx projectionShape None "customization" arg |> fst |> Render.printType)
            JsonSerializer.Serialize((declaration, arguments))

        let keys = declarationIds |> Map.map (fun id _ -> keyOf id)
        let sourceForSymbol =
            symbols |> Map.toList |> List.filter (fun (id, _) -> Map.containsKey id keys)
            |> List.sortBy (fun (id, _) -> table[id].Response.Target |> ValueOption.exists ((<>) (id / uom<_>)))
            |> List.fold (fun map (id, symbol) -> if Map.containsKey symbol.SymbolId map then map else Map.add symbol.SymbolId keys[id] map) Map.empty

        let exportsFor id =
            shape.Harvest.Exports |> List.choose (fun export ->
                match Map.tryFind export.Symbol.SymbolId shape.ExportTypes with
                | Some types when types.Declared = Some id -> Some(declaredPath { export.Symbol with Name = export.ExportName })
                | _ -> None)

        let mutable memberInfos = []
        let typeInfos =
            keys |> Map.toList |> List.map (fun (id, key) ->
                let facts = table[id]
                let symbol = symbols[id]
                let isInstantiation = facts.Response.Target |> ValueOption.exists ((<>) (id / uom<_>))
                let paths = if isInstantiation then [] else [declaredPath symbol] @ exportsFor id |> List.distinct
                let target = Map.tryFind id shape.DeclNames
                let external = GeneratorConfig.disposition ctx.Config facts.Origin <> Ship
                let properties, excluded = facts.Members |> List.partition (fun member_ -> not (member_.Symbol.Flags.HasFlag SymbolFlags.Method))
                let memberKeys =
                    properties |> List.distinctBy (fun m -> m.Symbol.Name) |> List.map (fun member_ ->
                        let name = member_.Symbol.Name
                        let memberKey = JsonSerializer.Serialize((key, name))
                        let declaring = member_.Symbol.ParentSymbolId |> ValueOption.toOption |> Option.bind (fun parent -> Map.tryFind parent sourceForSymbol) |> Option.defaultValue key
                        let mapped, _ = Shape.Spec.typeRef ctx projectionShape target (symbol.Name + "." + name) member_.TypeId
                        let mapped = if member_.Optional && not (match mapped with FsOption _ -> true | _ -> false) then FsOption mapped else mapped
                        let targets =
                            target |> Option.toList |> List.collect (fun target ->
                                shape.Decls |> List.collect (function
                                    | FsInterface decl when decl.Name = target -> decl.Members |> List.choose (function FsProperty p when p.Name = name -> Some(target, name, external) | _ -> None)
                                    | _ -> []))
                        memberInfos <- memberInfos @ [{Key=memberKey; Receiver=key; Declaring=declaring; Name=name; ReadOnly=member_.ReadOnly; Optional=member_.Optional; Type=mapped; Targets=targets}]
                        memberKey)
                {Key=key; Package=packageName ctx facts.Origin; Paths=paths; Declaration=declarationIds[id]
                 Bases=facts.BaseTypes |> List.choose (fun id -> Map.tryFind id keys)
                 Implemented=facts.ImplementedTypes |> List.choose (fun id -> Map.tryFind id keys)
                 Members=memberKeys; Target=target; External=external
                 Findings=findings |> List.filter (fun f -> f.Symbol = symbol.Name || f.Symbol.StartsWith(symbol.Name + "."))
                 Excluded=(excluded |> List.map (fun m -> "method:" + m.Symbol.Name)) @ (facts.IndexInfos |> List.map (fun _ -> "indexer"))})

        let typeInfos =
            typeInfos |> List.groupBy _.Key |> List.map (fun (_, infos) ->
                let first = List.head infos
                { first with Paths=infos |> List.collect _.Paths |> List.distinct
                             Members=infos |> List.collect _.Members |> List.distinct })
        let aliases =
            shape.Harvest.Exports |> List.choose (fun export ->
                Map.tryFind export.Symbol.SymbolId shape.ExportTypes
                |> Option.bind _.Declared
                |> Option.bind (fun id -> Map.tryFind id keys)
                |> Option.bind (fun key -> typeInfos |> List.tryFind (fun info -> info.Key = key))
                |> Option.bind (fun info ->
                    let owner = packageName ctx (Grouping.classify ctx.PackageDir (ValueSome export.Symbol))
                    let path = declaredPath { export.Symbol with Name = export.ExportName }
                    if owner = info.Package then None
                    else
                        Some { info with
                                   Key = JsonSerializer.Serialize((owner, path, info.Key))
                                   Package = owner
                                   Paths = [path]
                                   Bases = [info.Key] }))
        return ContractData.snapshot (typeInfos @ aliases) (memberInfos |> List.distinctBy _.Key) diagnostics
    }
