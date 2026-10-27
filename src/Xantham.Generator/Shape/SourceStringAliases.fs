module Xantham.Generator.Shape.SourceStringAliases

open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.Generator.Shape.Spec

let nameAliases: Pass<ShapeModel> =
    Pass.pure' "name-source-string-aliases" (fun ctx model ->
        let occurrences =
            model.Types
            |> Map.toList
            |> List.collect (fun (id, facts) -> facts.Members |> List.map (fun member_ -> id, member_))

        let members = occurrences |> List.map snd
        let path = ExportLayout.declPath (ExportLayout.modulePaths model) model

        let exportName export =
            String.concat
                "."
                (path export
                 @ [
                     underNamespace
                         (exportNamespacePath model.Harvest.Namespaces export)
                         (Naming.typeNameSegment (fsName (defaultExportName ctx) export))
                 ])

        let exports =
            model.Harvest.Exports
            |> List.groupBy _.Symbol.SymbolId
            |> List.map (fun (key, exports) -> key, exports |> List.map exportName |> List.distinct |> List.sort)
            |> Map.ofList

        let mutable taken =
            Set.union
                (model.DeclNames |> Map.toList |> List.map snd |> Set.ofList)
                (exports |> Map.toList |> List.collect snd |> Set.ofList)

        let claim preferred =
            let name =
                Seq.initInfinite (fun index ->
                    if index = 0 then
                        preferred
                    else
                        preferred + string (index + 1))
                |> Seq.find (fun name -> not (Set.contains name taken))

            taken <- Set.add name taken
            name

        let mutable reused = exports |> Map.toList |> List.collect snd |> Set.ofList

        let aliases =
            members
            |> List.collect (fun member_ ->
                match member_.SourceStringAnnotation with
                | Some(NamedStringAlias symbols) when Map.containsKey member_.TypeId model.DeclNames ->
                    symbols
                    |> List.mapi (fun index symbol ->
                        symbol, member_.TypeId, symbols |> List.tryItem (index + 1) |> Option.map _.SymbolId)
                | _ -> [])
            |> List.distinctBy (fun (symbol, _, _) -> symbol.SymbolId)
            |> List.sortBy (fun (symbol, _, _) ->
                symbol.DeclarationHandles |> ValueOption.defaultValue [||] |> Array.toList)
            |> List.map (fun (symbol, typeId, forward) ->
                let segment = Naming.typeNameSegment (symbol.SymbolName / uom<_>)

                let name =
                    match Map.tryFind symbol.SymbolId exports with
                    | Some names ->
                        Map.tryFind typeId model.DeclNames
                        |> Option.filter (fun name -> List.contains name names)
                        |> Option.defaultValue names.Head
                    | None ->
                        // An unexported alias keeps the name its type already renders under,
                        // so its group placement and module qualification are unchanged.
                        match Map.tryFind typeId model.DeclNames with
                        | Some original when
                            (original = segment || original.EndsWith("." + segment))
                            && not (Set.contains original reused)
                            ->
                            reused <- Set.add original reused
                            original
                        | _ -> claim segment

                symbol.SymbolId,
                {
                    Name = name
                    LiteralName = None
                    AliasSymbol = symbol
                    DeclaredTypeId = typeId
                    ForwardTo = forward
                })

        let declNames =
            aliases
            |> List.fold
                (fun names (key, alias) ->
                    match Map.tryFind key model.ExportTypes |> Option.bind _.Value with
                    | Some value when value <> alias.DeclaredTypeId && Map.tryFind value names = Some alias.Name ->
                        Map.add value (claim (alias.Name + "Value")) names
                    | _ -> names)
                model.DeclNames

        let sourceNames = aliases |> List.map (snd >> _.Name) |> Set.ofList

        let literals =
            aliases
            |> List.groupBy (snd >> _.DeclaredTypeId)
            |> List.choose (fun (typeId, aliases) ->
                let raw =
                    occurrences
                    |> List.filter (fun (_, member_) ->
                        member_.TypeId = typeId
                        && member_.SourceStringAnnotation = Some InlineStringLiterals)

                if not raw.IsEmpty then
                    let original = Map.find typeId model.DeclNames

                    let preferred =
                        raw
                        |> List.choose (fun (owner, member_) ->
                            Map.tryFind owner declNames
                            |> Option.map (fun owner -> owner + "." + Naming.typeNameSegment member_.Symbol.Name))
                        |> List.sort
                        |> List.tryHead
                        |> Option.defaultValue "StringLiteral"

                    let name =
                        if Set.contains original sourceNames then
                            claim preferred
                        else
                            original

                    Some(typeId, name)
                else
                    None)
            |> Map.ofList

        { model with
            DeclNames = declNames
            SourceStringAliases =
                aliases
                |> List.map (fun (key, alias) ->
                    key,
                    { alias with
                        LiteralName = Map.tryFind alias.DeclaredTypeId literals
                    })
                |> Map.ofList
        })

let reifyAliases: Pass<ShapeModel> =
    Pass.pure' "reify-source-string-aliases" (fun _ model ->
        let templates =
            model.Decls
            |> List.choose (function
                | FsStringEnum enum -> Some(enum.Name, enum)
                | _ -> None)
            |> Map.ofList

        let aliases = model.SourceStringAliases |> Map.toList |> List.map snd
        let sourceNames = aliases |> List.map _.Name |> Set.ofList

        let declarations =
            aliases
            |> List.collect (fun alias ->
                let originalName = Map.find alias.DeclaredTypeId model.DeclNames
                let template = Map.find originalName templates

                let order =
                    alias.AliasSymbol.DeclarationHandles
                    |> ValueOption.defaultValue [||]
                    |> Array.choose (fun raw -> NodeHandle.parse (raw / uom<_>) |> ValueOption.toOption)
                    |> Array.tryFind (fun handle -> handle.Kind = SyntaxKind.TypeAliasDeclaration)
                    |> Option.map (fun handle ->
                        {
                            File = handle.Path * uom<_>
                            NodeIndex = handle.Index * uom<_>
                        })

                [
                    match alias.ForwardTo with
                    | None ->
                        yield
                            FsStringEnum
                                { template with
                                    Name = alias.Name
                                    Order = order
                                }
                    | Some target ->
                        yield
                            FsAbbrev
                                {
                                    Name = alias.Name
                                    Docs = ""
                                    Tags = []
                                    Order = order
                                    TypeParameters = []
                                    Target = FsNamed model.SourceStringAliases[target].Name
                                    Value = None
                                }
                    match alias.LiteralName with
                    | Some name when name <> originalName -> yield FsStringEnum { template with Name = name }
                    | _ -> ()
                ])
            |> List.distinctBy (function
                | FsStringEnum enum -> enum.Name
                | FsAbbrev alias -> alias.Name
                | _ -> failwith "expected source string alias")

        let retained =
            model.Decls
            |> List.filter (function
                | FsStringEnum enum -> not (Set.contains enum.Name sourceNames)
                | FsAbbrev alias -> not (Set.contains alias.Name sourceNames)
                | _ -> true)

        { model with
            Decls = retained @ declarations
        })
