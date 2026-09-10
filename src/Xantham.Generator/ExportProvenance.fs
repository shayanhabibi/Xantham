/// Value-export reachability through the compiler's direct export tables. The flattened
/// `getExportsOfModule` list follows type-only stars to their original value symbols too.
module internal Xantham.Generator.ExportProvenance

open System.Collections.Generic
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Measure

type private Key =
    | Export of moduleId: int<symbolId> * name: string
    | Symbol of symbolId: int<symbolId>

type private Rule =
    | Present
    | Absent
    | Through of Key list
    | ThroughBoth of Key * Key

type private ModuleExports =
    {
        Direct: Map<string, SymbolResponse>
        All: Map<string, SymbolResponse>
        Stars: SymbolResponse list
    }

let private location (node: Node<'Tag>) =
    NodeHandle.format
        {
            Index = Node.index node
            Kind = node.Kind
            Path = Ast.path (Node.file node)
        }

let rec private ancestor kind (node: Node<AnyNode>) =
    if node.Kind = kind then
        Some node
    else
        Node.parent node |> ValueOption.toOption |> Option.bind (ancestor kind)

let rec private typeOnly (node: Node<AnyNode>) =
    let here =
        match node.Kind with
        | SyntaxKind.ExportDeclaration -> ExportDeclaration.isTypeOnly (Node.retag<AnyNode, ExportDeclaration> node)
        | SyntaxKind.ExportSpecifier -> ExportSpecifier.isTypeOnly (Node.retag<AnyNode, ExportSpecifier> node)
        | SyntaxKind.ImportSpecifier -> ImportSpecifier.isTypeOnly (Node.retag<AnyNode, ImportSpecifier> node)
        | SyntaxKind.ImportClause ->
            ImportClause.phaseModifier (Node.retag<AnyNode, ImportClause> node) = ValueSome SyntaxKind.TypeKeyword
        | SyntaxKind.ImportEqualsDeclaration ->
            ImportEqualsDeclaration.isTypeOnly (Node.retag<AnyNode, ImportEqualsDeclaration> node)
        | _ -> false

    here || (Node.parent node |> ValueOption.exists typeOnly)

/// One module inventory's cache. Symbol ids are
/// session-local graph keys only; the catalog's persistent declaration identity is unchanged.
let reader (ctx: Context) =
    let symbols = Dictionary<int<symbolId>, SymbolResponse>()
    let modules = Dictionary<int<symbolId>, ModuleExports>()
    let files = Dictionary<string<declFile>, Ast.SourceFile voption>()
    let rules = Dictionary<Key, Rule>()
    let present = HashSet<Key>()

    let remember (symbol: SymbolResponse) =
        symbols[symbol.SymbolId] <- symbol
        symbol

    let node handle =
        async {
            match NodeHandle.parse handle with
            | ValueNone -> return None
            | ValueSome handle ->
                let! source =
                    async {
                        match files.TryGetValue (String.tag<declFile> handle.Path) with
                        | true, source -> return source
                        | _ ->
                            let! source = ctx.Session.getSourceFile (DocumentIdentifier.FileName handle.Path)
                            files[String.tag<declFile> handle.Path] <- source
                            return source
                    }

                return
                    source
                    |> ValueOption.toOption
                    |> Option.map (fun file -> Node.ofIndex<AnyNode> file handle.Index)
        }

    let symbolAt (node: Node<'Tag>) =
        async {
            let! symbol = ctx.Session.getSymbolAtLocation (location node)
            return symbol |> ValueOption.toOption |> Option.map remember
        }

    let origin (symbol: SymbolResponse) =
        async {
            if symbol.Flags.HasFlag SymbolFlags.Alias then
                let! target = ctx.Session.getAliasedSymbol symbol.Id
                return remember target
            else
                return symbol
        }

    let moduleExports moduleId =
        async {
            match modules.TryGetValue moduleId with
            | true, exports -> return exports
            | _ ->
                let! direct = ctx.Session.getExportsOfSymbol (Measure.Int.untag moduleId)
                let! all = ctx.Session.getExportsOfModule (Measure.Int.untag moduleId)

                let inventory (values: SymbolResponse[] voption) =
                    values
                    |> ValueOption.defaultValue [||]
                    |> Array.map (fun symbol -> symbol.Name, remember symbol)
                    |> Map.ofArray

                let direct = inventory direct

                let! stars =
                    direct
                    |> Map.toList
                    |> List.filter (fun (_, symbol) -> symbol.Flags.HasFlag SymbolFlags.ExportStar)
                    |> List.collect (fun (_, symbol) ->
                        symbol.Declarations |> ValueOption.defaultValue [||] |> Array.toList)
                    |> List.map (fun handle ->
                        async {
                            match! node handle with
                            | Some node when node.Kind = SyntaxKind.ExportDeclaration && not (typeOnly node) ->
                                match
                                    ExportDeclaration.moduleSpecifier (Node.retag<AnyNode, ExportDeclaration> node)
                                with
                                | ValueSome specifier -> return! symbolAt specifier
                                | ValueNone -> return None
                            | _ -> return None
                        })
                    |> Async.Sequential

                let exports =
                    {
                        Direct = direct
                        All = inventory all
                        Stars = stars |> Array.choose id |> Array.toList
                    }

                modules[moduleId] <- exports
                return exports
        }

    let immediate (symbol: SymbolResponse) =
        async {
            let! target = ctx.Session.getImmediateAliasedSymbol symbol.Id

            return
                match target with
                | ValueSome target -> Through [ Symbol(Measure.Int.tag<symbolId> (remember target).Id) ]
                | ValueNone -> Absent
        }

    let imported moduleNode name fallback =
        async {
            match! symbolAt moduleNode with
            | Some target -> return Through [ Export(target.SymbolId, name) ]
            | None -> return! immediate fallback
        }

    let aliasRule (symbol: SymbolResponse) (declaration: Node<AnyNode>) =
        async {
            if typeOnly declaration then
                return Absent
            else
                match declaration.Kind with
                | SyntaxKind.NamespaceExport
                | SyntaxKind.NamespaceImport -> return Present
                | SyntaxKind.ExportSpecifier ->
                    let specifier = Node.retag<AnyNode, ExportSpecifier> declaration

                    let name =
                        ExportSpecifier.propertyName specifier
                        |> ValueOption.orElse (ExportSpecifier.name specifier)

                    let moduleNode =
                        ancestor SyntaxKind.ExportDeclaration declaration
                        |> Option.bind (fun node ->
                            ExportDeclaration.moduleSpecifier (Node.retag<AnyNode, ExportDeclaration> node)
                            |> ValueOption.toOption)

                    match moduleNode, name with
                    | Some moduleNode, ValueSome name ->
                        return! imported moduleNode (name.Text |> ValueOption.defaultValue symbol.Name) symbol
                    | _ ->
                        let! target = ctx.Session.getExportSpecifierLocalTargetSymbol (location declaration)

                        match target with
                        | ValueSome target -> return Through [ Symbol(Measure.Int.tag<symbolId> (remember target).Id) ]
                        | ValueNone -> return! immediate symbol
                | SyntaxKind.ImportSpecifier
                | SyntaxKind.ImportClause ->
                    let name =
                        if declaration.Kind = SyntaxKind.ImportClause then
                            "default"
                        else
                            let specifier = Node.retag<AnyNode, ImportSpecifier> declaration

                            ImportSpecifier.propertyName specifier
                            |> ValueOption.bind _.Text
                            |> ValueOption.orElse (ImportSpecifier.name specifier |> ValueOption.bind _.Text)
                            |> ValueOption.defaultValue symbol.Name

                    match ancestor SyntaxKind.ImportDeclaration declaration with
                    | Some parent ->
                        match ImportDeclaration.moduleSpecifier (Node.retag<AnyNode, ImportDeclaration> parent) with
                        | ValueSome moduleNode -> return! imported moduleNode name symbol
                        | ValueNone -> return! immediate symbol
                    | None -> return! immediate symbol
                | _ -> return! immediate symbol
        }

    let rule key =
        async {
            match rules.TryGetValue key with
            | true, rule -> return rule
            | _ ->
                let! result =
                    async {
                        match key with
                        | Symbol symbolId ->
                            let symbol = symbols[symbolId]

                            if symbol.Flags.HasFlag SymbolFlags.Alias then
                                let declarations = symbol.Declarations |> ValueOption.defaultValue [||]
                                let! declarations = declarations |> Array.map node |> Async.Sequential

                                match declarations |> Array.choose id |> Array.tryHead with
                                | Some declaration -> return! aliasRule symbol declaration
                                | None -> return! immediate symbol
                            else
                                return
                                    if uint32 (symbol.Flags &&& SymbolFlags.Value) <> 0u then
                                        Present
                                    else
                                        Absent
                        | Export(moduleId, name) ->
                            let! exports = moduleExports moduleId

                            match Map.tryFind name exports.Direct with
                            | Some symbol -> return Through [ Symbol symbol.SymbolId ]
                            | None ->
                                match Map.tryFind "export=" exports.Direct with
                                | Some assignment ->
                                    let! target = origin assignment

                                    if name = "default" then
                                        return Through [ Symbol assignment.SymbolId ]
                                    else
                                        return ThroughBoth(Symbol assignment.SymbolId, Export(target.SymbolId, name))
                                | None ->
                                    match Map.tryFind name exports.All with
                                    | None -> return Absent
                                    | Some expected ->
                                        let! expected = origin expected

                                        let! paths =
                                            exports.Stars
                                            |> List.map (fun target ->
                                                async {
                                                    let! candidates = moduleExports target.SymbolId

                                                    match Map.tryFind name candidates.All with
                                                    | Some candidate ->
                                                        let! candidate = origin candidate

                                                        return
                                                            if candidate.Id = expected.Id then
                                                                Some(Export(target.SymbolId, name))
                                                            else
                                                                None
                                                    | None -> return None
                                                })
                                            |> Async.Sequential

                                        return Through(paths |> Array.choose id |> Array.toList)
                    }

                rules[key] <- result
                return result
        }

    let rec reaches seen key =
        async {
            if present.Contains key then
                return true
            elif Set.contains key seen then
                return false
            else
                let seen = Set.add key seen
                let! rule = rule key

                let! value =
                    match rule with
                    | Present -> async.Return true
                    | Absent -> async.Return false
                    | ThroughBoth(first, second) ->
                        async {
                            let! first = reaches seen first
                            if first then return! reaches seen second else return false
                        }
                    | Through keys ->
                        let rec any =
                            function
                            | [] -> async.Return false
                            | key :: rest ->
                                async {
                                    let! value = reaches seen key
                                    if value then return true else return! any rest
                                }

                        any keys
                // A cycle can make a path temporarily false. Cache only established paths
                // to actual value declarations, never a cycle-dependent negative result.
                if value then
                    present.Add key |> ignore

                return value
        }

    fun (moduleSymbol: SymbolResponse) name ->
        remember moduleSymbol |> ignore
        reaches Set.empty (Export(moduleSymbol.SymbolId, name))
