module Xantham.Fable.Reading.ModulesAndExports

open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Fable.Core
open Fable.Core.JsInterop
open Xantham.Fable.Types.Signal

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: ModulesAndExports) =
    match tag with
    | ModulesAndExports.ImportDeclaration mportDeclaration -> ()
    | ModulesAndExports.ImportClause mportClause -> ()
    | ModulesAndExports.NamespaceImport namespaceImport -> ()
    | ModulesAndExports.NamedImports namedImports -> ()
    | ModulesAndExports.ImportSpecifier mportSpecifier ->
        match
            ctx.checker.getSymbolAtLocation(mportSpecifier.name)
            |> Option.map ctx.checker.getAliasedSymbol
        with
        | Some symbol ->
            let declarations =
                symbol.declarations
                |> Option.map (
                    _.AsArray
                    >> Array.map ( ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx (fun tag ->
                        if GuardedData.Source.Keyed.has xanTag then
                            tag
                            |> GuardedData.Source.Keyed.getOrSetWith(fun _ -> Signal.source <| ModuleName "")
                            |> Signal.fulfillWith(fun _ -> GuardedData.Source.Keyed.get xanTag |> _.Value)
                        tag))
                    )
                |> Option.defaultValue [||]
            match declarations with
            | [||] -> ()
            | _ -> 
                let decl = declarations[0]
                xanTag.Builder
                |> Signal.fulfillWith (fun () -> decl.Builder.Value)
                xanTag.ExportBuilder
                |> Signal.fulfillWith (fun () -> decl.ExportBuilder.Value)
        | None -> Log.error "failed to get symbol"
    | ModulesAndExports.ImportEqualsDeclaration mportEqualsDeclaration ->
        match
            ctx.checker.getSymbolAtLocation !!mportEqualsDeclaration.moduleReference
            |> Option.map (fun symbol ->
                if symbol.flags.HasFlag Ts.SymbolFlags.Alias
                then ctx.checker.getAliasedSymbol symbol
                else symbol)
        with
        | Some symbol ->
            let declarations =
                symbol.declarations
                |> Option.map (
                    _.AsArray
                    >> Array.map ( ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx (fun tag ->
                        if GuardedData.Source.Keyed.has xanTag then
                            GuardedData.Source.Keyed.getOrSetWith(fun _ -> Signal.source <| ModuleName "") tag
                            |> Signal.fulfillWith(fun _ -> GuardedData.Source.Keyed.get xanTag |> _.Value)
                        tag))
                    )
                |> Option.defaultValue [||]
            match declarations with
            | [||] -> ()
            | _ -> 
                let decl = declarations[0]
                xanTag.Builder
                |> Signal.fulfillWith (fun () -> decl.Builder.Value)
                xanTag.ExportBuilder
                |> Signal.fulfillWith (fun () -> decl.ExportBuilder.Value)
        | None -> Log.error "failed to get symbol"
    | ModulesAndExports.AssertClause assertClause -> ()
    | ModulesAndExports.ExportAssignment exportAssignment -> ()
    | ModulesAndExports.ExportDeclaration exportDeclaration -> ()
    | ModulesAndExports.ExportSpecifier exportSpecifier ->
        let source =
            xanTag
            |> GuardedData.Source.getOrSetWith (fun () -> Signal.source <| ctx.moduleMap.Item(exportSpecifier.getSourceFile()))
            |> if GuardedData.Source.Keyed.has xanTag then
                fun signal ->
                    signal
                    |> Signal.fulfillWith(fun () -> (GuardedData.Source.Keyed.get xanTag).Value)
                    signal
               else id
        
        match ctx.checker.getExportSpecifierLocalTargetSymbol (!^ exportSpecifier) with
        | Some symbol ->
            let declarations =
                symbol.declarations
                |> Option.map (
                    _.AsArray
                    >> Array.map ( ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx (fun tag ->
                        if GuardedData.Source.Keyed.has xanTag then
                            GuardedData.Source.Keyed.getOrSetWith(fun _ -> Signal.source <| ModuleName "") tag
                            |> Signal.fulfillWith(fun _ -> GuardedData.Source.Keyed.get xanTag |> _.Value)
                        else ctx.routeSourceTo tag source
                        tag ))
                    )
                |> Option.defaultValue [||]
            match declarations with
            | [||] -> ()
            | _ -> 
                let decl = declarations[0]
                xanTag.TypeSignal
                |> Signal.fulfillWith (fun () -> decl.TypeSignal.Value)
                xanTag.Builder
                |> Signal.fulfillWith (fun () -> decl.Builder.Value)
        | None -> Log.error "failed to get symbol"
    | ModulesAndExports.NamedExports namedExports -> ()
    | ModulesAndExports.NamespaceExport namespaceExportDeclaration -> ()