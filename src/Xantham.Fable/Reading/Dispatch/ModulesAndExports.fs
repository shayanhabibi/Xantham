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
    | ModulesAndExports.ImportSpecifier mportSpecifier -> ()
    | ModulesAndExports.ImportEqualsDeclaration mportEqualsDeclaration -> ()
    | ModulesAndExports.AssertClause assertClause -> ()
    | ModulesAndExports.ExportAssignment exportAssignment -> ()
    | ModulesAndExports.ExportDeclaration exportDeclaration -> ()
    | ModulesAndExports.ExportSpecifier exportSpecifier ->
        match ctx.checker.getExportSpecifierLocalTargetSymbol (!^ exportSpecifier) with
        | Some symbol ->
            let declarations =
                symbol.declarations
                |> Option.map (
                    _.AsArray
                    >> Array.map (
                        ctx.CreateXanthamTag >> fst >> function
                            | TagState.Unvisited tag -> pushToStack ctx tag; tag
                            | TagState.Visited tag -> tag
                        )
                    )
                |> Option.defaultValue [||]
            match declarations with
            | [||] -> ()
            | _ -> ()
                // let decl = declarations[0]
                // GuardedData.TypeSignal.getOrSetDefault xanTag
                // |> Signal.fulfillWith (fun () -> (GuardedData.TypeSignal.getOrSetDefault decl).Value)
                // GuardedData.AstNodeBuilder.getOrSetDefault xanTag
                // |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault decl).Value)
        | None -> Log.error "failed to get symbol"
    | ModulesAndExports.NamedExports namedExports -> ()
    | ModulesAndExports.NamespaceExport namespaceExportDeclaration -> ()