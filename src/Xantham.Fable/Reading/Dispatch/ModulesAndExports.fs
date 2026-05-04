module Xantham.Fable.Reading.ModulesAndExports

open TypeScript
open Xantham.Fable
open Xantham.Fable.Types
open Fable.Core
open Fable.Core.JsInterop
open Xantham.Fable.Types.Signal

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (tag: ModulesAndExports) =
    let debugLocation moduleAndExportName =
        XanthamTag.debugLocationAndForget $"ModulesAndExports.dispatch | %s{moduleAndExportName}" xanTag
    match tag with
    | ModulesAndExports.ImportDeclaration _ -> nameof ModulesAndExports.ImportDeclaration |> debugLocation
    | ModulesAndExports.ImportClause _ -> nameof ModulesAndExports.ImportClause |> debugLocation
    | ModulesAndExports.NamespaceImport _ -> nameof ModulesAndExports.NamespaceImport |> debugLocation
    | ModulesAndExports.NamedImports _ -> nameof ModulesAndExports.NamedImports |> debugLocation
    | ModulesAndExports.ImportSpecifier mportSpecifier ->
        nameof ModulesAndExports.ImportSpecifier |> debugLocation
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
        nameof ModulesAndExports.ImportEqualsDeclaration |> debugLocation
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
    | ModulesAndExports.AssertClause _ -> nameof ModulesAndExports.AssertClause |> debugLocation
    | ModulesAndExports.ExportAssignment _ -> nameof ModulesAndExports.ExportAssignment |> debugLocation
    // TODO: Export declaration has to be reviewed, temporary patch wires the declarations in most common
    //       path and renames them appropriately
    | ModulesAndExports.ExportDeclaration exportDeclaration ->
        nameof ModulesAndExports.ExportDeclaration |> debugLocation
        let exportClause = exportDeclaration.exportClause
        match exportClause with
        | Some exportClause ->
            match exportClause with
            | Patterns.Node.NamedExportBindingsPatterns.NamedExports namedExports ->
                let namedTags =
                    namedExports.elements
                    |> _.AsArray
                    |> Array.map (ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx (XanthamTag.chainDebug xanTag))
                namedTags
                |> Array.tryHead
                |> Option.iter (fun tag ->
                    GuardedData.AstNodeBuilder.getOrSetDefault xanTag
                    |> Signal.fulfillWith(fun () -> tag.Builder.Value)
                    GuardedData.TypeSignal.getOrSetDefault xanTag
                    |> Signal.fulfillWith(fun () -> tag.TypeSignal.Value)
                    GuardedData.ExportBuilder.getOrSetDefault xanTag
                    |> Signal.fulfillWith(fun () -> tag.ExportBuilder.Value)
                    )
            | Patterns.Node.NamedExportBindingsPatterns.NamespaceExport namespaceExport ->
                match namespaceExport.name with
                | Patterns.Node.ModuleNamePatterns.Identifier name ->
                    ctx.checker.getSymbolAtLocation name
                    |> Option.map (fun symbol ->
                        if symbol.flags.HasFlag Ts.SymbolFlags.Alias
                        then ctx.checker.getAliasedSymbol symbol
                        else symbol)
                    |> function
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
                    | _ -> ()
        | _ -> ()
                
    | ModulesAndExports.ExportSpecifier exportSpecifier ->
        nameof ModulesAndExports.ExportSpecifier |> debugLocation
        
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
                |> Signal.fulfillWith (fun () ->
                    if exportSpecifier.propertyName.IsSome then
                        let name = NameHelpers.getName exportSpecifier.name
                        decl.Builder.Value
                        |> ValueOption.map (function
                            | SType.Class sclass ->
                                sclass.FullyQualifiedName[sclass.FullyQualifiedName.Length - 1] <- name
                                { sclass with Name = name }
                                |> SType.Class
                            | SType.Interface sInterfaceBuilder ->
                                sInterfaceBuilder.FullyQualifiedName[sInterfaceBuilder.FullyQualifiedName.Length - 1] <- name
                                { sInterfaceBuilder with Name = name }
                                |> SType.Interface
                            | SType.Enum sEnumTypeBuilder ->
                                sEnumTypeBuilder.FullyQualifiedName[sEnumTypeBuilder.FullyQualifiedName.Length - 1] <- name
                                { sEnumTypeBuilder with Name = name }
                                |> SType.Enum
                            | SType.EnumCase sEnumCaseBuilder ->
                                sEnumCaseBuilder.FullyQualifiedName[sEnumCaseBuilder.FullyQualifiedName.Length - 1] <- name
                                { sEnumCaseBuilder with Name = name }
                                |> SType.EnumCase
                            | s -> s
                            )
                    else
                    decl.Builder.Value
                    )
                xanTag.ExportBuilder
                |> Signal.fulfillWith (fun () ->
                    if exportSpecifier.propertyName.IsSome then
                        let name = NameHelpers.getName exportSpecifier.name
                        decl.ExportBuilder.Value
                        |> ValueOption.map (function
                            | STsExportDeclaration.Variable var ->
                                var.FullyQualifiedName[var.FullyQualifiedName.Length - 1] <- name
                                { var with Name = name }
                                |> STsExportDeclaration.Variable
                            | STsExportDeclaration.Interface sInterfaceBuilder ->
                                sInterfaceBuilder.FullyQualifiedName[sInterfaceBuilder.FullyQualifiedName.Length - 1] <- name
                                { sInterfaceBuilder with Name = name }
                                |> STsExportDeclaration.Interface
                            | STsExportDeclaration.Class sClassBuilder ->
                                sClassBuilder.FullyQualifiedName[sClassBuilder.FullyQualifiedName.Length - 1] <- name
                                { sClassBuilder with Name = name }
                                |> STsExportDeclaration.Class
                            | STsExportDeclaration.Enum sEnumTypeBuilder ->
                                sEnumTypeBuilder.FullyQualifiedName[sEnumTypeBuilder.FullyQualifiedName.Length - 1] <- name
                                { sEnumTypeBuilder with Name = name }
                                |> STsExportDeclaration.Enum
                            | STsExportDeclaration.Function sFunctionBuilder ->
                                sFunctionBuilder.FullyQualifiedName[sFunctionBuilder.FullyQualifiedName.Length - 1] <- name
                                { sFunctionBuilder with Name = name }
                                |> STsExportDeclaration.Function
                            | STsExportDeclaration.TypeAlias sAliasBuilder ->
                                sAliasBuilder.FullyQualifiedName[sAliasBuilder.FullyQualifiedName.Length - 1] <- name
                                { sAliasBuilder with Name = name }
                                |> STsExportDeclaration.TypeAlias
                            | STsExportDeclaration.Module sModuleBuilder ->
                                sModuleBuilder.FullyQualifiedName[sModuleBuilder.FullyQualifiedName.Length - 1] <- name
                                { sModuleBuilder with Name = name }
                                |> STsExportDeclaration.Module)
                    else
                    decl.ExportBuilder.Value
                    )
        | None -> Log.error "failed to get symbol"
    | ModulesAndExports.NamedExports _ -> nameof ModulesAndExports.NamedExports |> debugLocation
    | ModulesAndExports.NamespaceExport _ -> nameof ModulesAndExports.NamespaceExport |> debugLocation