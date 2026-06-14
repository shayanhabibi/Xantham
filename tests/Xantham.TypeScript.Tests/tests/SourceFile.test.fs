module SourceFile


open System
open Xantham.TypeScript.Types.Node
open Xantham.TypeScript.Types.Symbol
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha


let inline tests (runner: Spec.RunnerContext) : unit =
    // ----------------------------------------------------------------------------------------------
    //                                  SF | SOURCE FILES
    // ----------------------------------------------------------------------------------------------
    runner.testSuite "SF - Source File Model" <| fun _ ->
        // SF-1 — corpus is non-empty, so every proof below quantifies over real declarations.
        runner.testCase "SF-1 · program contains source files" <| fun _ ctx ->
            ctx.SourceFiles
            |> Flip.Expect.isNotEmpty "Program should not be empty"
        // SF-2 — a source file has a checker symbol iff it is an external module.
        //        Backs `ExternalModule.create`'s sourceSymbol guard (Option.defaultWith failwith).
        runner.testCase "SF-2 · external module iff source symbol" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.iter (fun sf ->
                match ctx.Checker.getSymbolAtLocation(sf) with
                | value when ts.isExternalModule sf -> Expect.isSome value $"External modules should have symbols, but {sf.fileName} did not have a symbol"
                | value -> Expect.isNone value $"Non external modules should not have symbols, but {sf.fileName} did"
                )
        // SF-3 — external-module symbols resolve and expose >=1 module specifier.
        //        Backs `ExternalModule.create`'s moduleSpecifierInvariant (NonEmptyArray).
        runner.testCase "SF-3 · external symbols resolve to module specifiers" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter ts.isExternalModule
            |> Array.map ctx.Checker.getSymbolAtLocation
            |> fun symbols ->
                symbols
                |> Chain.Expect.all _.IsSome "All external symbols should be resolved"
                |> Array.choose id
                |> Array.map ctx.Program.GetModuleSpecifier
                |> Flip.Expect.all (_.moduleSpecifiers >> Array.isEmpty >> not) "All external symbols should have a module specifier"
        // SF-4 — script (non-module) source files carry a `locals` map. Backs `Script` Locals.
        runner.testCase "SF-4 · script source has a 'locals' map" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter (not << ts.isExternalModule)
            |> Array.iter (fun sf ->
                sf?locals
                |> Option.ofObj
                |> Flip.Expect.isSome $"Non external modules should have a locals map, but {sf.fileName} did not"
                )
        // SF-5 — script source files have no `exports` map; this is what distinguishes script from module.
        runner.testCase "SF-5 · script source has no 'exports' map" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter (not << ts.isExternalModule)
            |> Array.iter (fun sf ->
                sf?exports
                |> Option.ofObj
                |> Flip.Expect.isNone $"Non external modules should not have an exports map, but {sf.fileName} did"
                )
        // SF-6 — external modules carry an `exports` map. Backs `ExternalModule.create`'s symbolExports guard.
        runner.testCase "SF-6 · external module has an 'exports' map" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter ts.isExternalModule
            // |> Array.filter (_.fileName.Contains("lib.esnext.iterator") >> not)
            |> Array.iter (fun sf ->
                let symbol = ctx.Checker.getSymbolAtLocation(sf)
                Expect.isSome symbol $"External modules should have a symbol, but {sf.fileName} did not"
                symbol.Value.exports
                |> Flip.Expect.isSome $"External modules should have an exports map, but {sf.fileName} did not"
                )
        // SF-7 — external modules also carry a `locals` map. Backs `ExternalModule.create`'s sourceFileLocals.
        runner.testCase "SF-7 · external module has a 'locals' map" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                let symbol = ctx.Checker.getSymbolAtLocation(sf)
                Expect.isSome symbol $"External modules should have a symbol, but {sf.fileName} did not"
                sf?locals
                |> Option.ofObj
                |> Flip.Expect.isSome $"External modules should have a locals map, but {sf.fileName} did not"
                )
        // SF-8 — every external module resolves a `package.json` (its own or a nearest ancestor's).
        //        Backs `Source.create`'s `packageJsonFields.Value` access.
        runner.testCase "SF-8 · external module has a package.json (self or ancestor)" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                sf.packageJsonFields 
                |> Flip.Expect.isSome $"External modules should have a package.json, but {sf.fileName} did not"
                )
        // SF-9 — that resolved `package.json` exposes a `version`. Backs `Source.create`'s version `.Value`.
        runner.testCase "SF-9 · external module is versioned (self or ancestor)" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                let fields = sf.packageJsonFields
                Expect.isSome fields $"External modules should have a package.json but {sf.fileName} did not"
                let fields = if fields.Value.version.IsNone then ctx.Program.GetClosestAncestorPackageJson _.version.IsSome sf.fileName else fields
                Expect.isSome fields.Value.version $"External modules should have a version in their package.json but {sf.fileName} did not: %A{sf.packageJsonFields.Value}"
                )
        // SF-10 — non default-lib script sources also resolve a `package.json`. Backs `Source.create` for scripts.
        runner.testCase "SF-10 · script (non default-lib) has a package.json (self or ancestor)" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter (ts.isExternalModule >> not)
            |> Array.filter (ctx.Program.isSourceFileDefaultLibrary >> not)
            |> Array.iter (fun sf ->
                sf.packageJsonFields
                |> Flip.Expect.isSome $"None-External modules should have a package.json, but {sf.fileName} did not"
                )
        // SF-11 — that `package.json` is both named and versioned.
        //         Backs `Source.create`'s `closestNamedAndVersionedPackageJsonFields.Value`.
        runner.testCase "SF-11 · script (non default-lib) has a named & versioned package.json" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.filter (ts.isExternalModule >> not)
            |> Array.filter (ctx.Program.isSourceFileDefaultLibrary >> not)
            |> Array.iter (fun sf ->
                sf.closestNamedAndVersionedPackageJsonFields
                |> Flip.Expect.isSome $"None-External modules should have a package.json, but {sf.fileName} did not"
                )

let inline wrapperTests (runner: Spec.RunnerContext) =
    // ----------------------------------------------------------------------------------------------
    //                                  XTK - WRAPPER TOTALITY
    // ----------------------------------------------------------------------------------------------
    runner.testSuite "XTK · Wrapper Totality" <| fun _ ->
        // XTK-1 — `Source.create` never throws on real input; this exercises every SF-* guard end to end.
        runner.testCase "XTK-1 · Source.create succeeds for every source file" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.iter (Node.SourceKind.create ctx.Program >> ignore)
        // XTK-2 — every external-module export classifies as a declaration or module export;
        //         `XanTagKind.Create` never returns `Ignore` in this position.
        runner.testCase "XTK-2 · external-module exports are TypeDeclaration or ModulesAndExports" <| fun _ ctx ->
             ctx.SourceFiles
            |> Array.iter (function
                | sf when ts.isExternalModule sf && (sf.fileName.Contains("lib.esnext.iterator") |> not) ->
                    ctx.Checker.getSymbolAtLocation sf
                    |> Option.map (ctx.Checker.getExportsOfModule >> _.AsArray >> Array.choose _.getDeclarations() >> Array.collect _.AsArray)
                    |> Option.get
                    |> Array.iter (
                        XanTagKind.Create
                        >> function
                            | XanTagKind.TypeDeclaration _
                            | XanTagKind.ModulesAndExports _ -> true
                            | _ -> false
                        >> Expect.isTrue
                        >> funApply "Top level statements should be declarations or module exports"
                        )
                | _ -> ()
                    )           
        // XTK-3 — every script top-level statement classifies the same way; `XanTagKind.Create` never `Ignore`s here.
        runner.testCase "XTK-3 · script statements are TypeDeclaration or ModulesAndExports" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.iter (function
                | sf when ts.isExternalModule sf && (sf.fileName.Contains("lib.esnext.iterator") |> not) -> ()
                | sf ->
                    sf.statements.AsArray
                    |> Array.iter (
                        XanTagKind.Create
                        >> function
                            | XanTagKind.TypeDeclaration _
                            | XanTagKind.ModulesAndExports _ -> true
                            | _ -> false
                        >> Expect.isTrue
                        >> funApply "Top level statements should be declarations or module exports"
                        )
                    )
        // XTK-4 — every class & interface member classifies. Backs `MemberDeclaration.Create` / `IsMemberDeclarationKind`.
        runner.testCase "XTK-4 · class & interface members parse as MemberDeclaration" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                _.statements.AsArray
                >> Array.map XanTagKind.Create
                >> Array.choose (function
                    | XanTagKind.TypeDeclaration decl ->
                        if decl.Symbol.IsSome then Some decl else None
                    | _ -> None
                    )
                >> Array.collect (function
                    | TypeDeclaration.Interface iface ->
                        iface.members.AsArray
                        |> Array.map XanTagKind.Create
                    | TypeDeclaration.HeritageClause _
                    | TypeDeclaration.ExpressionWithTypeArguments _
                    | TypeDeclaration.TypeParameter _
                    | TypeDeclaration.TypeAlias _ 
                    | TypeDeclaration.Enum _
                    | TypeDeclaration.EnumMember _
                    | TypeDeclaration.VariableStatement _
                    | TypeDeclaration.VariableDeclaration _
                    | TypeDeclaration.FunctionDeclaration _
                    | TypeDeclaration.Module _
                    | TypeDeclaration.Namespace _
                    | TypeDeclaration.ModuleBlock _ -> [||]
                    | TypeDeclaration.Class classDeclaration ->
                        classDeclaration.members.AsArray
                        |> Array.map XanTagKind.Create
                    )
                )
            |> Array.iter (_.IsMemberDeclaration >> Expect.isTrue >> funApply "Declaration members are parsed as MemberDecls")
        // XTK-5 — every symbol escapedName maps to a `SymbolName` (string or known InternalSymbolName). Backs `SymbolName.Create`.
        runner.testCase "XTK-5 · symbol escapedNames parse as SymbolName" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                _.statements.AsArray
                >> Array.map XanTagKind.Create
                >> Array.choose (function
                    | XanTagKind.TypeDeclaration decl ->
                        decl.Symbol |> ValueOption.toOption
                    | _ -> None
                    )
                )
            |> Array.iter (_.escapedName >> SymbolName.Create >> ignore)
        // XTK-6 — every symbol-table value declaration classifies (asserts no `Ignore`). Backs `XanTagKind.Create`.
        runner.testCase "XTK-6 · symbol-table value declarations classify under XanTagKind" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                Node.SourceKind.create ctx.Program
                >> function
                    | SourceKind.Script script -> Node.Script.getSymbolLocals script |> LocalSymbolTable.toSymbolTable |> _.Values |> Seq.toArray
                    | SourceKind.ExternalModule externalModule ->
                        [
                            Node.ExternalModule.getSymbolLocals externalModule
                            |> LocalSymbolTable.toSymbolTable
                            Node.ExternalModule.getSymbolExports externalModule
                            |> ExportSymbolTable.toSymbolTable
                        ]
                        |> Seq.collect _.Values
                        |> Seq.toArray
                >> Array.choose _.getDeclarations()
                >> Array.collect _.AsArray
                )
            |> Array.iter (XanTagKind.Create >> function
                | XanTagKind.TypeDeclaration _ | XanTagKind.ModulesAndExports _ -> ()
                | XanTagKind.Ignore k -> failtest $"Unexpected symbol declaration XanTagKind: {k.Value.kind.Name}"
                | kind -> failtest $"Unexpected symbol declaration XanTagKind: %s{kind.ToString()}"
                )
        // XTK-7 — every top-level statement kind is recognised. Backs `TopLevelStatements.IsTopLevelStatementKind`.
        runner.testCase "XTK-7 · top-level statements covered by TopLevelStatements" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.iter (
                _.statements.AsArray
                >> Array.iter (fun statement ->
                    TopLevelStatements.IsTopLevelStatementKind statement
                    |> Expect.isTrue
                    |> funApply $"Expected a top level statement kind, but got {statement.kind.Name} instead."
                    )
                )
        // XTK-8 — every exported-symbol declaration kind is recognised. Backs `TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind`.
        runner.testCase "XTK-8 · export-symbol declarations covered by TopLevelExportSymbolDeclarations" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                Node.SourceKind.create ctx.Program
                >> function
                    | SourceKind.ExternalModule externalModule ->
                        let globals = Node.ExternalModule.getSymbolGlobalExports externalModule
                        [
                            Node.ExternalModule.getSymbolExports externalModule
                            |> ExportSymbolTable.toSymbolTable
                            if globals.IsSome then globals.Value |> ExportSymbolTable.toSymbolTable
                        ]
                        |> Seq.collect _.Values
                        |> Seq.toArray
                    | _ -> [||]
                >> Array.choose _.getDeclarations()
                >> Array.collect _.AsArray
                )
            |> Array.iter (fun statement ->
                    TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind statement
                    |> Expect.isTrue
                    |> funApply $"Expected a top level statement kind, but got {statement.kind.Name} instead."
                )
        // XTK-9 — every local-symbol declaration kind is recognised. Backs `TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind`.
        runner.testCase "XTK-9 · local-symbol declarations covered by TopLevelLocalSymbolDeclarations" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                Node.SourceKind.create ctx.Program
                >> function
                    | SourceKind.Script script ->
                        Node.Script.getSymbolLocals script
                        |> LocalSymbolTable.toSymbolTable
                        |> _.Values
                        |> Seq.toArray
                    | SourceKind.ExternalModule externalModule ->
                        Node.ExternalModule.getSymbolLocals externalModule
                        |> LocalSymbolTable.toSymbolTable
                        |> _.Values
                        |> Seq.toArray
                >> Array.choose _.getDeclarations()
                >> Array.collect _.AsArray
                )
            |> Array.iter (fun statement ->
                    TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind statement
                    |> Expect.isTrue
                    |> funApply $"Expected a top level statement kind, but got {statement.kind.Name} instead."
                )
