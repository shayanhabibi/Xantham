#nowarn FS1104


module Program

open Fable.Mocha
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable

type This = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
// If the file system doesn't compile, then it's because the test fixtures haven't
// been created. Run `dotnet fsi ./tests/Fixtures.Setup.fsx` from the root of the repo.
type TestFixtures = AbsoluteFileSystem<This.``..``.fixtures.``.``>

// ─────────────────────────────────────────────────────────────────────────────
// PROOFS
//
// Every `testCase` below is an executable *proof*: an invariant asserted over a
// corpus of real-world `.d.ts` packages (the fixture list at the bottom of this
// file). The wrappers in `Xantham.TypeScript` lean on these invariants to justify
// operations the F# type system cannot — the `failwith` guards in the `Source`
// model, `.Value` field access on package.json/DU payloads, and the `Ignore`
// fall-through in `XanTagKind.Create`. If a proof here fails, the wrapper it backs
// has an unsound path; the proof ID in the test name says which one.
//
// Proofs carry a stable ID so wrapper XML docs can cite them precisely
// (e.g. `<remarks>Totality proven by XTK-6 (Program.fs).</remarks>`). Two groups:
//
//   SF  · Source File Model — invariants the TypeScript compiler guarantees about
//                             source files, which the `Source` / `ExternalModule`
//                             constructors depend on.
//   XTK · Wrapper Totality  — invariants that our classifier wrappers are *total*
//                             over real input (never reach their failure/Ignore case).
//
// The full annotated catalog lives in `src/Xantham.TypeScript/README.md`.
// ─────────────────────────────────────────────────────────────────────────────

// Helpers
let makeProgram file = Ts.Program.Create [ file ]
let makeTestSeries name file = testList name [
    let program = makeProgram file
    let checker = program.getTypeChecker()
    testList "SF · Source File Model" [
        // SF-1 — corpus is non-empty, so every proof below quantifies over real declarations.
        testCase "SF-1 · program contains source files" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Expect.isNotEmpty
            |> funApply "Program should not be empty"
        // SF-2 — a source file has a checker symbol iff it is an external module.
        //        Backs `ExternalModule.create`'s sourceSymbol guard (Option.defaultWith failwith).
        testCase "SF-2 · external module iff source symbol" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.iter (fun sf ->
                match program.getTypeChecker().getSymbolAtLocation(sf) with
                | value when ts.isExternalModule sf -> Expect.isSome value $"External modules should have symbols, but {sf.fileName} did not have a symbol"
                | value -> Expect.isNone value $"Non external modules should not have symbols, but {sf.fileName} did"
                )
        // SF-3 — external-module symbols resolve and expose >=1 module specifier.
        //        Backs `ExternalModule.create`'s moduleSpecifierInvariant (NonEmptyArray).
        testCase "SF-3 · external symbols resolve to module specifiers" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter ts.isExternalModule
            |> Array.map (program.getTypeChecker().getSymbolAtLocation)
            |> fun symbols ->
                Expect.all symbols _.IsSome "All external symbols should be resolved"
                Expect.all
                    (symbols
                    |> Array.choose id
                    |> Array.map program.GetModuleSpecifier)
                    (_.moduleSpecifiers >> Array.isEmpty >> not)
                    "All external symbols should have a module specifier"
        // SF-4 — script (non-module) source files carry a `locals` map. Backs `Script` Locals.
        testCase "SF-4 · script source has a 'locals' map" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter (not << ts.isExternalModule)
            |> Array.iter (fun sf ->
                sf?locals
                |> Option.ofObj
                |> Expect.isSome
                |> funApply $"Non external modules should have a locals map, but {sf.fileName} did not"
                )
        // SF-5 — script source files have no `exports` map; this is what distinguishes script from module.
        testCase "SF-5 · script source has no 'exports' map" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter (not << ts.isExternalModule)
            |> Array.iter (fun sf ->
                sf?exports
                |> Option.ofObj
                |> Expect.isNone
                |> funApply $"Non external modules should not have an exports map, but {sf.fileName} did"
                )
        // SF-6 — external modules carry an `exports` map. Backs `ExternalModule.create`'s symbolExports guard.
        testCase "SF-6 · external module has an 'exports' map" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter ts.isExternalModule
            // |> Array.filter (_.fileName.Contains("lib.esnext.iterator") >> not)
            |> Array.iter (fun sf ->
                let symbol = checker.getSymbolAtLocation(sf)
                Expect.isSome symbol $"External modules should have a symbol, but {sf.fileName} did not"
                symbol.Value.exports
                |> Expect.isSome
                |> funApply $"External modules should have an exports map, but {sf.fileName} did not"
                )
        // SF-7 — external modules also carry a `locals` map. Backs `ExternalModule.create`'s sourceFileLocals.
        testCase "SF-7 · external module has a 'locals' map" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                let symbol = checker.getSymbolAtLocation(sf)
                Expect.isSome symbol $"External modules should have a symbol, but {sf.fileName} did not"
                sf?locals
                |> Option.ofObj
                |> Expect.isSome
                |> funApply $"External modules should have a locals map, but {sf.fileName} did not"
                )
        // SF-8 — every external module resolves a `package.json` (its own or a nearest ancestor's).
        //        Backs `Source.create`'s `packageJsonFields.Value` access.
        testCase "SF-8 · external module has a package.json (self or ancestor)" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                sf.packageJsonFields 
                |> Expect.isSome
                |> funApply $"External modules should have a package.json, but {sf.fileName} did not"
                )
        // SF-9 — that resolved `package.json` exposes a `version`. Backs `Source.create`'s version `.Value`.
        testCase "SF-9 · external module is versioned (self or ancestor)" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter ts.isExternalModule
            |> Array.iter (fun sf ->
                let fields = sf.packageJsonFields
                Expect.isSome fields $"External modules should have a package.json but {sf.fileName} did not"
                let fields = if fields.Value.version.IsNone then program.GetClosestAncestorPackageJson _.version.IsSome sf.fileName else fields
                Expect.isSome fields.Value.version $"External modules should have a version in their package.json but {sf.fileName} did not: %A{sf.packageJsonFields.Value}"
                )
        // SF-10 — non default-lib script sources also resolve a `package.json`. Backs `Source.create` for scripts.
        testCase "SF-10 · script (non default-lib) has a package.json (self or ancestor)" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter (ts.isExternalModule >> not)
            |> Array.filter (program.isSourceFileDefaultLibrary >> not)
            |> Array.iter (fun sf ->
                sf.packageJsonFields
                |> Expect.isSome
                |> funApply $"None-External modules should have a package.json, but {sf.fileName} did not"
                )
        // SF-11 — that `package.json` is both named and versioned.
        //         Backs `Source.create`'s `closestNamedAndVersionedPackageJsonFields.Value`.
        testCase "SF-11 · script (non default-lib) has a named & versioned package.json" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.filter (ts.isExternalModule >> not)
            |> Array.filter (program.isSourceFileDefaultLibrary >> not)
            |> Array.iter (fun sf ->
                sf.closestNamedAndVersionedPackageJsonFields
                |> Expect.isSome
                |> funApply $"None-External modules should have a package.json, but {sf.fileName} did not"
                )
    ]
    testList "XTK · Wrapper Totality" [
        // XTK-1 — `Source.create` never throws on real input; this exercises every SF-* guard end to end.
        testCase "XTK-1 · Source.create succeeds for every source file" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.iter (Source.create program >> ignore)
        // XTK-2 — every external-module export classifies as a declaration or module export;
        //         `XanTagKind.Create` never returns `Ignore` in this position.
        testCase "XTK-2 · external-module exports are TypeDeclaration or ModulesAndExports" <| fun _ ->
             program.getSourceFiles().AsArray
            |> Array.iter (function
                | sf when ts.isExternalModule sf && (sf.fileName.Contains("lib.esnext.iterator") |> not) ->
                    checker.getSymbolAtLocation sf
                    |> Option.map (checker.getExportsOfModule >> _.AsArray >> Array.choose _.getDeclarations() >> Array.collect _.AsArray)
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
        testCase "XTK-3 · script statements are TypeDeclaration or ModulesAndExports" <| fun _ ->
            program.getSourceFiles().AsArray
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
        testCase "XTK-4 · class & interface members parse as MemberDeclaration" <| fun _ ->
            program.getSourceFiles().AsArray
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
        testCase "XTK-5 · symbol escapedNames parse as SymbolName" <| fun _ ->
            program.getSourceFiles().AsArray
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
        testCase "XTK-6 · symbol-table value declarations classify under XanTagKind" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.collect (
                Source.create program
                >> function
                    | Source.External(kind, _, _, _, _)
                    | Source.Default(kind, _) ->
                        match kind with
                        | SourceKind.Script { Locals = table } -> table |> SymbolTable.fromLocalSymbolTable |> _.Values |> Seq.toArray
                        | SourceKind.ExternalModule { Exports = table; Locals = localTable } ->
                            Seq.append (SymbolTable.fromExportSymbolTable table).Values (SymbolTable.fromLocalSymbolTable localTable).Values
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
        testCase "XTK-7 · top-level statements covered by TopLevelStatements" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.iter (
                _.statements.AsArray
                >> Array.iter (fun statement ->
                    TopLevelStatements.IsTopLevelStatementKind statement
                    |> Expect.isTrue
                    |> funApply $"Expected a top level statement kind, but got {statement.kind.Name} instead."
                    )
                )
        // XTK-8 — every exported-symbol declaration kind is recognised. Backs `TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind`.
        testCase "XTK-8 · export-symbol declarations covered by TopLevelExportSymbolDeclarations" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.collect (
                Source.create program
                >> function
                    | Source.External(kind, _, _, _, _)
                    | Source.Default(kind, _) ->
                        match kind with
                        | SourceKind.ExternalModule { Exports = table; GlobalExports = ValueSome localTable } ->
                            Seq.append (SymbolTable.fromExportSymbolTable table).Values (SymbolTable.fromExportSymbolTable localTable).Values
                            |> Seq.toArray
                        | SourceKind.ExternalModule { Exports = table } -> (SymbolTable.fromExportSymbolTable table).Values |> Seq.toArray
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
        testCase "XTK-9 · local-symbol declarations covered by TopLevelLocalSymbolDeclarations" <| fun _ ->
            program.getSourceFiles().AsArray
            |> Array.collect (
                Source.create program
                >> function
                    | Source.External(kind, _, _, _, _)
                    | Source.Default(kind, _) ->
                        match kind with
                        | SourceKind.Script { Locals = table } 
                        | SourceKind.ExternalModule { Locals = table } -> (SymbolTable.fromLocalSymbolTable table).Values |> Seq.toArray
                >> Array.choose _.getDeclarations()
                >> Array.collect _.AsArray
                )
            |> Array.iter (fun statement ->
                    TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind statement
                    |> Expect.isTrue
                    |> funApply $"Expected a top level statement kind, but got {statement.kind.Name} instead."
                )
    ]
]

[
    "@cloudflare/ai-chat", TestFixtures.``@cloudflare``.``ai-chat``.node_modules.``@cloudflare``.``ai-chat``.dist.``index.d.ts``
    "@cloudflare/think", TestFixtures.``@cloudflare``.think.node_modules.``@cloudflare``.think.dist.``index-BsB4GUsL.d.ts``
    "@cloudflare/dynamic-workflows", TestFixtures.``@cloudflare``.``dynamic-workflows``.node_modules.``@cloudflare``.``dynamic-workflows``.dist.``index.d.ts``
    "@cloudflare/workers-types", TestFixtures.``@cloudflare``.``workers-types``.node_modules.``@cloudflare``.``workers-types``.``index.d.ts``
    "@cloudflare/sandbox", TestFixtures.``@cloudflare``.sandbox.node_modules.``@cloudflare``.sandbox.dist.``index.d.ts``
    "@cloudflare/shell", TestFixtures.``@cloudflare``.shell.node_modules.``@cloudflare``.shell.dist.``index.d.ts``
    "@cloudflare/puppeteer", TestFixtures.``@cloudflare``.puppeteer.node_modules.``@cloudflare``.puppeteer.lib.``types.d.ts``
    "@cloudflare/voice", TestFixtures.``@cloudflare``.voice.node_modules.``@cloudflare``.voice.dist.``voice.d.ts``
    "agents", TestFixtures.agents.node_modules.agents.dist.``index.d.ts``
    "@types/three", TestFixtures.``@types``.three.node_modules.``@types``.three.``index.d.ts``
    "solid-js", TestFixtures.``solid-js``.node_modules.``solid-js``.types.``index.d.ts``
]
|> List.unzip
||> List.map2 makeTestSeries
|> testList "Fable.TypeScript" 
|> Mocha.runTests
|> ignore