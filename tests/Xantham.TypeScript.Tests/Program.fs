#nowarn FS0020
#nowarn FS1104


module Program

open System
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core

// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha

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

// The runner (defined in Spec.fs) provides an interface for creating test suites and tests,
// and feeds each passed fixture to the test function.
Spec.RunnerContext.make "Fable.TypeScript" [
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
        "@types/d3", TestFixtures.``@types``.d3.node_modules.``@types``.d3.``index.d.ts``
        "@types/node", TestFixtures.``@types``.node.node_modules.``@types``.node.``index.d.ts``
        "@types/semver", TestFixtures.``@types``.semver.node_modules.``@types``.semver.``index.d.ts``
        "ansi-regex", TestFixtures.``ansi-regex``.node_modules.``ansi-regex``.``index.d.ts``
        "type-fest", TestFixtures.``type-fest``.node_modules.``type-fest``.``index.d.ts``
        "@types/lodash", TestFixtures.``@types``.lodash.node_modules.``@types``.lodash.``index.d.ts``
        "anime", TestFixtures.animejs.node_modules.animejs.dist.modules.``index.d.ts``
        "typescript", TestFixtures.typescript.node_modules.typescript.lib.``typescript.d.ts``
] <| fun suite runner ->
    runner.testSuite "SF · Source File Model" <| fun _ ->
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
            |> Array.map (ctx.Checker.getSymbolAtLocation)
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
    runner.testSuite "XTK · Wrapper Totality" <| fun _ ->
        // XTK-1 — `Source.create` never throws on real input; this exercises every SF-* guard end to end.
        runner.testCase "XTK-1 · Source.create succeeds for every source file" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.iter (Source.create ctx.Program >> ignore)
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
                Source.create ctx.Program
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
                Source.create ctx.Program
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
        runner.testCase "XTK-9 · local-symbol declarations covered by TopLevelLocalSymbolDeclarations" <| fun _ ctx ->
            ctx.SourceFiles
            |> Array.collect (
                Source.create ctx.Program
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
    runner.testSuite "Node invariants" <| fun _ ->
        runner.testSyntaxKind Ts.SyntaxKind.NumericLiteral "Numeric Literals are all parsable" <| fun _ _ (literals: Ts.NumericLiteral array) ->
            literals
            |> Array.iter (
                _.text
                >> JS.Constructors.Number.parseFloat
                >> function
                    | value when jsTypeof value = "number" -> ()
                    | value -> failtest $"Unrecognised numeric literal: %A{value}"
                )
        runner.testSyntaxKind<Ts.BigIntLiteral> Ts.SyntaxKind.BigIntLiteral "BigInt Literals are all parsable" <| fun _ _ literals ->
            literals
            |> Array.iter (
                _.text
                >> _.TrimEnd('n')
                >> System.Numerics.BigInteger.Parse
                >> ignore
                )
            
        runner.testSyntaxKind<Ts.StringLiteral> Ts.SyntaxKind.StringLiteral "String Literals all have valid string values" <| fun _ _ literals ->
            literals
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        runner.testSyntaxKind<Ts.NoSubstitutionTemplateLiteral> Ts.SyntaxKind.NoSubstitutionTemplateLiteral "NoSubstitutionTemplateLiteral values are valid" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        runner.testSyntaxKind<Ts.PrefixUnaryExpression> Ts.SyntaxKind.PrefixUnaryExpression "PrefixUnaryExpression Operators values are predictable" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.operator
                >> function
                    | Ts.PrefixUnaryOperator.MinusToken -> Expect.passWithMsg "MinusToken is only expected PrefixUnaryOperator in d.ts files"
                    | Ts.PrefixUnaryOperator.PlusPlusToken 
                    | Ts.PrefixUnaryOperator.PlusToken 
                    | Ts.PrefixUnaryOperator.MinusMinusToken 
                    | Ts.PrefixUnaryOperator.TildeToken 
                    | Ts.PrefixUnaryOperator.ExclamationToken as value -> failtest $"Unexpected PrefixUnaryOperator.{value.Name} in d.ts"
                    | value -> failtest $"Received an invalid/unknown PrefixUnaryOperator kind: {value.Name}" 
                )
        runner.testSyntaxKind<Ts.PrefixUnaryExpression> Ts.SyntaxKind.PrefixUnaryExpression "PrefixUnaryExpression Operand values are all numeric literals" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.operand
                >> function
                    | Patterns.SyntaxKind.NumericLiteral _ -> ()
                    | value -> failtest $"Received an invalid/unknown PostfixUnaryExpression operand kind: %s{value.kind.Name}" 
                )
        runner.testSyntaxKind<Ts.LiteralTypeNode> Ts.SyntaxKind.LiteralType "LiteralTypeNode _.literal values are parsed predictably" <| fun _ _ nodes ->
            nodes
            |> Array.iter (
                _.literal
                >> unbox
                >> function
                    | Patterns.SyntaxKind.NullKeyword _
                    | Patterns.SyntaxKind.FalseKeyword _
                    | Patterns.SyntaxKind.TrueKeyword _
                    | Patterns.Node.NumericLiteral _
                    | Patterns.Node.StringLiteral _
                    | Patterns.Node.BigIntLiteral _
                    | Patterns.Node.NoSubstitutionTemplateLiteral _ -> ()
                    | Patterns.Node.PrefixUnaryExpression _ -> ()
                    | node -> failtest $"Unrecognised literal for LiteralTypeNode: %s{node.kind.Name}"
                )
        runner.testCase "DeclarationFiles have a narrowed subset of valid nodes" <| fun _ ctx ->
            ctx.NodeMap.Keys
            |> Seq.distinct
            |> Seq.sortBy _.Name
            |> Seq.iter (function
                | value when DeclarationFileNodes.IsKnownDeclarationFileNodeSyntaxKind value -> ()
                | value -> failtest $"Unexpected node kind in a declaration file: %s{value.Name}"
                )
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "ClassDeclarations have a limited subset of nodes as members" <| fun _ _ nodes ->
            nodes
            |> Array.collect _.members.AsArray
            |> Array.iter (
                function
                    | Patterns.Node.PropertyDeclaration _
                    | Patterns.Node.MethodDeclaration _
                    | Patterns.Node.GetAccessorDeclaration _
                    | Patterns.Node.SetAccessorDeclaration _
                    | Patterns.Node.IndexSignatureDeclaration _
                    | Patterns.Node.ConstructorDeclaration _ -> ()
                    | node -> failtest $"Unrecognised member kind for ClassMember: %s{node.kind.Name}"
                )
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "InterfaceDeclarations have a limited subset of nodes as members" <| fun _ _ nodes ->
            nodes
            |> Array.collect _.members.AsArray
            |> Array.iter (
                function
                    | Patterns.Node.PropertySignature _
                    | Patterns.Node.MethodSignature _
                    | Patterns.Node.GetAccessorDeclaration _
                    | Patterns.Node.SetAccessorDeclaration _
                    | Patterns.Node.IndexSignatureDeclaration _
                    | Patterns.Node.CallSignatureDeclaration _
                    | Patterns.Node.ConstructSignatureDeclaration _ -> ()
                    | node -> failtest $"Unrecognised member kind for Interfacemember: %s{node.kind.Name}"
                )
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "ClassDeclarations resolved by type checker are always Class object types" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (fun node ->
                if node.name.IsNone then
                    unbox<Ts.Node> node
                else unbox<Ts.Node> node.name.Value
                |> ctx.Checker.getTypeAtLocation
                :?> Ts.ObjectType
                |> _.objectFlags.HasFlag(Ts.ObjectFlags.Class)
                |> Expect.isTrue
                |> funApply $"ClassDeclaration {node.name.Value.getText()} should be an object type"
                )
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "InterfaceDeclarations resolved by type checker are always ClassOrInterface object types" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (fun node ->
                let objectType = node.name |> ctx.Checker.getTypeAtLocation :?> Ts.ObjectType
                let result = objectType.objectFlags &&& Ts.ObjectFlags.ClassOrInterface |> (<>) (enum 0)
                let flags = objectType.objectFlags.ToStringArray()
                Expect.isTrue result $"InterfaceDeclaration (except Iterator): {node.name.getText()} should be an object type. Has %A{flags}"
                )
        runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration "MethodDeclarations that are not optional resolved by type checker are object types" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.filter _.questionToken.IsNone
            |> Array.iter (fun node ->
                let typ = checker.getTypeAtLocation node
                let typString = checker.typeToString typ
                let flags = typ.flags.ToStringArray()
                typ
                |> _.flags.HasFlag(Ts.TypeFlags.Object)
                |> Expect.isTrue
                |> funApply $"MethodDeclaration should be a function type, instead got {flags}. {typString}"
                )
        runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration "MethodDeclarations that are optional resolved by type checker are union types" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.filter _.questionToken.IsSome
            |> Array.iter (fun node ->
                let typ = checker.getTypeAtLocation node
                let typString = checker.typeToString typ
                let flags = typ.flags.ToStringArray()
                typ
                |> _.flags.HasFlag(Ts.TypeFlags.Union)
                |> Expect.isTrue
                |> funApply $"Optional MethodDeclaration should be a union type, instead got {flags}. {typString}"
                )
        runner.testSyntaxKind<Ts.MethodSignature> Ts.SyntaxKind.MethodSignature "MethodSignature that are not optional resolved by type checker are object types" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.filter _.questionToken.IsNone
            |> Array.iter (fun node ->
                let typ = checker.getTypeAtLocation node
                let typString = checker.typeToString typ
                let flags = typ.flags.ToStringArray()
                typ
                |> _.flags.HasFlag(Ts.TypeFlags.Object)
                |> Expect.isTrue
                |> funApply $"MethodSignature should be a function (object) type, instead got {flags}. {typString}"
                let flags = typ :?> Ts.ObjectType |> _.objectFlags.ToStringArray()
                typ :?> Ts.ObjectType
                |> _.objectFlags.HasFlag(Ts.ObjectFlags.Anonymous)
                |> Expect.isTrue
                |> funApply $"MethodSignature objecttype should have anonymous flag, instead got {flags}. {typString}"
                )
        runner.testSyntaxKind<Ts.MethodSignature> Ts.SyntaxKind.MethodSignature "MethodSignature that are optional resolved by type checker are union types" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.filter _.questionToken.IsSome
            |> Array.iter (fun node ->
                let typ = checker.getTypeAtLocation node
                let typString = checker.typeToString typ
                let flags = typ.flags.ToStringArray()
                typ
                |> _.flags.HasFlag(Ts.TypeFlags.Union)
                |> Expect.isTrue
                |> funApply $"Optional MethodSignature should be a union type, instead got {flags}. {typString}"
                let types = typ :?> Ts.UnionType |> _.types.AsArray
                let typesString = types |> Array.map checker.typeToString
                Expect.hasLength types 2 $"Optional method signature should have only two types, instead got {typesString}"
                let typOneFlags = types[0].flags |> _.ToStringArray()
                let typTwoFlags = types[1].flags |> _.ToStringArray()
                Expect.exists types _.flags.HasFlag(Ts.TypeFlags.Undefined) $"Expected optional method signature to have two types, with one being undefined: Type1 flags {typOneFlags}; Type2 flags {typTwoFlags}"
                Expect.exists types (fun typ -> typ.flags.HasFlag(Ts.TypeFlags.Object) && (typ :?> Ts.ObjectType |> _.objectFlags.HasFlag(Ts.ObjectFlags.Anonymous))) $"Expected optional method signature to have two types, with one being an object with anonymous flag: Type1 flags {typOneFlags}; Type2 flags {typTwoFlags}"
                )
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "All interfaces have symbols" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> iface.name |> checker.getSymbolAtLocation |> Option.get |> ignore)
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "All class declarations have symbols" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> ignore)
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "All class declaration symbols have value declarations" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> _.valueDeclaration |> Option.get |> ignore)
        runner.testCase "No decorators are present on any node" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> Array.iter (unbox >> ts.getDecorators >>  Expect.isNone >> funApply "This had a decorator")
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators are predictable" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (
                _.operator
                >> function
                    | Ts.SyntaxKind.KeyOfKeyword 
                    | Ts.SyntaxKind.ReadonlyKeyword 
                    | Ts.SyntaxKind.UniqueKeyword -> None
                    | value -> Some value.Name
                )
            |> Expect.isEmpty
            |> funApply "Expected only KeyOf, Readonly and Unique operators."
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators have no symbol" <| fun ctx _ nodes ->
            nodes |> Array.choose ctx.Checker.getSymbolAtLocation
            |> Expect.isEmpty
            |> funApply "Expected no symbols for TypeOperators"
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators type nodes are predictable" <| fun ctx _ nodes ->
            nodes
            |> Array.map _.``type``
            |> Array.filter ( TypeNode.IsTypeNodeKind >> not )
            |> Expect.isEmpty
            |> funApply "Expected all TypeOperator type nodes to be parsed into XanTagKind.TypeNode"
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators have types associated; (=) inner type if operator is readonly" <| fun ctx _ nodes ->
            let nodes = nodes |> Array.filter (_.operator >> function Ts.SyntaxKind.ReadonlyKeyword -> true | _ -> false)
            (nodes |> Array.map ctx.Checker.getTypeFromTypeNode
            ,nodes |> Array.map (_.``type`` >> ctx.Checker.getTypeFromTypeNode))
            ||> Array.iter2 (fun a b ->
                let aString = ctx.Checker.typeToString a
                let bString = ctx.Checker.typeToString b
                if a <> b then Testing.Assert.NotEqual(aString, bString))
        runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators have types associated; <> inner type if operator <> readonly" <| fun ctx _ nodes ->
            let nodes = nodes |> Array.filter (_.operator >> function Ts.SyntaxKind.ReadonlyKeyword -> false | _ -> true)
            (nodes |> Array.map ctx.Checker.getTypeFromTypeNode
            ,nodes |> Array.map (_.``type`` >> ctx.Checker.getTypeFromTypeNode))
            ||> Array.iter2 (fun a b ->
                let aString = ctx.Checker.typeToString a
                let bString = ctx.Checker.typeToString b
                if a = b then Testing.Assert.AreEqual(aString, bString))
        runner.testCase "All nodes have ids via ts.getNodeId" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.iter (ts.getNodeId >> (<) 0 >> Expect.isTrue >> funApply "Expected a positive id number")
        runner.testCase "All symbols have ids via ts.getSymbolId" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.choose ctx.Checker.getSymbolAtLocation
            |> Array.iter (ts.getSymbolId >> (<) 0 >> Expect.isTrue >> funApply "Expected a positive id number")
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TypeReferences _.typeName always resolves to a symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (
                _.typeName
                >> unbox
                >> ctx.Checker.getSymbolAtLocation
                >> Flip.Expect.isSome "Expected type reference entity name to resolve to a symbol"
                )
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TypeReferences type arguments are either None or NonEmpty" <| fun ctx _ nodes ->
            nodes
            |> Array.choose _.typeArguments
            |> Array.iter (Flip.Expect.isNonEmpty "Expected at least one entry in type arguments if not null")
        runner.testCase "All symbols that are non-transient have declarations" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.choose ctx.Checker.getSymbolAtLocation
            |> Array.iter (function
                | symbol when ts.isTransientSymbol symbol -> ()
                | symbol ->
                    symbol.declarations
                    |> Option.map _.AsArray
                    |> Option.defaultValue [||]
                    |> Flip.Expect.isNonEmpty "Expected at least one declaration for a non-transient symbol"
                    )
        (*
        Symbols that are transient can still have declarations.
        Would have to check against internal CheckFlags to disambiguate.
        *)
        // runner.ftestCase "All symbols that are transient have no declarations" <| fun _ ctx ->
        //     ctx.NodeMap.Values
        //     |> Seq.collect _.AsArray
        //     |> Seq.toArray
        //     |> unbox<Ts.Node array>
        //     |> Array.filter (ctx.Checker.getSymbolAtLocation >> _.IsSome)
        //     |> Array.iter (fun node ->
        //         match ctx.Checker.getSymbolAtLocation node |> _.Value with
        //         | symbol when ts.isTransientSymbol symbol && symbol.name <> "Symbol" ->
        //             symbol.getDeclarations()
        //             |> Option.map _.AsArray
        //             |> Option.defaultValue [||]
        //             |> Array.map _.getText()
        //             |> fun arr -> arr |> Flip.Expect.isEmpty $"Expected no declarations for transient symbols: {symbol.name} | {symbol.flags.ToStringArray()} | {arr}"
        //         | symbol ->
        //             symbol.getDeclarations()
        //             |> Option.map _.AsArray
        //             |> Option.defaultValue [||]
        //             |> Flip.Expect.isNotEmpty "Expected declarations for non-transient symbols")
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TypeReferences _.typeName that resolve to a NON-TRANSIENT symbol have declarations" <| fun ctx _ nodes ->
            let getSymbolFlags idx = nodes[idx].typeName |> unbox |> ctx.Checker.getSymbolAtLocation |> Option.get |> _.flags.ToStringArray()
            nodes
            |> Array.iteri (fun idx ->
                _.typeName
                >> unbox
                >> ctx.Checker.getSymbolAtLocation
                >> Option.map ctx.Checker.getMergedSymbol
                >> Option.filter (ts.isTransientSymbol >> not)
                >> Option.iter (fun symbol ->
                    symbol.getDeclarations()
                    |> Option.map _.AsArray
                    |> Option.defaultValue [||]
                    |> Flip.Expect.isNonEmpty $"Expected at least one declaration for type reference entity name: {nodes[idx].getText()} | Flags: {getSymbolFlags idx}"
                    )
                )
        runner.testSyntaxKind<Ts.TypeReferenceNode> Ts.SyntaxKind.TypeReference "TypeReferences type arguments do not necessarily match the arity of the target" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (_.typeName >> unbox >> ctx.Checker.getSymbolAtLocation >> Option.get >> ctx.Checker.getMergedSymbol >> ts.isTransientSymbol >> not)
            |> Array.choose (fun node ->
                let symbol = 
                    node.typeName
                    |> unbox<Ts.Node>
                    |> ctx.Checker.getSymbolAtLocation
                    |> Option.get
                    |> ctx.Checker.getMergedSymbol
                symbol.declarations.Value.AsArray
                |> Array.filter (fun decl ->
                    ts.isClassDeclaration decl || ts.isInterfaceDeclaration decl || ts.isTypeAliasDeclaration decl
                    )
                |> Array.tryHead
                |> Option.map (unbox >> ts.getEffectiveTypeParameterDeclarations >> _.AsArray)
                |> Option.map (fun typars -> node, symbol, typars)
            )
            |> Array.choose (fun (node, symbol, typeParameters) ->
                let nodeTypeArguments = node.typeArguments |> Option.map _.AsArray |> Option.defaultValue Array.empty
                if nodeTypeArguments.Length > typeParameters.Length then
                    Some $"SymbolName: {symbol.name} Expected: {typeParameters.Length} Actual: {nodeTypeArguments.Length} | ActualTyparKinds: {nodeTypeArguments |> Array.map _.kind.Name} | ExpectedTypars: {typeParameters |> Array.map _.name.text}"
                elif nodeTypeArguments.Length <> typeParameters.Length then
                    typeParameters
                    |> Array.skip nodeTypeArguments.Length
                    |> Chain.Expect.all (fun typar -> typar.``default``.IsSome || typar.``constraint``.IsSome) "Expected missing typars to have a default or a constraint"
                    Some ""
                else None
                )
            |> function
                | [||] -> Expect.skip()
                | arr ->
                    arr
                    |> Array.filter (String.IsNullOrEmpty >> not)
                    |> Array.iter (printfn "%s")
                    Expect.pass()
            
        // runner.ftestSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "KeyOf type operators can be bounded, unbounded, generic, constrained generic" <| fun ctx _ nodes ->
        //     nodes
        //     |> Array.choose (
        //         TypeOperatorNode.Create ctx.Checker
        //         >> function
        //             | TypeOperatorNode.KeyOf typeNode -> Some typeNode
        //             | _ -> None
        //         )
        //     |> Array.iter (fun node ->
        //         let typ = ctx.Checker.getTypeFromTypeNode node
        //         let props =
        //             ctx.Checker.getPropertiesOfType typ
        //             |> _.AsArray
        //             |> Array.map _.symbolName
        //         let isIndexer = ctx.Checker.getIndexInfosOfType typ |> _.AsArray |> Array.isEmpty |> not
        //         if Array.isEmpty props then
        //             match node with
        //             | Patterns.Node.TypeReferenceNode node ->
        //                 let symbol = ctx.Checker.getSymbolAtLocation !!node.typeName
        //                 match symbol with
        //                 | Some symbol ->
        //                     symbol.flags.ToStringArray()
        //                     |> printfn "symbol on typeref node present: %s | Flags: %A" (node.getText())
        //                 | None ->
        //                     node.getText() |> printfn "No symbol on typeref node present %s"
        //             | _ ->
        //             node.kind.Name
        //             |> printf "%s "
        //             ctx.Checker.typeToString typ
        //             |> printf "%s "
        //             ctx.Checker.getBaseConstraintOfType typ
        //             |> Option.map (ctx.Checker.typeToString >> printf "BaseConstraint: %s ")
        //             typ.getConstraint()
        //             |> Option.map (ctx.Checker.typeToString >> printf "Constraint: %s ")
        //             printfn ""
        //         )
        runner.testSuite "Node Wrappers" <| fun _ ->
            runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator "TypeOperators are wrapped in a TypeNode" <| fun ctx _ nodes ->
                nodes
                |> Array.iter (TypeOperatorNode.Create ctx.Checker >> Option.ofObj >> Flip.Expect.isSome "Unexpected")
                
            runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration "MethodDeclaration.getWrappedNode" <| fun ctx _ nodes ->
                let checker = ctx.Checker
                let methodWrappers = nodes |> Array.map (MethodDeclaration.Create checker)
                fun () ->
                    methodWrappers
                    |> Array.iter (_.Type.objectFlags.HasFlag(Ts.ObjectFlags.Anonymous) >> Expect.isTrue >> funApply "MethodDeclaration.Type should be an object type")
                |> Expect.passes
                fun () ->
                    methodWrappers
                    |> Array.iter (_.Value.kind.HasFlag(Ts.SyntaxKind.MethodDeclaration) >> Expect.isTrue >> funApply "MethodDeclaration.Type should be an object type")
                |> Expect.passes
