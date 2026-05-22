#nowarn FS1104


module Program

open System.Collections.Generic
open Fable.Mocha
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core

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
let inline testIfNodes<'T> (nodeMap: Dictionary<Ts.SyntaxKind, ResizeArray<obj>>) syntaxKind name test =
    if nodeMap.ContainsKey(syntaxKind) && nodeMap[syntaxKind].AsArray.Length > 0
    then testCase name (fun () -> nodeMap[syntaxKind].AsArray |> unbox<'T array> |> test)
    else ptestCase $"SKIPPED: No values to test against | %s{name}" ignore
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
    testList "Node invariants" [
        let nodeMap = Dictionary<Ts.SyntaxKind, ResizeArray<obj>>()
        let rec crawl = fun node ->
            ts.forEachChild(node, fun node ->
            match nodeMap.TryGetValue(node.kind) with
            | true, nodes -> nodes.Add(node)
            | _ -> nodeMap[node.kind] <- ResizeArray [ box node ]
            crawl node
            JS.undefined
            ) |> ignore
        program.getSourceFiles().AsArray
        |> Array.iter crawl
        let inline getNodes syntaxKind: 'T array =
            match nodeMap.TryGetValue(syntaxKind) with
            | true, values -> unbox values.AsArray
            | _ -> [||]
        testIfNodes nodeMap Ts.SyntaxKind.NumericLiteral "Numeric Literals are all parsable" <| fun (literals: Ts.NumericLiteral array) ->
            literals
            |> Array.iter (
                _.text
                >> JS.Constructors.Number.parseFloat
                >> function
                    | value when jsTypeof value = "number" -> ()
                    | value -> failtest $"Unrecognised numeric literal: %A{value}"
                )
        testIfNodes<Ts.BigIntLiteral> nodeMap Ts.SyntaxKind.BigIntLiteral "BigInt Literals are all parsable" <| fun literals ->
            literals
            |> Array.iter (
                _.text
                >> _.TrimEnd('n')
                >> System.Numerics.BigInteger.Parse
                >> ignore
                )
            
        testIfNodes<Ts.StringLiteral> nodeMap Ts.SyntaxKind.StringLiteral "String Literals all have valid string values" <| fun literals ->
            literals
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        testIfNodes<Ts.NoSubstitutionTemplateLiteral> nodeMap Ts.SyntaxKind.NoSubstitutionTemplateLiteral "NoSubstitutionTemplateLiteral values are valid" <| fun nodes ->
            nodes
            |> Array.iter (
                _.text
                >> function
                    | Null -> failtest "String literal should not be null"
                    | "" -> Expect.passWithMsg "Empty string literals are valid"
                    | _ -> ()
                )
        testIfNodes<Ts.PrefixUnaryExpression> nodeMap Ts.SyntaxKind.PrefixUnaryExpression "PrefixUnaryExpression Operators values are predictable" <| fun nodes ->
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
        testIfNodes<Ts.PrefixUnaryExpression> nodeMap Ts.SyntaxKind.PrefixUnaryExpression "PrefixUnaryExpression Operand values are all numeric literals" <| fun nodes ->
            nodes
            |> Array.iter (
                _.operand
                >> function
                    | Patterns.SyntaxKind.NumericLiteral _ -> ()
                    | value -> failtest $"Received an invalid/unknown PostfixUnaryExpression operand kind: %s{value.kind.Name}" 
                )
        testIfNodes<Ts.LiteralTypeNode> nodeMap Ts.SyntaxKind.LiteralType "LiteralTypeNode _.literal values are parsed predictably" <| fun nodes ->
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
        testCase "DeclarationFiles have a narrowed subset of valid nodes" <| fun _ ->
            nodeMap.Keys
            |> Seq.distinct
            |> Seq.sortBy _.Name
            |> Seq.iter (function
                | value when DeclarationFileNodes.IsKnownDeclarationFileNodeSyntaxKind value -> ()
                | value -> failtest $"Unexpected node kind in a declaration file: %s{value.Name}"
                )
        testIfNodes<Ts.ClassDeclaration> nodeMap Ts.SyntaxKind.ClassDeclaration "ClassDeclarations have a limited subset of nodes as members" <| fun nodes ->
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
        testIfNodes<Ts.InterfaceDeclaration> nodeMap Ts.SyntaxKind.InterfaceDeclaration "InterfaceDeclarations have a limited subset of nodes as members" <| fun nodes ->
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
        testIfNodes<Ts.ClassDeclaration> nodeMap Ts.SyntaxKind.ClassDeclaration "ClassDeclarations resolved by type checker are always Class object types" <| fun nodes ->
            nodes
            |> Array.iter (fun node ->
                if node.name.IsNone then
                    unbox<Ts.Node> node
                else unbox<Ts.Node> node.name.Value
                |> checker.getTypeAtLocation
                :?> Ts.ObjectType
                |> _.objectFlags.HasFlag(Ts.ObjectFlags.Class)
                |> Expect.isTrue
                |> funApply $"ClassDeclaration {node.name.Value.getText()} should be an object type"
                )
        testIfNodes<Ts.InterfaceDeclaration> nodeMap Ts.SyntaxKind.InterfaceDeclaration "InterfaceDeclarations resolved by type checker are always ClassOrInterface object types" <| fun nodes ->
            nodes
            |> Array.iter (fun node ->
                let objectType = node.name |> checker.getTypeAtLocation :?> Ts.ObjectType
                let result = objectType.objectFlags &&& Ts.ObjectFlags.ClassOrInterface |> (<>) (enum 0)
                let flags = objectType.objectFlags.ToStringArray()
                Expect.isTrue result $"InterfaceDeclaration (except Iterator): {node.name.getText()} should be an object type. Has %A{flags}"
                )
        testIfNodes<Ts.MethodDeclaration> nodeMap Ts.SyntaxKind.MethodDeclaration "MethodDeclarations that are not optional resolved by type checker are object types" <| fun nodes ->
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
        testIfNodes<Ts.MethodDeclaration> nodeMap Ts.SyntaxKind.MethodDeclaration "MethodDeclarations that are optional resolved by type checker are union types" <| fun nodes ->
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
        testIfNodes<Ts.MethodSignature> nodeMap Ts.SyntaxKind.MethodSignature "MethodSignature that are not optional resolved by type checker are object types" <| fun nodes ->
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
        testIfNodes<Ts.MethodSignature> nodeMap Ts.SyntaxKind.MethodSignature "MethodSignature that are optional resolved by type checker are union types" <| fun nodes ->
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
        testIfNodes<Ts.InterfaceDeclaration> nodeMap Ts.SyntaxKind.InterfaceDeclaration "All interfaces have symbols" <| fun nodes ->
            nodes
            |> Array.iter (fun iface -> iface.name |> checker.getSymbolAtLocation |> Option.get |> ignore)
        testIfNodes<Ts.ClassDeclaration> nodeMap Ts.SyntaxKind.ClassDeclaration "All class declarations have symbols" <| fun nodes ->
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> ignore)
        testIfNodes<Ts.ClassDeclaration> nodeMap Ts.SyntaxKind.ClassDeclaration "All class declaration symbols have value declarations" <| fun nodes ->
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> _.valueDeclaration |> Option.get |> ignore)
        testCase "No decorators are present on any node" <| fun _ ->
            nodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> Array.iter (unbox >> ts.getDecorators >>  Expect.isNone >> funApply "This had a decorator")
            
        // testIfNodes Ts.SyntaxKind.ImportSpecifier "ImportSpecifier" <| fun _ ->
        //     (getNodes Ts.SyntaxKind.ImportSpecifier : Ts.ImportSpecifier array)
        //     |> Array.iter (
        //         _.propertyName
        //         >> Expect.isSome
        //         >> funApply ""
        //         )
        testList "Node Wrappers" [
            testIfNodes<Ts.MethodDeclaration> nodeMap Ts.SyntaxKind.MethodDeclaration "MethodDeclaration.getWrappedNode" <| fun nodes ->
                nodes
                |> Array.map (MethodDeclaration.Create checker)
                |> Array.iter (_.Type.objectFlags.HasFlag(Ts.ObjectFlags.Anonymous) >> Expect.isTrue >> funApply "MethodDeclaration.Type should be an object type")
                nodes
                |> Array.map (MethodDeclaration.Create checker)
                |> Array.iter (_.Value.kind.HasFlag(Ts.SyntaxKind.MethodDeclaration) >> Expect.isTrue >> funApply "MethodDeclaration.Type should be an object type")
        ]
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
    "@types/d3", TestFixtures.``@types``.d3.node_modules.``@types``.d3.``index.d.ts``
    "@types/node", TestFixtures.``@types``.node.node_modules.``@types``.node.``index.d.ts``
    "@types/semver", TestFixtures.``@types``.semver.node_modules.``@types``.semver.``index.d.ts``
    "ansi-regex", TestFixtures.``ansi-regex``.node_modules.``ansi-regex``.``index.d.ts``
    "type-fest", TestFixtures.``type-fest``.node_modules.``type-fest``.``index.d.ts``
    "@types/lodash", TestFixtures.``@types``.lodash.node_modules.``@types``.lodash.``index.d.ts``
    "anime", TestFixtures.animejs.node_modules.animejs.dist.modules.``index.d.ts``
    "typescript", TestFixtures.typescript.node_modules.typescript.lib.``typescript.d.ts``
]
|> List.unzip
||> List.map2 makeTestSeries
|> testList "Fable.TypeScript" 
|> Mocha.runTests
|> ignore