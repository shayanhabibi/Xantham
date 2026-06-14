module EnumMember

open System
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

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "EN · Enum Resolution" <| fun _ ->
    // ----------------------------------------------------------------------------------------------
    //                                  EN - ENUM RESOLUTION
    // ----------------------------------------------------------------------------------------------
    runner.testCase "EN-1.1 - Enum flag does not always co-occur with EnumLiteral (observed)" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Enum)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.EnumLiteral) >> not)
        |> Flip.Expect.skipIfEmpty
    runner.testCase "EN-1.2 - Enum flag does not always co-occur with NumberLiteral (observed)" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Enum)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.NumberLiteral) >> not)
        |> Flip.Expect.skipIfEmpty
    runner.testCase "EN-2 · EnumLiteral does not always co-occur with the Enum flag (observed)" <| fun test ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.EnumLiteral)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Enum) >> not)
        |> Flip.Expect.skipIfEmpty
    runner.testCase "EN-3 · Enum flag resolves to a symbol with an EnumDeclaration value declaration" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Enum)
        |> Array.iter (
            _.symbol.valueDeclaration
            >> Flip.Expect.wantSome ""
            >> _.kind
            >> (=) Ts.SyntaxKind.EnumDeclaration
            >> Flip.Expect.isTrue "" )
    runner.testCase "EN-4 · EnumLiterals with Enum or Union resolve to an EnumDeclaration value declaration" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.EnumLiteral)
        |> fun enumLiterals ->
            let check flag =
                enumLiterals
                |> Array.filter _.flags.HasFlag(flag)
                |> Array.iter (fun typ ->
                    Expect.isTrue (
                        typ.symbol.valueDeclaration
                        |> Chain.Expect.wantSome "Symbol should have value decl"
                        |> _.kind
                        |> (=) Ts.SyntaxKind.EnumDeclaration
                    ) "Expected type to have a value declaration that is an enum declaration"
                )
            check Ts.TypeFlags.Enum
            check Ts.TypeFlags.Union
    runner.testCase "EN-5 · EnumLiterals without Enum or Union resolve to an EnumMember value declaration" <| fun _ ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.EnumLiteral)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Enum) >> not)
        |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Union) >> not)
        |> Array.iter (fun typ ->
            typ.getSymbol()
            |> Chain.Expect.wantSome "EnumLiterals should always have a symbol"
            |> fun symbol ->
                symbol.valueDeclaration
                |> Flip.Expect.wantSome ""
                |> function
                    | Patterns.Node.EnumMember _ -> ()
                    | node -> failtest $"Expected value declaration to be an enum member, but got {node.kind.Name} instead. SymbolName: {symbol.escapedName |> SymbolName.Create}; NodeText: {node.getText()}; TypeText: {ctx.Checker.typeToString typ}"
            )
    let inline makeEnumLiteralCheck checkKind name idx =
        runner.testSyntaxKind<Ts.EnumMember> Ts.SyntaxKind.EnumMember $"EN-%i{idx} - EnumLiterals can have the %s{name} flag" <| fun ctx test nodes ->
            nodes
            |> Array.map ctx.Checker.getTypeAtLocation
            |> Array.filter _.flags.HasFlag(checkKind)
            |> function
                | arr when Array.isEmpty arr |> not -> Expect.pass()
                | _ ->
                ctx.Types.Value
                |> Array.filter _.flags.HasFlag(Ts.TypeFlags.EnumLiteral)
                |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Enum) >> not)
                |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Union) >> not)
                |> Array.filter _.flags.HasFlag(checkKind)
                |> function
                    | [| |] -> test.skip()
                    | _ -> Expect.pass()
    makeEnumLiteralCheck Ts.TypeFlags.StringLiteral "StringLiteral" 6
    makeEnumLiteralCheck Ts.TypeFlags.NumberLiteral "NumberLiteral" 7
    makeEnumLiteralCheck Ts.TypeFlags.BigIntLiteral "BigIntLiteral" 8
    makeEnumLiteralCheck Ts.TypeFlags.BooleanLiteral "BooleanLiteral" 9
    makeEnumLiteralCheck Ts.TypeFlags.Null "Null" 10
    // runner.testCase "EN-11 · Enum member types parse into an EnumMember" <| fun _ ctx ->
    //     ctx.Types.Value
    //     |> Array.filter _.flags.HasFlag(Ts.TypeFlags.EnumLiteral)
    //     |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Enum) >> not)
    //     |> Array.filter (_.flags.HasFlag(Ts.TypeFlags.Union) >> not)
    //     |> Array.map EnumMember.TryCreate
    //     |> Chain.Expect.skipIfEmpty
    //     |> Option.iter (Flip.Expect.all _.IsSome "")
    // runner.testSyntaxKind<Ts.EnumMember> Ts.SyntaxKind.EnumMember "EN-12 · Every EnumMember node parses into an EnumMember" <| fun ctx _ nodes ->
    //     nodes
    //     |> Array.map(fun node -> EnumMember.TryCreate(node, ctx.Checker))
    //     |> Chain.Expect.skipIfEmpty
    //     |> Option.iter (Flip.Expect.all _.IsSome "")
    // runner.testCase "EN-13 · Every enum member symbol parses into an EnumMember" <| fun _ ctx ->
    //     ctx.Symbols.Value
    //     |> Array.filter _.flags.HasFlag(Ts.SymbolFlags.EnumMember)
    //     |> Array.map (fun symbol -> EnumMember.TryCreate(symbol, ctx.Checker))
    //     |> Chain.Expect.skipIfEmpty
    //     |> Option.iter (Flip.Expect.all _.IsSome "")
        
    // This actually shows why we should resolve all declarations that contribute to an enum declaration, because
    // we otherwise gloss over any aliases existing within the declarations
    runner.testSyntaxKind<Ts.EnumMember> Ts.SyntaxKind.EnumMember "EN-14 · Not every EnumMember declaration is its symbol's canonical value declaration" <| fun ctx test nodes ->
        let nodeIds = nodes |> Array.map ts.getNodeId
        nodes
        |> Array.map (ctx.Checker.getTypeAtLocation >> _.unsafeGetCanonicalSymbol() >> _.valueDeclaration.Value >> ts.getNodeId)
        |> fun compIds ->
            if compIds = nodeIds then
                test.skip()
            else
                #if INTELLIJ_IDE
                printfn "%A\n%A" nodeIds compIds
                #endif
                Expect.pass()
    // runner.testSyntaxKind<Ts.EnumDeclaration> Ts.SyntaxKind.EnumDeclaration "EN-15 · An EnumDeclaration's members may resolve to a subset of EnumMember symbols (aliases)" <| fun ctx _ nodes ->
    //     nodes
    //     |> Array.map _.members.AsArray
    //     |> Array.filter (fun arr ->
    //         arr
    //         |> Array.map (fun node -> EnumMember.Create(node, ctx.Checker))
    //         |> Array.distinctBy _.SymbolKey
    //         |> function
    //             | carr when Array.length carr = Array.length arr -> false
    //             | carr when Array.length arr > Array.length carr -> true
    //             | _ -> failtest "Enum declaration should not have more members than declared"
    //         )
    //     |> Chain.Expect.skipIfEmpty
    //     |> Option.iter ignore
    runner.testSyntaxKind<Ts.EnumDeclaration> Ts.SyntaxKind.EnumDeclaration "EN-16 · Every EnumDeclaration resolves to a single-declaration symbol carrying the Enum flag" <| fun ctx _ nodes ->
        nodes
        |> Array.map (_.name >> ctx.Checker.getSymbolAtLocation)
        |> Chain.Expect.all _.IsSome ""
        |> Chain.Expect.all _.Value.valueDeclaration.IsSome ""
        |> Chain.Expect.all (_.Value.declarations >> Option.exists _.Count.Equals(1)) ""
        |> Flip.Expect.all _.Value.flags.HasFlag(Ts.SymbolFlags.Enum) ""
