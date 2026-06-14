module Identity

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

let inline tests (runner: Spec.RunnerContext) : unit =
    runner.testSuite "SY · Symbols & Identity" <| fun _ ->
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "SY-1 · All interfaces have symbols" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> iface.name |> checker.getSymbolAtLocation |> Option.get |> ignore)
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "SY-2 · All class declarations have symbols" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> ignore)
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "SY-3 · All class declaration symbols have value declarations" <| fun ctx _ nodes ->
            let checker = ctx.Checker
            nodes
            |> Array.iter (fun iface -> (iface.name |> Option.defaultValue !!iface) |> checker.getSymbolAtLocation |> Option.get |> _.valueDeclaration |> Option.get |> ignore)
        runner.testCase "SY-4 · All nodes have ids via ts.getNodeId" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.iter (ts.getNodeId >> (<) 0 >> Expect.isTrue >> funApply "Expected a positive id number")
        runner.testCase "SY-5 · All symbols have ids via ts.getSymbolId" <| fun _ ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.choose ctx.Checker.getSymbolAtLocation
            |> Array.iter (ts.getSymbolId >> (<) 0 >> Expect.isTrue >> funApply "Expected a positive id number")
        runner.testCase "SY-6 · All symbols that are non-transient have declarations" <| fun _ ctx ->
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
        runner.testCase "SY-7 · Transient symbols may still carry declarations (observed)" <| fun test ctx ->
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.choose ctx.Checker.getSymbolAtLocation
            |> Array.filter ts.isTransientSymbol
            |> Array.choose (fun symbol ->
                symbol.declarations
                |> Option.map _.AsArray
                )
            |> function
                | [||] -> test.skip()
                | _ -> Expect.pass()
        runner.testCase "SY-8 - Symbols with the Value flag and no Transient flag have a value declaration" <| fun _ ctx ->
            ctx.Symbols.Value
            |> Array.filter (_.flags >> (&&&) Ts.SymbolFlags.Value >> (<>) (enum 0))
            |> Array.filter (_.flags.HasFlag(Ts.SymbolFlags.Transient) >> not)
            |> Array.map _.valueDeclaration
            |> Chain.Expect.skipIfEmpty
            |> Option.iter ( Flip.Expect.all _.IsSome "" )
