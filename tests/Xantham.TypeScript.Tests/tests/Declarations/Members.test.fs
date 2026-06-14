module Members


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

let inline memberSymbols (runner: Spec.RunnerContext) : unit =
    // ----------------------------------------------------------------------------------------------
    //                                  MS - MEMBER SYMBOLS
    // ----------------------------------------------------------------------------------------------
    // How a class/interface member's name resolves to a symbol, whether that symbol carries a
    // `valueDeclaration`, and which `SymbolFlags` it bears. These back the wrappers that read a
    // member's symbol/value-declaration when projecting members into the 3-tier representation.
    runner.testSuite "MS · Member Symbols" <| fun _ ->
        runner.testSyntaxKind<Ts.PropertySignature> Ts.SyntaxKind.PropertySignature "MS-1 · PropertySignature names resolve to a Property symbol with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox >> ctx.Checker.getSymbolAtLocation)
            |> Array.iter (Chain.Expect.wantSome "PropertySignature should have a symbol associated with the name" >> fun symbol ->
                symbol.valueDeclaration
                |> Flip.Expect.isSome "PropertySignature should have a value declaration value"
                symbol.flags.HasFlag(Ts.SymbolFlags.Property)
                |> Flip.Expect.isTrue "PropertySignature symbol should have the Property symbol flag"
                )
        runner.testSyntaxKind<Ts.PropertyDeclaration> Ts.SyntaxKind.PropertyDeclaration  "MS-2 · PropertyDeclaration names resolve to a Property symbol with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox >> ctx.Checker.getSymbolAtLocation)
            |> Array.iter (Chain.Expect.wantSome "PropertyDeclaration should have a symbol associated with the name" >> fun symbol ->
                symbol.valueDeclaration
                |> Flip.Expect.isSome "PropertyDeclaration should have a value declaration value"
                symbol.flags.HasFlag(Ts.SymbolFlags.Property)
                |> Flip.Expect.isTrue "PropertyDeclaration symbol should have the Property symbol flag"
                )
        runner.testSyntaxKind<Ts.MethodSignature> Ts.SyntaxKind.MethodSignature "MS-3 · MethodSignature names resolve to a Method symbol with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox >> ctx.Checker.getSymbolAtLocation)
            |> Array.iter (Chain.Expect.wantSome "MethodSignature should have a symbol associated with the name" >> fun symbol ->
                symbol.valueDeclaration
                |> Flip.Expect.isSome "MethodSignature should have a value declaration value"
                symbol.flags.HasFlag(Ts.SymbolFlags.Method)
                |> Flip.Expect.isTrue "MethodSignature symbol should have the Method symbol flag"
                )
        runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration "MS-4 · MethodDeclaration names resolve to a Method symbol with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox >> ctx.Checker.getSymbolAtLocation)
            |> Array.iter (Chain.Expect.wantSome "MethodDeclarations should have a symbol associated with the name" >> fun symbol ->
                symbol.valueDeclaration
                |> Flip.Expect.isSome "MethodDeclarations should have a value declaration value"
                symbol.flags.HasFlag(Ts.SymbolFlags.Method)
                |> Flip.Expect.isTrue "MethodDeclarations symbol should have the Method symbol flag"
                )
        runner.testSyntaxKind<Ts.CallSignatureDeclaration> Ts.SyntaxKind.CallSignature "MS-5 · CallSignature resolves to a symbol with no value declaration" <| fun _ _ nodes ->
            // Call signatures do not resolve as a value declaration.
            nodes
            |> Array.map (_.getSymbol() >> Chain.Expect.wantSome "CallSignatures should have a symbol attached")
            |> Array.iter (fun symbol ->
                match symbol.symbolName with
                | SymbolName.InternalSymbol Ts.InternalSymbolName.Call -> ()
                | name -> failtest $"Expected CallSignature to have a symbol name of '__call', got %A{name} instead"
                symbol.valueDeclaration
                |> Flip.Expect.isNone "CallSignatures shouldn't have a value declaration"
                symbol.flags.HasFlag(Ts.SymbolFlags.Signature)
                |> Flip.Expect.isTrue "CallSignature symbols should have the Signature flag"
                )
        runner.testSyntaxKind<Ts.CallSignatureDeclaration> Ts.SyntaxKind.CallSignature "MS-6 · CallSignature symbols may carry multiple declarations (overloads)" <| fun _ _ nodes ->
            // If call signatures collect overloads into their symbols, then we would expect some
            // call signature symbols to have more than one declaration.
            nodes
            |> Array.map (_.getSymbol() >> Chain.Expect.wantSome "CallSignatures should have a symbol attached")
            |> Array.filter (fun symbol ->
                symbol.declarations
                |> Chain.Expect.wantSome "CallSignature should have at least one declaration"
                |> _.AsArray
                |> Chain.Expect.isNotEmpty "CallSignature should have at least one declaration"
                |> _.Length |> (<>) 1
                )
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "MS-7 · An interface may resolve to fewer unique call-signature symbols than members" <| fun _ _ nodes ->
            // If call signatures collect overloads into their symbols, then we would expect resolution of an interfaces symbols to
            // result in fewer symbols than declarations of a call signature.
            nodes
            |> Array.map (_.members.AsArray >> Array.choose (function Patterns.Node.CallSignatureDeclaration node -> Some node | _ -> None))
            |> Array.filter (Array.isEmpty >> not)
            |> Array.filter (fun callSignatures ->
                callSignatures
                |> Array.map _.symbol
                |> Array.distinctBy ts.getSymbolId
                |> _.Length |> (<>) callSignatures.Length
                )
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.CallSignatureDeclaration> Ts.SyntaxKind.CallSignature "MS-8 · CallSignatures may carry type parameters" <| fun _ _ nodes ->
            nodes
            |> Array.filter _.typeParameters.IsSome
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.GetAccessorDeclaration> Ts.SyntaxKind.GetAccessor "Get Accessors resolve to symbols with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox<Ts.Node> >> ctx.Checker.getSymbolAtLocation >> Flip.Expect.wantSome "All accessors should resolve to a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.GetAccessor
                |> Flip.Expect.isTrue "GetAccessor symbol has the GetAccessor symbol flag"
                symbol.valueDeclaration
                |> Flip.Expect.isSome "GetAccessor has a value declaration on its symbol"
                )
        runner.testSyntaxKind<Ts.SetAccessorDeclaration> Ts.SyntaxKind.SetAccessor "SetAccessor resolve to a symbol with a value declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> unbox<Ts.Node> >> ctx.Checker.getSymbolAtLocation >> Flip.Expect.wantSome "All accessor should resolve to a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.SetAccessor
                |> Flip.Expect.isTrue "SetAccessor symbol has the SetAccessor symbol flag"
                symbol.valueDeclaration
                |> Flip.Expect.isSome "SetAccessor has a value declaration on its symbol"
                )
        runner.testSyntaxKind<Ts.SetAccessorDeclaration> Ts.SyntaxKind.SetAccessor "SetAccessor symbols can have more than one declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (
                _.getSymbol()
                >> Flip.Expect.wantSome "SetAccessor should have a symbol"
                >> _.declarations
                >> Flip.Expect.wantSome "SetAccessor should have declarations"
                >> _.AsArray
                >> Array.length
                >> (=) 1
                >> not
                )
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.GetAccessorDeclaration> Ts.SyntaxKind.GetAccessor "GetAccessor symbols can have more than one declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (
                _.getSymbol()
                >> Flip.Expect.wantSome "GetAccessor should have a symbol"
                >> _.declarations
                >> Flip.Expect.wantSome "GetAccessor should have declarations"
                >> _.AsArray
                >> Array.length
                >> (=) 1
                >> not
                )
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.ConstructSignatureDeclaration> Ts.SyntaxKind.ConstructSignature "ConstructSignatures resolve to a symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.getSymbol() >> Flip.Expect.wantSome "ConstructSignature should resolve to a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.Signature
                |> Flip.Expect.isTrue "ConstructSignature symbol has the Signature symbol flag"
                symbol.declarations
                |> Flip.Expect.wantSome "ConstructSignature has declarations on its symbol"
                |> _.AsArray
                |> Flip.Expect.isNotEmpty "ConstructSignature has declarations on its symbol"
                )
        runner.testSyntaxKind<Ts.ConstructSignatureDeclaration> Ts.SyntaxKind.ConstructSignature "ConstructSignature symbols can have multiple declarations" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                _.getSymbol()
                >> Flip.Expect.wantSome "ConstructSignature should resolve to a symbol"
                >> _.declarations
                >> Flip.Expect.wantSome "ConstructSignature has declarations on its symbol"
                >> _.AsArray
                )
            |> Array.filter (_.Length.Equals(1) >> not)
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.ConstructorDeclaration> Ts.SyntaxKind.Constructor "Constructors resolve to a symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.getSymbol() >> Flip.Expect.wantSome "Constructor should resolve to a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.Constructor
                |> Flip.Expect.isTrue "Constructor symbol has the Constructor symbol flag"
                symbol.declarations
                |> Flip.Expect.wantSome "Constructor has declarations on its symbol"
                |> _.AsArray
                |> Flip.Expect.isNotEmpty "Constructor has declarations on its symbol"
                )
        runner.testSyntaxKind<Ts.ConstructorDeclaration> Ts.SyntaxKind.Constructor "Constructor symbols can have multiple declarations" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                _.getSymbol()
                >> Flip.Expect.wantSome "Constructor should resolve to a symbol"
                >> _.declarations
                >> Flip.Expect.wantSome "Constructor has declarations on its symbol"
                >> _.AsArray
                )
            |> Array.filter (_.Length.Equals(1) >> not)
            |> Chain.Expect.skipIfEmpty
            |> ignore
        runner.testSyntaxKind<Ts.IndexSignatureDeclaration> Ts.SyntaxKind.IndexSignature "IndexSignatureDeclaration resolve to a symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.getSymbol() >> Flip.Expect.wantSome "IndexSignature should resolve to a symbol")
            |> Array.iter (fun symbol ->
                symbol.flags.HasFlag Ts.SymbolFlags.Signature
                |> Flip.Expect.isTrue "IndexSignature symbol has the Signature symbol flag"
                symbol.declarations
                |> Flip.Expect.wantSome "IndexSignature has declarations on its symbol"
                |> _.AsArray
                |> Flip.Expect.isNotEmpty "IndexSignature has declarations on its symbol"
                )
        runner.testSyntaxKind<Ts.IndexSignatureDeclaration> Ts.SyntaxKind.IndexSignature "IndexSignatureDeclaration symbols can have multiple declarations" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                _.getSymbol()
                >> Flip.Expect.wantSome "IndexSignature should resolve to a symbol"
                >> _.declarations
                >> Flip.Expect.wantSome "IndexSignature has declarations on its symbol"
                >> _.AsArray
                )
            |> Array.filter (_.Length.Equals(1) >> not)
            |> Chain.Expect.skipIfEmpty
            |> ignore
