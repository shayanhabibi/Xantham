module UniqueESSymbolWrapper
open System
open Xantham.TypeScript.Types.Node
open Xantham.TypeScript.Types.Symbol
open Scriptorium.Nib.Assertion
open System.Collections.Generic
open EasyBuild.FileSystemProvider
open TypeScript
open Fable.Core.JsInterop
open Xantham.Fable
open Fable.Core
open Xantham.TypeScript
// We use our own mocha dsl so that it works better with IDE test runners for JS
open Xantham.Mocha
open Xantham.TypeScript.Types.Type
open Xantham.Mocha.Nib

let tests (runner: Spec.RunnerContext) : unit = runner.testSuite "UniqueESSymbol" <| fun _ ->
    let inline testCase name fn = runner.testCase name <| fun t ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.UniqueESSymbol)
        |> Array.map (fun typ -> typ :?> Ts.UniqueESSymbolType)
        |> fn ctx t
    let inline ftestCase name fn = runner.ftestCase name <| fun t ctx ->
        ctx.Types.Value
        |> Array.filter _.flags.HasFlag(Ts.TypeFlags.UniqueESSymbol)
        |> Array.map (fun typ -> typ :?> Ts.UniqueESSymbolType)
        |> fn ctx t
    let inline ptestCase name fn = runner.ptestCase name <| fun _ _ -> ()
    let typeNodeIsUniqueSymbol (ctx: Spec.SuiteContext) =
        satisfy (function
            | Type.TypeOperator (TypeOperator.Unique(Type.Keyword(TypeKeyword.Symbol, _)), tn) ->
                ctx.Checker.getTypeFromTypeNode tn
                |> Type.Kind.create ctx.Program
                |> function
                    | Kind.Primitive (Primitive.Literal (Literal.UniqueESSymbol _)) -> true
                    | _ -> false
            | _ -> false)
    testCase "have symbol of variable/property kind" <| fun ctx _ ->
        foreach (
            focus _.getCanonicalSymbol()
            >> Option.value
            >> forceError (inside (fun sym -> fun () -> sym |> Symbol.Kind.create ctx.Program |> ignore) doesNotThrow)
            >> focus (Symbol.Kind.create ctx.Program)
            >> assertion (function
                | Kind.Property _
                | Kind.Variable _ -> true
                | _ -> false) (sprintf "Expected symbol to be Property or Variable, not %A")
            >> branchInsideFor (function Kind.Property sym -> Choice1Of2 sym |> Some | Kind.Variable sym -> Choice2Of2 sym |> Some | _ -> None )
                ( // PropertyKind
                    tag "symbol"
                    >> satisfy (_.canonical >> _.Value >> _.kind >> function Ts.SyntaxKind.PropertyDeclaration | Ts.SyntaxKind.PropertySignature -> true | _ -> false)
                    >> tag "valueDeclaration"
                    // dont seem to be able to properly resolve back to the type node
                    >> branchInside
                        (Symbol.valueDeclaration >> function PropertyKind.Class node -> Choice1Of2 node | PropertyKind.Type node -> Choice2Of2 node)
                        (
                            tag "propertyDeclaration"
                            >> inside _.Value.name (satisfy (function
                                | Patterns.Node.PropertyNamePatterns.ComputedPropertyName _ -> true
                                | _ -> false
                                ))
                            >> forceError (inside _.Value.``type`` Option.isSome)
                            >> tag "type"
                            >> focus (_.Value.``type`` >> Option.get >> Node.Type.create ctx.Program)
                            >> typeNodeIsUniqueSymbol ctx
                        )
                        (
                            tag "propertySignature"
                            // unlike propertyDeclaration, propertySignature has name simplified to identifier
                            // The type can also yield 'symbol'
                            // We can actually resolve the type at the location to get the correct type, unlike with
                            // the property declaration
                            >> inside (_.Value >> ctx.Checker.getTypeAtLocation) (
                                tag "getTypeAtLocation"
                                >> forceError (inside (fun typ -> fun () -> typ |> Type.Kind.create ctx.Program |> ignore) doesNotThrow)
                                >> focus (Type.Kind.create ctx.Program)
                                >> satisfy (function
                                    | Kind.Primitive (Primitive.Literal (Literal.UniqueESSymbol _)) -> true
                                    | _ -> false
                                    )
                                )
                        )
                )
                ( // Variable
                    tag "symbol"
                    >> satisfy (_.canonical >> _.kind.Equals(Ts.SyntaxKind.VariableDeclaration))
                    >> tag "variableDeclaration"
                    >> inside Symbol.variableDeclaration (
                        tag "name"
                        >> inside _.name (satisfy (function
                            | Patterns.Node.BindingNamePatterns.Identifier _ -> true
                            | _ -> false
                            ))
                        >> popTag >> tag "type"
                        >> forceError (inside _.``type`` Option.isSome)
                        >> focus (_.``type`` >> Option.get >> Node.Type.create ctx.Program)
                        >> typeNodeIsUniqueSymbol ctx
                        )
                )
            )
    
        
