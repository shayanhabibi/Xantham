module Xantham.Generator.Tests.AliasDefaultArgumentsTests

open System
open System.IO
open Expecto
open Xantham.TypeScript.Wire
open Xantham.Generator

[<Tests>]
let tests =
    testCase "a bare generic alias reference carries its default arguments" <| fun _ ->
        let package = Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "catalog-alias-defaults-lab")
        let config = GeneratorConfig.loadFile (Path.Combine(package, "xantham.json"))

        async {
            let! mailbox, ctx = Bootstrap.start config package
            use _ = mailbox :> IDisposable
            let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
            let! resolve, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)

            let named id =
                Map.tryFind id resolve.Types
                |> Option.bind _.SymbolName
                |> Option.map string

            // `Tool<any, any>`, however many copies the checker interned: the alias body is the
            // union of the two tool interfaces.
            let tools =
                resolve.Types
                |> Map.toList
                |> List.map snd
                |> List.filter (fun facts ->
                    facts.Response.Flags.HasFlag TypeFlags.Union
                    && not facts.AliasDeclarations.IsEmpty
                    && (facts.UnionMembers |> List.map named |> List.choose id |> List.sort) = [ "DynamicTool"; "FunctionTool" ]
                    && facts.UnionMembers
                       |> List.forall (fun id ->
                           Map.tryFind id resolve.Types
                           |> Option.exists (fun item ->
                               item.TypeArguments
                               |> List.forall (fun argument ->
                                   Map.tryFind argument resolve.Types
                                   |> Option.exists (fun facts -> facts.Response.Flags.HasFlag TypeFlags.Any)))))

            Expect.isGreaterThanOrEqual tools.Length 2 "the constraint `T extends Tool` interns its own copy of the default application"

            let arguments = tools |> List.map (fun facts -> facts.DeclarationArguments |> List.map _.TypeId) |> List.distinct
            Expect.equal arguments.Length 1 "every copy carries the same alias arguments"
            Expect.equal arguments.Head.Length 2 "the arguments are the alias's two defaults"
        }
        |> Async.RunSynchronously
