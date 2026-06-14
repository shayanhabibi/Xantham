module ObjectFlags

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
    // ----------------------------------------------------------------------------------------------
    //                                  OF - OBJECT FLAGS
    // ----------------------------------------------------------------------------------------------
    runner.testSuite "OF · Object Flags" <| fun _ ->
        runner.testCase "OF-1 · Class/Interface object types with Reference have typars or thisType" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.Reference)
            |> Array.iter (fun typ ->
                match typ.objectFlags with
                | flags when flags.HasFlag(Ts.ObjectFlags.Interface) || flags.HasFlag(Ts.ObjectFlags.Class) ->
                    
                    typ :?> Ts.InterfaceType
                    |> _.typeParameters
                    |> Option.exists (_.AsArray >> Array.isEmpty >> not)
                    |> (||) (typ :?> Ts.InterfaceType |> _.thisType.IsSome)
                    |> Flip.Expect.isTrue $"{ctx.Checker.typeToString typ}"
                | _ -> ()
                )
        runner.testCase "OF-2 · Class/Interface object types without Reference have no typars or thisType" <| fun test ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter (_.objectFlags.HasFlag(Ts.ObjectFlags.Reference) >> not)
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.ClassOrInterface)
            |> function
                | [||] -> test.skip()
                | typs ->
                    typs
                    |> Array.iter (fun typ ->
                        typ :?> Ts.InterfaceType
                        |> _.typeParameters.IsNone
                        |> (&&) (typ :?> Ts.InterfaceType |> _.thisType.IsNone)
                        |> Flip.Expect.isTrue "If not a reference but is a class/interface, then has no typars"
                        )
        runner.testCase "OF-3 · ObjectFlags are mutually exclusive" <| fun _ ctx ->
            let exclusiveFlags = [
                Ts.ObjectFlags.Class
                Ts.ObjectFlags.Interface
                Ts.ObjectFlags.Tuple
                Ts.ObjectFlags.Mapped
                Ts.ObjectFlags.ReverseMapped
                Ts.ObjectFlags.EvolvingArray
                Ts.ObjectFlags.InstantiationExpressionType
                Ts.ObjectFlags.SingleSignatureType
            ]
            ctx.NodeMap.Values
            |> Seq.collect _.AsArray
            |> Seq.toArray
            |> unbox<Ts.Node array>
            |> Array.filter ts.isTypeNode
            |> Array.map (unbox<Ts.TypeNode> >> ctx.Checker.getTypeFromTypeNode)
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> Array.iter (fun typ ->
                let objectFlags =
                    typ :?> Ts.ObjectType
                    |> _.objectFlags
                exclusiveFlags
                |> List.filter objectFlags.HasFlag
                |> function
                    | [] | [ _ ] -> ()
                    | l -> failtest $"Expected no conflicting exclusive flags for ObjectTypes, but got %A{l}"
                )
        runner.testCase "OF-4 · Tuple object types never occur without the Reference flag" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter (_.objectFlags.HasFlag(Ts.ObjectFlags.Reference) >> not)
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.Tuple)
            |> Flip.Expect.isEmpty "Expected no Tuple Types without Reference flag"
        runner.testCase "OF-5 · ObjectFlags exclusive/inclusive map holds over the corpus" <| fun _ ctx ->
            let flags = [
                Ts.ObjectFlags.Class, "Class"
                Ts.ObjectFlags.Interface, "Interface"
                Ts.ObjectFlags.Reference, "Reference"
                Ts.ObjectFlags.Tuple, "Tuple"
                Ts.ObjectFlags.Anonymous, "Anonymous"
                Ts.ObjectFlags.Mapped, "Mapped"
                Ts.ObjectFlags.Instantiated, "Instantiated"
                Ts.ObjectFlags.ObjectLiteral, "ObjectLiteral"
                Ts.ObjectFlags.EvolvingArray, "EvolvingArray"
                Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties, "ObjectLiteralPatternWithComputedProperties"
                Ts.ObjectFlags.ReverseMapped, "ReverseMapped"
                Ts.ObjectFlags.JsxAttributes, "JsxAttributes"
                Ts.ObjectFlags.JSLiteral, "JSLiteral"
                Ts.ObjectFlags.FreshLiteral, "FreshLiteral"
                Ts.ObjectFlags.ArrayLiteral, "ArrayLiteral"
                Ts.ObjectFlags.SingleSignatureType, "SingleSignatureType"
                Ts.ObjectFlags.ClassOrInterface, "ClassOrInterface"
                Ts.ObjectFlags.ContainsSpread, "ContainsSpread"
                Ts.ObjectFlags.ObjectRestType, "ObjectRestType"
                Ts.ObjectFlags.InstantiationExpressionType, "InstantiationExpressionType"
            ]
            let flagTracker = flags |> List.map (fun (flag, _) -> KeyValuePair(flag, enum<Ts.ObjectFlags> 0)) |> Dictionary
            let registerFlags (input: Ts.ObjectFlags) =
                for flag, _ in flags do
                    if input.HasFlag(flag) then
                        flagTracker[flag] <- flagTracker[flag] ||| input
            let getName flag = flags |> List.find (fst >> (=) flag) |> snd
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.iter (_.objectFlags >> registerFlags)
            let flagMap = Map [
                    for kv in flagTracker do
                        let flagName = getName kv.Key
                        kv.Key,
                        flags
                        |> List.filter (fst >> kv.Value.HasFlag >> not)
                        |> List.map (snd >> sprintf "    Ts.ObjectFlags.%s")
                        |> String.concat "\n"
                        |> sprintf "Ts.ObjectFlags.%s, [\n%s\n]" flagName
            ]
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.iter (fun typ ->
                let objectFlags = typ.objectFlags
                flags
                |> List.filter (fst >> objectFlags.HasFlag)
                |> List.filter (fst >> Map.find >> funApply Spec.ObjectFlags.exclusiveMasks >> (&&&) objectFlags >> (<>) (enum 0))
                |> List.map (fst >> fun key -> flagMap[key])
                |> function
                    | [] -> ()
                    | incorrectMaskMaps ->
                    let typeFlags =
                        flags
                        |> List.filter (fst >> objectFlags.HasFlag)
                        |> List.map snd
                    incorrectMaskMaps
                    |> Flip.Expect.isEmpty (String.concat "\n" incorrectMaskMaps |> sprintf "%A got a different exclusive objectflag map:\n%s" typeFlags)
                )
