module TypeChecker

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

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "TC · Type Checker Resolution" <| fun _ ->
    runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration
        "TC-1 · ClassDeclarations resolved by type checker are always Class object types" <| fun ctx _ nodes ->
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
    runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration
        "TC-2 · InterfaceDeclarations resolved by type checker are always ClassOrInterface object types" <| fun ctx _ nodes ->
        nodes
        |> Array.iter (fun node ->
            let objectType = node.name |> ctx.Checker.getTypeAtLocation :?> Ts.ObjectType
            let result = objectType.objectFlags &&& Ts.ObjectFlags.ClassOrInterface |> (<>) (enum 0)
            let flags = objectType.objectFlags.ToStringArray()
            Expect.isTrue result $"InterfaceDeclaration (except Iterator): {node.name.getText()} should be an object type. Has %A{flags}"
            )
    runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration
        "TC-3 · MethodDeclarations that are not optional resolved by type checker are object types" <| fun ctx _ nodes ->
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
    runner.testSyntaxKind<Ts.MethodDeclaration> Ts.SyntaxKind.MethodDeclaration
        "TC-4 · MethodDeclarations that are optional resolved by type checker are union types" <| fun ctx _ nodes ->
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
    runner.testSyntaxKind<Ts.MethodSignature> Ts.SyntaxKind.MethodSignature
        "TC-5 · MethodSignature that are not optional resolved by type checker are object types" <| fun ctx _ nodes ->
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
    runner.testSyntaxKind<Ts.MethodSignature> Ts.SyntaxKind.MethodSignature
        "TC-6 · MethodSignature that are optional resolved by type checker are union types" <| fun ctx _ nodes ->
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
    runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator
        "TC-7 · TypeOperators have no symbol" <| fun ctx _ nodes ->
        nodes |> Array.choose ctx.Checker.getSymbolAtLocation
        |> Expect.isEmpty
        |> funApply "Expected no symbols for TypeOperators"
    runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator
        "TC-8 · TypeOperators have types associated; (=) inner type if operator is readonly" <| fun ctx _ nodes ->
        let nodes = nodes |> Array.filter (_.operator >> function Ts.SyntaxKind.ReadonlyKeyword -> true | _ -> false)
        (nodes |> Array.map ctx.Checker.getTypeFromTypeNode
        ,nodes |> Array.map (_.``type`` >> ctx.Checker.getTypeFromTypeNode))
        ||> Array.iter2 (fun a b ->
            let aString = ctx.Checker.typeToString a
            let bString = ctx.Checker.typeToString b
            if a <> b then Testing.Assert.NotEqual(aString, bString))
    runner.testSyntaxKind<Ts.TypeOperatorNode> Ts.SyntaxKind.TypeOperator
        "TC-9 · TypeOperators have types associated; <> inner type if operator <> readonly" <| fun ctx _ nodes ->
        let nodes = nodes |> Array.filter (_.operator >> function Ts.SyntaxKind.ReadonlyKeyword -> false | _ -> true)
        (nodes |> Array.map ctx.Checker.getTypeFromTypeNode
        ,nodes |> Array.map (_.``type`` >> ctx.Checker.getTypeFromTypeNode))
        ||> Array.iter2 (fun a b ->
            let aString = ctx.Checker.typeToString a
            let bString = ctx.Checker.typeToString b
            if a = b then Testing.Assert.AreEqual(aString, bString))
    runner.testCase "TC-10 · All TypeNodes resolve to a Type via the checker" <| fun _ ctx ->
        ctx.NodeMap.Values
        |> Seq.collect _.AsArray
        |> Seq.toArray
        |> unbox<Ts.Node array>
        |> Array.filter ts.isTypeNode
        |> Array.map (unbox<Ts.TypeNode> >> ctx.Checker.getTypeFromTypeNode >> Option.ofObj)
        |> Array.iter (Flip.Expect.isSome "Types are some")
