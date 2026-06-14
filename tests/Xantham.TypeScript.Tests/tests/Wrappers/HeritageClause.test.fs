module HeritageClauseWrapper
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

let inline parent (_: Spec.SuiteContext): Assertion<Ts.HeritageClause, _> =
    withTag "parent" <| assertion (
        fun clause ->
            clause.parent |> unbox<Ts.Node>
            |> _.kind
            |> function
                | Ts.SyntaxKind.InterfaceDeclaration -> true
                | Ts.SyntaxKind.ClassDeclaration -> true
                | _ -> false
        )
        (fun clause ->
            let parentKind = clause.parent |> unbox<Ts.Node> |> _.kind.Name
            sprintf "HeritageClause parent should be an InterfaceDeclaration or a ClassDeclaration, not %s\n\n%s" parentKind (Utils.inspect clause.parent)
        )
    

let inline token (_: Spec.SuiteContext): Assertion<Ts.HeritageClause, _> =
    withTag "token" <|
    projectedAssertion _.token
        (
            function
            | Ts.SyntaxKind.ImplementsKeyword 
            | Ts.SyntaxKind.ExtendsKeyword -> true
            | _ -> false
        )
        (fun _ -> _.Name >> sprintf "HeritageClause token should be an ExtendsKeyword, not %s")


let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "HeritageClause" <| fun _ ->
    runner.testSuite "Nodes" <| fun _ ->
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "Parent and Token are predictable" <| fun ctx _ nodes ->
            nodes
            |> foreach (
                tag "HeritageClause"
                >> parent ctx
                >> token ctx
                )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "Types is predictable" <| fun ctx _ nodes ->
            nodes
            |> assertThat (
                // For a class, it can only have one extends clause
                foreach (
                    tag "HeritageClause"
                    >> ifTrueThen (fun clause ->
                        clause.token.Equals(Ts.SyntaxKind.ExtendsKeyword)
                        && (clause.parent |> unbox<Ts.Node> |> _.kind.Equals(Ts.SyntaxKind.ClassDeclaration))
                        ) (projectedAssertion _.types (Seq.length >> (>) 2) (fun node col -> node.getFullText()))
                    )
                // An interface can extend multiple types
                >> exists (fun clause ->
                    clause.token.Equals(Ts.SyntaxKind.ExtendsKeyword)
                    && (clause.parent |> unbox<Ts.Node> |> _.kind.Equals(Ts.SyntaxKind.InterfaceDeclaration))
                    && clause.types.AsArray.Length > 1
                    )
                // An interface cannot have implements clauses
                >> not'(exists (fun clause ->
                    clause.token.Equals(Ts.SyntaxKind.ImplementsKeyword)
                    && (clause.parent |> unbox<Ts.Node> |> _.kind.Equals(Ts.SyntaxKind.InterfaceDeclaration))
                    ))
                // Heritage clause types can have type arguments
                >> exists (fun clause ->
                    clause.types.AsArray
                    |> Array.exists (_.typeArguments >> Option.bind NonEmptyArray.create >> Core.Option.isSome)
                    )
                >> foreach (
                    // always has at least one type argument
                    focus _.types.AsArray
                    >> isNotEmpty
                    )
                )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "Class heritage implements can be > 1" <| fun ctx _ ->
            assertThat (
                exists (fun clause ->
                    clause.token.Equals(Ts.SyntaxKind.ImplementsKeyword)
                    && (clause.parent |> unbox<Ts.Node> |> _.kind.Equals(Ts.SyntaxKind.ClassDeclaration))
                    && clause.types.AsArray.Length > 1
                )
                >> skipIfError
            )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "clauses with invalid types default to any" <| fun ctx _ ->
            Array.choose (
                _.types
                >> NonEmptyArray.create
                >> Option.bind (
                    NonEmptyArray.map ctx.Checker.getTypeFromTypeNode
                    >> NonEmptyArray.filter _.flags.Equals(Ts.TypeFlags.Any)
                    )
                )
            >> Array.collect _.Values
            >> foreach (
                tag "HeritageClause.types"
                >> ifTrueThenOrElse 
                    _.getCanonicalSymbol().IsSome
                    (
                        inside _.getCanonicalSymbol().Value (
                            tag "symbol"
                            >> apply (Utils.inspect >> printfn "%s")
                            )
                    )
                    (
                        apply (Utils.inspect >> printfn "%s")
                        >> apply (fun typ -> typ :?> Ts.ObjectType |> _.objectFlags.ToStringArray() |> printfn "%A")
                        >> focus (fun typ -> typ?intrinsicName : string option)
                        >> Option.value
                        >> isEqualTo "error"
                    )
                )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "can wrap clause types successfully" <| fun ctx _ ->
            Array.filter (_.types >> NonEmptyArray.create >> _.IsSome)
            >> foreach (
                tag "HeritageClause"
                >> tag "types"
                >> focus (_.types >> NonEmptyArray.create)
                >> Option.value
                >> focus _.Values
                >> foreach (
                    focus (Node.ExpressionWithTypeArguments.create ctx.Program)
                    >> (inside (fun expr -> fun () -> Node.ExpressionWithTypeArguments.getType expr |> ignore) (tag "getType" >> doesNotThrow) |> forceError)
                    >> inside
                           (fun expr -> fun () -> expr.Value |> Type.Kind.tryCreateFromNode ctx.Program |> ignore)
                           (tag "Type.Kind.tryCreateFromNode" >> doesNotThrow |> forceError)
                    >> focus (_.Value >> Type.Kind.tryCreateFromNode ctx.Program)
                    )
                )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "Sometimes types are unresolved/errors" <| fun ctx _ ->
            Array.filter (_.types >> NonEmptyArray.create >> _.IsSome)
            >> foreach (
                tag "HeritageClause"
                >> tag "types"
                >> focus (_.types >> NonEmptyArray.create)
                >> Option.value
                >> focus _.Values
                >> foreach (
                    focus (
                        Node.ExpressionWithTypeArguments.create ctx.Program
                        >> _.Value
                        >> Type.Kind.tryCreateFromNode ctx.Program
                        )
                    >> Result.isOk
                    >> skipIfError
                    )
                )
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "clause types are structural when not errors" <| fun ctx _ ->
            Array.choose (_.types >> NonEmptyArray.create)
            >> Array.collect _.Values
            >> foreach (
                tag "HeritageClause"
                >> projectedAssertion (Type.Kind.tryCreateFromNode ctx.Program >> Result.toOption) (Option.map _.IsStructural >> Option.defaultValue true) (
                    fun _ maybeType ->
                        maybeType
                        |> Option.map Type.Kind.toType 
                        |> Utils.inspect
                    ) |> forceError
                >> focus (Type.Kind.tryCreateFromNode ctx.Program)
                >> branchInsideResult
                    (
                        DU.ofCase "Structural" (function Kind.Structural structural -> Some structural | _ -> None)
                        >> apply (printfn "%A")
                    )
                    id
                )
        runner.testCase "HeritageClauses on declarations is either none, 1 or 2 (if class)" <| fun _ ctx ->
            ctx.Nodes
            |> foreach (
                branchInsideFor
                    (function
                        | Patterns.Node.InterfaceDeclaration node -> Choice1Of2 node |> Some
                        | Patterns.Node.ClassDeclaration node -> Choice2Of2 node |> Some
                        | _ -> None)
                    (
                        tag "Interface"
                        >> focus (_.heritageClauses >> Option.map _.AsArray >> Option.defaultValue [||])
                        >> hasLengthLessOrEqual 1
                    )
                    (
                        tag "Class"
                        >> focus (_.heritageClauses >> Option.map _.AsArray >> Option.defaultValue [||])
                        >> hasLengthLessOrEqual 2
                    )
                )
    runner.testSuite "Node.HeritageClause" <| fun _ ->
        runner.testSyntaxKind<Ts.HeritageClause> Ts.SyntaxKind.HeritageClause "Safely wrapped by Node.HeritageClause" <| fun ctx _ ->
            foreach (
                inside (fun node -> fun () -> Node.HeritageClause.create ctx.Program node |> ignore) doesNotThrow
                |> withTag "Node.HeritageClause.create"
                >> focus (Node.HeritageClause.create ctx.Program)
                >> tag "Node.HeritageClause"
                >> inside (fun node -> fun () -> Node.HeritageClause.types node |> ignore) doesNotThrow |> withTag "types"
                >> inside (fun node -> fun () -> Node.HeritageClause.heritageClause node |> ignore) doesNotThrow |> withTag "heritageClause"
                >> satisfy (fun node ->
                    if Node.HeritageClause.parentIsClass node then
                        not <| Node.HeritageClause.parentIsInterface node
                    else Node.HeritageClause.parentIsInterface node)
                >> ifTrueThenOrElse
                       Node.HeritageClause.parentIsClass
                       (tag "classLikeHeritageClause" >> focus Node.ClassLikeHeritageClause.tryFromHeritageClause >> Option.isSome)
                       (tag "typeHeritageClause" >> focus Node.TypeHeritageClause.tryFromHeritageClause >> Option.isSome)
                )
    runner.testSuite "Node.TypeHeritageClause" <| fun _ ->
        runner.testSyntaxKind<Ts.InterfaceDeclaration> Ts.SyntaxKind.InterfaceDeclaration "Can get type heritage clauses" <| fun ctx _ ->
            Array.choose (_.heritageClauses >> Option.bind NonEmptyArray.create)
            >> foreach (
                inside (fun clauses -> fun () -> NonEmptyArray.map (Node.HeritageClause.create ctx.Program) clauses |> ignore) doesNotThrow
                |> forceError
                >> focus (NonEmptyArray.map (Node.HeritageClause.create ctx.Program) >> Node.TypeHeritageClause.tryFromHeritageClauses)
                >> Option.value
                )
    runner.testSuite "Node.ClassLikeHeritageClause" <| fun _ ->
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Can get class like heritage clauses" <| fun ctx _ ->
            Array.choose (_.heritageClauses >> Option.bind NonEmptyArray.create)
            >> assertThat (
                foreach (
                    inside (fun clauses -> fun () -> NonEmptyArray.map (Node.HeritageClause.create ctx.Program) clauses |> ignore) doesNotThrow
                    |> forceError
                    >> focus (NonEmptyArray.map (Node.HeritageClause.create ctx.Program) >> Node.ClassLikeHeritageClause.tryFromHeritageClauses)
                    >> Option.value
                    )
                |> forceError
                )
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Examples of implements and extends co-occuring" <| fun ctx _ ->
                Array.choose (_.heritageClauses >> Option.bind NonEmptyArray.create)
                >> assertThat (
                        exists (
                            NonEmptyArray.map (Node.HeritageClause.create ctx.Program)
                            >> Node.ClassLikeHeritageClause.tryFromHeritageClauses
                            >> Option.exists _.IsImplementsAndExtends
                            )
                        >> skipIfError
                    )
