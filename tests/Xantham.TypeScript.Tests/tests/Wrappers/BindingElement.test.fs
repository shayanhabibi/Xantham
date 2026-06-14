module BindingElementWrapper
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

let arrayBindingPattern (runner: Spec.RunnerContext) : unit = runner.testSuite "ArrayBindingPattern" <| fun _ ->
    runner.testSuite "Node" <| fun _ ->
        runner.testSyntaxKind<Ts.ArrayBindingPattern> Ts.SyntaxKind.ArrayBindingPattern "parent is ParameterDeclaration or BindingElement" <| fun ctx _ ->
            projectedForEach _.parent (
                focus (snd >> unbox<Ts.Node> >> _.kind)
                >> assertion (function
                    | Ts.SyntaxKind.BindingElement
                    | Ts.SyntaxKind.Parameter -> true
                    | _ -> false
                    ) (fun parentKind -> $"Expected parent of a ObjectBindingPattern to be a binding element or parameter, not {parentKind.Name}")
                )
        

let objectBindingPattern (runner: Spec.RunnerContext) : unit = runner.testSuite "ObjectBindingPattern" <| fun _ ->
    runner.testSuite "Node" <| fun _ ->
        runner.testSyntaxKind<Ts.ObjectBindingPattern> Ts.SyntaxKind.ObjectBindingPattern "parent is ParameterDeclaration or BindingElement" <| fun ctx _ ->
            projectedForEach _.parent (
                focus (snd >> unbox<Ts.Node> >> _.kind)
                >> assertion (function
                    | Ts.SyntaxKind.BindingElement
                    | Ts.SyntaxKind.Parameter -> true
                    | _ -> false
                    ) (fun parentKind -> $"Expected parent of a ObjectBindingPattern to be a binding element or parameter, not {parentKind.Name}")
                )

let tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Binding Element" <| fun _ ->
    objectBindingPattern runner
    arrayBindingPattern runner
    runner.testSuite "Nodes" <| fun _ -> 
        runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "parent either objectbindingpattern or arraybinding pattern" <| fun ctx _ ->
            foreach (
                tag "BindingElement"
                >> tag "parent"
                >> focus _.parent
                >> inside (unbox<Ts.Node> >> _.kind) (
                    assertion (function
                        | Ts.SyntaxKind.ObjectBindingPattern | Ts.SyntaxKind.ArrayBindingPattern -> true
                        | _ -> false
                        ) (fun parentKind -> $"Expected parent of a BindingElement to be an ObjectBindingPattern or ArrayBindingPattern, not {parentKind.Name}")
                    )
                )
        runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "binding element never has an initializer" <| fun ctx _ ->
            foreach (
                tag "BindingElement"
                >> inside _.initializer (tag "initializer" >> Option.isNone)
                )
        runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "binding element can have a dotdotdottoken" <| fun ctx _ ->
            assertThat (
                tag "BindingElement"
                >> exists _.dotDotDotToken.IsSome
                >> skipIfError
                )
        runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "binding element can have name as patterns" <| fun ctx _ ->
            foreach (
                tag "BindingElement"
                >> inside _.name (
                    branchInside3
                        Patterns.Node.BindingNamePatterns.(|Identifier|ObjectBindingPattern|ArrayBindingPattern|)
                        (focus _.text >> isNotNull)
                        (
                            focus (_.parent >> unbox<Ts.BindingElement> >> _.propertyName)
                            >> Option.value
                            >> branchInside7
                                Patterns.Node.PropertyNamePatterns.(|Identifier|StringLiteral|NoSubstitutionTemplateLiteral|NumericLiteral|ComputedPropertyName|PrivateIdentifier|BigIntLiteral|)
                                (focus _.text >> isNotNull)
                                id
                                id
                                id
                                id
                                id
                                id
                        )
                        id
                    )
                )
        runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "binding element" <| fun ctx _ ->
            foreach (
                tag "BindingElement"
                >> focus (ctx.Checker.getTypeAtLocation >> Type.Kind.create ctx.Program)
                >> apply (function
                    | Kind.Structural (Structural.Union union) ->
                        union.Value |> union.Value.checker.typeToString |> printfn "%s"
                        Type.Union.types union |> NonEmptyArray.map _.ToString() |> printfn "%A\n"
                    | _ -> ()
                    )
                )
        // TODO - ??
        // runner.testSyntaxKind<Ts.BindingElement> Ts.SyntaxKind.BindingElement "binding element" <| fun ctx _ ->
        //     foreach (
        //         tag "BindingElement"
        //         >> projectedBranchInside3 (_.name >> Patterns.Node.BindingNamePatterns.(|Identifier|ObjectBindingPattern|ArrayBindingPattern|))
        //             (
        //                 inside (snd >> _.text) isNotNull
        //                 >> inside (fun (a,b) ->
        //                     match a.propertyName with
        //                     | None -> true
        //                     | Some t ->
        //                         if not (b.text = t?text) then
        //                             printfn "%A" (Utils.inspect a)
        //                             false
        //                         else true
        //                     ) isTrue
        //             )
        //             (inside (fst >> _.propertyName) Option.isSome)
        //             (inside (fst >> _.propertyName) Option.isSome)
        //         )
        