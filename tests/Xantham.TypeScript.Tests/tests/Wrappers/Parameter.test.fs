module ParameterWrapper
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

let tests (runner: Spec.RunnerContext) : unit = runner.testSuite "ParameterDeclaration" <| fun _ ->
    let inline testSyntaxKind name = runner.testSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter name
    let inline ftestSyntaxKind name = runner.ftestSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter name
    let inline ptestSyntaxKind name = runner.ptestSyntaxKind<Ts.ParameterDeclaration> Ts.SyntaxKind.Parameter name
    testSyntaxKind "binding name varies" <| fun ctx _ ->
        assertThat (
            tag "name"
            >> tag "identifier"
            >> exists (_.name >> unbox<Ts.Node> >> _.kind >> function
                | Ts.SyntaxKind.Identifier -> true
                | _ -> false
                )
            >> popTag >> tag "objectBindingPattern/arrayBindingPattern"
            >> exists (_.name >> unbox<Ts.Node> >> _.kind >> function
                    | Ts.SyntaxKind.ObjectBindingPattern -> true
                    | Ts.SyntaxKind.ArrayBindingPattern -> true
                    | _ -> false
                )
            >> skipIfError
        )
    testSyntaxKind "dotdotdottoken can be present, but is not always present" <| fun ctx _ ->
        assertThat (
            tag "dotdotdottoken"
            >> exists _.dotDotDotToken.IsSome
            >> exists _.dotDotDotToken.IsNone
            )
    testSyntaxKind "initializer is always none" <| fun ctx _ -> foreach (focus _.initializer >> Option.isNone)
    testSyntaxKind "has symbol" <| fun ctx _ -> foreach (
        tag "symbol"
        // checker doesnt yield symbol
        >> inside ctx.Checker.getSymbolAtLocation Option.isNone
        // always embedded
        >> inside (fun node -> node?symbol : Ts.Symbol option) Option.isSome
        // checker yields if name is identifier
        >> projectedInside (_.name >> unbox<Ts.Node>) (
            ifTrueThenOrElse
                (snd >> _.kind.Equals(Ts.SyntaxKind.Identifier))
                (
                    // yields symbol if identifier
                    inside (snd >> ctx.Checker.getSymbolAtLocation) Option.isSome
                    // yielded symbol same as embedded symbol
                    >> assertion (fun (node,name) ->
                           match ctx.Checker.getSymbolAtLocation name with
                           | Some symbol -> symbol.Equals(node?symbol)
                           | None -> false
                           )
                           (fun _ -> "")
                )
                (inside (snd >> ctx.Checker.getSymbolAtLocation) Option.isNone)
            )
        )
    testSyntaxKind "has type node" <| fun ctx _ ->
        foreach (
            tag "type"
            >> inside _.``type`` (
                // type node always present
                Option.value
                // can be parsed into node.type
                >> inside (fun node -> fun () -> Node.Type.create ctx.Program node |> ignore) doesNotThrow
                )
            )
    testSyntaxKind "symbol kind" <| fun ctx _ ->
        foreach (
            tag "symbol"
            >> inside _.name (
                tag "name"
                >> branchInside3
                       Patterns.Node.BindingNamePatterns.(|Identifier|ObjectBindingPattern|ArrayBindingPattern|)
                       (
                           tag "identifier"
                           >> focus ctx.Checker.getSymbolAtLocation
                           >> Option.value
                           // symbol has a parameter declaration
                           >> inside _.declarations (
                               Option.value
                               >> focus _.AsArray
                               >> exists _.kind.Equals(Ts.SyntaxKind.Parameter)
                               )
                           // can wrap into a symbol.kind
                           >> inside (fun node -> fun () -> Symbol.Kind.create ctx.Program node |> ignore) doesNotThrow
                           >> focus (Symbol.Kind.create ctx.Program)
                           // symbol kind came up as typeparameter in one case. This is still valid - it interfaces with IParameter
                           // >> assertion _.IsParameter (_.ToString() >> sprintf "Expected symbol kind to be Parameter, not %s")
                       )
                       (
                           tag "objectBindingPattern"
                           >> inside ctx.Checker.getSymbolAtLocation Option.isNone 
                           >> focus (_.parent >> fun node -> node?symbol : Ts.Symbol option)
                           >> Option.value
                           >> focus (ISymbol.create ctx.Program >> ISymbol.toSymbol)
                           >> inside (Symbol.Kind.create ctx.Program) (
                               assertion _.IsParameter (_.ToString() >> sprintf "Expected symbol kind to be Parameter, not %s")
                               )
                           // symbol name is a positional argument
                           >> inside _.symbolName (
                               fun symbolName ->
                                   $"Expected symbol name to be positional argument, not {symbolName.ToString()}"
                               |> assertion (function
                                   | SymbolName.String txt ->
                                       txt.StartsWith("__")
                                       && txt[2..] |> String.forall Char.IsDigit
                                   | _ -> false
                                       )
                               )
                       )
                       (
                           tag "arrayBindingPattern"
                           >> inside ctx.Checker.getSymbolAtLocation Option.isNone
                           >> focus (_.parent >> fun node -> node?symbol : Ts.Symbol option)
                           >> Option.value
                           >> focus (ISymbol.create ctx.Program >> ISymbol.toSymbol)
                           >> inside (Symbol.Kind.create ctx.Program) (
                               assertion _.IsParameter (_.ToString() >> sprintf "Expected symbol kind to be Parameter, not %s")
                               )
                           // symbol name is a positional argument
                           >> inside _.symbolName (
                               fun symbolName ->
                                   $"Expected symbol name to be positional argument, not {symbolName.ToString()}"
                               |> assertion (function
                                   | SymbolName.String txt ->
                                       txt.StartsWith("__")
                                       && txt[2..] |> String.forall Char.IsDigit
                                   | _ -> false
                                       )
                               )
                       )
                )
            )
    testSyntaxKind "tracer" <| fun ctx _ ->
        assertThat (
            foreach (
                tag "tracer"
                // can create tracer
                >> inside (fun node -> fun () -> NodeParameterTracer.create ctx.Program node |> ignore) doesNotThrow
                    |> forceError
                    |> withTag "create"
                >> focus (NodeParameterTracer.create ctx.Program)
                // can access parent
                >> inside (fun tracer -> fun () -> NodeParameterTracer.parent tracer |> ignore) doesNotThrow
                    |> withTag "parent"
                ) |> forceError
            // transform assertion subject to be premade array of tracers
            >> focus (Array.map (NodeParameterTracer.create ctx.Program))
            >> tag "bindingPattern"
            // run through tracer functions to see that things work as expected
            >> exists NodeParameterTracer.isBindingPattern
            >> exists (NodeParameterTracer.isBindingPattern >> not)
            >> popTag >> tag "isRestParameter"
            >> exists NodeParameterTracer.isRestParameter 
            >> exists (NodeParameterTracer.isRestParameter >> not) 
            >> popTag >> tag "nameOrNone/nameOrPosition"
            >> exists (NodeParameterTracer.nameOrNone >> _.IsNone) 
            >> exists (NodeParameterTracer.nameOrNone >> _.IsSome) 
            >> exists (NodeParameterTracer.nameOrPosition >> _.IsChoice1Of2) 
            >> exists (NodeParameterTracer.nameOrPosition >> _.IsChoice2Of2)
            >> popTag
            >> foreach (
                forceError (
                    inside (fun tracer -> fun () -> NodeParameterTracer.typeNode tracer |> ignore) doesNotThrow |> withTag "typeNode"
                    >> inside (fun tracer -> fun () -> NodeParameterTracer.type_ tracer |> ignore) doesNotThrow |> withTag "type_"
                    >> inside (fun tracer -> fun () -> NodeParameterTracer.symbol tracer |> ignore) doesNotThrow |> withTag "symbol"
                )
                >> inside NodeParameterTracer.symbol (
                    focus NarrowedTracer.value
                    >> inside Symbol.tryParameterDeclaration Option.isSome
                    >> inside Symbol.isTransient isFalse
                    )
                )
        )
    let parentKinds = HashSet<string>()
    afterTests "Print parent kinds" <| fun _ ->
        if parentKinds.Count = 0 then () else
        parentKinds
        |> Seq.sort
        |> Seq.toArray
        |> printfn "ParameterDeclaration.parent missing kinds: %A"
    testSyntaxKind "parent kind printer" <| fun ctx _ ->
        foreach (
            tag "parent"
            >> focus _.parent
            >> ifTrueThen
                (unbox<Ts.Declaration> >> _.kind >> function
                    | Ts.SyntaxKind.ConstructSignature
                    | Ts.SyntaxKind.IndexSignature
                    | Ts.SyntaxKind.SetAccessor
                    | Ts.SyntaxKind.GetAccessor
                    | Ts.SyntaxKind.Constructor
                    | Ts.SyntaxKind.MethodDeclaration
                    | Ts.SyntaxKind.FunctionDeclaration
                    | Ts.SyntaxKind.MethodSignature
                    | Ts.SyntaxKind.CallSignature 
                    | Ts.SyntaxKind.FunctionType 
                    | Ts.SyntaxKind.ConstructorType -> false
                    | _ -> true)
                (apply (unbox<Ts.Declaration> >> _.kind.Name >> parentKinds.Add >> ignore))
            )
    
        
        
