module ClassWrapper
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

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Class" <| fun _ ->
    runner.testSuite "Nodes" <| fun _ ->
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Not all have names" <| fun ctx _ nodes ->
            nodes
            |> Array.filter _.name.IsNone
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Parsed into DeclarationKind" <| fun ctx _ nodes ->
            nodes
            |> Array.map (Node.DeclarationKind.create ctx.Program)
            |> Flip.Expect.all _.IsClass ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Can have typars" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (unbox >> ts.getEffectiveTypeParameterDeclarations >> NonEmptyArray.create >> _.IsSome)
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Can have modifiers" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (_.modifiers >> Option.bind NonEmptyArray.create)
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Can have heritage clauses" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (fun node -> node.heritageClauses |> Option.bind NonEmptyArray.create )
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "does not always have heritage clauses" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.exists (_.heritageClauses >> Option.bind NonEmptyArray.create >> _.IsNone) ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "can have members" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.exists (_.members >> NonEmptyArray.create >> _.IsSome) ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "does not always have members" <| fun ctx _ nodes ->
            nodes
            |> Array.filter (_.members >> NonEmptyArray.create >> _.IsNone)
            |> Flip.Expect.skipIfEmpty
        runner.testSuite "Global Member Checks" <| fun ctx ->
            let inline (==>) a b = KeyValuePair(a,b)
            let kinds = Dictionary<Ts.SyntaxKind, int> [
                Ts.SyntaxKind.PropertyDeclaration ==> 0
                Ts.SyntaxKind.GetAccessor ==> 0
                Ts.SyntaxKind.SetAccessor ==> 0
                Ts.SyntaxKind.Constructor ==> 0
                Ts.SyntaxKind.MethodDeclaration ==> 0
            ]
            runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Members are predictable" <| fun ctx _ nodes ->
                nodes
                |> Array.collect _.members.AsArray
                |> assertThat (
                    all <| fun node ->
                        match kinds.TryGetValue(node.kind) with
                        | true, value ->
                            kinds[node.kind] <- value + 1
                            true
                        | _ -> false
                    )
            afterTests "Check PropertyKinds" <| fun _ ->
                kinds
                |> Seq.iter (fun kv ->
                    if kv.Value = 0 then
                        failwithf "Expected at least one %s" kv.Key.Name
                )
            let symbolKinds = HashSet<string>()
            runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Symbol declared type is always class even when symbol is not" <| fun ctx _ nodes ->
                nodes
                |> Array.filter _.name.IsSome
                |> Array.map (
                    _.name.Value
                    >> ctx.Checker.getSymbolAtLocation
                    >> Option.get
                    >> ISymbol.create ctx.Program
                    )
                |> projectedForEach
                    (
                        fun isymbol ->
                            isymbol
                            |> ISymbol.toSymbol
                            |> Symbol.Kind.create ctx.Program
                            |> fun kind ->
                                Symbol.foldToTransientKind kind |> function
                                    | Transient.Kind.Class _ -> ()
                                    | _ -> kind |> sprintf "%A" |> symbolKinds.Add |> ignore
                            isymbol
                        >> ISymbol.declaredType
                        >> Type.Kind.create ctx.Program
                    )(
                        assertion
                            (snd >> function
                            | Kind.Structural (Structural.Class _ | Structural.TypeReference (TypeReference.Class _)) -> true
                            | _ -> false)
                            (sprintf "Expected a class type. Got: %A")
                    )
            afterTests "Check SymbolKinds" <| fun _ ->
                symbolKinds
                |> assertThat (assertion (Seq.isEmpty >> not) (fun _ -> "Expected some symbol kinds other than class. Got none"))
    runner.testSuite "Type" <| fun _ ->
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Type is always a class like type" <| fun ctx _ nodes ->
            nodes
            |> Array.map (ctx.Checker.getTypeAtLocation >> Type.Kind.create ctx.Program)
            |> Flip.Expect.all (function
                | Kind.Structural (
                    Structural.TypeReference (
                        TypeReference.Class _
                        )
                  | Structural.Class _
                    ) -> true
                | x ->
                    printfn "Unexpected type: %A" x
                    false
                ) ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Reference like type always has type arguments" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.all (fun node ->
                let typ = ctx.Checker.getTypeAtLocation node |> Type.Kind.create ctx.Program
                match typ with
                | Kind.Structural (Structural.TypeReference (TypeReference.Class x)) ->
                    x.Value
                    |> ctx.Checker.getTypeArguments
                    |> NonEmptyArray.create
                    |> Core.Option.isSome
                | _ -> true
                ) ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Reference like type always has type parameters" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.all (fun node ->
                let typ = ctx.Checker.getTypeAtLocation node |> Type.Kind.create ctx.Program
                match typ with
                | Kind.Structural (Structural.TypeReference (TypeReference.Class _)) ->
                    ts.getEffectiveTypeParameterDeclarations !!node
                    |> NonEmptyArray.create
                    |> Core.Option.isSome
                | _ -> true
                ) ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Non Reference like type never has type parameters" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.all (fun node ->
                let typ = ctx.Checker.getTypeAtLocation node |> Type.Kind.create ctx.Program
                match typ with
                | Kind.Structural (Structural.Class _) ->
                    ts.getEffectiveTypeParameterDeclarations !!node
                    |> NonEmptyArray.create
                    |> Core.Option.isNone
                | _ -> true
                ) ""
        runner.testCase "never has signatures" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.Class)
            |> Flip.Expect.all (fun typ ->
                ctx.Checker.getSignaturesOfType(typ, Ts.SignatureKind.Construct ||| Ts.SignatureKind.Call)
                |> NonEmptyArray.create
                |> Core.Option.isNone
                ) ""
        runner.testCase "never has index infos unless the parents of that index declaration is an interface" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.Class)
            |> foreach (
                    assertion (fun typ ->
                        ctx.Checker.getIndexInfosOfType(typ)
                        |> NonEmptyArray.create
                        |> Option.filter (_.Values >> Array.forall (
                            _.declaration
                            >> Option.exists (
                                _.parent
                                >> unbox
                                >> Node.DeclarationKind.create ctx.Program
                                >> _.IsInterface
                                )
                            >> not)
                        )
                        |> Core.Option.isNone
                        ) (fun typ ->
                        let infos = ctx.Checker.getIndexInfosOfType(typ).AsArray
                        (Utils.inspect infos,
                        infos
                        |> Array.map (fun x ->
                            x.declaration
                            |> Option.map (_.parent >> unbox >> Node.Kind.create ctx.Program >> _.ToString())
                            )
                        |> Utils.inspect)
                        ||> sprintf "Expected no index infos for type: %A\n\n\tIndex Info: %s\n\n\tIndex Info Declaration Parents:%A" (Type.Kind.create ctx.Program typ)
                        )
                    )
        runner.testCase "Always has a symbol" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.filter _.flags.HasFlag(Ts.TypeFlags.Object)
            |> unbox<Ts.ObjectType array>
            |> Array.filter _.objectFlags.HasFlag(Ts.ObjectFlags.Class)
            |> Flip.Expect.all _.getCanonicalSymbol().IsSome ""
    runner.testSuite "Symbols" <| fun _ ->
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "If have name - have symbol attached" <| fun ctx _ nodes ->
            nodes
            |> Array.choose _.name
            |> Array.map ctx.Checker.getSymbolAtLocation
            |> Array.iter (Flip.Expect.isSome "Class should always have a symbol")
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Can have symbol embedded" <| fun ctx _ nodes ->
            nodes
            |> Array.map (fun n -> (n?symbol:Ts.Symbol) |> Option.ofObj)
            |> Flip.Expect.exists _.IsSome ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Symbol ALWAYS Embedded" <| fun ctx _ nodes ->
            nodes
            |> Array.map (fun n -> (n?symbol:Ts.Symbol) |> Option.ofObj)
            |> Flip.Expect.all _.IsSome ""
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Symbol Embedded not always same as name symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.filter _.name.IsSome
            |> Array.filter (fun n ->
                (n?symbol:Ts.Symbol)
                |> SymbolKey.fromSymbol
                |> (<>) (ctx.Checker.getSymbolAtLocation n.name.Value |> _.Value |> SymbolKey.fromSymbol)
                )
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "embedded symbol and name symbol same CANONICAL symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.filter _.name.IsSome
            |> Array.iter (fun n ->
                let embedded =
                    (n?symbol:Ts.Symbol)
                    |> ISymbol.create ctx.Program
                let nameSym =
                    n.name.Value |> ctx.Checker.getSymbolAtLocation |> _.Value
                    |> ISymbol.create ctx.Program
                let embeddedId = ISymbol.symbolKey embedded
                ISymbol.symbolKey nameSym
                |> Flip.Expect.equal embeddedId $"Class name symbol and embedded symbol should have the same canonical symbol. Embedded: {embedded.ToString()}. True: {nameSym.ToString()}"
                )
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "not always a Class [/transient class] kind" <| fun ctx _ nodes ->
            nodes
            |> Array.map (fun n -> (n?symbol:Ts.Symbol) |> SymbolTracer.create ctx.Program)
            |> Array.filter (_.Value >> Symbol.foldToTransientKind >> _.IsClass >> not)
            |> Flip.Expect.skipIfEmpty
        
        runner.testSyntaxKind<Ts.ClassDeclaration> Ts.SyntaxKind.ClassDeclaration "Always a symbol kind with a class declaration" <| fun ctx _ nodes ->
            nodes
            |> Array.map (fun n -> (n?symbol:Ts.Symbol) |> SymbolTracer.create ctx.Program)
            |> Flip.Expect.all (_.Value >> Symbol.foldToTransientKind >> function
                | Transient.Kind.Class kind -> kind |> Symbol.classDeclaration |> Chain.Expect.isNotNull "" |> ignore; true
                | Transient.Kind.Property property -> property |> Symbol.tryClassDeclaration |> Core.Option.isSome
                | Transient.Kind.Function ``function`` -> ``function`` |> Symbol.tryClassDeclaration |> Core.Option.isSome
                | Transient.Kind.Interface nterface -> nterface |> Symbol.tryClassDeclaration |> Core.Option.isSome
                | Transient.Kind.ValueModule valueModule -> valueModule |> Symbol.tryClassDeclaration |> Core.Option.isSome
                | Transient.Kind.NamespaceModule namespaceModule -> namespaceModule |> Symbol.tryClassDeclaration |> Core.Option.isSome
                | Transient.Kind.ConstEnum _
                | Transient.Kind.EnumMember _
                | Transient.Kind.TypeEnum _
                | Transient.Kind.Parameter _
                | Transient.Kind.Variable _
                | Transient.Kind.TypeLiteral _
                | Transient.Kind.ObjectLiteral _
                | Transient.Kind.Method _
                | Transient.Kind.Constructor _
                | Transient.Kind.GetAccessor _
                | Transient.Kind.SetAccessor _
                | Transient.Kind.Signature _
                | Transient.Kind.TypeParameter _
                | Transient.Kind.Unknown _
                | Transient.Kind.TypeAlias _ -> false
                ) ""
            
        
