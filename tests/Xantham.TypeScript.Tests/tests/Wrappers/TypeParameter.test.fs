module TypeParameterWrapper
open System
open Xantham.TypeScript.Types.Node
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
open Xantham.TypeScript.Types.Type

let inline getSymbol (ctx: Spec.SuiteContext) (typeParameter: Ts.TypeParameterDeclaration) =
    typeParameter.name
    |> ctx.Checker.getSymbolAtLocation
    |> Chain.Expect.wantSome "Type Parameter should always have a symbol from its identifier"
let inline getType (ctx: Spec.SuiteContext) (typeParameter: Ts.TypeParameterDeclaration) =
    typeParameter
    |> ctx.Checker.getTypeAtLocation
    |> Chain.Expect.isNotNull "Checker should not fail to retrieve a type for a type parameter"

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Type Parameter Declaration and Wrapper" <| fun _ ->
    runner.testSuite "Fields" <| fun _ ->
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Expression field always null" <| fun ctx _ nodes ->
            nodes
            |> Array.map _.expression
            |> Array.iter (Flip.Expect.isNone "Type Parameter nodes should never have an expression")
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Can have modifiers" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (_.modifiers >> Option.bind NonEmptyArray.create)
            |> Flip.Expect.skipIfEmpty
    runner.testSuite "Symbols" <| fun _ ->
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Name always yields symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.map (_.name >> ctx.Checker.getSymbolAtLocation)
            |> Array.iter (Flip.Expect.isSome "Type Parameter should always have a symbol")
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Always have embedded symbol" <| fun ctx _ nodes ->
            nodes
            |> Array.map (fun n -> n?symbol |> Option.ofObj)
            |> Array.iter (Flip.Expect.isSome "Type Parameter should always have a symbol")
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter
            "Embedded symbol <> name symbol sometimes unless you use `getMergedSymbol`" <| fun ctx _ nodes ->
                nodes
                |> Array.choose (fun typeParam ->
                    let checkerSymbol =
                        ctx.Checker.getSymbolAtLocation typeParam.name
                        |> Chain.Expect.wantSome "Type Parameter should always have a symbol on its identifier"
                    let embeddedSymbol =
                        (typeParam?symbol : Ts.Symbol)
                        |> Option.ofObj
                        |> Chain.Expect.wantSome "Type Parameter should always have an embedded symbol"
                    let checkerSymbolId = SymbolKey.get checkerSymbol
                    let embeddedSymbolId = SymbolKey.get embeddedSymbol
                    if checkerSymbolId <> embeddedSymbolId then
                        Some (checkerSymbol, embeddedSymbol)
                    else None)
                |> Chain.Expect.skipIfEmpty
                |> Option.iter (Array.iter (fun (checkerSymbol, embeddedSymbol) ->
                        let checkerSymbol = ISymbol.create ctx.Program checkerSymbol
                        let embeddedSymbol = ISymbol.create ctx.Program embeddedSymbol
                        checkerSymbol
                        |> ISymbol.symbolKey
                        |> Flip.Expect.equal (embeddedSymbol |> ISymbol.symbolKey) $"Type Parameter should have the same embedded and non embedded symbol. Embedded: {embeddedSymbol.ToString()}. True: {checkerSymbol.ToString()}"
                    ))
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Symbol always has TypeParameter symbol flag" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (
                getSymbol ctx
                >> _.flags.HasFlag(Ts.SymbolFlags.TypeParameter)
                >> Flip.Expect.isTrue "Type Parameter symbol should have the TypeParameter symbol flag"
                )
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Can have transient flag" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.exists (
                getSymbol ctx
                >> _.flags.HasFlag(Ts.SymbolFlags.Transient)
                ) "TypeParameters can have the transient flag"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Not always with transient flag" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.exists (
                getSymbol ctx
                >> _.flags.HasFlag(Ts.SymbolFlags.Transient)
                >> not
                ) "TypeParameters should not always have the transient flag"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Can have multiple declarations" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.exists (
                getSymbol ctx
                >> _.declarations
                >> Option.exists (NonEmptyArray.create >> Option.exists (_.Length >> (<>) 1))
                ) "TypeParameters can have multiple declarations on the symbol"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Always has at least one declaration" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.all (
                getSymbol ctx
                >> _.declarations
                >> Option.exists (NonEmptyArray.create >> Option.isSome)
                ) "TypeParameters should always have at least one declaration on the symbol"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Can have decls of different types" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (getSymbol ctx >> _.declarations >> Option.bind NonEmptyArray.create)
            |> Array.choose (
                NonEmptyArray.choose (function Patterns.Node.TypeParameterDeclaration _ -> None | x -> Some x)
                )
            |> Array.collect _.Values
            |> Chain.Expect.skipIfEmpty
            |> Option.iter (Array.iter (_.kind.Name >> printfn "%s"))
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Declared type is not necessarily same as type of decl" <| fun ctx _ nodes ->
            nodes
            |> Array.map (getSymbol ctx >> ISymbol.create ctx.Program)
            |> Array.mapi (fun idx symbol ->
                let nodeType = nodes[idx] |> getType ctx
                let declaredType = ISymbol.declaredType symbol
                let nodeTypeKey = TypeKey.get nodeType
                let declaredTypeKey = TypeKey.get declaredType
                declaredTypeKey, nodeTypeKey
                )
            |> Flip.Expect.exists (fun (dtk, ntk) -> dtk <> ntk) "Declared type is not always the same as the type of the declaration node"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Name can be _" <| fun ctx _ nodes ->
            nodes
            |> Array.filter _.name.text.Equals("_")
            |> Array.map (
                getSymbol ctx
                >> ISymbol.create ctx.Program
                >> ISymbol.name
                )
            |> Flip.Expect.all (function
                | SymbolName.String "_" -> true
                | _ -> false
                ) "SymbolName was different to \"_\""
        runner.testCase "Symbols that are TypeParameters always have typeparam decl" <| fun _ ctx ->
            ctx.Symbols.Value
            |> Array.map (SymbolTracer.create ctx.Program)
            |> Array.choose (_.Value >> Symbol.foldToTransientKind >> function
                | Transient.Kind.TypeParameter tp -> Some tp
                | _ -> None)
            |> Array.iter (
                Symbol.canonicalDeclaration
                >> _.kind.Name
                >> Flip.Expect.equal Ts.SyntaxKind.TypeParameter.Name "TypeParameter should always have a TypeParameter declaration"
                )
    runner.testSuite "Types" <| fun _ ->
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Is always Any or TypeParameter" <| fun ctx _ nodes ->
            nodes
            |> Array.iter (
                getType ctx
                >> _.flags
                >> (&&&) (Ts.TypeFlags.Any ||| Ts.TypeFlags.TypeParameter)
                >> (<>) (enum 0)
                >> Flip.Expect.isTrue "Type Parameter type should be Any or TypeParameter")
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter
            "Any if the parent is a MappedTypeNode or InferTypeNode" <| fun ctx _ nodes ->
                nodes
                |> Flip.Expect.all (fun typeParam ->
                    if getType ctx typeParam |> _.flags.HasFlag(Ts.TypeFlags.Any) |> not then true else
                    match unbox<Ts.Node> typeParam.parent with
                    | Patterns.Node.MappedTypeNode _ | Patterns.Node.InferTypeNode _ -> true
                    | _ -> false
                    ) "Type Param type is Any if parent is a mapped type node or an infer type node"
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter
            "TypeParameter if the parent is not MappedTypeNode/InferTypeNode" <| fun ctx _ nodes ->
                nodes
                |> Flip.Expect.all (fun typeparam ->
                    if getType ctx typeparam |> _.flags.HasFlag(Ts.TypeFlags.TypeParameter) |> not then true else
                    match unbox<Ts.Node> typeparam.parent with
                    | Patterns.Node.MappedTypeNode _ | Patterns.Node.InferTypeNode _ -> false
                    | _ -> true
                    ) "Type Param type is TypeParameter if parent is not a mapped type node or an infer type node"
    runner.testSuite "Symbol & Type Interactions" <| fun _ ->
        runner.testCase "A TypeParameter type symbol (when determined from a type, not a typeparameterdeclaration) is not always a typeparameter" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.choose (Type.Kind.create ctx.Program >> function
                | Kind.Instantiable (Instantiable.NonPrimitive (InstantiableNonPrimitive.TypeParameter typar)) -> Some typar
                | _ -> None
                )
            |> Array.map _.Value.unsafeGetCanonicalSymbol()
            |> Flip.Expect.exists (_.flags.HasFlag(Ts.SymbolFlags.TypeParameter) >> not) "Determining a type as a type parameter does not necessarily mean the symbol is a typeparameter symbol"
        runner.testCase "All TypeParameter types (when determined from a type, not a typeparameterdeclaration) without typeparameter symbols always are named 'this'" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.mapi (fun idx -> Type.Kind.create ctx.Program >> function
                | Kind.Instantiable (Instantiable.NonPrimitive (InstantiableNonPrimitive.TypeParameter typar)) -> Some (idx, typar.Value.unsafeGetCanonicalSymbol())
                | _ -> None
                )
            |> Array.choose id
            |> Array.filter (snd >> _.flags.HasFlag(Ts.SymbolFlags.TypeParameter) >> not)
            |> Flip.Expect.all (fun (idx, sym) ->
                let isym = ISymbol.create ctx.Program sym
                let symDeclaredTypeFlags = ISymbol.declaredType isym |> _.flags
                let typName = ctx.Checker.typeToString ctx.Types.Value[idx]
                if typName <> "this" then printfn "Type name: %s" typName
                if symDeclaredTypeFlags.HasFlag(Ts.TypeFlags.TypeParameter) then printfn "%A" <| symDeclaredTypeFlags.ToStringArray()
                ISymbol.create ctx.Program sym
                |> ISymbol.declaredType
                |> _.flags.HasFlag(Ts.TypeFlags.TypeParameter)
                |> not
                && ctx.Checker.typeToString ctx.Types.Value[idx] = "this"
                ) ""
        runner.testCase "All TypeParameter types (when determined from a type, not a typeparameterdeclaration) with typeparameter symbols always have declared types with typeparameter flag and are not named 'this'" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.mapi (fun idx -> Type.Kind.create ctx.Program >> function
                | Kind.Instantiable (Instantiable.NonPrimitive (InstantiableNonPrimitive.TypeParameter typar)) -> Some (idx, typar.Value.unsafeGetCanonicalSymbol())
                | _ -> None
                )
            |> Array.choose id
            |> Array.filter (snd >> _.flags.HasFlag(Ts.SymbolFlags.TypeParameter))
            |> Flip.Expect.all (fun (idx, sym) ->
                let isym = ISymbol.create ctx.Program sym
                let symDeclaredTypeFlags = ISymbol.declaredType isym |> _.flags
                let typName = ctx.Checker.typeToString ctx.Types.Value[idx]
                if typName = "this" then printfn "Type name: %s" typName
                if not <| symDeclaredTypeFlags.HasFlag(Ts.TypeFlags.TypeParameter) then printfn "%A" <| symDeclaredTypeFlags.ToStringArray()
                ISymbol.create ctx.Program sym
                |> ISymbol.declaredType
                |> _.flags.HasFlag(Ts.TypeFlags.TypeParameter)
                && ctx.Checker.typeToString ctx.Types.Value[idx] <> "this"
                ) ""
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Symbol declared type (from typeparamdecl) is ALWAYS TypeParameter" <| fun ctx _ nodes ->
            nodes
            |> Flip.Expect.all (
                getSymbol ctx
                >> ISymbol.create ctx.Program
                >> ISymbol.declaredType
                >> _.flags.HasFlag(Ts.TypeFlags.TypeParameter)
                ) "Type Parameter symbol declared type should always be TypeParameter"
    runner.testSuite "TypeParameterTracers" <| fun _ ->
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter from node tracer" <| fun ctx _ nodes ->
            nodes
            |> Array.map (NodeTypeParameterTracer.create ctx.Program >> Wrapped.TypeParameter.fromNodeTracer)
            |> Array.iter ignore
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter symbol can yield multiple nodetypeparametertracers" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                NodeTypeParameterTracer.create ctx.Program
                >> Wrapped.TypeParameter.fromNodeTracer
                >> _.Symbol
                >> SymbolTypeParameterTracer.nodes
                )
            |> Flip.Expect.exists (_.Length >> (<>) 1) ""
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter symbol can yield multiple DISTINCT nodetypeparametertracers" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                NodeTypeParameterTracer.create ctx.Program
                >> Wrapped.TypeParameter.fromNodeTracer
                >> _.Symbol
                >> SymbolTypeParameterTracer.nodes
                >> NonEmptyArray.distinctBy _.NodeKey
                )
            |> Flip.Expect.exists (_.Length >> (<>) 1) ""
        
        // This test is REDUNDANT.
        // Our current implementation retrieves the type from the declaredType of the symbol to ensure we always get
        // a type of the typeparameter kind. This means it always be the same type per symbol.
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter symbol never yields more than ONE UNIQUE type tracer" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                NodeTypeParameterTracer.create ctx.Program
                >> Wrapped.TypeParameter.fromNodeTracer
                >> _.Symbol
                >> SymbolTypeParameterTracer.nodes
                >> NonEmptyArray.map NodeTypeParameterTracer.type_
                >> NonEmptyArray.distinctBy _.TypeKey
                )
            |> Flip.Expect.all (_.Length >> (=) 1) ""
        runner.testCase "Wrapped type parameter from type tracer" <| fun _ ctx ->
            ctx.Types.Value
            |> Array.choose (TypeTypeParameterTracer.tryCreate ctx.Program)
            |> Array.map Wrapped.TypeParameter.fromTypeTracer
            |> Array.iter ignore
        runner.testCase "Wrapped type parameter from symbol tracer" <| fun _ ctx ->
            ctx.Symbols.Value
            |> Array.choose (SymbolTypeParameterTracer.tryCreate ctx.Program)
            |> Array.map Wrapped.TypeParameter.fromSymbolTracer
            |> Array.iter ignore
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter can have alias nodes" <| fun ctx _ nodes ->
            nodes
            |> Array.map (
                Wrapped.TypeParameter.srtpCreate ctx.Program
                >> _.AliasNodes
                )
            |> Flip.Expect.exists _.IsSome ""
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameter can have modifiers" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (
                NodeTypeParameterTracer.create ctx.Program
                >> Wrapped.TypeParameter.fromNodeTracer
                >> Wrapped.TypeParameter.modifiers
                )
            |> Flip.Expect.skipIfEmpty
        runner.testSyntaxKind<Ts.TypeParameterDeclaration> Ts.SyntaxKind.TypeParameter "Wrapped type parameters can have other declarations" <| fun ctx _ nodes ->
            nodes
            |> Array.choose (
                NodeTypeParameterTracer.create ctx.Program
                >> Wrapped.TypeParameter.fromNodeTracer
                >> Wrapped.TypeParameter.nonCanonicalDeclarations
                )
            |> Flip.Expect.skipIfEmpty
        runner.testCase "Can collect type parameter wrappers for nodes on demand" <| fun _ ctx ->
            ctx.Nodes
            |> Array.choose (Wrapped.TypeParameter.collectTypeParametersForNode ctx.Program)
            |> Flip.Expect.isNotEmpty ""
    runner.testSuite "TypeParameter container nodes" <| fun _ ->
        let typeParameterKinds = Dictionary<Ts.SyntaxKind, int * int>()
        let countNode _ (node: Ts.Node) =
            ts.getEffectiveTypeParameterDeclarations !!node
            |> NonEmptyArray.create
            |> Option.map (fun _ ->
                match typeParameterKinds.TryGetValue node.kind with
                | true, (l,r) -> typeParameterKinds[node.kind] <- (l + 1, r + 1)
                | _ -> typeParameterKinds.Add(node.kind, (1,1))
                )
            |> Option.defaultWith (fun () ->
                match typeParameterKinds.TryGetValue node.kind with
                | true, (l,r) -> typeParameterKinds[node.kind] <- (l, r + 1)
                | _ -> typeParameterKinds.Add(node.kind, (0,1))
                )
        afterTests "Print results" <| fun _ ->
            typeParameterKinds
            |> Seq.filter (_.Value >> fst >> (<>) 0)
            |> Seq.filter (_.Key >> Spec.NodeKinds.kindsWithTypars.Contains >> not)
            |> Seq.map (fun kv ->
                let countWithTypars,count = kv.Value
                kv.Key.Name, (countWithTypars, count)
                )
            |> Seq.sortByDescending (snd >> fst)
            |> function
                | results when Seq.isEmpty results -> ()
                | results ->
                    results
                    |> Seq.iter (fun (k, (c,n)) ->
                        eprintfn "%s: %d with typars, %d total" k c n
                        )
                    printfn ""
                    printfn "Have typars:"
                    results
                    |> Seq.iter (fst >> printfn "Ts.SyntaxKind.%s")
        runner.testCase "Investigate what nodes can have type parameters when using ts.getEffectiveTypeParameterDeclarations" <| fun _ ctx ->
            ctx.Nodes
            |> Array.iter (countNode ctx)
        runner.testCase "works generally for declaration kind" <| fun _ ctx ->
            ctx.Nodes
            |> Array.choose (Node.DeclarationKind.tryCreate ctx.Program)
            |> Flip.Expect.exists (Wrapped.TypeParameter.collectFor ctx.Program >> Option.isSome) "Should have type parameter wrappers for this node kind"
    runner.testSuite "Other" <| fun _ ->
        runner.testCase "Typar Behaviour" <| fun _ ctx ->
            ctx.Nodes
            |> Array.collect (fun n ->
                // try
                    unbox<Ts.DeclarationWithTypeParameters> n
                    |> ts.getEffectiveTypeParameterDeclarations
                    |> NonEmptyArray.create
                    |> Option.map _.Values
                    |> Option.defaultValue [||]
                // with _ -> [||]
                )
            |> fun nodes ->
                nodes
                |> Flip.Expect.all (
                    getSymbol ctx
                    >> ISymbol.create ctx.Program
                    >> ISymbol.declaredType
                    >> _.flags.HasFlag(Ts.TypeFlags.TypeParameter)
                    ) "Type Parameter symbol declared type should always be TypeParameter"
                nodes
                |> Flip.Expect.all (fun typeparam ->
                    if getType ctx typeparam |> _.flags.HasFlag(Ts.TypeFlags.TypeParameter) |> not then true else
                    match unbox<Ts.Node> typeparam.parent with
                    | Patterns.Node.MappedTypeNode _ | Patterns.Node.InferTypeNode _ -> false
                    | _ -> true
                    ) "Type Param type is TypeParameter if parent is not a mapped type node or an infer type node"
                nodes
                |> Flip.Expect.all (fun typeParam ->
                    if getType ctx typeParam |> _.flags.HasFlag(Ts.TypeFlags.Any) |> not then true else
                    match unbox<Ts.Node> typeParam.parent with
                    | Patterns.Node.MappedTypeNode _ | Patterns.Node.InferTypeNode _ -> true
                    | _ -> false
                    ) "Type Param type is Any if parent is a mapped type node or an infer type node"
                
                nodes
                |> Array.iter (
                    getType ctx
                    >> _.flags
                    >> (&&&) (Ts.TypeFlags.Any ||| Ts.TypeFlags.TypeParameter)
                    >> (<>) (enum 0)
                    >> Flip.Expect.isTrue "Type Parameter type should be Any or TypeParameter")
                nodes
                |> Flip.Expect.all (
                    getSymbol ctx
                    >> _.declarations
                    >> Option.exists (NonEmptyArray.create >> Option.isSome)
                    ) "TypeParameters should always have at least one declaration on the symbol"
