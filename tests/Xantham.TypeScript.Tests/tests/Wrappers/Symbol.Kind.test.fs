module SymbolKind


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
open Xantham.TypeScript.Types.Node

let inline tests (runner: Spec.RunnerContext) : unit = runner.testSuite "Symbol Kinds" <| fun _ ->
    runner.testCase "SymbolKinds safely wrap into Symbol.Kind" <| fun _ ctx ->
        let inline checkValueDeclaration (syntaxKindExpected: Ts.SyntaxKind) (declaration: Ts.Node) =
            Chain.Expect.isNotNull "Value Declaration is not null" declaration
            |> _.kind.Equals(syntaxKindExpected)
            |> Flip.Expect.isTrue $"Value Declaration should be {syntaxKindExpected.Name} but got {declaration.kind.Name}"
        let checkValueDecl = function
            | Symbol.Kind.Class kind ->
                checkValueDeclaration Ts.SyntaxKind.ClassDeclaration kind.valueDeclaration
            | Symbol.Kind.Parameter kind ->
                match kind.valueDeclaration with
                | ParameterKind.Binding node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.BindingElement
                | ParameterKind.Simple node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.Parameter
            | Symbol.Kind.Variable kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.VariableDeclaration
            | Symbol.Kind.Property kind ->
                match kind.valueDeclaration with
                | PropertyKind.Class node -> 
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.PropertyDeclaration
                | PropertyKind.Type node -> 
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.PropertySignature
            | Symbol.Kind.EnumMember kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.EnumMember
            | Symbol.Kind.Function kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.FunctionDeclaration
            | Symbol.Kind.ConstEnum kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.EnumDeclaration
            | Symbol.Kind.ValueModule kind ->
                match kind.valueDeclaration with
                | ModuleKind.Declaration node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.ModuleDeclaration
                | ModuleKind.Source node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.SourceFile
            | Symbol.Kind.Method kind ->
                match kind.valueDeclaration with
                | MethodKind.Class node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.MethodDeclaration
                | MethodKind.Type node ->
                    kind.valueDeclaration.Value
                    |> checkValueDeclaration Ts.SyntaxKind.MethodSignature
            | Symbol.Kind.GetAccessor kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.GetAccessor
            | Symbol.Kind.SetAccessor kind ->
                kind.valueDeclaration
                |> checkValueDeclaration Ts.SyntaxKind.SetAccessor
            | _ -> ()
        let checkCanonical = function
            | Transient.Kind.Class kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "Class Declaration is not null"
            | Transient.Kind.Parameter kind ->
                match kind.canonical with
                | ParameterKind.Binding node ->
                    node.Value.Value |> Flip.Expect.isNotNull "ParameterKind.Binding"
                | ParameterKind.Simple node ->
                    node.Value.Value |> Flip.Expect.isNotNull "ParameterKind.Simple"
            | Transient.Kind.Variable kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "Variable"
            | Transient.Kind.Property kind ->
                match kind.canonical with
                | PropertyKind.Class node -> 
                    node.Value |> Flip.Expect.isNotNull "PropertyKind.Class"
                | PropertyKind.Type node -> 
                    node.Value |> Flip.Expect.isNotNull "PropertyKind.Type"
            | Transient.Kind.EnumMember kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "EnumMember"
            | Transient.Kind.Function kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "Function"
            | Transient.Kind.Interface kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "Interface"
            | Transient.Kind.ConstEnum kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "ConstEnum"
            | Transient.Kind.TypeEnum kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "TypeEnum"
            | Transient.Kind.ValueModule kind ->
                match kind.canonical with
                | ModuleKind.Declaration node ->
                    node.Value |> Flip.Expect.isNotNull "ValueModule"
                | ModuleKind.Source source ->
                    Node.SourceKind.toSourceFile source |> Flip.Expect.isNotNull "ValueModule"
            | Transient.Kind.NamespaceModule kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "NamespaceModule"
            | Transient.Kind.TypeLiteral kind -> failtest "TypeLiteral is not supported"
            | Transient.Kind.ObjectLiteral kind -> failtest "ObjectLiteral is not supported"
            | Transient.Kind.Method kind ->
                match kind.canonical with
                | MethodKind.Class node ->
                    node.Value |> Flip.Expect.isNotNull "MethodKind.Class"
                | MethodKind.Type node ->
                    node.Value |> Flip.Expect.isNotNull "MethodKind.Type"
            | Transient.Kind.Constructor kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "Constructor"
            | Transient.Kind.GetAccessor kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "GetAccessor"
            | Transient.Kind.SetAccessor kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "SetAccessor"
            | Transient.Kind.Signature kind ->
                match kind.canonical with
                | SignatureKind.Call node ->
                    node.Value |> Flip.Expect.isNotNull "SignatureKind.Call"
                | SignatureKind.Construct node ->
                    node.Value |> Flip.Expect.isNotNull "SignatureKind.Construct"
                | SignatureKind.Index node ->
                    node.Value |> Flip.Expect.isNotNull "SignatureKind.Index"
            | Transient.Kind.TypeParameter kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "TypeParameter"
            | Transient.Kind.TypeAlias kind ->
                kind.canonical
                |> Flip.Expect.isNotNull "TypeAlias"
            | _ -> ()
        let checkTypes = function
            | Transient.Kind.Class kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Class Declaration is not null"
            | Transient.Kind.Parameter kind ->
                kind.canonicalWithType |> snd |> Flip.Expect.isNotNull "ParameterKind"
            | Transient.Kind.Variable kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Variable"
            | Transient.Kind.Property kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Property"
            | Transient.Kind.EnumMember kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "EnumMember"
            | Transient.Kind.Function kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Function"
            | Transient.Kind.Interface kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Interface"
            | Transient.Kind.ConstEnum kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "ConstEnum"
            | Transient.Kind.TypeEnum kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "TypeEnum"
            | Transient.Kind.ValueModule kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "ValueModule"
            | Transient.Kind.NamespaceModule kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "NamespaceModule"
            | Transient.Kind.Method kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Method"
            | Transient.Kind.Constructor kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Constructor"
            | Transient.Kind.GetAccessor kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "GetAccessor"
            | Transient.Kind.SetAccessor kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "SetAccessor"
            | Transient.Kind.Signature kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "Signature"
            | Transient.Kind.TypeParameter kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "TypeParameter"
            | Transient.Kind.TypeAlias kind ->
                kind.canonicalWithType |> snd
                |> Flip.Expect.isNotNull "TypeAlias"
            | _ -> ()
        ctx.Symbols.Value
        |> Array.map (Symbol.createKind ctx.Program)
        |> Array.iter (fun symKind ->
            checkValueDecl symKind
            let symKind = Symbol.foldToTransientKind symKind
            checkCanonical symKind
            checkTypes symKind
            )
    runner.testCase "Symbol declaration nodes are a subset of DeclarationFileNodes" <| fun _ ctx ->
        ctx.Symbols.Value
        |> Array.choose _.declarations
        |> Array.collect _.AsArray
        |> Array.map (Node.DeclarationKind.tryCreate ctx.Program)
        |> Flip.Expect.all _.IsSome "A Symbol had a declaration node that was uncaptured by the DeclarationKind subset."
