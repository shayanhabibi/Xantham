#nowarn FS0020
#nowarn FS1104


module Spec

open System.Collections.Generic
open System.ComponentModel
open EasyBuild.FileSystemProvider
open Fable.Core
open TypeScript
open Xantham.Fable
open Xantham.Mocha

type This = AbsoluteFileSystem<__SOURCE_DIRECTORY__>
// If the file system doesn't compile, then it's because the test fixtures haven't
// been created. Run `dotnet fsi ./tests/Fixtures.Setup.fsx` from the root of the repo.
type TestFixtures = AbsoluteFileSystem<This.``..``.fixtures.``.``>
let fixtures = [
        "@cloudflare/ai-chat", TestFixtures.``@cloudflare``.``ai-chat``.node_modules.``@cloudflare``.``ai-chat``.dist.``index.d.ts``
        "@cloudflare/think", TestFixtures.``@cloudflare``.think.node_modules.``@cloudflare``.think.dist.``index-BsB4GUsL.d.ts``
        "@cloudflare/dynamic-workflows", TestFixtures.``@cloudflare``.``dynamic-workflows``.node_modules.``@cloudflare``.``dynamic-workflows``.dist.``index.d.ts``
        "@cloudflare/workers-types", TestFixtures.``@cloudflare``.``workers-types``.node_modules.``@cloudflare``.``workers-types``.``index.d.ts``
        "@cloudflare/sandbox", TestFixtures.``@cloudflare``.sandbox.node_modules.``@cloudflare``.sandbox.dist.``index.d.ts``
        "@cloudflare/shell", TestFixtures.``@cloudflare``.shell.node_modules.``@cloudflare``.shell.dist.``index.d.ts``
        "@cloudflare/puppeteer", TestFixtures.``@cloudflare``.puppeteer.node_modules.``@cloudflare``.puppeteer.lib.``types.d.ts``
        "@cloudflare/voice", TestFixtures.``@cloudflare``.voice.node_modules.``@cloudflare``.voice.dist.``voice.d.ts``
        "agents", TestFixtures.agents.node_modules.agents.dist.``index.d.ts``
        "@types/three", TestFixtures.``@types``.three.node_modules.``@types``.three.``index.d.ts``
        "solid-js", TestFixtures.``solid-js``.node_modules.``solid-js``.types.``index.d.ts``
        "@types/d3", TestFixtures.``@types``.d3.node_modules.``@types``.d3.``index.d.ts``
        "@types/node", TestFixtures.``@types``.node.node_modules.``@types``.node.``index.d.ts``
        "@types/semver", TestFixtures.``@types``.semver.node_modules.``@types``.semver.``index.d.ts``
        "ansi-regex", TestFixtures.``ansi-regex``.node_modules.``ansi-regex``.``index.d.ts``
        "type-fest", TestFixtures.``type-fest``.node_modules.``type-fest``.``index.d.ts``
        "@types/lodash", TestFixtures.``@types``.lodash.node_modules.``@types``.lodash.``index.d.ts``
        "anime", TestFixtures.animejs.node_modules.animejs.dist.modules.``index.d.ts``
        "typescript", TestFixtures.typescript.node_modules.typescript.lib.``typescript.d.ts``
]
type RunnerContext = {
    Suites: SuiteContext array
} with
    member this.suites = this.Suites
    static member inline make runnerName ([<InlineIfLambda>] fn: Suite -> RunnerContext -> unit) =
        Expecto.testSuite runnerName (fun suite -> 
            fn suite { Suites = fixtures |> List.map (fun (name, file) -> SuiteContext.Create name file) |> Array.ofList })
        ()
    member inline this.testSuite name ([<InlineIfLambda>] fn: Suite -> unit) = testSuite name fn
    member inline this.ftestSuite name ([<InlineIfLambda>] fn: Suite -> unit) = ftestSuite name fn
    member inline this.ptestSuite name ([<InlineIfLambda>] fn: Suite -> unit) = ptestSuite name fn
    // Apply all changes done to one member to the others
    member inline this.testCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) =
        testSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun testContext ->
                    fn testContext suiteContext
                ))
    member inline this.ptestCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) = 
        ptestSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun testContext ->
                    fn testContext suiteContext
                ))
    member inline this.ftestCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) = 
        ftestSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun testContext ->
                    fn testContext suiteContext
                ))
    // Apply all changes done to one member to the others
    member inline this.testSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) = 
        testSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun test ->
                    match suiteContext.NodeMap.TryGetValue(kind) with
                    | true, nodes when nodes.Count > 0 -> fn suiteContext test (unbox nodes.AsArray)
                    | _ ->
                        let runnable = test.runnable()
                        runnable.title <- "[SKIPPED] No " + kind.Name + " nodes || " + runnable.title
                        test.skip()
                ))
    member inline this.ftestSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) = 
        ftestSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun test ->
                    match suiteContext.NodeMap.TryGetValue(kind) with
                    | true, nodes when nodes.Count > 0 -> fn suiteContext test (unbox nodes.AsArray)
                    | _ ->
                        let runnable = test.runnable()
                        runnable.title <- "[SKIPPED] No " + kind.Name + " nodes || " + runnable.title
                        test.skip()
                ))
    member inline this.ptestSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) =
        ptestSuite name (fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun test ->
                    match suiteContext.NodeMap.TryGetValue(kind) with
                    | true, nodes when nodes.Count > 0 -> fn suiteContext test (unbox nodes.AsArray)
                    | _ ->
                        let runnable = test.runnable()
                        runnable.title <- "[SKIPPED] No " + kind.Name + " nodes || " + runnable.title
                        test.skip()
                ))

and SuiteContext = {
    Name: string
    EntryFile: string
    Program: Ts.Program
    Checker: Ts.TypeChecker
    SourceFiles: Ts.SourceFile array
    NodeMap: Dictionary<Ts.SyntaxKind, ResizeArray<obj>>
    Nodes: Ts.Node array
    Types: Lazy<Ts.Type array>
    Symbols: Lazy<Ts.Symbol array>
} with
    static member inline Create (name: string) (entryFile: string) =
        let program = Ts.Program.Create [ entryFile ]
        let checker = program.getTypeChecker()
        let sourceFiles = program.getSourceFiles().AsArray
        let nodeMap = Dictionary<Ts.SyntaxKind, ResizeArray<obj>>()
        let rec crawl = fun node ->
            ts.forEachChild(node, fun node ->
                match nodeMap.TryGetValue(node.kind) with
                | true, nodes -> nodes.Add(node)
                | _ -> nodeMap[node.kind] <- ResizeArray [ box node ]
                crawl node
                JS.undefined
                ) |> ignore
        sourceFiles
        |> Array.map (fun sf ->
            match nodeMap.TryGetValue(sf.kind) with
            | true, nodes -> nodes.Add(sf)
            | _ -> nodeMap[sf.kind] <- ResizeArray [ box sf ]
            sf
            )
        |> Array.iter crawl
        let nodes = nodeMap.Values |> Seq.collect _.AsArray |> Seq.toArray |> unbox<Ts.Node array>
        {
            EntryFile = entryFile
            Name = name
            Program = program
            Checker = checker
            SourceFiles = sourceFiles
            NodeMap = nodeMap
            Types =
                lazy
                nodes
                |> Array.choose (fun node ->
                    try checker.getTypeAtLocation(node) |> Some
                    with _ -> None
                    )
                |> Array.distinctBy _.id
            Symbols =
                lazy
                nodes |> Array.choose checker.getSymbolAtLocation
            Nodes = nodes
        }

module NodeKinds =
    let kindsWithTypars = Set [
        Ts.SyntaxKind.MethodSignature
        Ts.SyntaxKind.MethodDeclaration
        Ts.SyntaxKind.InterfaceDeclaration
        Ts.SyntaxKind.TypeAliasDeclaration
        Ts.SyntaxKind.FunctionDeclaration
        Ts.SyntaxKind.ConstructSignature
        Ts.SyntaxKind.FunctionType
        Ts.SyntaxKind.ClassDeclaration
        Ts.SyntaxKind.CallSignature
        Ts.SyntaxKind.ConstructorType
    ]

module TypeFlags =
    /// <summary>
    /// Tracks what type flags are never seen in combination with other type flags across the corpus.
    /// </summary>
    let exclusiveMap = Map [
        Ts.TypeFlags.Boolean, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.Enum, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.BigInt, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.StringLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.NumberLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.BooleanLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.EnumLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.BigIntLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.ESSymbol, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.UniqueESSymbol, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.Void, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
            Ts.TypeFlags.Narrowable
        ]
        Ts.TypeFlags.Undefined, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
            Ts.TypeFlags.Narrowable
        ]
        Ts.TypeFlags.Null, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
            Ts.TypeFlags.Narrowable
        ]
        Ts.TypeFlags.Never, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
            Ts.TypeFlags.Narrowable
        ]
        Ts.TypeFlags.TypeParameter, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.Object, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.Union, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.Intersection, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.Index, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
        ]
        Ts.TypeFlags.IndexedAccess, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.Conditional, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.Substitution, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.NonPrimitive, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.TemplateLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
        ]
        Ts.TypeFlags.StringMapping, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
        ]
        Ts.TypeFlags.Literal, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.Unit, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.Freshable, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.StringOrNumberLiteral, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.PossiblyFalsy, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.StringLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
        ]
        Ts.TypeFlags.NumberLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.BigIntLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.BooleanLike, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.EnumLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.ESSymbolLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
        ]
        Ts.TypeFlags.VoidLike, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
            Ts.TypeFlags.StructuredOrInstantiable
            Ts.TypeFlags.Narrowable
        ]
        Ts.TypeFlags.UnionOrIntersection, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.StructuredType, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Index
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
            Ts.TypeFlags.InstantiablePrimitive
            Ts.TypeFlags.Instantiable
        ]
        Ts.TypeFlags.TypeVariable, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.InstantiableNonPrimitive, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.Index
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.TemplateLiteral
            Ts.TypeFlags.StringMapping
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.StringLike
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.InstantiablePrimitive
        ]
        Ts.TypeFlags.InstantiablePrimitive, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.TypeParameter
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.IndexedAccess
            Ts.TypeFlags.Conditional
            Ts.TypeFlags.Substitution
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
            Ts.TypeFlags.TypeVariable
            Ts.TypeFlags.InstantiableNonPrimitive
        ]
        Ts.TypeFlags.Instantiable, [
            Ts.TypeFlags.Boolean
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.EnumLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.Object
            Ts.TypeFlags.Union
            Ts.TypeFlags.Intersection
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.PossiblyFalsy
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.BooleanLike
            Ts.TypeFlags.EnumLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
            Ts.TypeFlags.UnionOrIntersection
            Ts.TypeFlags.StructuredType
        ]
        Ts.TypeFlags.StructuredOrInstantiable, [
            Ts.TypeFlags.Enum
            Ts.TypeFlags.BigInt
            Ts.TypeFlags.StringLiteral
            Ts.TypeFlags.NumberLiteral
            Ts.TypeFlags.BooleanLiteral
            Ts.TypeFlags.BigIntLiteral
            Ts.TypeFlags.ESSymbol
            Ts.TypeFlags.UniqueESSymbol
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.NonPrimitive
            Ts.TypeFlags.Literal
            Ts.TypeFlags.Unit
            Ts.TypeFlags.Freshable
            Ts.TypeFlags.StringOrNumberLiteral
            Ts.TypeFlags.NumberLike
            Ts.TypeFlags.BigIntLike
            Ts.TypeFlags.ESSymbolLike
            Ts.TypeFlags.VoidLike
        ]
        Ts.TypeFlags.Narrowable, [
            Ts.TypeFlags.Void
            Ts.TypeFlags.Undefined
            Ts.TypeFlags.Null
            Ts.TypeFlags.Never
            Ts.TypeFlags.VoidLike
        ]
    ]
    let exclusiveMasks =
        exclusiveMap
        |> Map.map (fun _ flags -> flags |> List.reduce (|||))
module ObjectFlags =
    let exclusiveMap: Map<Ts.ObjectFlags, Ts.ObjectFlags list> = Map [

        Ts.ObjectFlags.Class, [
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.Interface, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ClassOrInterface, [
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.Reference, [
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.Tuple, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.Anonymous, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
        ]
        Ts.ObjectFlags.Mapped, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.Instantiated, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ObjectLiteral, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.EvolvingArray, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ReverseMapped, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.JsxAttributes, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.JSLiteral, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.FreshLiteral, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ArrayLiteral, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ContainsSpread, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.ObjectRestType, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.InstantiationExpressionType
        ]
        Ts.ObjectFlags.InstantiationExpressionType, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.SingleSignatureType
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
        ]
        Ts.ObjectFlags.SingleSignatureType, [
            Ts.ObjectFlags.Class
            Ts.ObjectFlags.Interface
            Ts.ObjectFlags.Reference
            Ts.ObjectFlags.Tuple
            Ts.ObjectFlags.Anonymous
            Ts.ObjectFlags.Mapped
            Ts.ObjectFlags.Instantiated
            Ts.ObjectFlags.ObjectLiteral
            Ts.ObjectFlags.EvolvingArray
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties
            Ts.ObjectFlags.ReverseMapped
            Ts.ObjectFlags.JsxAttributes
            Ts.ObjectFlags.JSLiteral
            Ts.ObjectFlags.FreshLiteral
            Ts.ObjectFlags.ArrayLiteral
            Ts.ObjectFlags.ClassOrInterface
            Ts.ObjectFlags.ContainsSpread
            Ts.ObjectFlags.ObjectRestType
            Ts.ObjectFlags.InstantiationExpressionType
        ]
    ]
    let exclusiveMasks =
        exclusiveMap
        |> Map.map (fun _ flags -> flags |> List.reduce (|||))


module SyntaxKind =
    module HasSymbol =
        let private alwaysMask =
            Ts.SyntaxKind.PrivateIdentifier
            ||| Ts.SyntaxKind.DefaultKeyword
            ||| Ts.SyntaxKind.QualifiedName
            ||| Ts.SyntaxKind.ComputedPropertyName
            ||| Ts.SyntaxKind.ThisType
            ||| Ts.SyntaxKind.ImportType
            ||| Ts.SyntaxKind.PropertyAccessExpression
        let private someMask =
            Ts.SyntaxKind.NumericLiteral
            ||| Ts.SyntaxKind.StringLiteral
            ||| Ts.SyntaxKind.Identifier
            ||| Ts.SyntaxKind.SourceFile
        let private neverMask =
            Ts.SyntaxKind.EndOfFileToken
            ||| Ts.SyntaxKind.BigIntLiteral
            ||| Ts.SyntaxKind.NoSubstitutionTemplateLiteral
            ||| Ts.SyntaxKind.TemplateHead
            ||| Ts.SyntaxKind.TemplateMiddle
            ||| Ts.SyntaxKind.TemplateTail
            ||| Ts.SyntaxKind.DotDotDotToken
            ||| Ts.SyntaxKind.MinusToken
            ||| Ts.SyntaxKind.QuestionToken
            ||| Ts.SyntaxKind.ConstKeyword
            ||| Ts.SyntaxKind.ExportKeyword
            ||| Ts.SyntaxKind.FalseKeyword
            ||| Ts.SyntaxKind.InKeyword
            ||| Ts.SyntaxKind.NullKeyword
            ||| Ts.SyntaxKind.TrueKeyword
            ||| Ts.SyntaxKind.VoidKeyword
            ||| Ts.SyntaxKind.PrivateKeyword
            ||| Ts.SyntaxKind.ProtectedKeyword
            ||| Ts.SyntaxKind.PublicKeyword
            ||| Ts.SyntaxKind.StaticKeyword
            ||| Ts.SyntaxKind.AbstractKeyword
            ||| Ts.SyntaxKind.AssertsKeyword
            ||| Ts.SyntaxKind.AnyKeyword
            ||| Ts.SyntaxKind.BooleanKeyword
            ||| Ts.SyntaxKind.DeclareKeyword
            ||| Ts.SyntaxKind.IntrinsicKeyword
            ||| Ts.SyntaxKind.NeverKeyword
            ||| Ts.SyntaxKind.OutKeyword
            ||| Ts.SyntaxKind.ReadonlyKeyword
            ||| Ts.SyntaxKind.NumberKeyword
            ||| Ts.SyntaxKind.ObjectKeyword
            ||| Ts.SyntaxKind.StringKeyword
            ||| Ts.SyntaxKind.SymbolKeyword
            ||| Ts.SyntaxKind.UndefinedKeyword
            ||| Ts.SyntaxKind.UnknownKeyword
            ||| Ts.SyntaxKind.BigIntKeyword
            ||| Ts.SyntaxKind.OverrideKeyword
            ||| Ts.SyntaxKind.TypeParameter
            ||| Ts.SyntaxKind.Parameter
            ||| Ts.SyntaxKind.PropertySignature
            ||| Ts.SyntaxKind.PropertyDeclaration
            ||| Ts.SyntaxKind.MethodSignature
            ||| Ts.SyntaxKind.MethodDeclaration
            ||| Ts.SyntaxKind.Constructor
            ||| Ts.SyntaxKind.GetAccessor
            ||| Ts.SyntaxKind.SetAccessor
            ||| Ts.SyntaxKind.CallSignature
            ||| Ts.SyntaxKind.ConstructSignature
            ||| Ts.SyntaxKind.IndexSignature
            ||| Ts.SyntaxKind.TypePredicate
            ||| Ts.SyntaxKind.TypeReference
            ||| Ts.SyntaxKind.FunctionType
            ||| Ts.SyntaxKind.ConstructorType
            ||| Ts.SyntaxKind.TypeQuery
            ||| Ts.SyntaxKind.TypeLiteral
            ||| Ts.SyntaxKind.ArrayType
            ||| Ts.SyntaxKind.TupleType
            ||| Ts.SyntaxKind.OptionalType
            ||| Ts.SyntaxKind.RestType
            ||| Ts.SyntaxKind.UnionType
            ||| Ts.SyntaxKind.IntersectionType
            ||| Ts.SyntaxKind.ConditionalType
            ||| Ts.SyntaxKind.InferType
            ||| Ts.SyntaxKind.ParenthesizedType
            ||| Ts.SyntaxKind.TypeOperator
            ||| Ts.SyntaxKind.IndexedAccessType
            ||| Ts.SyntaxKind.MappedType
            ||| Ts.SyntaxKind.LiteralType
            ||| Ts.SyntaxKind.NamedTupleMember
            ||| Ts.SyntaxKind.TemplateLiteralType
            ||| Ts.SyntaxKind.TemplateLiteralTypeSpan
            ||| Ts.SyntaxKind.ObjectBindingPattern
            ||| Ts.SyntaxKind.ArrayBindingPattern
            ||| Ts.SyntaxKind.BindingElement
            ||| Ts.SyntaxKind.PrefixUnaryExpression
            ||| Ts.SyntaxKind.ExpressionWithTypeArguments
            ||| Ts.SyntaxKind.VariableStatement
            ||| Ts.SyntaxKind.VariableDeclaration
            ||| Ts.SyntaxKind.VariableDeclarationList
            ||| Ts.SyntaxKind.FunctionDeclaration
            ||| Ts.SyntaxKind.ClassDeclaration
            ||| Ts.SyntaxKind.InterfaceDeclaration
            ||| Ts.SyntaxKind.TypeAliasDeclaration
            ||| Ts.SyntaxKind.EnumDeclaration
            ||| Ts.SyntaxKind.ModuleDeclaration
            ||| Ts.SyntaxKind.ModuleBlock
            ||| Ts.SyntaxKind.NamespaceExportDeclaration
            ||| Ts.SyntaxKind.ImportEqualsDeclaration
            ||| Ts.SyntaxKind.ImportDeclaration
            ||| Ts.SyntaxKind.ImportClause
            ||| Ts.SyntaxKind.NamespaceImport
            ||| Ts.SyntaxKind.NamedImports
            ||| Ts.SyntaxKind.ImportSpecifier
            ||| Ts.SyntaxKind.ExportAssignment
            ||| Ts.SyntaxKind.ExportDeclaration
            ||| Ts.SyntaxKind.NamedExports
            ||| Ts.SyntaxKind.NamespaceExport
            ||| Ts.SyntaxKind.ExportSpecifier
            ||| Ts.SyntaxKind.ExternalModuleReference
            ||| Ts.SyntaxKind.HeritageClause
            ||| Ts.SyntaxKind.EnumMember
        let (|Always|Some|None|) (node: Ts.Node) =
            if node.kind &&& alwaysMask = node.kind then Always()
            elif node.kind &&& someMask = node.kind then Some()
            else None()
    module HasSymbolEmbedded =
        let private alwaysMask =
            Ts.SyntaxKind.TypeParameter
            ||| Ts.SyntaxKind.Parameter
            ||| Ts.SyntaxKind.PropertySignature
            ||| Ts.SyntaxKind.PropertyDeclaration
            ||| Ts.SyntaxKind.MethodSignature
            ||| Ts.SyntaxKind.MethodDeclaration
            ||| Ts.SyntaxKind.Constructor
            ||| Ts.SyntaxKind.GetAccessor
            ||| Ts.SyntaxKind.SetAccessor
            ||| Ts.SyntaxKind.CallSignature
            ||| Ts.SyntaxKind.ConstructSignature
            ||| Ts.SyntaxKind.IndexSignature
            ||| Ts.SyntaxKind.FunctionType
            ||| Ts.SyntaxKind.ConstructorType
            ||| Ts.SyntaxKind.TypeLiteral
            ||| Ts.SyntaxKind.MappedType
            ||| Ts.SyntaxKind.VariableDeclaration
            ||| Ts.SyntaxKind.FunctionDeclaration
            ||| Ts.SyntaxKind.ClassDeclaration
            ||| Ts.SyntaxKind.InterfaceDeclaration
            ||| Ts.SyntaxKind.TypeAliasDeclaration
            ||| Ts.SyntaxKind.EnumDeclaration
            ||| Ts.SyntaxKind.ModuleDeclaration
            ||| Ts.SyntaxKind.NamespaceExportDeclaration
            ||| Ts.SyntaxKind.ImportEqualsDeclaration
            ||| Ts.SyntaxKind.NamespaceImport
            ||| Ts.SyntaxKind.ImportSpecifier
            ||| Ts.SyntaxKind.ExportAssignment
            ||| Ts.SyntaxKind.NamespaceExport
            ||| Ts.SyntaxKind.ExportSpecifier
            ||| Ts.SyntaxKind.EnumMember
        let private someMask =
            Ts.SyntaxKind.BindingElement
            ||| Ts.SyntaxKind.ImportClause
            ||| Ts.SyntaxKind.ExportDeclaration
            ||| Ts.SyntaxKind.SourceFile
        let private neverMask =
            Ts.SyntaxKind.EndOfFileToken
            ||| Ts.SyntaxKind.NumericLiteral
            ||| Ts.SyntaxKind.BigIntLiteral
            ||| Ts.SyntaxKind.StringLiteral
            ||| Ts.SyntaxKind.NoSubstitutionTemplateLiteral
            ||| Ts.SyntaxKind.TemplateHead
            ||| Ts.SyntaxKind.TemplateMiddle
            ||| Ts.SyntaxKind.TemplateTail
            ||| Ts.SyntaxKind.DotDotDotToken
            ||| Ts.SyntaxKind.MinusToken
            ||| Ts.SyntaxKind.QuestionToken
            ||| Ts.SyntaxKind.Identifier
            ||| Ts.SyntaxKind.ConstKeyword
            ||| Ts.SyntaxKind.ExportKeyword
            ||| Ts.SyntaxKind.FalseKeyword
            ||| Ts.SyntaxKind.InKeyword
            ||| Ts.SyntaxKind.NullKeyword
            ||| Ts.SyntaxKind.TrueKeyword
            ||| Ts.SyntaxKind.VoidKeyword
            ||| Ts.SyntaxKind.PrivateKeyword
            ||| Ts.SyntaxKind.ProtectedKeyword
            ||| Ts.SyntaxKind.PublicKeyword
            ||| Ts.SyntaxKind.StaticKeyword
            ||| Ts.SyntaxKind.AbstractKeyword
            ||| Ts.SyntaxKind.AssertsKeyword
            ||| Ts.SyntaxKind.AnyKeyword
            ||| Ts.SyntaxKind.BooleanKeyword
            ||| Ts.SyntaxKind.DeclareKeyword
            ||| Ts.SyntaxKind.IntrinsicKeyword
            ||| Ts.SyntaxKind.NeverKeyword
            ||| Ts.SyntaxKind.OutKeyword
            ||| Ts.SyntaxKind.ReadonlyKeyword
            ||| Ts.SyntaxKind.NumberKeyword
            ||| Ts.SyntaxKind.ObjectKeyword
            ||| Ts.SyntaxKind.StringKeyword
            ||| Ts.SyntaxKind.SymbolKeyword
            ||| Ts.SyntaxKind.UndefinedKeyword
            ||| Ts.SyntaxKind.UnknownKeyword
            ||| Ts.SyntaxKind.BigIntKeyword
            ||| Ts.SyntaxKind.OverrideKeyword
            ||| Ts.SyntaxKind.TypePredicate
            ||| Ts.SyntaxKind.TypeReference
            ||| Ts.SyntaxKind.TypeQuery
            ||| Ts.SyntaxKind.ArrayType
            ||| Ts.SyntaxKind.TupleType
            ||| Ts.SyntaxKind.OptionalType
            ||| Ts.SyntaxKind.RestType
            ||| Ts.SyntaxKind.UnionType
            ||| Ts.SyntaxKind.IntersectionType
            ||| Ts.SyntaxKind.ConditionalType
            ||| Ts.SyntaxKind.InferType
            ||| Ts.SyntaxKind.ParenthesizedType
            ||| Ts.SyntaxKind.TypeOperator
            ||| Ts.SyntaxKind.IndexedAccessType
            ||| Ts.SyntaxKind.LiteralType
            ||| Ts.SyntaxKind.NamedTupleMember
            ||| Ts.SyntaxKind.TemplateLiteralType
            ||| Ts.SyntaxKind.TemplateLiteralTypeSpan
            ||| Ts.SyntaxKind.ObjectBindingPattern
            ||| Ts.SyntaxKind.ArrayBindingPattern
            ||| Ts.SyntaxKind.PrefixUnaryExpression
            ||| Ts.SyntaxKind.ExpressionWithTypeArguments
            ||| Ts.SyntaxKind.VariableStatement
            ||| Ts.SyntaxKind.VariableDeclarationList
            ||| Ts.SyntaxKind.ModuleBlock
            ||| Ts.SyntaxKind.ImportDeclaration
            ||| Ts.SyntaxKind.NamedImports
            ||| Ts.SyntaxKind.NamedExports
            ||| Ts.SyntaxKind.ExternalModuleReference
            ||| Ts.SyntaxKind.HeritageClause
        let (|Always|Some|None|) (node: Ts.Node) =
            if node.kind &&& alwaysMask = node.kind then Always()
            elif node.kind &&& someMask = node.kind then Some()
            else None()
    module HasType =
        let private alwaysMask =
            Ts.SyntaxKind.EndOfFileToken
            ||| Ts.SyntaxKind.NumericLiteral
            ||| Ts.SyntaxKind.BigIntLiteral
            ||| Ts.SyntaxKind.StringLiteral
            ||| Ts.SyntaxKind.NoSubstitutionTemplateLiteral
            ||| Ts.SyntaxKind.TemplateHead
            ||| Ts.SyntaxKind.TemplateMiddle
            ||| Ts.SyntaxKind.TemplateTail
            ||| Ts.SyntaxKind.DotDotDotToken
            ||| Ts.SyntaxKind.MinusToken
            ||| Ts.SyntaxKind.QuestionToken
            ||| Ts.SyntaxKind.Identifier
            ||| Ts.SyntaxKind.PrivateIdentifier
            ||| Ts.SyntaxKind.ConstKeyword
            ||| Ts.SyntaxKind.DefaultKeyword
            ||| Ts.SyntaxKind.ExportKeyword
            ||| Ts.SyntaxKind.FalseKeyword
            ||| Ts.SyntaxKind.InKeyword
            ||| Ts.SyntaxKind.NullKeyword
            ||| Ts.SyntaxKind.TrueKeyword
            ||| Ts.SyntaxKind.VoidKeyword
            ||| Ts.SyntaxKind.PrivateKeyword
            ||| Ts.SyntaxKind.ProtectedKeyword
            ||| Ts.SyntaxKind.PublicKeyword
            ||| Ts.SyntaxKind.StaticKeyword
            ||| Ts.SyntaxKind.AbstractKeyword
            ||| Ts.SyntaxKind.AssertsKeyword
            ||| Ts.SyntaxKind.AnyKeyword
            ||| Ts.SyntaxKind.BooleanKeyword
            ||| Ts.SyntaxKind.DeclareKeyword
            ||| Ts.SyntaxKind.IntrinsicKeyword
            ||| Ts.SyntaxKind.NeverKeyword
            ||| Ts.SyntaxKind.OutKeyword
            ||| Ts.SyntaxKind.ReadonlyKeyword
            ||| Ts.SyntaxKind.NumberKeyword
            ||| Ts.SyntaxKind.ObjectKeyword
            ||| Ts.SyntaxKind.StringKeyword
            ||| Ts.SyntaxKind.SymbolKeyword
            ||| Ts.SyntaxKind.UndefinedKeyword
            ||| Ts.SyntaxKind.UnknownKeyword
            ||| Ts.SyntaxKind.BigIntKeyword
            ||| Ts.SyntaxKind.OverrideKeyword
            ||| Ts.SyntaxKind.QualifiedName
            ||| Ts.SyntaxKind.ComputedPropertyName
            ||| Ts.SyntaxKind.TypeParameter
            ||| Ts.SyntaxKind.Parameter
            ||| Ts.SyntaxKind.PropertySignature
            ||| Ts.SyntaxKind.PropertyDeclaration
            ||| Ts.SyntaxKind.MethodSignature
            ||| Ts.SyntaxKind.MethodDeclaration
            ||| Ts.SyntaxKind.Constructor
            ||| Ts.SyntaxKind.GetAccessor
            ||| Ts.SyntaxKind.SetAccessor
            ||| Ts.SyntaxKind.CallSignature
            ||| Ts.SyntaxKind.ConstructSignature
            ||| Ts.SyntaxKind.IndexSignature
            ||| Ts.SyntaxKind.TypePredicate
            ||| Ts.SyntaxKind.TypeReference
            ||| Ts.SyntaxKind.FunctionType
            ||| Ts.SyntaxKind.ConstructorType
            ||| Ts.SyntaxKind.TypeQuery
            ||| Ts.SyntaxKind.TypeLiteral
            ||| Ts.SyntaxKind.ArrayType
            ||| Ts.SyntaxKind.TupleType
            ||| Ts.SyntaxKind.OptionalType
            ||| Ts.SyntaxKind.RestType
            ||| Ts.SyntaxKind.UnionType
            ||| Ts.SyntaxKind.IntersectionType
            ||| Ts.SyntaxKind.ConditionalType
            ||| Ts.SyntaxKind.InferType
            ||| Ts.SyntaxKind.ParenthesizedType
            ||| Ts.SyntaxKind.ThisType
            ||| Ts.SyntaxKind.TypeOperator
            ||| Ts.SyntaxKind.IndexedAccessType
            ||| Ts.SyntaxKind.MappedType
            ||| Ts.SyntaxKind.LiteralType
            ||| Ts.SyntaxKind.NamedTupleMember
            ||| Ts.SyntaxKind.TemplateLiteralType
            ||| Ts.SyntaxKind.TemplateLiteralTypeSpan
            ||| Ts.SyntaxKind.ImportType
            ||| Ts.SyntaxKind.ObjectBindingPattern
            ||| Ts.SyntaxKind.ArrayBindingPattern
            ||| Ts.SyntaxKind.BindingElement
            ||| Ts.SyntaxKind.PropertyAccessExpression
            ||| Ts.SyntaxKind.PrefixUnaryExpression
            ||| Ts.SyntaxKind.ExpressionWithTypeArguments
            ||| Ts.SyntaxKind.VariableStatement
            ||| Ts.SyntaxKind.VariableDeclaration
            ||| Ts.SyntaxKind.VariableDeclarationList
            ||| Ts.SyntaxKind.FunctionDeclaration
            ||| Ts.SyntaxKind.ClassDeclaration
            ||| Ts.SyntaxKind.InterfaceDeclaration
            ||| Ts.SyntaxKind.TypeAliasDeclaration
            ||| Ts.SyntaxKind.EnumDeclaration
            ||| Ts.SyntaxKind.ModuleDeclaration
            ||| Ts.SyntaxKind.ModuleBlock
            ||| Ts.SyntaxKind.NamespaceExportDeclaration
            ||| Ts.SyntaxKind.ImportEqualsDeclaration
            ||| Ts.SyntaxKind.ImportDeclaration
            ||| Ts.SyntaxKind.NamespaceImport
            ||| Ts.SyntaxKind.NamedImports
            ||| Ts.SyntaxKind.ImportSpecifier
            ||| Ts.SyntaxKind.ExportAssignment
            ||| Ts.SyntaxKind.ExportDeclaration
            ||| Ts.SyntaxKind.NamedExports
            ||| Ts.SyntaxKind.NamespaceExport
            ||| Ts.SyntaxKind.ExportSpecifier
            ||| Ts.SyntaxKind.ExternalModuleReference
            ||| Ts.SyntaxKind.HeritageClause
            ||| Ts.SyntaxKind.EnumMember
        let private someMask =
            Ts.SyntaxKind.ImportClause
            ||| Ts.SyntaxKind.SourceFile
        let (|Always|Some|None|) (node: Ts.Node) =
            if node.kind &&& alwaysMask = node.kind then Always()
            elif node.kind &&& someMask = node.kind then Some()
            else None()
