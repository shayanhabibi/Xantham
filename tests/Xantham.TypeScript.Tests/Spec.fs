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

type RunnerContext = private {
    Suites: SuiteContext array
} with
    member this.suites = this.Suites
    static member inline make runnerName (testNameFileMap: (string * string) list) ([<InlineIfLambda>] fn: Suite -> RunnerContext -> unit) =
        testSuite runnerName <| fun suite -> 
            fn suite { Suites = testNameFileMap |> List.map (fun (name, file) -> SuiteContext.Create name file) |> Array.ofList }
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    member inline this.testSuiteImpl (suiteFn: string -> (Suite -> unit) -> Suite) name ([<InlineIfLambda>] fn: Suite -> unit) =
        suiteFn name fn
        |> _.bail(false)
        |> ignore
    member inline this.testSuite name ([<InlineIfLambda>] fn: Suite -> unit) = this.testSuiteImpl testSuite name fn
    member inline this.ftestSuite name ([<InlineIfLambda>] fn: Suite -> unit) = this.testSuiteImpl ftestSuite name fn
    member inline this.ptestSuite name ([<InlineIfLambda>] fn: Suite -> unit) = this.testSuiteImpl ptestSuite name fn
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    member inline this.testCaseImpl (suiteFn: string -> (Suite -> unit) -> Suite) name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) =
        suiteFn name <| fun ctx ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun testContext ->
                    fn testContext suiteContext
                |> ignore
                )
        |> _.bail(false)
        |> ignore
        
    member inline this.testCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) = this.testCaseImpl testSuite name fn
    member inline this.ptestCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) = this.testCaseImpl ptestSuite name fn
    member inline this.ftestCase name ([<InlineIfLambda>] fn: Context -> SuiteContext -> unit) = this.testCaseImpl ftestSuite name fn
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    member inline this.testSyntaxKindImpl<'T> (suiteFn: string -> (Suite -> unit) -> Suite) kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) =
        suiteFn name <| fun _ ->
            this.suites
            |> Array.iter (fun suiteContext ->
                testCase suiteContext.Name <| fun test ->
                    match suiteContext.NodeMap.TryGetValue(kind) with
                    | true, nodes when nodes.Count > 0 -> fn suiteContext test (unbox nodes.AsArray)
                    | _ ->
                        let runnable = test.runnable()
                        runnable.title <- "[SKIPPED] No " + kind.Name + " nodes || " + runnable.title
                        test.skip()
                |> ignore
                )
        |> _.bail(false)
        |> ignore
    member inline this.testSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) = this.testSyntaxKindImpl testSuite kind name fn
    member inline this.ftestSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) = this.testSyntaxKindImpl ftestSuite kind name fn
    member inline this.ptestSyntaxKind<'T> kind name ([<InlineIfLambda>] fn: SuiteContext -> Context -> 'T array -> unit) = this.testSyntaxKindImpl ptestSuite kind name fn

and SuiteContext = {
    Name: string
    EntryFile: string
    Program: Ts.Program
    Checker: Ts.TypeChecker
    SourceFiles: Ts.SourceFile array
    NodeMap: Dictionary<Ts.SyntaxKind, ResizeArray<obj>>
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
        sourceFiles |> Array.iter crawl
        {
            EntryFile = entryFile
            Name = name
            Program = program
            Checker = checker
            SourceFiles = sourceFiles
            NodeMap = nodeMap
        }
