module Spec

open EasyBuild.FileSystemProvider
open FSharp.SystemCommandLine
open Fake.Core
open Fake.Core.Context
open Fake.DotNet.Testing
open Fake.JavaScript
open Microsoft.FSharp.Reflection
open FsToolkit.ErrorHandling.Operator.Option

[<Literal>]
let __REPOSITORY_DIRECTORY__ = __SOURCE_DIRECTORY__ + "/.."
type Root = AbsoluteFileSystem<__REPOSITORY_DIRECTORY__>

let inline funApply value fn = fn value

let mutable context: ActionContext option = None
let mutable project: string option = None
let mutable dependencyBuilder: unit -> unit = ignore
let inline runTarget target =
    fun ctx ->
        context <- Some ctx
        dependencyBuilder()
        Target.runOrDefaultWithArguments target
let inline runTargetWithProj target proj =
    fun ctx ->
        context <- Some ctx
        project <- Some proj
        dependencyBuilder()
        Target.runOrDefaultWithArguments target
let setDependencies fn = dependencyBuilder <- fn
        
[<AutoOpen>]
module DirectoryManagement =
    open Fake.IO.Globbing.Operators
    let sourceFiles =
        !! "**/*.fs"
        -- "**/obj/**/*.*"
        -- "**/AssemblyInfo.fs"
        -- "**/Xantham.Fable/TypeScript.fs"
    
    module Projects =
        module Directory =
            type Common = Root.src.``Xantham.Common``
            type Generator = Root.src.``Xantham.Generator``
            type Fable = Root.src.``Xantham.Fable``
            type FableCore = Root.src.``Xantham.Fable.Core``
            type Decoder = Root.src.``Xantham.Decoder``
        module FsProj =
            let [<Literal>] Common = Directory.Common.``Xantham.Common.fsproj``
            let [<Literal>] Generator = Directory.Generator.``Xantham.Generator.fsproj``
            let [<Literal>] Fable = Directory.Fable.``Xantham.Fable.fsproj``
            let [<Literal>] FableCore = Directory.FableCore.``Xantham.Fable.Core.fsproj``
            let [<Literal>] Decoder = Directory.Decoder.``Xantham.Decoder.fsproj``

    module Tests =
        module Directory =
            type Tests = Root.tests
            type Fable = Root.tests.``Xantham.Fable.Tests``
            type Generator = Root.tests.``Xantham.Generator.Tests``
            type TypeScript = Root.tests.``Xantham.TypeScript.Tests``
        module FsProj =
            let [<Literal>] Fable = Directory.Fable.``Xantham.Fable.Tests.fsproj``
            let [<Literal>] Generator = Directory.Generator.``Xantham.Generator.Tests.fsproj``
            let [<Literal>] TypeScript = Directory.Tests.``Xantham.TypeScript.Tests``.``Xantham.TypeScript.Tests.fsproj``

    module Solutions =
        let [<Literal>] Xantham = Root.``Xantham.slnx``

[<AutoOpen>]
module GitManagement =
    let [<Literal>] githubUsername = "GitHub Action"
    let [<Literal>] githubEmail = "41898282+github-actions[bot]@users.noreply.github.com"
    let [<Literal>] gitCiPrefix = "-c user.name=\"" + githubUsername + "\" -c user.email=\"" + githubEmail + "\""
    let [<Literal>] gitCiCommand = "git " + gitCiPrefix
    let gitCiArgs = [
        "-c"
        $"user.name=\"{githubUsername}\""
        "-c"
        $"user.email=\"{githubEmail}\""
    ]
#nowarn 3391
[<AutoOpen>]
module CliApiManagement =
    let inline private parseTarget<'T> (input: string) =
        let token =
            String.split '-' input
            |> List.last
        Reflection.constructors<'T>
        |> Array.pick (fun (unionName, unionBuilder) ->
            if unionName = token then
                unionBuilder [||]
                |> unbox<'T>
                |> Some
            else None)
    [<RequireQualifiedAccess>]
    type HouseKeeping =
        | clean
        | fableClean
        | format
    [<RequireQualifiedAccess>]
    type ProjectManagement =
        | restore
        | build
        | pack
        | publish
        | compile
        | publishNpm
    [<RequireQualifiedAccess>]
    type DotNetTestManagement =
        | setup
        | test
        | postTest
    [<RequireQualifiedAccess>]
    type FableTestManagement =
        | setup
        | test
        | postTest
    [<RequireQualifiedAccess>]
    type AuxiliaryTests =
        | signalSetup
        | signalTest
        | signalPost
        | loggingSetup
        | loggingTest
        | loggingPost
    [<RequireQualifiedAccess>]
    type DocManagement =
        | build
        | watch
        
    type HouseKeeping with
        static member formatAction =
            Input.option<bool> "--format"
            |> Input.description "Run fantomas before any build/compile actions"
            |> Input.arity Arity.Zero
            |> Input.recursive
        static member ghKey =
            Input.optionMaybe<string> "--gh-key"
            |> Input.helpName "API-KEY"
            |> Input.arity Arity.ExactlyOne
            |> Input.desc "GH-Key for pushing to GitHub."
        static member nugetKey =
            Input.optionMaybe<string> "--nuget-key"
            |> Input.arity Arity.ExactlyOne
            |> Input.helpName "API-KEY"
            |> Input.desc "Nuget API key for pushing packages to nuget.org."
        member this.targetName = $"housekeeping-{this}"
        static member parseTarget = parseTarget<HouseKeeping>
        static member inline op_Implicit(this: HouseKeeping) = this.targetName
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | HouseKeeping.clean -> "Clean dotnet build artifacts" |> commandWithDescription
            | HouseKeeping.fableClean -> "Clean fable build/run artifacts" |> commandWithDescription
            | HouseKeeping.format -> "Format the code" |> commandWithDescription
        member this.action =
            match this with
            | HouseKeeping.format -> HouseKeeping.formatAction |> Some
            | _ -> None
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
    type ProjectManagement with
        member this.targetName = $"project-{this}"
        static member inline op_Implicit(this: ProjectManagement) = this.targetName
        static member parseTarget = parseTarget<ProjectManagement>
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | ProjectManagement.restore -> "Restore dotnet dependencies" |> commandWithDescription
            | ProjectManagement.build -> "Build dotnet projects" |> commandWithDescription
            | ProjectManagement.compile -> "Compile fable projects" |> commandWithDescription
            | ProjectManagement.pack -> "Pack dotnet projects" |> commandWithDescription
            | ProjectManagement.publish -> "Publish dotnet projects" |> commandWithDescription
            | ProjectManagement.publishNpm -> "Publish xantham npm packages" |> commandWithDescription
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
        
    type DotNetTestManagement with
        member this.targetName = $"dotnet-test-{this}"
        static member parseTarget = parseTarget<DotNetTestManagement>
        static member inline op_Implicit(this: DotNetTestManagement) = this.targetName
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | DotNetTestManagement.setup -> "Setup dotnet test dependencies" |> commandWithDescription
            | DotNetTestManagement.test -> "Run dotnet tests" |> commandWithDescription
            | DotNetTestManagement.postTest -> "Run dotnet post-test actions" |> commandWithDescription
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
    type FableTestManagement with
        member this.targetName = $"fable-test-{this}"
        static member inline op_Implicit(this: FableTestManagement) = this.targetName
        static member parseTarget = parseTarget<FableTestManagement>
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | FableTestManagement.setup -> "Setup fable test dependencies" |> commandWithDescription
            | FableTestManagement.test -> "Run fable tests" |> commandWithDescription
            | FableTestManagement.postTest -> "Run fable post-test actions" |> commandWithDescription
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
        static member quickOption =
            Input.option<bool> "--quick"
            |> Input.arity Arity.Zero
            |> Input.alias "-q"
            |> Input.description "Skip setup steps, such as installing dependencies"
            |> Input.recursive
        static member cleanInstallOption =
            Input.option<bool> "--clean-install"
            |> Input.arity Arity.Zero
            |> Input.alias "--ci"
            |> Input.description "Run npm operations with --ci"
    type AuxiliaryTests with
        member this.targetName = $"aux-test-{this}"
        static member inline op_Implicit(this: AuxiliaryTests) = this.targetName
        static member parseTarget = parseTarget<AuxiliaryTests>
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | AuxiliaryTests.loggingSetup -> "Setup fable logging test dependencies" |> commandWithDescription
            | AuxiliaryTests.loggingTest -> "Run fable logging tests" |> commandWithDescription
            | AuxiliaryTests.loggingPost -> "Run fable logging post-test actions" |> commandWithDescription
            | AuxiliaryTests.signalSetup -> "Setup fable signal test dependencies" |> commandWithDescription
            | AuxiliaryTests.signalTest -> "Run fable signal tests" |> commandWithDescription
            | AuxiliaryTests.signalPost -> "Run fable signal post-test actions" |> commandWithDescription
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
    type DocManagement with
        member this.targetName = $"doc-{this}"
        static member inline op_Implicit(this: DocManagement) = this.targetName
        static member parseTarget = parseTarget<DocManagement>
        member this.commandName = this.ToString()
        member inline private this.commandImpl nameFn =
            let inline commandWithDescription desc =
                command (nameFn this) {
                    description desc
                    inputs Input.context
                    setAction (runTarget this)
                }
            match this with
            | DocManagement.build -> "Build documentation" |> commandWithDescription
            | DocManagement.watch -> "Watch documentation" |> commandWithDescription
        member this.command = this.commandImpl _.commandName
        member this.runCommand = this.commandImpl _.targetName
        

    module Options =
        let nugetKey = HouseKeeping.nugetKey
        let ghKey = HouseKeeping.ghKey
        let format = HouseKeeping.formatAction
        let quick = FableTestManagement.quickOption
        let cleanInstall = FableTestManagement.cleanInstallOption
        let skipTests = 
            Input.option<bool> "--skip-tests"
            |> Input.arity Arity.Zero
            |> Input.description "Skip running tests"
        let watch =
            Input.option<bool> "--watch"
            |> Input.arity Arity.Zero
            |> Input.description "Run in watch mode."
    type Args =
        static let hasFlag (input: ActionInput<'T>) = context |> Option.map (_.ParseResult >> input.GetValue)
        static let hasFlagDef (input: ActionInput<'T>) value = hasFlag input |> Option.defaultValue value
        static let hasFlagOpt (input: ActionInput<'T option>) = context >>= (_.ParseResult >> input.GetValue)
        static let hasCommand (commandString: string) = context |> Option.exists (_.ParseResult.Tokens >> Seq.exists (fun token ->
            token.Type = System.CommandLine.Parsing.TokenType.Command && token.Value = commandString
            ))
        static member npmCi = hasFlagDef Options.cleanInstall false
        static member nugetKey = hasFlagOpt Options.nugetKey
        static member ghKey = hasFlagOpt Options.ghKey
        static member quick = hasFlagDef Options.quick false
        static member format = hasFlagDef Options.format false
        static member skipTests = hasFlagDef Options.skipTests false
        static member watch = hasFlagDef Options.watch false || hasCommand "watch"
        
    let publishingOptions: ActionInput list = [
        Options.nugetKey
        Options.ghKey
    ]
    let npmOptions: ActionInput list = [
        Options.cleanInstall
    ]
    let testOptions: ActionInput list = [
        Options.quick
        Options.skipTests
        Options.cleanInstall
        Options.watch
    ]
    let globalOptions: ActionInput list = [
        Options.quick 
        Options.format 
    ]
    
        


[<AutoOpen>]
module FakeInitializationAndUtilities =
    let private root = Root.``.``
    // Credit SAFE STACK
    let initializeContext () =
        let execContext = FakeExecutionContext.Create false "build.fsx" []
        setExecutionContext (RuntimeContext.Fake execContext)
    
    let private createProcess exe args dir =
        CreateProcess.fromRawCommand exe args
        |> CreateProcess.withWorkingDirectory dir
        |> CreateProcess.ensureExitCode
    
    let dotnet args dir =
        createProcess "dotnet" args dir |> Proc.run |> ignore
    
    let fable args dir = dotnet ("fable" :: args) dir
    
    let mocha args dir =
        createProcess Npm.defaultNpmParams.NpmFilePath ("exec" :: "--" :: "mocha" :: args) dir |> Proc.run |> ignore
    
    let private gitCi args dir =
        createProcess gitCiCommand args dir |> Proc.run |> ignore
    
    module Npm =
        let private setDir dir = fun p -> { p with Npm.NpmParams.WorkingDirectory = dir }
        let cleanInstall = setDir >> Npm.cleanInstall
        let install = setDir >> Npm.install
        let test = setDir >> Npm.runTest "test"
        let runScript command = setDir root |> Npm.run command
    module Git =
        open Fake.Tools.Git
        let inline private run command = CommandHelper.directRunGitCommandAndFail root command
        let pushTags pass  =
            run $"{gitCiPrefix} push --tags origin"
            pass
        let pushBranch branchName pass =
            run $"{gitCiPrefix} push origin {branchName}"
            pass
        let pushBranchAndTags branchName pass =
            pushBranch branchName pass
            |> pushTags
        let branchName () = Information.getBranchName root
        let pushCurrentBranch pass = branchName () |> pushBranch |> funApply pass
        let pushCurrentBranchAndTags pass = branchName () |> pushBranchAndTags |> funApply pass
        let commitFiles msg files =
            files |> List.iter (Staging.stageFile root >> ignore)
            Commit.exec root msg
        let tagBranch tag = Branches.tag root tag