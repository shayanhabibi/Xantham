module Spec

open EasyBuild.FileSystemProvider
open Fake.Core
open Fake.Core.Context
open Fake.DotNet.Testing
open Fake.JavaScript

[<Literal>]
let __REPOSITORY_DIRECTORY__ = __SOURCE_DIRECTORY__ + "/.."
type Root = AbsoluteFileSystem<__REPOSITORY_DIRECTORY__>

let inline funApply value fn = fn value

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
        module FsProj =
            let [<Literal>] Fable = Directory.Fable.``Xantham.Fable.Tests.fsproj``
            let [<Literal>] Generator = Directory.Generator.``Xantham.Generator.Tests.fsproj``

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
[<AutoOpen>]
module CliApiManagement =
#nowarn 3391
    [<RequireQualifiedAccess>]
    type Ops =
        | clean
        | fableClean
        | watchDocs
        | build
        | pack
        | push
        | setupFableTest
        | setupTest
        | fableTest
        | test
        | fableTestSignal
        | postFableTest
        | postTest
        | runAllTests
        | tests
        | restore
        | format
        | fableTestWatch
        | fableBuild
        | watch
        static member inline op_Implicit (op: Ops): string = op.ToString()
#onwarn 3391
    
    module Cli =
        let spec = """
Usage:
    xantham [options]
    xantham build [options]
    xantham format [options]
    xantham watch (fable | docs) [options] [npm]
    xantham test (dotnet | fable | signal) [options] [npm]
    xantham tests [options] [npm]
    xantham run <TARGET> [options] [npm]
Npm Options [npm]:
    --ci                    When performing installation of dependencies, use the `ci` command.
    --release               Build with Fable `-c Release`
Options [options]:
    -h, --help              Show this help message.
    -q, --quick             Skip setup steps, such as installing dependencies (for local environments).
    --format                Format the code before committing, pushing, or at the end of other operations..
    --skip-tests            Skip running tests.
    --nuget-key <API-KEY>   The NuGet API key to use when pushing packages.
    --gh-key <PAT>          The GitHub API key to use when pushing commits et al.
    --dry-debug             Shows the dependency list for the command and args.
"""
        let parser = Docopt(spec)

    type Args =
        static let mutable args = None
        static let hasFlag value =
            args |> Option.exists (DocoptResult.hasFlag $"--{value}")
        static let getFlag value =
            args |> Option.bind (DocoptResult.tryGetArgument value)
        static member setArgs argsv =
            args <- (Cli.parser: Docopt).Parse(argsv) |> Some
        static member npmCi = hasFlag "ci"
        static member skipTests = hasFlag "skip-tests"
        static member nugetKey = getFlag "nuget-key"
        static member ghKey = getFlag "gh-key"
        static member dryDebug = hasFlag "dry-debug"
        static member quick = hasFlag "quick"
        static member format = hasFlag "format"
        static member help = hasFlag "help"

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