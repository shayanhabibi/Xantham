module internal Xantham.Generator.Customization.Compile

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Security

let validate compiler (files: (string * string) list) (contracts: string list) =
    async {
        let directory, references = ContractData.compilerInfo compiler

        let workspace =
            Path.Combine(Path.GetFullPath directory, Guid.NewGuid().ToString("N"))

        Directory.CreateDirectory workspace |> ignore

        try
            let sources = files |> List.filter (fst >> fun name -> name.EndsWith ".fs")

            for name, content in sources do
                let path = Path.GetFullPath(Path.Combine(workspace, name))

                if
                    not (
                        path.StartsWith(
                            workspace + string Path.DirectorySeparatorChar,
                            StringComparison.OrdinalIgnoreCase
                        )
                    )
                then
                    invalidOp "customization/invalid-validation-path"

                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                File.WriteAllText(path, content)

            let witness =
                contracts
                |> List.mapi (fun i name -> $"type Contract{i} = {name}")
                |> String.concat "\n"

            File.WriteAllText(
                Path.Combine(workspace, "Contracts.fs"),
                "module XanthamCustomizationContracts\n" + witness
            )

            for name in [ "Directory.Build.props"; "Directory.Build.targets" ] do
                File.WriteAllText(Path.Combine(workspace, name), "<Project />")

            let escape value = SecurityElement.Escape value

            let items =
                sources
                |> List.map (fun (name, _) -> $"<Compile Include='{escape name}' />")
                |> String.concat ""

            let refs =
                references
                |> List.mapi (fun i path ->
                    $"<Reference Include='Dependency{i}'><HintPath>{escape (Path.GetFullPath path)}</HintPath></Reference>")
                |> String.concat ""

            File.WriteAllText(
                Path.Combine(workspace, "Validation.fsproj"),
                $"<Project Sdk='Microsoft.NET.Sdk'><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup><ItemGroup>{items}<Compile Include='Contracts.fs' /><PackageReference Include='Fable.Core' Version='5.2.0' />{refs}</ItemGroup></Project>"
            )

            let start =
                ProcessStartInfo(
                    "dotnet",
                    WorkingDirectory = workspace,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true
                )

            for argument in [ "build"; "Validation.fsproj"; "--nologo"; "-v:q"; "-nodeReuse:false" ] do
                start.ArgumentList.Add argument

            use child = Process.Start start
            let stdout = child.StandardOutput.ReadToEndAsync()
            let stderr = child.StandardError.ReadToEndAsync()
            use timeout = new CancellationTokenSource(TimeSpan.FromMinutes 2.)

            try
                do! child.WaitForExitAsync(timeout.Token) |> Async.AwaitTask
            with :? OperationCanceledException ->
                child.Kill(true)
                invalidOp "customization/compile-timeout"

            let! output = stdout |> Async.AwaitTask
            let! errors = stderr |> Async.AwaitTask

            if child.ExitCode <> 0 then
                let diagnostic = (output + errors).Replace(workspace, "<validation>")
                invalidOp ("customization/compile-failed: " + diagnostic)
        finally
            Directory.Delete(workspace, true)
    }
