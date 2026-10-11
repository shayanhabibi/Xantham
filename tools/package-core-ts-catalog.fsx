open System
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text.Json
open System.Xml.Linq

let repository = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))
let defaultProject = Path.Combine(repository, "src", "Xantham.Fable.Core.TS")

let digest path =
    File.ReadAllBytes path |> SHA256.HashData |> Convert.ToHexStringLower

// Tracked text must have the same fingerprint on LF and CRLF checkouts.
let textDigest path =
    File.ReadAllText(path).Replace("\r\n", "\n")
    |> System.Text.Encoding.UTF8.GetBytes
    |> SHA256.HashData
    |> Convert.ToHexStringLower

let options = JsonSerializerOptions(WriteIndented = true, NewLine = "\n")

let write path value =
    File.WriteAllText(path, JsonSerializer.Serialize(value, options) + "\n")

let generationInputs projectDirectory =
    use rootPackage =
        JsonDocument.Parse(File.ReadAllText(Path.Combine(repository, "package.json")))

    let producer = Path.Combine(repository, "tools", "fable-core-ts-input")

    let project =
        XDocument.Load(Path.Combine(projectDirectory, "Xantham.Fable.Core.TS.fsproj"))

    let bindings =
        project.Descendants(XName.Get "Compile")
        |> Seq.map (fun item ->
            let file = item.Attribute(XName.Get "Include").Value

            {|
                file = file.Replace('\\', '/')
                sha256 = textDigest (Path.Combine(projectDirectory, file))
            |})
        |> Seq.toArray

    {|
        schemaVersion = 1
        typescript = rootPackage.RootElement.GetProperty("devDependencies").GetProperty("typescript").GetString()
        producer =
            [|
                for file in [ "entry.d.ts"; "package.json" ] ->
                    {|
                        file = file
                        sha256 = textDigest (Path.Combine(producer, file))
                    |}
            |]
        catalogueSha256 = textDigest (Path.Combine(projectDirectory, "declarations.json"))
        generationConfigurationSha256 =
            textDigest (Path.Combine(repository, "tools", "fable-core-ts-input", "xantham.json"))
        bindings = bindings
    |}

let generationStamp projectDirectory =
    Path.Combine(projectDirectory, "catalogue-inputs.json")

let validateGeneration projectDirectory =
    let expected =
        JsonSerializer.Serialize(generationInputs projectDirectory, options) + "\n"

    let stamp = generationStamp projectDirectory

    if
        not (File.Exists stamp)
        || File.ReadAllText(stamp).Replace("\r\n", "\n") <> expected
    then
        failwith
            "Core.TS catalogue or binding inputs changed. Regenerate with build.fsx -- generate --only compiler-lib before packing."

let buildStamp assembly = assembly + ".catalogue-build.json"

let recordBuild projectDirectory assembly =
    write
        (buildStamp assembly)
        {|
            bindings = (generationInputs projectDirectory).bindings
            assemblySha256 = digest assembly
        |}

let validateBuild projectDirectory assembly =
    let expected =
        JsonSerializer.Serialize(
            {|
                bindings = (generationInputs projectDirectory).bindings
                assemblySha256 = digest assembly
            |},
            options
        )
        + "\n"

    let stamp = buildStamp assembly

    if not (File.Exists stamp) || File.ReadAllText(stamp) <> expected then
        failwith
            "Core.TS assembly does not match its binding build record. Build Core.TS before packing with --no-build."

let prepare projectDirectory assembly version =
    let projectDirectory = Path.GetFullPath projectDirectory
    validateGeneration projectDirectory
    validateBuild projectDirectory assembly

    let configPath =
        Path.Combine(repository, "tools", "fable-core-ts-input", "xantham.json")

    use config = JsonDocument.Parse(File.ReadAllText configPath)

    if not (config.RootElement.GetProperty("declarationCatalog").GetBoolean()) then
        failwith "Core.TS regeneration must enable declarationCatalog before packaging its catalogue."

    let catalogPath = Path.Combine(projectDirectory, "declarations.json")
    use catalog = JsonDocument.Parse(File.ReadAllText catalogPath)
    let root = catalog.RootElement

    if root.GetProperty("schemaVersion").GetInt32() <> 2 then
        failwith "Core.TS packaging requires a portable schema-2 catalogue. Regenerate compiler-lib."

    let compatibility = root.GetProperty "compatibility"
    let owner = root.GetProperty("owner").GetString()
    let declarations = root.GetProperty("declarations").EnumerateArray() |> Seq.toArray

    if String.IsNullOrWhiteSpace owner || declarations.Length = 0 then
        failwith "Core.TS catalogue must identify its owner and contain declarations."

    let inputs = generationInputs projectDirectory

    let output = Path.Combine(projectDirectory, "obj", "xantham")
    Directory.CreateDirectory output |> ignore
    let payload = Path.Combine(output, "declarations.json.br")

    do
        use input = File.OpenRead catalogPath
        use file = File.Create payload
        use encoder = new BrotliStream(file, CompressionLevel.SmallestSize)
        input.CopyTo encoder

    let descriptor =
        {|
            schemaVersion = 1
            packageId = "Xantham.Fable.Core.TS"
            packageVersion = version
            owner = owner
            owners = root.GetProperty "owners"
            catalogue = "xantham/declarations.json.br"
            catalogueSha256 = digest payload
            assembly = "lib/net8.0/Xantham.Fable.Core.TS.dll"
            assemblySha256 = digest assembly
            compatibility = compatibility
            generationConfigurationSha256 = inputs.generationConfigurationSha256
            bindings = inputs.bindings
        |}

    write (Path.Combine(output, "catalogue.json")) descriptor

    printfn
        "Prepared Core.TS %s catalogue: %d declarations, %d compressed bytes."
        version
        declarations.Length
        (FileInfo(payload).Length)

match fsi.CommandLineArgs[1..] with
| [| "stamp" |] -> write (generationStamp defaultProject) (generationInputs defaultProject)
| [| "record-build"; project; assembly |] -> recordBuild project assembly
| [| "prepare"; project; assembly; version |] -> prepare project assembly version
| [| "prepare" |] ->
    let project =
        XDocument.Load(Path.Combine(defaultProject, "Xantham.Fable.Core.TS.fsproj"))

    let version = project.Descendants(XName.Get "Version") |> Seq.exactlyOne |> _.Value

    prepare
        defaultProject
        (Path.Combine(defaultProject, "bin", "Release", "net8.0", "Xantham.Fable.Core.TS.dll"))
        version
| _ ->
    eprintfn
        "Usage: dotnet fsi tools/package-core-ts-catalog.fsx -- prepare [project-directory assembly-path package-version]"

    exit 1
