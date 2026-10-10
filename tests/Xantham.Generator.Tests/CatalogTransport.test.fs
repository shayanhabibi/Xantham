module Xantham.Generator.Tests.CatalogTransportTests

open System
open System.IO
open System.IO.Compression
open System.Text
open Expecto
open Xantham.Generator

let private compress (bytes: byte[]) =
    use output = new MemoryStream()
    do
        use writer = new BrotliStream(output, CompressionLevel.SmallestSize, true)
        writer.Write(bytes, 0, bytes.Length)
    output.ToArray()

let private assertReleased path =
    use file = new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
    ()

let private rejected reason limit path =
    let error =
        try
            use document = CatalogTransport.readJsonWithLimit limit path
            failtest "payload unexpectedly accepted"
        with :? InvalidDataException as error -> error
    Expect.stringContains error.Message path "diagnostic identifies input"
    Expect.stringContains (error.ToString().ToLowerInvariant()) reason "diagnostic identifies failure"
    if File.Exists path then assertReleased path

[<Tests>]
let tests = testList "catalog transport" [
    testCase "one-byte input reads and zero output reads preserve decoder state" <| fun () ->
        let value = String.replicate 10000 "abcdefghijklmnopqrstuvwxyz"
        let bytes = compress (Encoding.UTF8.GetBytes ("\"" + value + "\""))
        use input =
            { new MemoryStream(bytes) with
                override this.Read(buffer, offset, count) =
                    base.Read(buffer, offset, min count 1) }
        use decoder = new CatalogTransport.StrictBrotliStream(input)
        let buffer = Array.zeroCreate<byte> 7
        Expect.equal (decoder.Read(buffer, 0, 0)) 0 "empty read consumes nothing"
        use output = new MemoryStream()
        let mutable count = decoder.Read(buffer, 0, buffer.Length)
        while count > 0 do
            output.Write(buffer, 0, count)
            Expect.equal (decoder.Read(buffer, 0, 0)) 0 "empty reads preserve progress"
            count <- decoder.Read(buffer, 0, buffer.Length)
        Expect.equal (Encoding.UTF8.GetString(output.ToArray())) ("\"" + value + "\"") "short-read output"
    for name in ["custom.data"; "declarations.json.BR"] do
        testCase ("inclusive UTF-8 byte limit " + name) <| fun () ->
            use scratch = Scratch.directory "catalog-transport"
            let path = Path.Combine(scratch.Path, name)
            let bytes = Encoding.UTF8.GetBytes """{"value":"é"}"""
            File.WriteAllBytes(path, if name.EndsWith(".BR") then compress bytes else bytes)
            do
                use document = CatalogTransport.readJsonWithLimit (int64 bytes.Length) path
                Expect.equal (document.RootElement.GetProperty("value").GetString()) "é" "decoded value"
            assertReleased path
            rejected "limit" (int64 bytes.Length - 1L) path
    testCase "all strict prefixes reject incomplete Brotli" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "prefix.json.br")
        let bytes = compress (Encoding.UTF8.GetBytes """{"value":true}""")
        for length in 0 .. bytes.Length - 1 do
            File.WriteAllBytes(path, bytes |> Array.take length)
            rejected "brotli" 1000L path
    testCase "complete JSON before Brotli end marker rejects" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "unfinished.json.br")
        use encoded = new MemoryStream()
        use writer = new BrotliStream(encoded, CompressionLevel.SmallestSize, true)
        let json = Encoding.UTF8.GetBytes "{}"
        writer.Write(json, 0, json.Length)
        writer.Flush()
        let partial = encoded.ToArray()
        use input = new MemoryStream(partial)
        use decoder = new BrotliStream(input, CompressionMode.Decompress)
        let output = Array.zeroCreate<byte> 2
        Expect.equal (decoder.Read(output, 0, 2)) 2 "fixture already contains complete JSON"
        Expect.sequenceEqual output json "decoded complete value"
        File.WriteAllBytes(path, partial)
        rejected "truncated" 1000L path
    testCase "trailing bytes and concatenated streams reject" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "trailing.json.br")
        let bytes = compress (Encoding.UTF8.GetBytes "{}")
        for suffix in [[|0uy|]; bytes] do
            File.WriteAllBytes(path, Array.append bytes suffix)
            rejected "trailing" 1000L path
    for compressed in [false; true] do
        testCase (sprintf "malformed JSON and empty JSON %b" compressed) <| fun () ->
            use scratch = Scratch.directory "catalog-transport"
            let path = Path.Combine(scratch.Path, if compressed then "bad.json.br" else "bad.json")
            for json in [""; "{"; "{}{}"] do
                let bytes = Encoding.UTF8.GetBytes json
                File.WriteAllBytes(path, if compressed then compress bytes else bytes)
                rejected "json" 1000L path
    testCase "invalid Brotli rejects without JSON fallback" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "plain.json.br")
        File.WriteAllText(path, """{"value":true}""")
        rejected "brotli" 1000L path
    testCase "missing input identifies path" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        rejected "file" 1000L (Path.Combine(scratch.Path, "missing.json"))
    testCase "large output preserves decoder state" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "large.json.br")
        let value = String.replicate 100000 "é"
        let bytes = Encoding.UTF8.GetBytes ("{\"value\":\"" + value + "\"}")
        File.WriteAllBytes(path, compress bytes)
        use document = CatalogTransport.readJsonWithLimit (int64 bytes.Length) path
        Expect.equal (document.RootElement.GetProperty("value").GetString()) value "output larger than stream buffer"
    testCase "writer emits exact UTF-8 without BOM" <| fun () ->
        use scratch = Scratch.directory "catalog-transport"
        let path = Path.Combine(scratch.Path, "written.json.br")
        let json = """{"value":"é"}"""
        CatalogTransport.writeBrotli path json
        use file = File.OpenRead path
        use decoder = new BrotliStream(file, CompressionMode.Decompress)
        use output = new MemoryStream()
        decoder.CopyTo output
        Expect.sequenceEqual (output.ToArray()) (Encoding.UTF8.GetBytes json) "independent decoder"
]

let private loadConfig (json: string) =
    use scratch = Scratch.directory "catalog-config"
    let path = Path.Combine(scratch.Path, "xantham.json")
    File.WriteAllText(path, json)
    GeneratorConfig.loadFile path

[<Tests>]
let configTests = testList "catalog compression config" [
    for json, enabled, compression in [
        "{}", false, CatalogCompression.Uncompressed
        """{"declarationCatalog":false}""", false, CatalogCompression.Uncompressed
        """{"declarationCatalog":true}""", true, CatalogCompression.Uncompressed
        """{"declarationCatalog":{"enabled":true}}""", true, CatalogCompression.Uncompressed
        """{"declarationCatalog":{"enabled":true,"compression":"none"}}""", true, CatalogCompression.Uncompressed
        """{"declarationCatalog":{"enabled":true,"compression":"brotli"}}""", true, CatalogCompression.Brotli
        """{"declarationCatalog":{"enabled":false,"compression":"brotli"}}""", false, CatalogCompression.Brotli
    ] do
        testCase json <| fun () ->
            let config = loadConfig json
            Expect.equal config.DeclarationCatalog enabled "emission"
            Expect.equal config.DeclarationCatalogCompression compression "transport"
            let copied = { config with ModuleName = Some "Copy" }
            Expect.equal copied.DeclarationCatalogCompression compression "record copy"
    for value in ["null"; "1"; "\"brotli\""; "{}"; "{\"enabled\":1}"; "{\"enabled\":true,\"compression\":null}"; "{\"enabled\":true,\"compression\":1}"; "{\"enabled\":true,\"compression\":\"gzip\"}"] do
        testCase ("reject " + value) <| fun () ->
            Expect.throwsC
                (fun () -> loadConfig ("{\"declarationCatalog\":" + value + "}") |> ignore)
                (fun error -> Expect.stringContains error.Message "declarationCatalog" "option diagnostic")
    testCase "schema offers boolean or explicit enabled and compression" <| fun () ->
        use document = System.Text.Json.JsonDocument.Parse(Xantham.Cli.Schema.json())
        let properties = document.RootElement.GetProperty("properties")
        Expect.isFalse (fst (properties.TryGetProperty("declarationCatalogCompression"))) "combined JSON property"
        let forms = properties.GetProperty("declarationCatalog").GetProperty("oneOf").EnumerateArray() |> Seq.toArray
        Expect.equal (forms[0].GetProperty("type").GetString()) "boolean" "legacy form"
        let options = forms[1]
        Expect.sequenceEqual (options.GetProperty("required").EnumerateArray() |> Seq.map _.GetString()) ["enabled"] "required emission flag"
        Expect.sequenceEqual (options.GetProperty("properties").GetProperty("compression").GetProperty("enum").EnumerateArray() |> Seq.map _.GetString()) ["none"; "brotli"] "supported codecs"
    testCase "producer disk output and text API preserve identical JSON bytes" <| fun () ->
        use scratch = Scratch.directory "catalog-output"
        let package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "fixtures", "catalog-portability-lab", "node_modules", "catalog-portability-owner-lab"))
        let config = { GeneratorConfig.Default with DeclarationCatalog = true; ModuleName = Some "Transport.Root"; Lib = Some ["esnext"]; Types = Some [] }
        let plain = Path.Combine(scratch.Path, "plain")
        let compressed = Path.Combine(scratch.Path, "brotli")
        let report = Pipeline.run config package plain |> Async.RunSynchronously
        let brotliConfig = { config with DeclarationCatalogCompression = CatalogCompression.Brotli }
        let compressedReport = Pipeline.run brotliConfig package compressed |> Async.RunSynchronously
        Expect.contains report.OutputFiles "declarations.json" "plain name"
        Expect.contains compressedReport.OutputFiles "declarations.json.br" "compressed name"
        Expect.isFalse (File.Exists(Path.Combine(compressed, "declarations.json"))) "only requested output"
        use file = File.OpenRead(Path.Combine(compressed, "declarations.json.br"))
        use decoder = new BrotliStream(file, CompressionMode.Decompress)
        use output = new MemoryStream()
        decoder.CopyTo output
        Expect.sequenceEqual (output.ToArray()) (File.ReadAllBytes(Path.Combine(plain, "declarations.json"))) "identical JSON bytes"
        for name in report.OutputFiles |> List.filter (fun name -> name <> "declarations.json") do
            Expect.sequenceEqual (File.ReadAllBytes(Path.Combine(compressed, name))) (File.ReadAllBytes(Path.Combine(plain, name))) name
        let rendered = Pipeline.generate brotliConfig package |> Async.RunSynchronously
        let json = rendered.Files |> List.find (fst >> (=) "declarations.json") |> snd
        Expect.sequenceEqual (Encoding.UTF8.GetBytes json) (output.ToArray()) "generation returns JSON text"
        let disabled = Path.Combine(scratch.Path, "disabled")
        let disabledReport = Pipeline.run { brotliConfig with DeclarationCatalog = false } package disabled |> Async.RunSynchronously
        Expect.isFalse (disabledReport.OutputFiles |> List.exists (fun name -> name.StartsWith("declarations.json"))) "disabled emission"
]
