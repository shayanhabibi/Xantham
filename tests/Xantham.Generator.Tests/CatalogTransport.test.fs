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
