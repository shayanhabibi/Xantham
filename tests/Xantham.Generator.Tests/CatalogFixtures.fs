module Xantham.Generator.Tests.CatalogFixtures

open System.IO
open System.IO.Compression
open System.Text
open Xantham.Generator

let write compression (path: string) (text: string) =
    match compression with
    | CatalogCompression.Uncompressed -> File.WriteAllText(path, text)
    | CatalogCompression.Brotli ->
        use file = File.Create path
        use encoder = new BrotliStream(file, CompressionLevel.SmallestSize)
        let bytes = Encoding.UTF8.GetBytes text
        encoder.Write(bytes, 0, bytes.Length)

/// Converts prepared JSON references with an independent encoder before a consumer runs.
let references compression (config: GeneratorConfig) package =
    match compression with
    | CatalogCompression.Uncompressed -> config
    | CatalogCompression.Brotli ->
        let paths =
            config.DeclarationReferences
            |> List.map (fun reference ->
                let path =
                    if Path.IsPathRooted reference then
                        reference
                    else
                        Path.Combine(package, reference)

                let compressed = path + ".br"

                if File.Exists path then
                    write compression compressed (File.ReadAllText path)

                compressed)

        { config with
            DeclarationReferences = paths
        }
