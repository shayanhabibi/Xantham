module Xantham.Generator.Tests.CatalogLibraryLab

open System.IO
open Xantham.Generator

let private repository =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

let fixture =
    Path.Combine(repository, "tests", "fixtures", "catalog-library-entry-lab")

let config =
    // Worker declarations anchor typeof globalThis; scripthost alone cannot catalogue it.
    { GeneratorConfig.load fixture with
        Lib = Some [ "es5"; "webworker"; "scripthost" ]
        Types = Some []
        DeclarationCatalog = true
    }

let withProducer run =
    use scratch = Scratch.directory "catalog-library-producer"
    let package = Path.Combine(scratch.Path, "package")
    Directory.CreateDirectory package |> ignore

    File.WriteAllText(
        Path.Combine(package, "package.json"),
        """{"name":"catalog-library-owner-lab","version":"1.0.0","types":"index.d.ts"}"""
    )

    File.WriteAllText(Path.Combine(package, "index.d.ts"), "export {};\n")

    let producer =
        { config with
            ModuleName = Some "Library.Producer"
            CompilerLib =
                { CompilerLibConfig.Default with
                    ModuleName = Some "Library.Owner"
                }
        }

    let output = Path.Combine(scratch.Path, "producer")
    let generated = Pipeline.run producer package output |> Async.RunSynchronously

    let library =
        generated.OutputFiles
        |> List.find (fun name -> name.StartsWith "groups/" && name.EndsWith ".fs")

    run (Path.Combine(output, "declarations.json")) (Path.Combine(output, library))
