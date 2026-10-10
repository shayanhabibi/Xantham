module internal Xantham.Generator.CatalogLibrary

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Xantham.Generator.CatalogCompatibility

/// Logical ownership and release fingerprint of installed TypeScript library declarations.
let tryIdentity compiler root package version (file: string) =
    let library =
        match file.Split '/' with
        | [| "lib"; name |] ->
            name.StartsWith("lib.", StringComparison.Ordinal)
            && name.EndsWith(".d.ts", StringComparison.Ordinal)
        | _ -> false

    if not (library && CatalogCompiler.isLibraryPackage package) then
        None
    else
        let manifest = Path.Combine(root, "package.json")

        let fail message =
            failwith $"declaration catalog: TypeScript library {manifest}: {message}"

        use document = JsonDocument.Parse(File.ReadAllText manifest)

        let text name =
            match document.RootElement.TryGetProperty(name: string) with
            | true, value when value.ValueKind = JsonValueKind.String -> value.GetString()
            | _ -> fail $"missing or invalid {name}"

        if text "name" <> package then
            fail "package name mismatch"

        let release = text "version"

        if String.IsNullOrWhiteSpace release || release <> version then
            fail "package version mismatch"

        let revision =
            match document.RootElement.TryGetProperty "gitHead" with
            | false, _ -> None
            | true, value when value.ValueKind = JsonValueKind.String && validRevision (value.GetString()) ->
                Some(value.GetString().ToLowerInvariant())
            | _ -> fail "invalid gitHead; expected full hexadecimal revision"

        match compiler with
        | TypeScriptPackage(expectedVersion, expectedRevision, _) ->
            if release <> expectedVersion then
                fail "compiler library version mismatch"

            if revision <> Some expectedRevision then
                fail "compiler library revision mismatch"
        | Binary _ -> ()

        let metadata =
            JsonSerializer.Serialize
                {|
                    name = "typescript"
                    version = release
                    gitHead = revision
                |}

        let fingerprint =
            Encoding.UTF8.GetBytes metadata |> SHA256.HashData |> Convert.ToHexStringLower

        Some("typescript", fingerprint)
