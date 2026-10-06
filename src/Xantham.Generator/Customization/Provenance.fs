module internal Xantham.Generator.Customization.Provenance

open System
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Security.Cryptography

let profile (extensions: GeneratorExtension list) =
    JsonSerializer.Serialize(
        {|
            contractVersion = 1
            extensions =
                extensions
                |> List.map (fun e ->
                    {|
                        id = e.Identity.Id
                        version = e.Identity.Version
                        configuration = e.Identity.Configuration |> Map.toArray
                    |})
        |}
    )

let attach (extensions: GeneratorExtension list) (companions: (string * string) list) (files: (string * string) list) =
    if List.isEmpty extensions then
        files
    else
        files
        |> List.map (fun (name, content) ->
            if name <> "manifest.json" then
                name, content
            else
                let node = JsonNode.Parse content
                let descriptor = JsonNode.Parse(profile extensions)

                let artifacts =
                    companions
                    |> List.map (fun (name, source) ->
                        {|
                            file = name
                            api =
                                SHA256.HashData(Encoding.UTF8.GetBytes(source: string))
                                |> Convert.ToHexStringLower
                        |})

                descriptor["companions"] <- JsonSerializer.SerializeToNode artifacts
                node["customizations"] <- descriptor
                name, node.ToJsonString(JsonSerializerOptions(WriteIndented = true)) + "\n")
