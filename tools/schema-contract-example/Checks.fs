namespace SchemaContractExample

open System
open System.ComponentModel
open System.IO
open NJsonSchema
open Xantham.Generator

type Probe =
    {
        [<Description("A field added independently of the config model.")>]
        FreshField: string
    }

type AcronymProbe = { URLPrefix: string }

module Checks =
    let private equal label expected actual =
        if actual <> expected then
            failwithf "%s: expected %A, got %A" label expected actual

    let private load (json: string) =
        let path =
            Path.Combine(Path.GetTempPath(), $"xantham-schema-{Guid.NewGuid():N}.json")

        try
            File.WriteAllText(path, json)

            try
                Ok(GeneratorConfig.loadFile path)
            with error ->
                Error error.Message
        finally
            if File.Exists path then
                File.Delete path

    let private mapped arity =
        { GeneratorConfig.Default with
            Groups =
                Map.ofList
                    [
                        "dep",
                        GroupDisposition.Map(
                            Map.ofList
                                [
                                    "Item",
                                    {
                                        FSharpName = "Other.Item"
                                        Arity = arity
                                    }
                                ]
                        )
                    ]
        }

    let run (schema: JsonSchema) : unit =
        let defaults = GeneratorConfig.Default
        let accept json config = json, Some config
        let reject json = json, None

        let cases =
            [
                accept "{}" defaults
                accept
                    """{"module":"Example.Binding"}"""
                    { defaults with
                        ModuleName = Some "Example.Binding"
                    }
                accept """{"types":[]}""" { defaults with Types = Some [] }
                accept
                    """{"types":["node"]}"""
                    { defaults with
                        Types = Some [ "node" ]
                    }
                accept """{"resolveNoInfer":false}""" defaults
                accept """{"resolveNoInfer":true}""" { defaults with ResolveNoInfer = true }
                accept
                    """{"groups":{"one":"ship","two":"reference","three":"widen"}}"""
                    { defaults with
                        Groups = Map.ofList [ "one", Ship; "two", Reference; "three", Widen ]
                    }
                accept """{"groups":{"dep":{"map":{"Item":"Other.Item"}}}}""" (mapped 0)
                accept """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item"}}}}}""" (mapped 0)
                accept """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":1}}}}}""" (mapped 1)
                accept
                    """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":2147483647}}}}}"""
                    (mapped Int32.MaxValue)
                accept
                    """{"groups":{"dep":{"map":{}}}}"""
                    { defaults with
                        Groups = Map.ofList [ "dep", GroupDisposition.Map Map.empty ]
                    }
                reject """{"types":null}"""
                reject """{"types":"node"}"""
                reject """{"types":[42]}"""
                reject """{"resolveNoInfer":null}"""
                reject """{"resolveNoInfer":1}"""
                reject """{"groups":{"dep":"map"}}"""
                reject """{"groups":{"dep":null}}"""
                reject """{"groups":{"dep":"unknown"}}"""
                reject """{"groups":{"dep":{"map":null}}}"""
                reject """{"groups":{"dep":{"map":{"Item":{"arity":1}}}}}"""
                reject """{"groups":{"dep":{"map":{"Item":{"name":42}}}}}"""
                reject """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":null}}}}}"""
                reject """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":"1"}}}}}"""
                reject """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":2147483648}}}}}"""
            ]

        for json, expected in cases do
            equal $"schema acceptance for {json}" expected.IsSome (schema.Validate(json).Count = 0)

            match expected, load json with
            | Some config, Ok actual -> equal $"loaded values for {json}" config actual
            | None, Error _ -> ()
            | Some _, Error error -> failwithf "Loader refused %s: %s" json error
            | None, Ok config -> failwithf "Loader accepted invalid input %s: %A" json config

        // Existing loader permissiveness and string checks differ from the sample schema.
        let disagreements =
            [
                """{"module":null}""", false, Some defaults
                """{"module":42}""", false, Some defaults
                """{"groups":null}""", false, Some defaults
                """{"groups":[]}""", false, Some defaults
                """{"unexpected":true}""", false, Some defaults
                """{"types":[""]}""", true, None
                """{"types":[" "]}""", true, None
                """{"groups":{"dep":{"map":{"Item":{"name":"Other.Item","arity":-1}}}}}""", false, Some(mapped -1)
            ]

        for json, schemaAccepts, expected in disagreements do
            equal $"known schema outcome for {json}" schemaAccepts (schema.Validate(json).Count = 0)

            match expected, load json with
            | Some config, Ok actual -> equal $"known loader values for {json}" config actual
            | None, Error _ -> ()
            | Some _, Error error -> failwithf "Known loader acceptance changed for %s: %s" json error
            | None, Ok config -> failwithf "Known loader refusal changed for %s: %A" json config

        let probeJson = (SchemaContract.generate typeof<Probe>).ToJson()
        let probe = JsonSchema.FromJsonAsync(probeJson).GetAwaiter().GetResult()
        equal "independent field generated" true (probe.Properties.ContainsKey "freshField")

        equal
            "description belongs to the declaration"
            "A field added independently of the config model."
            probe.Properties["freshField"].Description

        equal "probe string accepted" true (probe.Validate("""{"freshField":"hello"}""").Count = 0)
        equal "probe number refused" false (probe.Validate("""{"freshField":42}""").Count = 0)
        equal "probe required field" false (probe.Validate("{}").Count = 0)
        let acronym = SchemaContract.generate typeof<AcronymProbe>
        equal "library field naming policy is shared" true (acronym.Properties.ContainsKey "uRLPrefix")

        printfn
            "Passed %d contract cases, %d known disagreements, and the independent description probe."
            cases.Length
            disagreements.Length
