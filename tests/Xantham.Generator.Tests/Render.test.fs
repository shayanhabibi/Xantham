/// The renderer against hand-built models: identifier escaping, the printed source, and the
/// manifest, each asserted byte-for-byte - the renderer's contract is byte-identical output.
module Xantham.Generator.Tests.RenderTests

open Expecto
open Xantham.Generator

let private renderAll (model: RenderModel) =
    let rendered, findings = Async.RunSynchronously(Pipeline.runTier Build.context Render.passes model)
    Expect.isEmpty findings "render passes never degrade"
    rendered.Files |> Map.ofList

let private baseModel =
    { ModuleName = "TestPkg"
      PackageName = "test-pkg"
      Decls = []
      Findings = []
      Files = [] }

[<Tests>]
let renderTests =
    testList "render" [
        testCase "identifiers are kept verbatim until F# rejects them" <| fun _ ->
            Expect.equal (Render.ident "onlyFirst") "onlyFirst" "plain"
            Expect.equal (Render.ident "type") "``type``" "keyword"
            Expect.equal (Render.ident "utf-8") "``utf-8``" "not identifier-shaped"
            Expect.equal (Render.ident "_tag") "_tag" "leading underscore is fine"

        testCase "a qualified templated name escapes per segment" <| fun _ ->
            Expect.equal (Render.printType (FsNamed "TypeScript.Lib.RegExp")) "TypeScript.Lib.RegExp" "qualified"
            Expect.equal (Render.printType (FsNamed "Pkg.type")) "Pkg.``type``" "keyword segment"

        testCase "the printed source is exactly the golden text" <| fun _ ->
            let model =
                { baseModel with
                    Decls =
                        [ FsInterface
                              { Name = "Options"
                                Docs = "Opts."
                                Tags = []
                                Order = None
                                Inherits = []
                                Members =
                                  [ FsProperty
                                        { Name = "onlyFirst"
                                          Docs = ""
                                          Tags = []
                                          ReadOnly = true
                                          Type = FsBool }
                                    FsProperty
                                        { Name = "count"
                                          Docs = ""
                                          Tags = []
                                          ReadOnly = false
                                          Type = FsOption FsFloat }
                                    FsMethod
                                        { Name = "reset"
                                          Docs = ""
                                          Tags = []
                                          Parameters =
                                            [ { Name = "hard"
                                                Optional = true
                                                Rest = false
                                                Type = FsOption FsBool } ]
                                          Return = FsNamed "Options" } ]
                                CreateOverloads =
                                  [ [ { Name = "onlyFirst"
                                        Optional = false
                                        Rest = false
                                        Type = FsBool }
                                      { Name = "count"
                                        Optional = true
                                        Rest = false
                                        Type = FsOption FsFloat } ] ] }
                          FsStringEnum
                              { Name = "TimeUnit"
                                Docs = ""
                                Tags = []
                                Order = None
                                Cases =
                                  [ { Name = "Ms"
                                      CompiledName = Some "ms"
                                      CompiledValue = None }
                                    { Name = "Auto"
                                      CompiledName = None
                                      CompiledValue = None }
                                    { Name = "N1_5"
                                      CompiledName = None
                                      CompiledValue = Some(LitNumber 1.5) } ] }
                          FsAbbrev
                              { Name = "TimerCallback"
                                Docs = ""
                                Tags = []
                                Order = None
                                Target = FsDelegate([ FsNamed "Options" ], FsUnit) }
                          FsExports
                              [ { Name = "make"
                                  Docs = ""
                                  Tags = []
                                  Binding = ImportNamed "make"
                                  Body =
                                    ExportFunction(
                                        [ { Name = "options"
                                            Optional = true
                                            Rest = false
                                            Type = FsOption(FsNamed "Options") } ],
                                        FsNamed "Options"
                                    ) }
                                { Name = "Timer"
                                  Docs = ""
                                  Tags = []
                                  Binding = ImportNamed "Timer"
                                  Body = ExportConstructor([], FsNamed "Options") }
                                { Name = "globals"
                                  Docs = ""
                                  Tags = []
                                  Binding = ImportNamed "globals"
                                  Body = ExportValue(FsArray FsFloat) } ] ] }

            let expected =
                String.concat
                    "\n"
                    [ "// <auto-generated>"
                      "//   Generated by Xantham.Generator from test-pkg."
                      "//   Do not edit by hand - regenerate instead."
                      "// </auto-generated>"
                      "module rec TestPkg"
                      ""
                      "open System"
                      "open Fable.Core"
                      "open Fable.Core.JsInterop"
                      ""
                      "/// <summary>"
                      "/// Opts."
                      "/// </summary>"
                      "[<Interface>]"
                      "type Options ="
                      "    abstract onlyFirst: bool"
                      "    abstract count: float option with get, set"
                      "    abstract reset: ?hard: bool -> Options"
                      "    [<ParamObject; Emit(\"$0\")>]"
                      "    static member Create (onlyFirst: bool, ?count: float) : Options = jsNative"
                      ""
                      "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                      "type TimeUnit ="
                      "    | [<CompiledName(\"ms\")>] Ms"
                      "    | Auto"
                      "    | [<CompiledValue(1.5)>] N1_5"
                      ""
                      "type TimerCallback = Action<Options>"
                      ""
                      "/// <summary>The package's value exports, each bound to its import.</summary>"
                      "[<Erase>]"
                      "type Exports ="
                      "    [<Import(\"make\", \"test-pkg\")>]"
                      "    static member make (?options: Options) : Options = jsNative"
                      "    [<Import(\"Timer\", \"test-pkg\"); EmitConstructor>]"
                      "    static member Timer () : Options = jsNative"
                      "    [<Import(\"globals\", \"test-pkg\")>]"
                      "    static member globals: float[] = jsNative"
                      "" ]

            Expect.equal (renderAll model |> Map.find "TestPkg.fs") expected "the source golden"

        testCase "the manifest reports per-symbol tiers with pass provenance" <| fun _ ->
            let model =
                { baseModel with
                    Decls =
                        [ FsInterface
                              { Name = "Options"
                                Docs = ""
                                Tags = []
                                Order = None
                                Inherits = []
                                Members = []
                                CreateOverloads = [] } ]
                    Findings =
                        [ { Pass = "shape-interfaces"
                            Symbol = "Options.legacy"
                            Tier = Widened
                            Message = "legacy widened" }
                          { Pass = "audit-coverage"
                            Symbol = "Dropped"
                            Tier = Escape
                            Message = "export not represented in the generated output" } ] }

            let expected =
                String.concat
                    "\n"
                    [ "{"
                      "  \"package\": \"test-pkg\","
                      "  \"module\": \"TestPkg\","
                      "  \"counts\": {"
                      "    \"exact\": 0,"
                      "    \"ergonomic\": 0,"
                      "    \"widened\": 1,"
                      "    \"escape\": 1"
                      "  },"
                      "  \"symbols\": ["
                      "    {"
                      "      \"name\": \"Options\","
                      "      \"tier\": \"widened\","
                      "      \"findings\": ["
                      "        {"
                      "          \"pass\": \"shape-interfaces\","
                      "          \"tier\": \"widened\","
                      "          \"message\": \"legacy widened\""
                      "        }"
                      "      ]"
                      "    },"
                      "    {"
                      "      \"name\": \"Dropped\","
                      "      \"tier\": \"escape\","
                      "      \"findings\": ["
                      "        {"
                      "          \"pass\": \"audit-coverage\","
                      "          \"tier\": \"escape\","
                      "          \"message\": \"export not represented in the generated output\""
                      "        }"
                      "      ]"
                      "    }"
                      "  ]"
                      "}"
                      "" ]

            Expect.equal (renderAll model |> Map.find "manifest.json") expected "the manifest golden"
    ]
