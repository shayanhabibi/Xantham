module Xantham.Generator.Tests.Tests.RenderCollection

open System.Collections.Generic
open Expecto
open Fabulous.AST
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types

// renderRoot used to call renderModuleInterface unconditionally on every
// top-level module, while renderModule guards the same call with
// `Members.Count > 0`. A module that exists only to host nested types or
// submodules (no static members of its own) therefore got a bare
// `type X =` emitted at the top of the file with no body — invalid F#.
// The fix mirrors the renderModule guard inside renderRoot.

let private emptyModule name = {
    Module.Name = name
    Types = Dictionary<string, Anchored.TypeRender>()
    Members = Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>()
    Modules = Dictionary<string, Module>()
}

let private rootWith (modules: Module seq) =
    let dict = Dictionary<string, Module>()
    for m in modules do dict.Add(m.Name, m)
    {
        RootModule.Types = Dictionary<string, Anchored.TypeRender>()
        Members = Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>()
        Modules = dict
    }

let private renderRootToString (ctx: GeneratorContext) (root: RootModule) =
    Ast.Oak() {
        Ast.AnonymousModule() {
            renderRoot ctx root
        }
    }
    |> Gen.mkOak
    |> Gen.run

// Matches a top-level `type X =` declaration that has no members, no inheritance,
// no `interface end`, no `class end` — just a bare `type X =` followed by blank
// space until the next top-level construct or end of file.
let private bareTypeDefnRegex =
    System.Text.RegularExpressions.Regex(
        @"^type\s+\S+\s*=\s*$\s*(?=^(?:type\s|module\s|namespace\s|let\s|\z))",
        System.Text.RegularExpressions.RegexOptions.Multiline)

[<Tests>]
let tests =
    testList "renderRoot — empty top-level module type-facade" [
        testCase "empty-member top-level module emits no bare `type =` facade" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let root = rootWith [ emptyModule "Cloudflare" ]
            let output = renderRootToString ctx root
            // The bug previously emitted a bare `type ICloudflare =` (the
            // module-name-as-interface facade) with no body, which is invalid F#.
            Expect.isFalse
                (bareTypeDefnRegex.IsMatch output)
                (sprintf "expected no bare `type X =` facade in output; got:\n%s" output)

        testCase "multiple empty-member top-level modules emit no bare facades" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let root = rootWith [
                emptyModule "Cloudflare"
                emptyModule "@cloudflare"
            ]
            let output = renderRootToString ctx root
            Expect.isFalse
                (bareTypeDefnRegex.IsMatch output)
                (sprintf "expected no bare `type X =` facades; got:\n%s" output)
    ]
