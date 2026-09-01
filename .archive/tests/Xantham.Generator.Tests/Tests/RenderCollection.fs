module Xantham.Generator.Tests.Tests.RenderCollection

open System.Collections.Generic
open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

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

        // When a child module has package-level Members, the
        // generated facade type (`I<ModuleName>`) is emitted INSIDE that
        // child module's own block rather than at the parent level. Aligning
        // emission location with the canonical anchor lets sibling type
        // references inside the facade resolve by short name.
        //
        // Concretely, for a function at FQN [Cloudflare; DynamicWorkflows; doThing]:
        //   Pre-fix output (BUG):
        //     module Cloudflare =
        //         type IDynamicWorkflows =     <- column 4: inside Cloudflare, NOT inside DynamicWorkflows
        //         module DynamicWorkflows =
        //             type ...
        //   Post-fix output:
        //     module Cloudflare =
        //         module DynamicWorkflows =
        //             type IDynamicWorkflows = <- column 8: inside DynamicWorkflows
        //             type ...
        testCase "module with non-empty Members emits its facade inside its own block" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let func =
                Function.create "doThing" (primitive TypeKindPrimitive.String)
                |> Function.withPath [ "Cloudflare"; "DynamicWorkflows" ]
            registerAnchorFromExport ctx (ResolvedExport.Function [ func ])
            let root = RootModule.collectModules ctx
            let output = renderRootToString ctx root
            // The facade's indentation pinpoints which module hosts it.
            // Pre-fix it sits at column 4 (Cloudflare's block); post-fix
            // it sits at column 8 (DynamicWorkflows' block).
            let facadeLine =
                output.Split('\n')
                |> Array.tryFind (fun line -> line.Contains "type IDynamicWorkflows")
            match facadeLine with
            | None ->
                failtestf "expected the IDynamicWorkflows facade to be present; got:\n%s" output
            | Some line ->
                let leadingSpaces = line.Length - line.TrimStart(' ').Length
                Expect.isGreaterThan leadingSpaces 4
                    (sprintf "expected facade to be nested inside `module DynamicWorkflows = ` (indent > 4); got line %A in:\n%s" line output)

        // F# is single-pass. When a child module hosts BOTH a
        // package-level facade AND synthetic transient submodules
        // (created for inline parameter-shape literals, callable-type
        // bodies, etc.), the facade and the module's own types reference
        // those submodules by name. Those references only resolve if the
        // submodules are emitted before the references. `renderModule`
        // therefore emits submodules first, then types, then the facade.
        //
        // This test constructs a function whose inline parameter literal
        // forces a synthetic submodule to be created, then asserts the
        // submodule appears before the facade in the rendered output.
        testCase "submodules emitted before facade in same module" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // A function `doThing(context: { x: string })` — the inline
            // `{ x: string }` parameter shape becomes a synthetic submodule
            // named after the function (Pascal-cased "DoThing") hosting a
            // type for that shape. Both that submodule and the facade end
            // up in `module DynamicWorkflows = ...`.
            let inlineShape =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "x" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.wrap
            let func =
                Function.create "doThing" (primitive TypeKindPrimitive.String)
                |> Function.withParameters [ Parameter.create "context" inlineShape ]
                |> Function.withPath [ "Cloudflare"; "DynamicWorkflows" ]
            registerAnchorFromExport ctx (ResolvedExport.Function [ func ])
            let root = RootModule.collectModules ctx
            let output = renderRootToString ctx root
            let lines = output.Split('\n')
            let lineIndexOf (predicate: string -> bool) =
                lines
                |> Array.tryFindIndex predicate
            // Look for the synthetic submodule for the function and the
            // facade inside DynamicWorkflows. The submodule's exact name
            // depends on the case-modification rules; the structural
            // property we care about is "submodule-line < facade-line".
            let facadeIdx =
                lineIndexOf (fun l -> l.Contains "type IDynamicWorkflows")
            let submoduleIdx =
                lineIndexOf (fun l ->
                    let t = l.TrimStart(' ')
                    t.StartsWith("module DoThing") || t.StartsWith("module rec DoThing"))
            match submoduleIdx, facadeIdx with
            | Some sIdx, Some fIdx ->
                Expect.isLessThan sIdx fIdx
                    (sprintf "expected synthetic `module DoThing` to appear before `type IDynamicWorkflows` (so the facade can reference it); got submodule at line %d, facade at line %d in:\n%s" sIdx fIdx output)
            | None, _ ->
                failtestf "expected a `module DoThing` synthetic submodule to be present; got:\n%s" output
            | _, None ->
                failtestf "expected the IDynamicWorkflows facade to be present; got:\n%s" output

        // Each generated nested module is emitted as `module rec X = ...`.
        // F# is single-pass; without `rec`, types and the static-class facade
        // inside a module can only reference siblings that come *before*
        // them in source order. TypeScript declarations have no such
        // ordering constraint (interface/type augmentation is free-form,
        // types reference each other in any order), so the natural
        // analogue is `module rec`. This test pins the `rec` keyword in
        // the emitted `Ast.Module` blocks.
        testCase "nested modules emitted with `module rec` to match TS forward-ref semantics" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let func =
                Function.create "doThing" (primitive TypeKindPrimitive.String)
                |> Function.withPath [ "Cloudflare"; "DynamicWorkflows" ]
            registerAnchorFromExport ctx (ResolvedExport.Function [ func ])
            let root = RootModule.collectModules ctx
            let output = renderRootToString ctx root
            // Both the outer `Cloudflare` module and the inner
            // `DynamicWorkflows` module must carry the `rec` keyword.
            Expect.isTrue
                (output.Contains "module rec Cloudflare")
                (sprintf "expected outer `module rec Cloudflare`; got:\n%s" output)
            Expect.isTrue
                (output.Contains "module rec DynamicWorkflows")
                (sprintf "expected inner `module rec DynamicWorkflows`; got:\n%s" output)

        // Regression test for double-anchoring on inline parameter literals.
        // Before fix: `anchorPreludeExportScope` pre-anchored the literal's
        // transient path AND passed the result to `anchorPreludeAnchorScope`,
        // which anchored it AGAIN through the renderScope's Root transient.
        // The literal's leaf segment got duplicated, producing a path like
        // `DoThing/Context/Context/type Context` (3 module levels + type).
        // The reference site only knows about `DoThing.Context` (one level
        // deep), so the doubled emission produced unresolvable references.
        //
        // The structural fix passes the export's anchor (function MemberPath)
        // to `anchorPreludeAnchorScope` rather than pre-anchoring. The Transient
        // arm there does the single anchoring step itself.
        testCase "inline parameter literal anchors once (not twice)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let inlineShape =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "x" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.wrap
            let func =
                Function.create "doThing" (primitive TypeKindPrimitive.String)
                |> Function.withParameters [ Parameter.create "context" inlineShape ]
                |> Function.withPath [ "Cloudflare"; "DynamicWorkflows" ]
            registerAnchorFromExport ctx (ResolvedExport.Function [ func ])
            let root = RootModule.collectModules ctx
            let output = renderRootToString ctx root
            // The pre-fix output nested `module Context` *inside* `module DoThing`
            // wrapping a same-named `type Context`. With single anchoring the
            // type lives directly under `module DoThing` with no intermediate
            // module. Detect the regression by looking for the doubled-name
            // pattern `module rec Context = ... type Context` (any whitespace).
            let doubledModuleType =
                System.Text.RegularExpressions.Regex(
                    @"module(?:\s+rec)?\s+Context\s*=\s*\n\s+type\s+Context\b",
                    System.Text.RegularExpressions.RegexOptions.Compiled)
            Expect.isFalse
                (doubledModuleType.IsMatch output)
                (sprintf "expected no `module Context = type Context` doubled-name pattern (anchor-double-counting regression); got:\n%s" output)

        // A TypeAlias whose body is a TypeLiteral with an inline-literal
        // property used to emit the synthetic enum at the parent's TypePath
        // (with the parent's name), colliding with the parent's actual
        // TypeDefn emission. First-registered won via
        // Render.Collection.combine, so the wrong variant survived and the
        // parent appeared to be the inline literal.
        //
        // Root cause: in prerender's Union literal-only branch and
        // Intersection branch, the renderScope's `Root` was set to bare
        // `TransientTypePath.Anchored` even though the TypeRef went through
        // `createTransientPath` which grafts onto `scope.PathContext`. The
        // two paths disagreed; anchoring used `Root`, references used the
        // TypeRef.
        //
        // Fix: resolve `Root` against `scope.PathContext` the same way
        // `createTransientPath` does, so the property-name-appended
        // PathContext becomes the synthetic's anchor location.
        testCase "inline-literal property does not collide with parent alias name" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // type MyAlias = { score?: 1 | 2 | 3 }
            let inlineUnion =
                Union.create [
                    Literal.createInt 1 |> Literal.wrap
                    Literal.createInt 2 |> Literal.wrap
                    Literal.createInt 3 |> Literal.wrap
                ]
            let body =
                TypeLiteral.empty
                |> TypeLiteral.addMember (
                    Property.create "score" inlineUnion
                    |> Property.optional
                    |> Property.wrap)
                |> TypeLiteral.wrap
            let alias =
                TypeAlias.create body "MyAlias"
                |> TypeAlias.withPath [ QualifiedNamePart.Normal "Cloudflare"; QualifiedNamePart.Normal "TestPkg" ]
            registerAnchorFromExport ctx (ResolvedExport.TypeAlias alias)
            let root = RootModule.collectModules ctx
            let output = renderRootToString ctx root
            // Pre-fix the parent alias `type MyAlias` would be emitted as the
            // numeric enum body (`| ``1`` = 1 | ``2`` = 2 | ``3`` = 3`)
            // because the synthetic enum collided on the parent's name and
            // won the combine. The actual TypeLiteral body's properties
            // (the `score` abstract member) would be missing or moved.
            let parentAsEnum =
                System.Text.RegularExpressions.Regex(
                    @"type\s+MyAlias\s*=\s*\n\s*\|\s*``1``",
                    System.Text.RegularExpressions.RegexOptions.Compiled)
            Expect.isFalse
                (parentAsEnum.IsMatch output)
                (sprintf "expected `type MyAlias` to render as a TypeLiteral with members (not as a numeric enum); got:\n%s" output)
            // Post-fix the parent alias renders with the `score` abstract member.
            let scoreMember =
                System.Text.RegularExpressions.Regex(
                    @"abstract\s+score\s*:",
                    System.Text.RegularExpressions.RegexOptions.Compiled)
            Expect.isTrue
                (scoreMember.IsMatch output)
                (sprintf "expected `abstract score:` member in MyAlias; got:\n%s" output)
    ]
