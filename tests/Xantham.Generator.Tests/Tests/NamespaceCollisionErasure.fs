module Xantham.Generator.Tests.Tests.NamespaceCollisionErasure

(*
Coverage plane for the NAMESPACE/DEF COLLISION ERASURE (2026-07-05). A TS name declared
as BOTH an interface and a namespace collides in F#: the module wins the name slot and the
interface def is dropped, so a type-ref to the dropped interface dangles (FS0039). Such a
ref has no faithful F# form (a namespace used as a value type is untypeable), so prerender's
Interface AND Class arms erase it to `obj` when its flattened path is in ctx.ModuleTypePaths
(populated before processExports by markModuleTypePaths). Ledgered `namespace-as-type-erased`.
The set is EMPTY outside the multi-unit pipeline, so ordinary refs are never affected.
*)

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

let private flat (path: TypePath) =
    TypePath.flatten path |> List.map Name.Case.valueOrModified |> String.concat "."

/// Prerender a ResolvedType against a ctx whose ModuleTypePaths holds the given paths,
/// returning the (expected, actual) tuple `testTypeRef` produces for `Flip.Expect.equal`.
let private renderWith (modulePaths: string list) (expected: string) (rt: ResolvedType) =
    let ctx = GeneratorContext.Empty
    modulePaths |> List.iter (fun p -> ctx.ModuleTypePaths.Add p |> ignore)
    TestHelper.prerender ctx rt
    |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef expected

[<Tests>]
let tests =
    testList "NamespaceCollisionErasure" [

        testCase "an interface whose path is a known module erases to obj" <| fun _ ->
            let iface = Interface.create "Cloudflare"
            let path = Path.fromInterface iface
            // The erasure lifts nullable (option<obj>) — the faithful rendering of an
            // untypeable namespace-as-value ref.
            Interface.wrap iface
            |> renderWith [ flat path ] "option<obj>"
            ||> Flip.Expect.equal "collision -> option<obj>"

        testCase "an interface NOT colliding renders as its name" <| fun _ ->
            // A sourceless mock interface roots under the default `Global` module.
            Interface.create "Plain"
            |> Interface.wrap
            |> renderWith [] "Global.Plain"
            ||> Flip.Expect.equal "no collision -> name"

        testCase "a different-named module in the set does not erase an unrelated interface" <| fun _ ->
            Interface.create "Keep"
            |> Interface.wrap
            |> renderWith [ "SomeOther.Module" ] "Global.Keep"
            ||> Flip.Expect.equal "unrelated set -> name kept"

        testCase "empty ModuleTypePaths never erases (pipeline-inactive default)" <| fun _ ->
            Interface.create "Cloudflare"
            |> Interface.wrap
            |> renderWith [] "Global.Cloudflare"
            ||> Flip.Expect.equal "empty set -> name"
    ]
