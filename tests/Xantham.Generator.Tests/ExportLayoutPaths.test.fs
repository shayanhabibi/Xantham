module Xantham.Generator.Tests.ExportLayoutPathsTests

open Expecto
open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.Generator.Shape

let private runtime value = value * uom<importSpecifier>
let private ambient value = AmbientModule(runtime value)

[<Tests>]
let tests =
    testList
        "Export layout paths"
        [ testCase "owners normalize the effective entry specifier" <| fun _ ->
              Expect.equal (ExportLayout.ownerOf (runtime "pkg") FromModule) EntryModule "entry"
              Expect.equal
                  (ExportLayout.ownerOf (runtime "pkg") (ExportOrigin.StringFromAmbientModule "pkg"))
                  EntryModule
                  "equal ambient"

          testCase "preferred paths cover entry child unrelated scoped colon and globals" <| fun _ ->
              let preferred = ExportLayout.preferredPath (runtime "pkg")
              Expect.equal (preferred true EntryModule) [] "entry"
              Expect.equal (preferred true (ambient "pkg/strict")) [ "Strict" ] "child"
              Expect.equal (preferred true (ambient "other/deep")) [ "Other"; "Deep" ] "unrelated"
              Expect.equal (preferred true (ambient "@scope/name/sub")) [ "Scope"; "Name"; "Sub" ] "scoped"
              Expect.equal (preferred true (ambient "cloudflare:email")) [ "Cloudflare"; "Email" ] "colon"
              Expect.equal (preferred false GlobalScope) [] "global only"
              Expect.equal (preferred true GlobalScope) [ "Globals" ] "mixed global and module"

          testCase "allocation is deterministic and separates normalized collisions" <| fun _ ->
              let owners = [ ambient "foo-bar"; ambient "foo_bar" ]
              let allocated = ExportLayout.allocate (runtime "pkg") [] owners
              let reversed = ExportLayout.allocate (runtime "pkg") [] (List.rev owners)
              Expect.equal allocated reversed "input order"
              Expect.equal allocated[ambient "foo-bar"] [ "FooBar_570186124ca5" ] "hyphen"
              Expect.equal allocated[ambient "foo_bar"] [ "FooBar_0dae04ab293a" ] "underscore"

          testCase "declarations reserve module paths and the Exports leaf" <| fun _ ->
              let owner = ambient "other"
              let allocated = ExportLayout.allocate (runtime "pkg") [ "Other" ] [ owner ]
              Expect.equal allocated[owner] [ "Other_f60acb6ef7d5" ] "module collision"
              Expect.equal
                  (ExportLayout.containerName [ "Exports" ] EntryModule [])
                  "Exports_923fe53966c6"
                  "root leaf collision"

          testCase "prefix collisions separate owners at the first ambiguous segment" <| fun _ ->
              let first = ambient "foo-bar/x"
              let second = ambient "foo_bar/y"
              let allocated = ExportLayout.allocate (runtime "pkg") [] [ first; second ]
              Expect.equal allocated[first].Tail [ "X" ] "first tail"
              Expect.equal allocated[second].Tail [ "Y" ] "second tail"
              Expect.notEqual allocated[first].Head allocated[second].Head "distinct parents"

          testCase "different separators cannot merge exact normalized paths" <| fun _ ->
              let slash = ambient "foo/bar"
              let colon = ambient "foo:bar"
              let allocated = ExportLayout.allocate (runtime "pkg") [] [ slash; colon ]
              Expect.notEqual allocated[slash] allocated[colon] "exact path collision"

          testCase "root Exports reserves the sibling module path" <| fun _ ->
              let child = ambient "pkg/exports"
              let allocated = ExportLayout.allocate (runtime "pkg") [] [ EntryModule; child ]
              Expect.notEqual allocated[child] [ "Exports" ] "reserved root leaf"

          testCase "nested Exports cannot become a module beside its parent's container" <| fun _ ->
              let parent = ambient "pkg/strict"
              let child = ambient "pkg/strict/exports"
              let allocated = ExportLayout.allocate (runtime "pkg") [] [ parent; child ]
              Expect.notEqual allocated[child] [ "Strict"; "Exports" ] "reserved nested leaf"

          testCase "allocated candidates cannot collide with declarations" <| fun _ ->
              let owner = ambient "other"
              let allocated =
                  ExportLayout.allocate (runtime "pkg") [ "Other"; "Other_f60acb6ef7d5" ] [ owner ]
              Expect.notEqual allocated[owner] [ "Other_f60acb6ef7d5" ] "candidate reserved" ]
