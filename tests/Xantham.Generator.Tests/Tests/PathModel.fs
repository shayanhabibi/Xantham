module Xantham.Generator.Tests.Tests.PathModel

(*
Isolated unit tests for the PATH ALGEBRA contracts in src/Xantham.Generator/Types/NamePath.fs.

These pin the pure functions that the placement / double-graft bugs violate, exercising them
DIRECTLY (not via prerender) with the NamePath constructors:
  - ModulePath.flatten            (root-relative segment list, drops empty names)
  - TypePath.flatten              (= ModulePath.flatten parent @ [ name ])
  - TransientTypePath.toAnchored  (own segments only, no anchor)
  - TransientTypePath.anchor      (prepends the anchor's pathTrace; one case per AnchorPath)
  - AnchorPath.traceToParentModule (one case per AnchorPath constructor)
  - Path.getRelativePath          (strips common module prefix / localise)
*)

open Expecto
open Xantham.Decoder
open Xantham.Generator.NamePath

/// Render a Name<'u> list as a dotted source string.
let dot (names: Name<'u> list) =
    names |> List.map Name.Case.valueOrModified |> String.concat "."

/// Render an untyped Name list as a dotted source string.
let dotName (names: Name list) =
    names |> List.map Name.Case.valueOrModified |> String.concat "."

// ---------------------------------------------------------------------------
// ModulePath.flatten — root-relative segment list, drops empty names
// ---------------------------------------------------------------------------

[<Tests>]
let modulePathFlatten =
    testList "ModulePath.flatten" [
        testCase "single root module" <| fun _ ->
            ModulePath.init "Root"
            |> ModulePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root"

        testCase "nested chain is root-first" <| fun _ ->
            ModulePath.createFromList [ "Root"; "Foo"; "Bar" ]
            |> ModulePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root.Foo.Bar"

        testCase "drops empty leaf name" <| fun _ ->
            // a "" leaf (e.g. an erased type-alias body anchor) must not appear as a segment
            ModulePath.init "Root"
            |> ModulePath.create ""
            |> ModulePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root"

        testCase "drops empty interior name" <| fun _ ->
            ModulePath.init "Root"
            |> ModulePath.create ""
            |> ModulePath.create "Leaf"
            |> ModulePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root.Leaf"

        testCase "empty root collapses to empty list" <| fun _ ->
            ModulePath.init ""
            |> ModulePath.flatten
            |> Flip.Expect.equal "" []
    ]

// ---------------------------------------------------------------------------
// TypePath.flatten — = ModulePath.flatten parent @ [ name ]
// ---------------------------------------------------------------------------

[<Tests>]
let typePathFlatten =
    testList "TypePath.flatten" [
        testCase "parent module trace plus leaf type name" <| fun _ ->
            ModulePath.createFromList [ "Root"; "Foo" ]
            |> TypePath.create "Bar"
            |> TypePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root.Foo.Bar"

        testCase "type at module root" <| fun _ ->
            ModulePath.init "Root"
            |> TypePath.create "Type"
            |> TypePath.flatten
            |> dot
            |> Flip.Expect.equal "" "Root.Type"

        testCase "equals ModulePath.flatten parent @ [ name ]" <| fun _ ->
            let parent = ModulePath.createFromList [ "A"; "B"; "C" ]
            let typePath = TypePath.create "Leaf" parent
            let viaContract = ModulePath.flatten parent @ [ Name.Pascal.create "Leaf" ]
            TypePath.flatten typePath
            |> dot
            |> Flip.Expect.equal "" (dot viaContract)
    ]

// ---------------------------------------------------------------------------
// TransientTypePath.toAnchored — OWN segments only (no anchor prefix)
// ---------------------------------------------------------------------------

[<Tests>]
let transientTypeToAnchored =
    testList "TransientTypePath.toAnchored" [
        testCase "Anchored has no own segments" <| fun _ ->
            TransientTypePath.Anchored
            |> TransientTypePath.toAnchored
            |> Flip.Expect.equal "" []

        testCase "AnchoredAndMoored carries only its own name" <| fun _ ->
            TransientTypePath.AnchoredAndMoored(Name.Pascal.create "Foo")
            |> TransientTypePath.toAnchored
            |> dotName
            |> Flip.Expect.equal "" "Foo"

        testCase "Moored prepends the transient module trace" <| fun _ ->
            // _module_ "T"; _type_ "C"  -> own segments T.C, no anchor
            pathCe { _module_ "T"; _type_ "C" }
            |> TransientTypePath.toAnchored
            |> dotName
            |> Flip.Expect.equal "" "T.C"

        testCase "Moored multi-level module trace" <| fun _ ->
            pathCe { _module_ "A"; _module_ "B"; _type_ "C" }
            |> TransientTypePath.toAnchored
            |> dotName
            |> Flip.Expect.equal "" "A.B.C"
    ]

// ---------------------------------------------------------------------------
// AnchorPath.traceToParentModule — one case per AnchorPath constructor.
// Contract: returns (parentModule, pathTrace) where pathTrace is the names
// BELOW the parent module, EXCLUDING the leaf's own name for Module/Type, but
// INCLUDING intermediate type/member names for Member/Parameter/TypeParam.
// ---------------------------------------------------------------------------

/// Render a (ModulePath, Name list) trace pair as "module || trace".
let traceStr (modulePath: ModulePath, trace: Name list) =
    let m = ModulePath.flatten modulePath |> dot
    let t = trace |> dotName
    sprintf "%s || %s" m t

[<Tests>]
let anchorTraceToParentModule =
    testList "AnchorPath.traceToParentModule" [
        testCase "Module case: module is itself, empty trace" <| fun _ ->
            ModulePath.createFromList [ "Root"; "Foo" ]
            |> AnchorPath.Module
            |> AnchorPath.traceToParentModule
            |> traceStr
            |> Flip.Expect.equal "" "Root.Foo || "

        testCase "Type case: parent module, trace is the type name" <| fun _ ->
            ModulePath.createFromList [ "Root"; "Foo" ]
            |> TypePath.create "Bar"
            |> AnchorPath.Type
            |> AnchorPath.traceToParentModule
            |> traceStr
            |> Flip.Expect.equal "" "Root.Foo || Bar"

        testCase "Member on type: parent module, trace is [type; member]" <| fun _ ->
            ModulePath.init "Root"
            |> TypePath.create "Foo"
            |> MemberPath.createOnType "op"
            |> AnchorPath.Member
            |> AnchorPath.traceToParentModule
            |> traceStr
            |> Flip.Expect.equal "" "Root || Foo.op"

        testCase "Member on module: parent module, trace is just [member]" <| fun _ ->
            ModulePath.createFromList [ "Root"; "Mod" ]
            |> MemberPath.createOnModule "fn"
            |> AnchorPath.Member
            |> AnchorPath.traceToParentModule
            |> traceStr
            |> Flip.Expect.equal "" "Root.Mod || fn"
    ]

// ---------------------------------------------------------------------------
// TransientTypePath.anchor — prepends the anchor's pathTrace.
// Assert the exact combined ANCHORED TypePath for each representative
// AnchorPath case (Type, Member, Module).
// ---------------------------------------------------------------------------

let anchorTypeStr (anchor: AnchorPath) (transient: TransientTypePath) =
    TransientTypePath.anchor anchor transient
    |> TypePath.flatten
    |> dot

[<Tests>]
let transientTypeAnchor =
    testList "TransientTypePath.anchor" [
        testCase "Anchored on a Type anchor collapses to the anchor type" <| fun _ ->
            // empty transient -> the combined trace is just the anchor's own [type]
            let anchor =
                ModulePath.init "Root"
                |> TypePath.create "Foo"
                |> AnchorPath.Type
            anchorTypeStr anchor TransientTypePath.Anchored
            |> Flip.Expect.equal "" "Root.Foo"

        testCase "AnchoredAndMoored on a Type anchor nests under the anchor type" <| fun _ ->
            let anchor =
                ModulePath.init "Root"
                |> TypePath.create "Foo"
                |> AnchorPath.Type
            anchorTypeStr anchor (TransientTypePath.AnchoredAndMoored(Name.Pascal.create "Bar"))
            |> Flip.Expect.equal "" "Root.Foo.Bar"

        testCase "Moored on a Type anchor appends module+type trace" <| fun _ ->
            let anchor =
                ModulePath.init "A"
                |> TypePath.create "B"
                |> AnchorPath.Type
            anchorTypeStr anchor (pathCe { _module_ "T"; _type_ "C" })
            |> Flip.Expect.equal "" "A.B.T.C"

        testCase "AnchoredAndMoored on a Member anchor nests under member" <| fun _ ->
            // Member anchor's trace is [Type; member]; the transient type sits below it.
            let anchor =
                ModulePath.init "Root"
                |> TypePath.create "Foo"
                |> MemberPath.createOnType "op"
                |> AnchorPath.Member
            anchorTypeStr anchor (TransientTypePath.AnchoredAndMoored(Name.Pascal.create "MType"))
            |> Flip.Expect.equal "" "Root.Foo.Op.MType"

        testCase "AnchoredAndMoored on a Module anchor nests directly under module" <| fun _ ->
            let anchor =
                ModulePath.createFromList [ "Root"; "Mod" ]
                |> AnchorPath.Module
            anchorTypeStr anchor (TransientTypePath.AnchoredAndMoored(Name.Pascal.create "Foo"))
            |> Flip.Expect.equal "" "Root.Mod.Foo"
    ]

// ---------------------------------------------------------------------------
// Path.getRelativePath — strips the common module prefix (localise).
// ---------------------------------------------------------------------------

let relStr (target: TypePath) (from: AnchorPath) =
    Path.getRelativePath target from |> dot

[<Tests>]
let getRelativePath =
    testList "Path.getRelativePath" [
        testCase "shared module prefix is stripped to the leaf type" <| fun _ ->
            let target =
                ModulePath.createFromList [ "Root"; "Foo" ]
                |> TypePath.create "Bar"
            let from =
                ModulePath.createFromList [ "Root"; "Foo" ]
                |> TypePath.create "Local"
                |> AnchorPath.Type
            relStr target from
            |> Flip.Expect.equal "" "Bar"

        testCase "partial shared prefix keeps divergent tail" <| fun _ ->
            let target =
                ModulePath.createFromList [ "Shared"; "TargetMod" ]
                |> TypePath.create "Target"
            let from =
                ModulePath.createFromList [ "Shared"; "LocalMod" ]
                |> MemberPath.createOnModule "fn"
                |> AnchorPath.Member
            relStr target from
            |> Flip.Expect.equal "" "TargetMod.Target"

        testCase "no shared prefix yields full target path" <| fun _ ->
            let target =
                ModulePath.createFromList [ "TargetRoot"; "TargetMod" ]
                |> TypePath.create "Target"
            let from =
                ModulePath.createFromList [ "LocalRoot"; "LocalMod" ]
                |> MemberPath.createOnModule "fn"
                |> AnchorPath.Member
            relStr target from
            |> Flip.Expect.equal "" "TargetRoot.TargetMod.Target"

        // MODULE-GLOBAL ROOT-ANCHOR (the 2026-07-05 globals-holder fix). A module-scope
        // global (`declare namespace M { export const x: T }`) is collected into a holder
        // type emitted at namespace ROOT. Localising its type ref against the member's OWN
        // module `M` strips M's prefix -> bare `Target`, which is out of scope in the
        // root-emitted holder (and shadow-binds a bare `SharedLiterals.X` across the
        // namespace-rec assembly boundary). Localising against a ROOT anchor (empty module
        // trace) keeps the full `M.Target` qualification. These two cases pin both sides.
        testCase "in-module anchor strips to bare leaf (the over-strip bug)" <| fun _ ->
            let target =
                ModulePath.createFromList [ "M" ]
                |> TypePath.create "Target"
            let inModule =
                ModulePath.createFromList [ "M" ]
                |> MemberPath.createOnModule "x"
                |> AnchorPath.Member
            relStr target inModule
            |> Flip.Expect.equal "in-module strips M" "Target"

        testCase "root anchor (empty module) keeps the M. qualification" <| fun _ ->
            let target =
                ModulePath.createFromList [ "M" ]
                |> TypePath.create "Target"
            let rootAnchor =
                ModulePath.init ""
                |> MemberPath.createOnModule "x"
                |> AnchorPath.Member
            relStr target rootAnchor
            |> Flip.Expect.equal "root keeps M.Target" "M.Target"
    ]
