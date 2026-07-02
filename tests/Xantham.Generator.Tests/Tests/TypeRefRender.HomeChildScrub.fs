module Xantham.Generator.Tests.Tests.TypeRefRenderHomeChildScrub

(*
Coverage plane for the HOME-CHILD DEF/REF CLOSURE SCRUB — the Anchored
`TypeRefRender.scrubUndefinedHomeChildren` walk (Types/RenderScope.Anchored.fs), the
ledgered "around" for the multi-member shared-rt class (one rt referenced through several
members stamps N per-site leafs while the rt-keyed TypeStore registers ONE def — history in
partition-gate.baseline 2026-07-04 (6)/(7)/(8)).

CONTRACT UNDER TEST: a Path atom whose MODULE chain passes through a canonical home path
(`homePaths`, dotted `valueOrModified` segments) and whose own flattened path is absent from
the materialized def-set (`definedPaths`) can never resolve — it degrades to `obj` and the
ledger callback fires with the matched home. Everything else passes through unchanged, and
the walk is INACTIVE until armed (empty `homePaths`) — production arms it only in
`emitCanonicalPreludeScopes`' post-drive second round, when the def-set is final. That
end-to-end round (re-anchoring every canonical scope with the scrub armed) is pinned by the
partition gate (Zod unit at 0 + `home-child-scrub:*` ledger entries) rather than here: its
inputs are the full driver state, not a unit-testable seam.
*)

open System.Collections.Generic
open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types.Anchored

// ---------------------------------------------------------------------------
// Construction helpers (mirroring TypeRefRender.SubstituteForHeritage.fs)
// ---------------------------------------------------------------------------

let private atom (a: TypeRefAtom) : TypeRefRender = { Kind = TypeRefKind.Atom a; Nullable = false }
let private intrinsic (s: string) = atom (TypeRefAtom.Intrinsic s)
let private molecule (m: TypeRefMolecule) : TypeRefRender = { Kind = TypeRefKind.Molecule m; Nullable = false }
let private pathAtom (modules: string list) (name: string) =
    ModulePath.createFromList modules |> TypePath.create name |> TypeRefAtom.Path |> atom

let private homes (paths: string list) = HashSet<string>(paths)
let private defs (paths: string list) = HashSet<string>(paths)

/// Run the scrub capturing ledger callbacks.
let private scrub homePaths definedPaths render =
    let ledgered = ResizeArray<string>()
    let result = TypeRefRender.scrubUndefinedHomeChildren homePaths definedPaths ledgered.Add render
    result, List.ofSeq ledgered

let private expectObj (render: TypeRefRender) =
    match render.Kind with
    | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> ()
    | _ -> failtest "expected the atom to degrade to obj"

let private expectPath (modules: string list) (name: string) (render: TypeRefRender) =
    match render.Kind with
    | TypeRefKind.Atom (TypeRefAtom.Path p) ->
        let flat = TypePath.flatten p |> List.map Name.Case.valueOrModified
        Expect.equal flat (modules @ [ name ]) "path preserved"
    | _ -> failtest "expected a preserved Path atom"

// The canonical shapes from the measured class: a home type used as a module segment.
let private homeSet = homes [ "SharedLiterals.Home" ]

[<Tests>]
let homeChildScrubTests =
    testList "Anchored.scrubUndefinedHomeChildren (home-child def/ref closure)" [

        testCase "INACTIVE when homePaths is empty — undefined child survives" <| fun _ ->
            let render = pathAtom [ "SharedLiterals"; "Home" ] "Ghost"
            let result, ledgered = scrub (homes []) (defs []) render
            expectPath [ "SharedLiterals"; "Home" ] "Ghost" result
            Expect.isEmpty ledgered "inactive scrub must not ledger"

        testCase "undefined home-child degrades to obj and ledgers the home" <| fun _ ->
            let render = pathAtom [ "SharedLiterals"; "Home" ] "Ghost"
            let result, ledgered = scrub homeSet (defs []) render
            expectObj result
            Expect.equal ledgered [ "SharedLiterals.Home" ] "ledger keyed by the matched home"

        testCase "DEFINED home-child is preserved, ledger silent" <| fun _ ->
            let render = pathAtom [ "SharedLiterals"; "Home" ] "Code"
            let result, ledgered = scrub homeSet (defs [ "SharedLiterals.Home.Code" ]) render
            expectPath [ "SharedLiterals"; "Home" ] "Code" result
            Expect.isEmpty ledgered "defined children never ledger"

        testCase "path NOT under any home is untouched (even when undefined)" <| fun _ ->
            let render = pathAtom [ "Workers" ] "Request"
            let result, ledgered = scrub homeSet (defs []) render
            expectPath [ "Workers" ] "Request" result
            Expect.isEmpty ledgered "non-home paths are out of scope"

        testCase "the home's OWN path is not a home-child — untouched" <| fun _ ->
            // The atom's PARENT chain must pass through the home; the home type itself
            // (parent = SharedLiterals) is a def, not a child ref.
            let render = pathAtom [ "SharedLiterals" ] "Home"
            let result, ledgered = scrub homeSet (defs []) render
            expectPath [ "SharedLiterals" ] "Home" result
            Expect.isEmpty ledgered "the home itself is not scrubbed"

        testCase "undefined GRANDCHILD under a home degrades (depth-general prefix match)" <| fun _ ->
            let render = pathAtom [ "SharedLiterals"; "Home"; "Code" ] "Deep"
            let result, ledgered = scrub homeSet (defs [ "SharedLiterals.Home.Code" ]) render
            expectObj result
            Expect.equal ledgered [ "SharedLiterals.Home" ] "grandchild attributes to the home"

        testCase "defined grandchild is preserved" <| fun _ ->
            let render = pathAtom [ "SharedLiterals"; "Home"; "Code" ] "Deep"
            let result, _ =
                scrub homeSet (defs [ "SharedLiterals.Home.Code"; "SharedLiterals.Home.Code.Deep" ]) render
            expectPath [ "SharedLiterals"; "Home"; "Code" ] "Deep" result

        testCase "molecule walk: undefined child inside Prefix args degrades, sibling arg survives" <| fun _ ->
            let ghost = pathAtom [ "SharedLiterals"; "Home" ] "Ghost"
            let head = intrinsic "proptypekey"
            let result, ledgered =
                molecule (TypeRefMolecule.Prefix (head, [ ghost; intrinsic "string" ]))
                |> scrub homeSet (defs [])
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix (_, [ a; b ])) ->
                expectObj a
                match b.Kind with
                | TypeRefKind.Atom (TypeRefAtom.Intrinsic "string") -> ()
                | _ -> failtest "sibling intrinsic arg must survive"
            | _ -> failtest "expected Prefix with 2 args"
            Expect.equal ledgered [ "SharedLiterals.Home" ] "one scrub, one ledger entry"

        testCase "Intrinsic and Widget atoms are untouched" <| fun _ ->
            let result, ledgered =
                molecule (TypeRefMolecule.Union [ intrinsic "bool"; atom (TypeRefAtom.Widget (Ast.LongIdent "W")) ])
                |> scrub homeSet (defs [])
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Union [ a; b ]) ->
                (match a.Kind with
                 | TypeRefKind.Atom (TypeRefAtom.Intrinsic "bool") -> ()
                 | _ -> failtest "intrinsic must survive")
                match b.Kind with
                | TypeRefKind.Atom (TypeRefAtom.Widget _) -> ()
                | _ -> failtest "widget must survive"
            | _ -> failtest "expected 2-member Union"
            Expect.isEmpty ledgered "nothing to ledger"

        testCase "Nullable flag survives a scrub" <| fun _ ->
            let render = { pathAtom [ "SharedLiterals"; "Home" ] "Ghost" with Nullable = true }
            let result, _ = scrub homeSet (defs []) render
            Expect.isTrue result.Nullable "Nullable preserved"
            expectObj { result with Nullable = false }
    ]
