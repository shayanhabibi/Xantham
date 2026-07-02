module Xantham.Generator.Tests.Tests.AnchoredRegistration

(*
Coverage plane for two 2026-07-04 seams:

1. CHILD-PRESERVING REPLACEMENT — `GeneratorContext.Anchored.addOrReplace`
   (Types/Generator.fs). Registration is last-wins, but a RE-anchor (the scrub-armed
   second export pass) re-renders against the prelude cache, and cache-hits
   under-register nested children that only FRESH minting places in the store — so a
   replacement scope could silently LOSE materialized child defs (measured:
   ServiceWorkerGlobalScope's nested globals vanished while their refs survived the
   def/ref-closure scrub). Contract: replacing a Choice2Of2 scope UNIONS the previous
   registration's Anchors into the new one — a re-anchor may improve the scope's own
   render, never shrink its children; entries present in BOTH keep the NEW value.

2. ABBREVIATION-LEGALITY VERDICT — `Render.aliasBodyVerdict`
   (Generator/RenderScope.Anchored.fs), shared by the Transient AND Concrete alias
   arms (the twin arms — the Concrete one is where top-level exported aliases anchor).
   A body atom carrying the alias's OWN path (the prerender cycle-break placeholder:
   `type X<'T> = option<U3<X,X,X>>`, FS0953) or a declared typar absent from the
   post-scrub body (FS0035 — the arity-changing fix is the pinned +196 regression)
   makes the abbreviation illegal: verdict `None`, ledgered by class, and the caller
   emits an empty same-arity interface instead. Legal bodies come back localised.
*)

open System.Collections.Generic
open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Anchored
open Mocking.ArenaInterner.ResolvedType

// ---------------------------------------------------------------------------
// Construction helpers (idioms from TypeRefRender.HomeChildScrub.fs)
// ---------------------------------------------------------------------------

let private atom (a: TypeRefAtom) : TypeRefRender = { Kind = TypeRefKind.Atom a; Nullable = false }
let private intrinsic (s: string) = atom (TypeRefAtom.Intrinsic s)
let private molecule (m: TypeRefMolecule) : TypeRefRender = { Kind = TypeRefKind.Molecule m; Nullable = false }
let private typePath (modules: string list) (name: string) =
    ModulePath.createFromList modules |> TypePath.create name
let private pathAtom (modules: string list) (name: string) =
    typePath modules name |> TypeRefAtom.Path |> atom

let private neverForced : Lazy<TypeRender> = lazy (failwith "the merge must never force a render")

let private scopeFor (rt: ResolvedType) (path: TypePath) (children: (ResolvedType * (TypePath * Render)) list) : RenderScope =
    let anchors = Dictionary<ResolvedType, TypePath * Render>()
    for key, entry in children do anchors.Add(key, entry)
    {
        Type = rt
        Root = Choice1Of2 path
        TypeRef = pathAtom [ "M" ] "Parent"
        Render = (pathAtom [ "M" ] "Parent", neverForced)
        Anchors = anchors
    }

let private childEntry (modules: string list) (name: string) : TypePath * Render =
    typePath modules name, (pathAtom modules name, neverForced)

[<Tests>]
let childPreservingReplacementTests =
    testList "Anchored.addOrReplace (child-preserving replacement)" [

        testCase "replacement UNIONS the previous registration's children" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let parentRt = Interface.create "Parent" |> Interface.wrap
            let childRt = Interface.create "Child" |> Interface.wrap
            let parentPath = typePath [ "M" ] "Parent"
            scopeFor parentRt parentPath [ childRt, childEntry [ "M"; "Parent" ] "Child" ]
            |> Choice2Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            // The re-anchor: same key, EMPTY Anchors (the cache-hit under-registration shape).
            scopeFor parentRt parentPath []
            |> Choice2Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            match GeneratorContext.Anchored.tryGetResolvedType ctx parentRt with
            | ValueSome (Choice2Of2 scope) ->
                Expect.isTrue (scope.Anchors.ContainsKey childRt) "the first pass's child survives the replacement"
                Expect.equal scope.Anchors.Count 1 "exactly the union of both registrations"
            | _ -> failtest "expected the replaced Choice2Of2 scope"

        testCase "entries present in BOTH registrations keep the NEW value" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let parentRt = Interface.create "Parent2" |> Interface.wrap
            let childRt = Interface.create "Child2" |> Interface.wrap
            let parentPath = typePath [ "M" ] "Parent2"
            scopeFor parentRt parentPath [ childRt, childEntry [ "M"; "Parent2" ] "OldLeaf" ]
            |> Choice2Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            scopeFor parentRt parentPath [ childRt, childEntry [ "M"; "Parent2" ] "NewLeaf" ]
            |> Choice2Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            match GeneratorContext.Anchored.tryGetResolvedType ctx parentRt with
            | ValueSome (Choice2Of2 scope) ->
                let path, _ = scope.Anchors[childRt]
                Expect.equal (Name.Case.valueOrModified path.Name) "NewLeaf" "the replacement's entry wins on conflict"
            | _ -> failtest "expected the replaced Choice2Of2 scope"

        testCase "a Choice1Of2 (ref-only) replacement does not resurrect scope semantics" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let parentRt = Interface.create "Parent3" |> Interface.wrap
            let childRt = Interface.create "Child3" |> Interface.wrap
            let parentPath = typePath [ "M" ] "Parent3"
            scopeFor parentRt parentPath [ childRt, childEntry [ "M"; "Parent3" ] "Child3" ]
            |> Choice2Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            pathAtom [ "M" ] "Parent3"
            |> Choice1Of2
            |> GeneratorContext.Anchored.addResolvedType ctx parentRt
            match GeneratorContext.Anchored.tryGetResolvedType ctx parentRt with
            | ValueSome (Choice1Of2 _) -> ()
            | _ -> failtest "a ref-only registration replaces wholesale (no scope to merge into)"
    ]

// ---------------------------------------------------------------------------
// aliasBodyVerdict
// ---------------------------------------------------------------------------

let private verdictFor (name: string) (inScope: Set<string>) (body: TypeRefRender) =
    let ctx = GeneratorContext.Empty
    let anchorPath = typePath [ "M" ] name |> AnchorPath.create
    let verdict = Render.aliasBodyVerdict ctx anchorPath (Name.Pascal.create name) inScope body
    verdict, ctx

let private ledgerCount (ctx: GeneratorContext) (key: string) =
    match ctx.AdvisoryLedger.TryGetValue key with
    | true, n -> n
    | _ -> 0

[<Tests>]
let aliasBodyVerdictTests =
    testList "Render.aliasBodyVerdict (abbreviation legality)" [

        testCase "SELF-REFERENTIAL body -> None, ledgered cyclic-alias-body" <| fun _ ->
            // type Self = option<U3<Self, Self, Self>> — the cycle-break placeholder shape.
            let body =
                molecule (TypeRefMolecule.Union [ pathAtom [ "M" ] "Self"; intrinsic "bool" ])
            let verdict, ctx = verdictFor "Self" Set.empty body
            Expect.isNone verdict "self-referential abbreviation must degrade"
            Expect.equal (ledgerCount ctx "cyclic-alias-body:Self") 1 "ledgered as cyclic"

        testCase "PHANTOM TYPAR (declared, absent from body) -> None, ledgered phantom-typar-alias" <| fun _ ->
            let verdict, ctx = verdictFor "Ghosted" (Set.singleton "'T") (intrinsic "string")
            Expect.isNone verdict "unused-typar abbreviation is illegal F#"
            Expect.equal (ledgerCount ctx "phantom-typar-alias:Ghosted") 1 "ledgered as phantom"

        testCase "typar USED in the body -> legal abbreviation" <| fun _ ->
            let body = molecule (TypeRefMolecule.Prefix (intrinsic "option", [ intrinsic "'T" ]))
            let verdict, ctx = verdictFor "Uses" (Set.singleton "'T") body
            Expect.isSome verdict "a typar-using body stays an abbreviation"
            Expect.equal (ledgerCount ctx "cyclic-alias-body:Uses" + ledgerCount ctx "phantom-typar-alias:Uses") 0 "nothing ledgered"

        testCase "zero-typar body referencing OTHER types -> legal abbreviation" <| fun _ ->
            let verdict, _ = verdictFor "Plain" Set.empty (pathAtom [ "M" ] "Other")
            Expect.isSome verdict "ordinary alias body is legal"

        testCase "self-reference nested deep inside molecules is still caught" <| fun _ ->
            let inner = molecule (TypeRefMolecule.Prefix (intrinsic "option", [ pathAtom [ "M" ] "Deep" ]))
            let body = molecule (TypeRefMolecule.Tuple [ intrinsic "string"; inner ])
            let verdict, ctx = verdictFor "Deep" Set.empty body
            Expect.isNone verdict "the walk reaches nested atoms"
            Expect.equal (ledgerCount ctx "cyclic-alias-body:Deep") 1 "ledgered"

        testCase "a DIFFERENT type with the same name under another module is NOT self" <| fun _ ->
            let verdict, _ = verdictFor "Twin" Set.empty (pathAtom [ "Other" ] "Twin")
            Expect.isSome verdict "path comparison is full-chain, not name-based"
    ]

// ---------------------------------------------------------------------------
// Per-export scope-store memoization
// ---------------------------------------------------------------------------

[<Tests>]
let exportScopeStoreMemoizationTests =
    testList "registerAnchorFromExport (memoized per-export scope stores)" [

        // Nested children register into the export's RenderScopeStore only when their
        // refs mint FRESH — a re-anchor pass (the scrub-armed second export pass) that
        // rebuilt stores from scratch would cache-hit those renders and under-register,
        // silently shrinking the child set (measured: the Get/Fetch/Service residue —
        // refs living inside pass-1-preserved children, never re-anchored under the
        // armed scrubs). Contract: the SAME store instance serves every pass.
        testCase "the second pass reuses the first pass's store instance" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let export = Interface.create "Exported" |> ResolvedExport.Interface
            RenderScope_Anchored.registerAnchorFromExport ctx export
            let firstStore = ctx.ExportScopeStores[export]
            RenderScope_Anchored.registerAnchorFromExport ctx export
            Expect.equal ctx.ExportScopeStores.Count 1 "one store per export"
            Expect.isTrue
                (obj.ReferenceEquals(ctx.ExportScopeStores[export], firstStore))
                "the memoized store instance is reused across passes"
    ]
