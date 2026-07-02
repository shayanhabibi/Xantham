module Xantham.Generator.Tests.Tests.TypeRefRenderSubstituteForHeritage

(*
Coverage plane for the ORPHAN-TYPAR GUARD — the Anchored `TypeRefRender.substituteForHeritage`
walk (Types/RenderScope.Anchored.fs). This is the walk the anchoring stage runs on every
member/function/alias-body/heritage render BETWEEN anchor and localise (`Render.anchorScrubLocalise`,
Generator/RenderScope.Anchored.fs): a typar atom not in the in-scope set — a FREE typar leaking
from a collapsed intermediate alias body — is rewritten to `obj`, the only faithful rendering of
a type variable that is unreachable at the emission site.

PLACEMENT CONSTRAINT (pinned by the Widget-opacity case below): `localise` rewrites EVERY atom
to an opaque Widget, so the guard sees typars only while they are still `Intrinsic` atoms. Running
the walk post-localise is a silent no-op — this is exactly the bug the first (reverted) render-time
guard had. The typar atom carries its QUOTED name (`'T`) because `Name.Typar` normalization bakes
the quote into the stored name (`sourceNormalizeForTypeParameter = sprintf "'%s"`); the in-scope
set must therefore hold quoted names — `Render.typarNameSet` pins that convention.

Adjacent-test audit note (2026-07-01): this file previously tested `Prelude.substituteForHeritage`
— a structurally-identical twin with ZERO production callers (both live call sites type-resolve to
the Anchored walk). The dead twin was removed and this plane retargeted at the live one.
*)

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Types.Anchored

// ---------------------------------------------------------------------------
// Construction helpers (Anchored shapes are public DUs — direct construction)
// ---------------------------------------------------------------------------

let private atom (a: TypeRefAtom) : TypeRefRender = { Kind = TypeRefKind.Atom a; Nullable = false }
let private intrinsic (s: string) = atom (TypeRefAtom.Intrinsic s)
let private molecule (m: TypeRefMolecule) : TypeRefRender = { Kind = TypeRefKind.Molecule m; Nullable = false }
let private substitute inScope render = TypeRefRender.substituteForHeritage inScope render

let private expectIntrinsic (expected: string) (render: TypeRefRender) =
    match render.Kind with
    | TypeRefKind.Atom (TypeRefAtom.Intrinsic s) -> Expect.equal s expected "intrinsic atom"
    | _ -> failtest $"expected Intrinsic '{expected}' atom"

let private inScopeT = Set.singleton "'T"

[<Tests>]
let conventionTests =
    testList "typar name convention (quoted)" [
        // The quote is BAKED into the stored typar name at normalization — the atom string and
        // the in-scope set share the same quoted form. If this pin breaks, the guard silently
        // stops matching (either erasing everything in scope or nothing at all).
        testCase "Name.Typar.create bakes the leading quote" <| fun _ ->
            Name.Typar.create "T"
            |> Name.Case.valueOrModified
            |> Flip.Expect.equal "typar names carry their quote" "'T"

        testCase "Render.typarNameSet yields the quoted set matching atom strings" <| fun _ ->
            let tp name : Prelude.TypeParameterRender<Prelude.TypeRefRender, Name<Case.typar>> = {
                Metadata = {
                    Path = Path.create TransientTypePath.Anchored
                    Original = Path.create TransientTypePath.Anchored
                    Source = ValueNone
                    FullyQualifiedName = ValueNone
                }
                Name = Name.Typar.create name
                Constraint = ValueNone
                Default = ValueNone
                Documentation = []
            }
            Render.typarNameSet [ tp "T"; tp "Env" ]
            |> Flip.Expect.equal "quoted, ready for Set.contains against atom strings" (Set.ofList [ "'T"; "'Env" ])
    ]

[<Tests>]
let substituteForHeritageTests =
    testList "Anchored.substituteForHeritage (the live orphan-typar walk)" [

        testCase "wildcard _ erases to obj" <| fun _ ->
            substitute Set.empty (intrinsic "_")
            |> expectIntrinsic "obj"

        testCase "orphan typar erases to obj" <| fun _ ->
            substitute inScopeT (intrinsic "'U")
            |> expectIntrinsic "obj"

        testCase "in-scope typar is preserved" <| fun _ ->
            substitute inScopeT (intrinsic "'T")
            |> expectIntrinsic "'T"

        testCase "non-typar intrinsic is untouched" <| fun _ ->
            substitute Set.empty (intrinsic "string")
            |> expectIntrinsic "string"

        testCase "Path atom is untouched" <| fun _ ->
            let path = ModulePath.createFromList [ "M" ] |> TypePath.create "Widget"
            let result = substitute Set.empty (atom (TypeRefAtom.Path path))
            match result.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Path p) -> Expect.equal p path "path preserved"
            | _ -> failtest "expected Path atom"

        // A Widget atom is OPAQUE to the guard — even if it wraps a typar ident. This pins the
        // placement constraint: the guard MUST run before localise (which widgetizes every atom),
        // or it becomes a silent no-op.
        testCase "Widget atom is opaque — a widget-wrapped typar is NOT erased" <| fun _ ->
            let widget = atom (TypeRefAtom.Widget (Ast.LongIdent "'S"))
            let result = substitute Set.empty widget
            match result.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Widget _) -> ()
            | _ -> failtest "expected the opaque Widget to pass through unchanged"

        testCase "Union members are walked" <| fun _ ->
            let result =
                molecule (TypeRefMolecule.Union [ intrinsic "'U"; intrinsic "'T" ])
                |> substitute inScopeT
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Union [ a; b ]) ->
                expectIntrinsic "obj" a
                expectIntrinsic "'T" b
            | _ -> failtest "expected 2-member Union"

        testCase "Function parameters and return are walked" <| fun _ ->
            let result =
                molecule (TypeRefMolecule.Function ([ intrinsic "'U" ], intrinsic "'V"))
                |> substitute Set.empty
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Function ([ p ], r)) ->
                expectIntrinsic "obj" p
                expectIntrinsic "obj" r
            | _ -> failtest "expected 1-param Function"

        testCase "Tuple members are walked" <| fun _ ->
            let result =
                molecule (TypeRefMolecule.Tuple [ intrinsic "'U"; intrinsic "string" ])
                |> substitute Set.empty
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Tuple [ a; b ]) ->
                expectIntrinsic "obj" a
                expectIntrinsic "string" b
            | _ -> failtest "expected 2-member Tuple"

        testCase "Prefix args are walked; in-scope args preserved" <| fun _ ->
            let head = ModulePath.createFromList [ "M" ] |> TypePath.create "Container" |> TypeRefAtom.Path |> atom
            let result =
                molecule (TypeRefMolecule.Prefix (head, [ intrinsic "'U"; intrinsic "'T" ]))
                |> substitute inScopeT
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix (_, [ a; b ])) ->
                expectIntrinsic "obj" a
                expectIntrinsic "'T" b
            | _ -> failtest "expected Prefix with 2 args"

        testCase "deep nesting: orphan inside Prefix inside Union is erased" <| fun _ ->
            let inner = molecule (TypeRefMolecule.Prefix (intrinsic "option", [ intrinsic "'S" ]))
            let result =
                molecule (TypeRefMolecule.Union [ inner; intrinsic "bool" ])
                |> substitute inScopeT
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Union [ u; _ ]) ->
                match u.Kind with
                | TypeRefKind.Molecule (TypeRefMolecule.Prefix (_, [ arg ])) -> expectIntrinsic "obj" arg
                | _ -> failtest "expected inner Prefix"
            | _ -> failtest "expected Union"

        testCase "Nullable flag is preserved through substitution" <| fun _ ->
            let result = substitute Set.empty { intrinsic "_" with Nullable = true }
            Expect.isTrue result.Nullable "Nullable preserved"

        // KNOWN GAP (adjacent to the obj-given-args conformance class, 27 errors at the 511
        // floor): when a Prefix HEAD itself erases to obj, the args should collapse with it —
        // `obj` is non-generic, so `obj<A, B>` is invalid F# (FS0017/FS0035). The current walk
        // keeps the args. The live obj<...> emissions come from a DIFFERENT arm (erased-union
        // heads), so this collapse is latent here — but it is this walk's correct contract.
        ptestCase "erased Prefix head collapses its args (pending)" <| fun _ ->
            let result =
                molecule (TypeRefMolecule.Prefix (intrinsic "'Dead", [ intrinsic "string" ]))
                |> substitute Set.empty
            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix (head, args)) ->
                expectIntrinsic "obj" head
                Expect.isEmpty args "an erased head has no arity — args must collapse"
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> ()
            | _ -> failtest "expected erased-head Prefix to collapse toward obj"
    ]
