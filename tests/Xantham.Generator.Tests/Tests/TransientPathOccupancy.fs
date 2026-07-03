module Xantham.Generator.Tests.Tests.TransientPathOccupancy

(*
Coverage plane for PATH OCCUPANCY at the transient-path mint (2026-07-05). A NAMELESS
hoisted literal takes its entire path identity from the scope's PathContext (the owning
member/parameter name), so N>1 structurally-distinct types reaching ONE context claimed
the SAME path: their defs merged at collection (the FS0438 duplicate-member class,
ledgered same-path-def-merge — XOR arms, union arms, overload-param literals) and their
refs rendered self-unions (U2<X, X>). `createTransientPath` now claims paths per scope:
the FIRST occupant keeps today's behavior verbatim; each later DISTINCT type re-homes to
a `Case{n}` CHILD of the context path (a child, not a sibling suffix — the only shape the
export-anchored def walk and the member-anchored ref atom both reach). Named transients
keep first-wins semantics and only REGISTER their claim.
*)

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

let private literal (propName: string) (typ: TypeKindPrimitive) =
    TypeLiteral.empty
    |> TypeLiteral.addMember (Property.create propName (primitive typ) |> Property.wrap)
    |> TypeLiteral.wrap

let private spine (p: TransientTypePath) =
    TransientTypePath.toAnchored p |> List.map _.ValueOrSource

let private mint scope rt =
    RenderScopeStore.TypeRefAtom.createTransientPath scope rt TransientTypePath.Anchored
    |> ignore

[<Tests>]
let tests =
    testList "createTransientPath — path occupancy" [

        testCase "first occupant stores the context path (unchanged behavior)" <| fun _ ->
            let scope = RenderScopeStore.create () |> fun s -> RenderScopeStore.appendStringToPathContext s "Inputs"
            let rt = literal "a" TypeKindPrimitive.String
            mint scope rt
            spine scope.TypeStore[rt]
            |> Flip.Expect.equal "first occupant keeps the context name" [ "Inputs" ]

        testCase "a second DISTINCT type re-homes to a Case2 child of the context path" <| fun _ ->
            let scope = RenderScopeStore.create () |> fun s -> RenderScopeStore.appendStringToPathContext s "Inputs"
            let rt1 = literal "a" TypeKindPrimitive.String
            let rt2 = literal "b" TypeKindPrimitive.Number
            mint scope rt1
            mint scope rt2
            spine scope.TypeStore[rt2]
            |> Flip.Expect.equal "second occupant nests under the context path" [ "Inputs"; "Case2" ]

        testCase "a third distinct type takes Case3" <| fun _ ->
            let scope = RenderScopeStore.create () |> fun s -> RenderScopeStore.appendStringToPathContext s "Inputs"
            mint scope (literal "a" TypeKindPrimitive.String)
            mint scope (literal "b" TypeKindPrimitive.Number)
            let rt3 = literal "c" TypeKindPrimitive.Boolean
            mint scope rt3
            spine scope.TypeStore[rt3]
            |> Flip.Expect.equal "occupancy counts up" [ "Inputs"; "Case3" ]

        testCase "a re-homed type's ref atom is pinned and stable across re-visits" <| fun _ ->
            let scope = RenderScopeStore.create () |> fun s -> RenderScopeStore.appendStringToPathContext s "Inputs"
            let rt1 = literal "a" TypeKindPrimitive.String
            let rt2 = literal "b" TypeKindPrimitive.Number
            mint scope rt1
            mint scope rt2
            let pinned = scope.SuffixedAtoms[rt2]
            mint scope rt2
            scope.SuffixedAtoms[rt2]
            |> Flip.Expect.equal "re-visit returns the pinned Case2 atom, never a re-graft" pinned
            spine pinned
            |> Flip.Expect.equal "the atom carries ONLY the Case2 leaf (member-anchored refs re-derive the context)" [ "Case2" ]

        testCase "the first occupant is never suffixed, before or after a collision" <| fun _ ->
            let scope = RenderScopeStore.create () |> fun s -> RenderScopeStore.appendStringToPathContext s "Inputs"
            let rt1 = literal "a" TypeKindPrimitive.String
            mint scope rt1
            mint scope (literal "b" TypeKindPrimitive.Number)
            mint scope rt1
            scope.SuffixedAtoms.ContainsKey rt1
            |> Flip.Expect.isFalse "first occupant keeps the nameless original"
            spine scope.TypeStore[rt1]
            |> Flip.Expect.equal "first occupant's stored path is untouched" [ "Inputs" ]

        testCase "a NAMED transient registers its claim so a later nameless mint cannot collide" <| fun _ ->
            let scope = RenderScopeStore.create ()
            let named = literal "n" TypeKindPrimitive.String
            TransientTypePath.createOnTransientModule "Inputs" TransientModulePath.Anchored
            |> RenderScopeStore.TypeRefAtom.createTransientPath scope named
            |> ignore
            let scoped = RenderScopeStore.appendStringToPathContext scope "Inputs"
            let nameless = literal "m" TypeKindPrimitive.Number
            mint scoped nameless
            spine scoped.TypeStore[nameless]
            |> Flip.Expect.equal "nameless mint yields to the named claim" [ "Inputs"; "Case2" ]
    ]
