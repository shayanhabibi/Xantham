module Xantham.Generator.Tests.Tests.TypeShapeRender

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

// Exhaustive coverage of Render.TypeShapes.fs (the interface/class TypeDefn ASSEMBLY pass)
// exercised end-to-end through the `renderTypeDefn` harness, which prerenders + anchors a
// `ResolvedType` and emits the full F# type DEFINITION string.
//
// `Interface.render` (Render.TypeShapes.fs:13-43) assembles a `TypeLikeRender` from an
// `Interface`: it maps `shape.TypeParameters` -> `TypeParameters`, `shape.Heritage.Extends`
// -> `Inheritance` (each base run through `ctx.PreludeGetTypeRef`), and folds members into
// `Members`/`Functions`. `TypeLikeRender.renderInterface` (TypeRender.Render.fs:569-601) then
// turns that into a `TypeDefn` widget: empty body -> `Ast.InterfaceEnd` (`= interface end`),
// non-empty -> `Ast.TypeDefn` with `inherit` clauses + members, plus a postfix typar list.
//
// All output strings below were obtained by probing with `renderTypeDefn` FIRST, then asserted
// verbatim.
let private render = Xantham.Generator.Tests.Tests.TypeDefnRenderHarness.renderTypeDefn

/// Build an `Interface` ResolvedType with the given heritage bases (each a plain non-generic
/// interface by name). The mock `Interface.create` seeds `Heritage = { Extends = [] }`; there is
/// no mock helper to set heritage, so we record-update it directly.
let private interfaceExtending name (baseNames: string list) =
    { (Interface.create name) with
        Heritage = {
            Extends =
                baseNames
                |> List.map (fun b -> TypeReference.create (Interface.create b |> Interface.wrap)) } }
    |> Interface.wrap

[<Tests>]
let tests =
    testList "TypeShapeRender" [
        // An interface with no members and no heritage collapses to the F# empty-interface
        // form. `renderInterface` (TypeRender.Render.fs:590-591) chooses `Ast.InterfaceEnd`
        // when the member collection is empty, producing `= interface end` on one line.
        testCase "empty interface renders `interface end`" <| fun _ ->
            (Interface.create "Empty" |> Interface.wrap)
            |> render
            |> Flip.Expect.equal "empty interface TypeDefn"
                "type Empty = interface end"

        // A single declared type parameter is emitted as a postfix `<'T>` list
        // (`renderInterface` line 601 attaches `typeParams`). With no members the body is still
        // `interface end`.
        testCase "generic interface with one type parameter renders `<'T>`" <| fun _ ->
            (Interface.create "Box"
             |> Interface.withTypeParameters [ TypeParameter.create "T" ]
             |> Interface.wrap)
            |> render
            |> Flip.Expect.equal "1-typar interface TypeDefn"
                "type Box<'T> = interface end"

        // Two type parameters. NOTE the emitted order is REVERSED vs. source order
        // (`[T; U]` -> `<'U, 'T>`): `Interface.render` (Render.TypeShapes.fs:27-29) maps
        // `shape.TypeParameters` in order, but the downstream fold/anchor accumulation reverses
        // them — the same reversal observed for member emission. We assert the ACTUAL order.
        testCase "generic interface with two type parameters renders reversed `<'U, 'T>`" <| fun _ ->
            (Interface.create "Pair"
             |> Interface.withTypeParameters [ TypeParameter.create "T"; TypeParameter.create "U" ]
             |> Interface.wrap)
            |> render
            |> Flip.Expect.equal "2-typar interface TypeDefn (reversed)"
                "type Pair<'U, 'T> = interface end"

        // A type parameter carrying an `extends` constraint emits an F# `when 'T :> Base`
        // subtype constraint inside the postfix typar list.
        testCase "generic interface with a constrained type parameter renders `when 'T :> Base`" <| fun _ ->
            (Interface.create "Constrained"
             |> Interface.withTypeParameters [
                 TypeParameter.create "T"
                 |> TypeParameter.withConstraint (Interface.create "BaseA" |> Interface.wrap) ]
             |> Interface.wrap)
            |> render
            |> Flip.Expect.equal "constrained-typar interface TypeDefn"
                "type Constrained<'T when 'T :> BaseA> = interface end"

        // An interface extending a GENERIC base (`extends BaseG<T>`) DOES emit an `inherit`
        // clause. The generic base resolves to a `Prefix` molecule, and `renderInheritance`'s
        // `isInterfaceBase` (TypeRender.Render.fs:545) keeps `Prefix` molecules — so the
        // `inherit BaseG<'T>` survives the localise pass. Body is a multi-line `interface ... end`.
        testCase "interface extending a generic base renders `inherit BaseG<'T>`" <| fun _ ->
            ({ (Interface.create "DerivedG"
                |> Interface.withTypeParameters [ TypeParameter.create "T" ]) with
                Heritage = {
                    Extends = [
                        TypeReference.create (
                            Interface.create "BaseG"
                            |> Interface.withTypeParameters [ TypeParameter.create "T" ]
                            |> Interface.wrap)
                        |> TypeReference.withTypeArguments [ TypeParameter.create "T" |> TypeParameter.wrap ] ] } }
             |> Interface.wrap)
            |> render
            |> Flip.Expect.equal "generic-base inheritance TypeDefn"
                (String.concat "\n" [
                    "type DerivedG<'T> ="
                    "    interface"
                    "        inherit BaseG<'T>"
                    "    end"
                ])

        // BUG: an interface extending a single NON-GENERIC interface base SHOULD emit
        // `inherit BaseA`, but the `inherit` clause is silently DROPPED — the interface renders
        // as if it had no heritage at all.
        //
        // Root cause: `RenderScope.Anchored.localise` (src/Xantham.Generator/Types/
        // RenderScope.Anchored.fs:109-116) rewrites EVERY `Atom` into a `TypeRefAtom.Widget`
        // (resolving the path to a bare `LongIdent`). `anchorAndLocalise` runs this over every
        // Inheritance entry (RenderScope.Anchored.fs:168-170). By the time
        // `renderInheritance.isInterfaceBase` (src/Xantham.Generator/Generator/
        // TypeRender.Render.fs:542-546) inspects the base, a non-generic interface base is a
        // `Widget` atom — which `isInterfaceBase` rejects (it only accepts `Atom (Path _)` and
        // `Molecule (Prefix _)`). So the legitimate interface base is treated like a dropped
        // scalar and the `inherit` is omitted. A GENERIC base survives only because it is a
        // `Prefix` molecule. Probed Inheritance entry kind: `Atom (Widget ...)`, count 1.
        //
        // The fix belongs in either: (a) `isInterfaceBase` recognising a localised interface
        // `Widget` atom, or (b) preserving a path-classification flag through `localise` so a
        // non-generic interface base remains identifiable.
        testCase "interface extending a non-generic base emits `inherit BaseA`" <| fun _ ->
            interfaceExtending "Derived" [ "BaseA" ]
            |> render
            |> Flip.Expect.equal "non-generic single-base inheritance TypeDefn"
                (String.concat "\n" [
                    "type Derived ="
                    "    interface"
                    "        inherit BaseA"
                    "    end"
                ])

        // BUG (same root cause): extending MULTIPLE non-generic bases SHOULD emit one `inherit`
        // per base; instead all are dropped and the type renders as `interface end`. Same
        // `localise` -> `Widget` -> `isInterfaceBase` rejection as above
        // (RenderScope.Anchored.fs:109-116 + TypeRender.Render.fs:542-546).
        testCase "interface extending multiple non-generic bases emits one `inherit` each" <| fun _ ->
            interfaceExtending "Derived2" [ "BaseA"; "BaseB" ]
            |> render
            |> Flip.Expect.equal "non-generic multi-base inheritance TypeDefn"
                (String.concat "\n" [
                    "type Derived2 ="
                    "    interface"
                    "        inherit BaseA"
                    "        inherit BaseB"
                    "    end"
                ])

    ]
