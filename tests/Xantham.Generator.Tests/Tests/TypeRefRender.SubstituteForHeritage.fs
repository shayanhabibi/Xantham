module Xantham.Generator.Tests.Tests.TypeRefRenderSubstituteForHeritage

open Expecto
open Xantham
open Xantham.Generator
open Xantham.Generator.Types

module Atom =
    let intrinsic = RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic
    let concretePath = RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath
    let transientPath = RenderScopeStore.TypeRefAtom.Unsafe.createTransientPath
    let widget = RenderScopeStore.TypeRefAtom.Unsafe.createWidget
module Molecule =
    let prefix = RenderScopeStore.TypeRefMolecule.Unsafe.createPrefix
let wrapAtom = RenderScopeStore.TypeRef.Unsafe.createAtom
let wrapMolecule = RenderScopeStore.TypeRef.Unsafe.createMolecule
    
let private createIntrinsicAtom (s: string) : TypeRefRender =
    { Kind = Atom.intrinsic s |> wrapAtom; Nullable = false }

let private createPrefixMolecule (prefix: TypeRefRender) (args: TypeRefRender list) : TypeRefRender =
    { Kind = wrapMolecule (Molecule.prefix <|| (prefix, args)); Nullable = false }

[<Tests>]
let substituteForHeritageTests =
    testList "Prelude.substituteForHeritage" [
        testCase "replaces wildcard _ with obj" <| fun _ ->
            let wildcard = createIntrinsicAtom "_"
            let result = Prelude.TypeRefRender.substituteForHeritage Set.empty wildcard

            match result.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> ()
            | _ -> failtest "Should have replaced _ with obj"

        testCase "preserves in-scope typars" <| fun _ ->
            let typar = createIntrinsicAtom "'T"
            let inScopeTypars = Set.singleton "'T"
            let result = Prelude.TypeRefRender.substituteForHeritage inScopeTypars typar

            match result.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic "'T") -> ()
            | _ -> failtest "Should preserve in-scope typar"

        testCase "replaces orphan typars with obj" <| fun _ ->
            let typar = createIntrinsicAtom "'U"
            let inScopeTypars = Set.singleton "'T"
            let result = Prelude.TypeRefRender.substituteForHeritage inScopeTypars typar

            match result.Kind with
            | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> ()
            | _ -> failtest "Should replace orphan typar with obj"

        testCase "walks nested Prefix molecules" <| fun _ ->
            let prefixAtom = createIntrinsicAtom "List"
            let wildcardArg = createIntrinsicAtom "_"
            let prefix = createPrefixMolecule prefixAtom [wildcardArg]
            let result = Prelude.TypeRefRender.substituteForHeritage Set.empty prefix

            match result.Kind with
            | TypeRefKind.Molecule (TypeRefMolecule.Prefix (_, args)) ->
                match args with
                | [arg] ->
                    (match arg.Kind with
                     | TypeRefKind.Atom (TypeRefAtom.Intrinsic "obj") -> ()
                     | _ -> failtest "Should have substituted wildcard in args")
                | _ -> failtest "Expected single arg"
            | _ -> failtest "Expected Prefix molecule"

        testCase "preserves Nullable flag" <| fun _ ->
            let wildcard = { Kind = wrapAtom(Atom.intrinsic "_"); Nullable = true }
            let result = Prelude.TypeRefRender.substituteForHeritage Set.empty wildcard

            Expect.isTrue result.Nullable "Should preserve Nullable flag"
    ]
