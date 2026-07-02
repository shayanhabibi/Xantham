module Xantham.Generator.Generator.Render

open System
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Fabulous.AST
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

open Xantham.Generator.Types

//
[<EntryPoint>]
let main argv =
    // `--recipe R --out-dir D` emits partitioned units (Emission.fs); no args keeps
    // the legacy stdout monolith byte-for-byte (the whole-artifact gates consume it
    // until the per-unit gates replace them — docs/PLAN.md Phase 1).
    let emission =
        match argv |> Array.toList with
        | [ "--recipe"; recipePath; "--out-dir"; outDir ] ->
            match RecipeLoad.load recipePath with
            | Error errors ->
                errors |> List.iter (eprintfn "recipe error: %s")
                failwithf "recipe %s did not load (%d error(s))" recipePath errors.Length
            | Ok recipe -> Some(recipePath, outDir, recipe)
        | [] -> None
        | other -> failwithf "unrecognized arguments %A (expected: --recipe <path> --out-dir <dir> | no args)" other
    // Opaque-handle path rewrites are EMISSION-ONLY: without a recipe the list is
    // empty and the legacy monolith (and every gate that reads it) is untouched.
    let opaqueHandles =
        emission
        |> Option.map (fun (_, _, recipe) -> OpaqueHandleSubstitution.handlesOf recipe)
        |> Option.defaultValue []
    let file = IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
    let tree = Decoder.Runtime.create file
    let interner = tree.GetArenaInterner()
    let generatorContext: GeneratorContext =
         GeneratorContext.EmptyWithCustomisation (fun customiser ->
         {
             customiser with
                 // Faithful TS-stdlib (lib.es) -> F#/Fable name + arg substitution. The single
                 // source of truth lives in LibEsSubstitution (unit-tested in isolation): it maps
                 // each stdlib name to its Fable equivalent and recovers element args for bare
                 // generic-collection references (`Array<'T>` -> `ResizeArray<'T>`).
                 Customisation.Interceptors.ResolvedTypePrelude = fun _ -> LibEsSubstitution.prelude
                 Customisation.Interceptors.IgnorePathRender.Source = function
                     | QualifiedNamePart.Normal(text)
                     | QualifiedNamePart.Abnormal(text,_) ->
                         text.Contains("babel", StringComparison.OrdinalIgnoreCase)
                         || text.Contains("typescript", StringComparison.OrdinalIgnoreCase)
                 // The synthetic "Typescript" module parent is never emitted as a
                 // real F# module: it is synthesized from a `typescript` source
                 // attribution (the same source the IgnorePathRender above already
                 // suppresses). Types carrying it — whether lib.es or workers-types'
                 // own `Response`/`Request`/`WebSocket` (which the TS checker
                 // attributes to a `typescript` origin) — would otherwise reference a
                 // `Typescript.X` that has no definition. Prune the phantom parent
                 // unconditionally so the reference resolves to the emitted type.
                 Customisation.Interceptors.Paths.TypePaths = fun ctx typ s ->
                     TypePath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                     |> OpaqueHandleSubstitution.rewriteTypePath opaqueHandles
                 Customisation.Interceptors.Paths.MemberPaths = fun ctx typ s ->
                     MemberPath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                     
         })
    // Phase 1: count shared object-literals (those reached through >1 owner) on a throwaway
    // context and assign each a canonical SharedLiterals home, so the real prerender below roots
    // them there (every reference resolves to one absolute path; the single def is emitted once).
    ArenaInterner.markSharedLiteralsFromExports generatorContext interner
    ArenaInterner.prerenderTypeAliases generatorContext interner
    // ArenaInterner.prerenderFromGraph generatorContext interner
    ArenaInterner.processExports generatorContext interner
    let rootModule = RootModule.collectModules generatorContext
    match emission with
    | Some(recipePath, outDir, recipe) ->
        let recipeDir = IO.Path.GetDirectoryName(IO.Path.GetFullPath recipePath)
        let supportLibrary = IO.Path.GetFullPath(IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable.Core/Library.fs"))
        let emitted = Emission.emitUnits generatorContext (Some supportLibrary) (Emission.planUnits recipeDir recipe) rootModule outDir
        for unit, file, lines in emitted do
            printfn $"unit: {unit.Lib} -> {file} ({lines} lines)"
        0
    | None ->
    let renders =
        rootModule
        // |> _.Modules["Typescript"]
        // |> renderModule generatorContext
        |> renderRoot generatorContext
    // generatorContext.AnchorRenders
    // |> Seq.take 100
    // |> Seq.choose (_.Value >> function
    //     | Choice1Of2 x ->
    //         Ast.Value("_", "jsNative", TypeRefRender.Anchored.render x)
    //         |> Choice1Of2
    //         |> Some
    //     | Choice2Of2 x ->
    //         match x.Render.Deconstruct() |> snd |> _.Value with
    //         | TypeRender.EnumUnion enumUnion ->
    //             LiteralUnionRender.renderEnum generatorContext enumUnion
    //             |> Choice2Of2
    //             |> Some
    //         | TypeDefn typeLikeRender ->
    //             TypeLikeRender.renderClass generatorContext typeLikeRender
    //             |> Choice2Of2
    //             |> Some
    //         | _ -> None
    //         )
    // |> fun x ->
    Ast.Oak() {
        Ast.AnonymousModule() {
            // Emit definitions for any erased-union arity beyond Fable.Core's U2..U9
            // (recorded while rendering above). Without these, a TS union of 10+
            // members references an undefined `U10`/`U15`/... type.
            for i in erasedUnion.UnionLengths |> Seq.sort do
                SpecialRender.renderErasedUnion i
            renders
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> printfn "%s"
    // |> Seq.iter (printfn "%A")
    0