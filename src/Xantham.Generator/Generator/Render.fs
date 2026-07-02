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
    // Partitioned emission is the ONLY artifact (the legacy stdout monolith was
    // retired 2026-07-03 — a kept monolith is only a counter-example; borrowing
    // from it invites drift). The recipe is therefore a required input.
    let recipePath, outDir, recipe =
        match argv |> Array.toList with
        | [ "--recipe"; recipePath; "--out-dir"; outDir ] ->
            match RecipeLoad.load recipePath with
            | Error errors ->
                errors |> List.iter (eprintfn "recipe error: %s")
                failwithf "recipe %s did not load (%d error(s))" recipePath errors.Length
            | Ok recipe -> recipePath, outDir, recipe
        | other -> failwithf "usage: --recipe <path> --out-dir <dir> (got %A)" other
    let opaqueHandles = OpaqueHandleSubstitution.handlesOf recipe
    // Erased tops: the generator-internal "Empty" module (chunk-modules' VALUE-export
    // stubs — mangled, release-unstable names; not consumer API) drops at emission with
    // a ledger entry; references rewrite to the Erased.* advisory aliases.
    // RECIPE-DEP enforcement (json-schema-typed/eventsource/@types) is STAGED but
    // DISABLED 2026-07-03: enabling it vanishes the JSONSchema-family canonical
    // SharedLiterals homes (defs land under a dropped module while references survive;
    // Zod unit 18→96). One unresolved interaction between erased-module drop and
    // canonical-home minting — diagnosed fresh as the next PLAN item, not iterated blind.
    // Enforcement ENABLED 2026-07-03 after the def/ref-completeness root fix
    // (emitCanonicalPreludeScopes force-prerenders home-table entries with no
    // rendered definition — 194 latent cases measured, ledgered as
    // shared-home-forced-def; the enforcement-blocking family was 8 refs of one).
    let erasedTops: string list =
        // "Intl": lib-es NAMESPACED surface (Intl.DateTimeFormat, ...) — the name-keyed
        // LibEsSubstitution cannot see namespace-qualified stdlib refs; no Fable
        // equivalent exists, so it degrades through the same Erased.* alias machinery.
        "Empty" :: "Intl" :: OpaqueHandleSubstitution.erasedTopsOf recipe
    // Publish-ordered unit boundaries: shared synthetic homes mint under the earliest
    // owner unit's top module (Types/Generator.fs SyntheticPlacementOrder).
    let placementOrder =
        recipe.Entries
        |> List.choose (fun e -> e.Lib |> Option.map (fun _ -> Emission.packageTopModule e.Package))
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
                     |> OpaqueHandleSubstitution.rewriteTypePath opaqueHandles erasedTops
                 Customisation.Interceptors.Paths.MemberPaths = fun ctx typ s ->
                     MemberPath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s

         })
    // Same mutable caches, publish order attached: shared synthetic homes mint under
    // their earliest owner unit (see SyntheticPlacementOrder).
    // Opaque-handle tops join ErasedRoots for HOME PLACEMENT: a shared literal owned
    // ONLY by opaque-package contexts is that package's internals — its home mints
    // under the package top, where the overlay drop + handle rewrite collapse it
    // onto the handle (the same machinery as erased-owned literals).
    let syntheticErasedRoots = erasedTops @ (opaqueHandles |> List.map fst)
    let generatorContext = { generatorContext with SyntheticPlacementOrder = placementOrder; ErasedRoots = syntheticErasedRoots }
    // Phase 1: count shared object-literals (those reached through >1 owner) on a throwaway
    // context and assign each a canonical SharedLiterals home, so the real prerender below roots
    // them there (every reference resolves to one absolute path; the single def is emitted once).
    ArenaInterner.markSharedLiteralsFromExports generatorContext interner
    ArenaInterner.prerenderTypeAliases generatorContext interner
    // ArenaInterner.prerenderFromGraph generatorContext interner
    ArenaInterner.processExports generatorContext interner
    let rootModule = RootModule.collectModules generatorContext
    let recipeDir = IO.Path.GetDirectoryName(IO.Path.GetFullPath recipePath)
    let supportLibrary = IO.Path.GetFullPath(IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable.Core/Library.fs"))
    let emitted = Emission.emitUnits generatorContext (Some supportLibrary) erasedTops (Emission.planUnits recipeDir recipe) rootModule outDir
    for unit, file, lines in emitted do
        printfn $"unit: {unit.Lib} -> {file} ({lines} lines)"
    0