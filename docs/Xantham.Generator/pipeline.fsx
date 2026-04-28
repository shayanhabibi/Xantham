(**
---
title: Pipeline
category: Xantham.Generator
categoryindex: 3
index: 5
---

# End-to-end pipeline

This page walks the reference generator entry point — `Generator/Render.fs`
in `Xantham.Generator` — top to bottom and explains what each step is
responsible for. The script form below mirrors the actual `[<EntryPoint>]`
function with commentary in between.
*)

(**
## 1. Decode the JSON input

The generator never reads `.d.ts` directly. It reads the JSON wire format
written by `Xantham.Fable` and decodes it into a navigable graph:
*)

(*** hide ***)
#I "../../src/Xantham.Generator/bin/Debug/net10.0"
#r "Xantham.Generator.dll"
#r "Xantham.Decoder.dll"
#r "Fabulous.AST.dll"
#r "Fantomas.Core.dll"
module Pipeline =
    open System
    open Fabulous.AST
    open Xantham
    open Xantham.Generator
    open Xantham.Generator.Generator
    open Xantham.Generator.NamePath
    open Xantham.Decoder
    open Xantham.Decoder.ArenaInterner
    open Xantham.Generator.Types

    let main () =
(**
*)
        let file =
            IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
        let tree = Decoder.Runtime.create file
        let interner = tree.GetArenaInterner()

(**
`tree` is a `XanthamTree` (see [Xantham.Decoder](../Xantham.Decoder/index.html));
`interner` is the lazy resolved object graph. Every `TypeKey` reference in
the source data has been replaced with `Lazy<ResolvedType>` and forcing a
node returns the fully shelled record.

## 2. Build a `GeneratorContext`

The context carries four caches and a customisation record. The example
below pins the customisation that ships with the reference generator:
*)

        let generatorContext: GeneratorContext =
            GeneratorContext.EmptyWithCustomisation (fun customiser ->
                { customiser with
                    Customisation.Interceptors.ResolvedTypePrelude = fun _ -> function
                        | ResolvedType.Interface { IsLibEs = true }
                        | ResolvedType.Class { IsLibEs = true }
                        | ResolvedType.Enum { IsLibEs = true } -> fun renderScope ->
                            { renderScope with Render = Render.RefOnly renderScope.TypeRef }
                        | _ -> id
                    Customisation.Interceptors.IgnorePathRender.Source = function
                        | QualifiedNamePart.Normal(text)
                        | QualifiedNamePart.Abnormal(text, _) ->
                            text.Contains("babel", StringComparison.OrdinalIgnoreCase)
                            || text.Contains("typescript", StringComparison.OrdinalIgnoreCase)
                    Customisation.Interceptors.Paths.TypePaths = fun _ typ s ->
                        match typ with
                        | Choice1Of4 { IsLibEs = true }
                        | Choice2Of4 { IsLibEs = true }
                        | Choice3Of4 { IsLibEs = true }
                        | Choice4Of4 { IsLibEs = true } ->
                            TypePath.pruneParent
                                (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                        | _ -> s
                    Customisation.Interceptors.Paths.MemberPaths = fun _ typ s ->
                        match typ with
                        | Choice1Of2 { IsLibEs = true }
                        | Choice2Of2 { IsLibEs = true } ->
                            MemberPath.pruneParent
                                (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                        | _ -> s })

(**
See [GeneratorContext](generator-context.html) for what each interceptor
does. In short: lib-ES types become references-only, the synthetic
`Typescript` module is pruned from their paths, and the `babel` /
`typescript` qualified-name sources are filtered before path construction.

## 3. Prelude pass

`prerenderTypeAliases` walks every type alias in the interner and seeds
`ctx.TypeAliasRemap`. After this pass, looking up an alias yields the
`TypeRefRender` of its underlying target — generators can dereference
chains of aliases without re-walking the graph for every hop:
*)

        ArenaInterner.prerenderTypeAliases generatorContext interner

(**
The commented-out `prerenderFromGraph` line in the original entry point is
the thoroughness alternative: walk the dependency graph topologically and
seed prelude renders for every node. It is not needed when the export pass
is exhaustive but is occasionally useful for diagnostics.

## 4. Export pass

`processExports` is where the real work happens. For every entry in the
exported declaration map it:

1. Computes a concrete `TypePath` / `MemberPath` from the export's
   `QualifiedName` and `Source` (with `Paths` interceptors applied).
2. Renders the declaration using the transient/prelude scope, registering
   any nested transient types it encounters in a `RenderScopeStore`.
3. Anchors and localises every `TypeRefRender` against the concrete root.
4. Stores the result on `ctx.AnchorRenders` (running the `AnchoredRender`
   interceptor on the way in).
*)

        ArenaInterner.processExports generatorContext interner

(**
After this call, `ctx.AnchorRenders` is the canonical source of truth for
"what does the output look like".

## 5. Module collection

The anchor store is flat. To emit valid F#, types and members must be
grouped by the module they live in. `RootModule.collectModules` walks
`ctx.AnchorRenders`, partitions by the `ModulePath` of each anchored
render, and produces a nested `RootModule` / `Module` tree:
*)

        let renders =
            RootModule.collectModules generatorContext
            |> renderRoot generatorContext

(**
`renderRoot` lowers the tree into a sequence of Fabulous.AST module nodes —
one for the root declarations and one for each nested module — preserving
the original module structure of the source.

## 6. Emit

Finally, wrap everything in an `Ast.Oak` (Fabulous's top-level F# document
node), run it through `Gen.mkOak >> Gen.run`, and print the result:
*)

        Ast.Oak() {
            Ast.AnonymousModule() {
                renders
            }
        }
        |> Gen.mkOak
        |> Gen.run
        |> printfn "%s"

(*** hide ***)
        0

(**
## What you write vs what you get for free

The generator distinguishes the **machinery** from the **policy**. Out of
the box you get:

* path construction (`TypeRefRender.Paths.fs`)
* prelude / export passes (`ArenaInterner.prerenderTypeAliases`,
  `ArenaInterner.processExports`)
* anchor-and-localise (`Anchored.TypeRefRender.anchorAndLocalise`)
* module collection (`RootModule.collectModules`)
* lowering to Fabulous.AST (`TypeRefRender.render`,
  `TypeRender.render*` family)

What you supply:

* the JSON input file
* a `Customisation` describing your project's conventions
* (optional) replacement passes — the reference entry point shows how to
  swap or augment any step by reading from `ctx.AnchorRenders` and emitting
  bespoke widgets before assembling the final `Oak`.
*)
