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
#I "../../src/Xantham.Generator/bin/Release/net10.0"
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

## 2. Build a `GeneratorContext` with `Customisation` hooks

The context carries four caches and a customisation record that dispatches
through hook slots at seven pipeline stages. The example below builds the
customisation that ships with the reference generator using chainable `add*`
helpers:
*)

        let renderScopeBuildHook: SkippableHook<RenderScope> =
            fun ctx rctx scope ->
                match rctx.Owner with
                | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
                    SkippableHookResult.Replace { scope with Render = ValueSome (Render.RefOnly scope.TypeRef) }
                | _ -> SkippableHookResult.Pass

        let pruneTypescriptParentType: SkippableHook<TypePath> =
            fun ctx rctx path ->
                match rctx.Owner with
                | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
                    SkippableHookResult.Replace
                        (TypePath.pruneParent
                            (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") path)
                | _ -> SkippableHookResult.Pass

        let pruneTypescriptParentMember: SkippableHook<MemberPath> =
            fun ctx rctx path ->
                match rctx.Owner with
                | ValueSome (RenderOwner.Type (ResolvedType.Interface { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Class { IsLibEs = true }))
                | ValueSome (RenderOwner.Type (ResolvedType.Enum { IsLibEs = true })) ->
                    SkippableHookResult.Replace
                        (MemberPath.pruneParent
                            (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") path)
                | _ -> SkippableHookResult.Pass

        let generatorContext: GeneratorContext =
            GeneratorContext.EmptyWithCustomisation
                (Customisation.addRenderScopeBuild renderScopeBuildHook
                 >> Customisation.addPathResolutionType pruneTypescriptParentType
                 >> Customisation.addPathResolutionMember pruneTypescriptParentMember)

(**
The customisation composes three hooks:

* **`RenderScopeBuild`** — wraps lib-ES type definitions in `RefOnly` to emit
  only type references.
* **`PathResolutionType`** — prunes the synthetic `Typescript` module from
  lib-ES type paths.
* **`PathResolutionMember`** — prunes the synthetic `Typescript` module from
  member paths under lib-ES types.

See [GeneratorContext](generator-context.html) for hook semantics and
[Pipeline stages](#pipeline-stages) below for all seven hook slots.

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
## Pipeline stages

The customisation record dispatches through seven hook slots, one per pipeline
stage. Each slot is either a `HookSlot<'T>` (non-skippable) or
`SkippableHookSlot<'T>` (can return `Skip` to elide processing). Handlers
register via `Customisation.add{Slot}` chainable methods; registration order
determines execution order (latest-registered runs first).

| Stage | Slot Type | Accepts | Emits | Purpose |
|-------|-----------|---------|-------|---------|
| **PathResolutionType** | `SkippableHookSlot<TypePath>` | `RenderContext` + `TypePath` | `TypePath \| Skip` | Rewrite type paths; `Skip` elides the type. Called before any render scope enters. |
| **PathResolutionMember** | `SkippableHookSlot<MemberPath>` | `RenderContext` + `MemberPath` | `MemberPath \| Skip` | Rewrite member paths; `Skip` elides the member. Called during path resolution. |
| **TypeRefBuild** | `HookSlot<TypeRefRender>` | `RenderContext` + `TypeRefRender` | `TypeRefRender` | Rewrite a resolved type reference. Runs when building inline type annotations. |
| **TypeRefEmit** | `HookSlot<WidgetBuilder<Type>>` | `RenderContext` + `WidgetBuilder<Type>` | `WidgetBuilder<Type>` | Rewrite the low-level F# type widget. Runs during AST emission. |
| **RenderScopeBuild** | `SkippableHookSlot<RenderScope>` | `RenderContext` + `RenderScope` | `RenderScope \| Skip` | Rewrite a prelude render scope (mutations before caching); `Skip` cancels caching and uses a fallback. |
| **TypeDefBuild*** | `SkippableHookSlot<Concrete.*Render>` | `RenderContext` + shape-specific render | same | Rewrite type definition before lowering to AST (separate slots per shape: Class, Alias, Enum, StringUnion). |
| **TypeDefEmit** | `HookSlot<WidgetBuilder<TypeDefn>>` | `RenderContext` + `WidgetBuilder<TypeDefn>` | `WidgetBuilder<TypeDefn>` | Rewrite the emitted type definition widget. |
| **Anchored*** | `SkippableHookSlot<Anchored.*>` | `RenderContext` + anchored render | same | Final chance to rewrite before storing in `AnchorRenders` (separate slots per shape: `AnchoredRef`, `AnchoredScope`). |

`RenderContext` carries:
- `Owner` — `ValueSome (RenderOwner.Type owner)` or `ValueSome (RenderOwner.Export owner)` or `ValueNone`.
- `Position` — `RenderPosition.RefPos TypeRefPosition` (for TypeRef*), `RenderPosition.PathPos PathPosition` (for PathResolution*), or `RenderPosition.NotApplicable`.
- `Render` — `ValueSome renderMode` when inside a `RenderScope`, else `ValueNone`.
- `Stage` — the current pipeline stage.

Handlers are strict functional transformations — no mutations. Multiple
handlers compose left-to-right; if any returns `Skip`, downstream handlers
are not invoked.

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
* a `Customisation` built using `add*` hooks that intercept at the stages above
* (optional) replacement passes — the reference entry point shows how to
  swap or augment any step by reading from `ctx.AnchorRenders` and emitting
  bespoke widgets before assembling the final `Oak`.
*)
