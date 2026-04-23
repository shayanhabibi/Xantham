[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module TypeParameter =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (typar: TypeParameter) (metadata: RenderMetadata) =
        {
            Prelude.TypeParameterRender.Name = typar.Name
            Metadata = metadata
            Constraint =
                typar.Constraint
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Documentation = typar.Documentation
        }
    let render (ctx: GeneratorContext) scopeStore (transientPathCtx: TransientModulePath voption) (typar: TypeParameter) =
        let transientPath =
            transientPathCtx
            |> ValueOption.map TransientTypeParameterPath.graft
            |> ValueOption.defaultWith (fun () ->
                TransientTypePath.AnchoredAndMoored(Name.Pascal.fromCase typar.Name)
                |> TransientPath.create
                )
            |> Path.create
        renderWithMetadata ctx scopeStore typar {
            Path = transientPath
            Original = transientPath
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
    