[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRenders

module TypeParameter =
    let renderWithMetadata (ctx: GeneratorContext) (typar: TypeParameter) (metadata: RenderMetadata) =
        {
            TypeParameterRender.Name = typar.Name
            Metadata = metadata
            Constraint =
                typar.Constraint
                |> Option.map (_.Value >> ctx.render)
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (_.Value >> ctx.render)
                |> Option.toValueOption
            Documentation = typar.Documentation
        }
    let render (ctx: GeneratorContext) (typar: TypeParameter) =
        renderWithMetadata ctx typar RenderMetadata.empty
    
    let inline renderWithPath (ctx: GeneratorContext) (typar: TypeParameter) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx typar
