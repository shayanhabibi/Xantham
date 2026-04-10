[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender

module TypeParameter =
    let render (ctx: GeneratorContext) (typar: TypeParameter) =
        {
            TypeParameterRender.Name = typar.Name
            Constraint =
                typar.Constraint
                |> Option.map (_.Value >> TypeRefRender.prerender ctx)
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (_.Value >> TypeRefRender.prerender ctx)
                |> Option.toValueOption
            Documentation = typar.Documentation
        }

