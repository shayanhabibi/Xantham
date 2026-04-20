[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
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
    let render (ctx: GeneratorContext) scopeStore (typar: TypeParameter) =
        renderWithMetadata ctx scopeStore typar {
            Path = Path.create TransientTypePath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
    