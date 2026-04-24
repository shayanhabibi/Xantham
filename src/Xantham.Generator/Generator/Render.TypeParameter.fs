[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module TypeParameter =
    let render (ctx: GeneratorContext) scopeStore (typar: TypeParameter)  =
        let metadata = {
            Path = Path.create TransientTypePath.Anchored
            Original = Path.create TransientTypePath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
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
    