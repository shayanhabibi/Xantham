[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open SignalsDotnet
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath
open Xantham.Generator
open Xantham.Generator.Types

module TypeParameter =
    let render (ctx: GeneratorContext) scopeStore (typar: TypeParameter): TypeParameterRender =
        let path = TransientTypePath.AnchoredAndMoored (Name.Pascal.fromCase typar.Name)
        {
            Metadata = RenderMetadata.create path
            TypeParameterRender.Name = typar.Name
            Constraint =
                typar.Constraint
                |> Option.map (_.Value >> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value))
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (_.Value >> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value))
                |> Option.toValueOption
            Documentation = typar.Documentation
        }
    let renderFromTypePath (ctx: GeneratorContext) scopeStore (anchor: TypePath) (typar: TypeParameter): TypeParameterRender =
        let path = TypeParamPath.createOnType typar.Name anchor
        let anchorPath = AnchorPath.create path
        let metadata = RenderMetadata.create anchorPath
        {
            Metadata = metadata
            Name = typar.Name
            Constraint =
                typar.Constraint
                |> Option.map (_.Value >> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value))
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (_.Value >> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value))
                |> Option.toValueOption
            Documentation = typar.Documentation
        }