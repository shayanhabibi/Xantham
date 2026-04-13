[<AutoOpen>]
module Xantham.Generator.Generator.Render_Parameter

open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender


module Parameter =
    let renderWithMetadata (ctx: GeneratorContext) (param: Parameter) (metadata: RenderMetadata) =
        let ref =
            ctx.render param.Type.Value
        let traits =
            if param.IsOptional || ref.Nullable then
                TypedNameTraits.Optional
            else TypedNameTraits.None
            |||
            if param.IsSpread then
                TypedNameTraits.ParamArray
            else TypedNameTraits.None
        {
            TypedNameRender.Name = param.Name
            Metadata = metadata
            Type = ref
            Traits = traits
            TypeParameters = [||]
            Documentation = param.Documentation
        }
        
    let render ctx param = renderWithMetadata ctx param RenderMetadata.empty
    
    let inline renderWithPath ctx param path =
        RenderMetadata.create path
        |> renderWithMetadata ctx param