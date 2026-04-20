[<AutoOpen>]
module Xantham.Generator.Generator.Render_Parameter

open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath


module Parameter =
    let renderWithMetadata (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (param: Parameter) (metadata: RenderMetadata) =
        {
            Metadata = metadata
            Prelude.TypedNameRender.Name = param.Name
            Type = ctx.PreludeGetTypeRef ctx scopeStore param.Type
            Traits = Set [
                if param.IsOptional then RenderTraits.Optional
                if param.IsSpread then RenderTraits.ParamArray
            ]
            TypeParameters = []
            Documentation = param.Documentation
        }
        
    let render ctx scopeStore param =
        renderWithMetadata ctx scopeStore param 
            { Path = Path.create (TransientParameterPath.AnchoredAndMoored param.Name)
              Source = ValueNone
              FullyQualifiedName = ValueNone }