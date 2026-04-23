[<AutoOpen>]
module Xantham.Generator.Generator.Render_Parameter

open System.ComponentModel
open Microsoft.FSharp.Core.CompilerServices
open Xantham.Decoder
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
        
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (param: Parameter): TypedNameRender<TypeRefRender,Name<Case.camel>,'a> =
        let path = Path.create (TransientParameterPath.AnchoredAndMoored param.Name)
        renderWithMetadata ctx scopeStore param 
            { Path = path
              Original = path
              Source = ValueNone
              FullyQualifiedName = ValueNone }