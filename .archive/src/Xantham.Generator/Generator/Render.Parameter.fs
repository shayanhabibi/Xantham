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
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (param: Parameter)  =
        let path = Path.create (TransientParameterPath.AnchoredAndMoored param.Name)
        let metadata =
            { Path = path
              Original = path
              Source = ValueNone
              FullyQualifiedName = ValueNone }
        let scopeStore =
            param.Name
            |> RenderScopeStore.appendNameToPathContext scopeStore
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