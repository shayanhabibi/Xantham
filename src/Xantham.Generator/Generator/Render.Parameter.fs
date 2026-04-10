[<AutoOpen>]
module Xantham.Generator.Generator.Render_Parameter

open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender


module Parameter =
    let render (ctx: GeneratorContext) (param: Parameter) =
        let ref =
            param.Type.Value
            |> TypeRefRender.prerender ctx
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
            Type = ref
            Traits = traits
            TypeParameters = [||]
            Documentation = param.Documentation
        }