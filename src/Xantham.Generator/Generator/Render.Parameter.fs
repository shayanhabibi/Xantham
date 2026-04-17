[<AutoOpen>]
module Xantham.Generator.Generator.Render_Parameter

open System.ComponentModel
open SignalsDotnet
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator


module Parameter =
    let renderWithMetadata (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (metadata: RenderMetadata) (param: Parameter)  =
        {
            Metadata = metadata
            Prelude.TypedNameRender.Name = param.Name
            Type = GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value) param.Type.Value
            Traits = Set [
                if param.IsOptional then RenderTraits.Optional
                if param.IsSpread then RenderTraits.ParamArray
            ]
            TypeParameters = []
            Documentation = param.Documentation
        }
        
    let renderFromMemberPath (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (memberPath: MemberPath) (param: Parameter) =
        (memberPath, 0)
        ||> ParameterPath.createWithName param.Name
        |> RenderMetadata.create 
        |> renderWithMetadata ctx scopeStore
        |> funApply param
    let render (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (param: Parameter): TypedNameRender =
        TransientParameterPath.AnchoredAndMoored param.Name
        |> RenderMetadata.create
        |> renderWithMetadata ctx scopeStore
        |> funApply param
    /// <summary>
    /// Routes to renderFromMemberPath if the given anchorpath matches to a memberpath; otherwise routes through render
    /// </summary>
    let renderWithAnchorPath (ctx: GeneratorContext) (scopeStore: RenderScopeStore) (anchorPath: AnchorPath) (param: Parameter): TypedNameRender =
        match anchorPath with
        | AnchorPath.Parameter _
        | AnchorPath.Type _
        | AnchorPath.Module _
        | AnchorPath.TypeParam _ ->
            render ctx scopeStore param
        | AnchorPath.Member memberPath ->
            renderFromMemberPath ctx scopeStore memberPath param