module Xantham.SimpleGenerator.Generator.FunctionRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getFunctionPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyFunction>) = fun functionParentPath ->
    genCache.pathResolver.Prerenderer functionParentPath (Function.toMasterKey key)
    
let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyFunction>) =
    let name = Function.name key |> PatternContext.value |> Name.Camel.create
    let parameterFn =
        Function.parameters key
        |> ParameterRender.prerender genCache
    let returnTypeFn =
        Function.type' key
        |> PatternContext.value
        |> GeneratorContext.getTypeRef genCache
    let typeParamFns =
        Function.typeParameters key
        |> ValueOption.map (
            PatternContext.Array.cmap (TypeParameterRender.prerender genCache)
            >> PatternContext.value
            )
        |> ValueOption.defaultValue [||]
    let fnMasterKey = Function.toMasterKey key
    fun functionParentPath ->
        let functionPath =
            GeneratorContext.createPathFrom genCache functionParentPath fnMasterKey
        {
            FunctionRender.Name = name
            Parameters = parameterFn functionPath
            ReturnType = returnTypeFn functionPath
            TypeParameters = Array.mapApply functionPath typeParamFns
        }

let renderAbstract (genCache: GeneratorContext) (render: FunctionRender) = fun functionPath ->
    Ast.AbstractMember(
        Name.Case.valueOrModified render.Name,
        Ast.Funs(
            render.Parameters
            |> ParameterRenderArray.prerenderNamelessTypes genCache functionPath
            |> _.Default,
            render.ReturnType
            |> TypeRefRender.toWidget genCache functionPath
            )
        )

let renderStaticAbstract (genCache: GeneratorContext) (render: FunctionRender) = fun functionPath ->
    renderAbstract genCache render functionPath |> _.toStatic()

let renderMember (genCache: GeneratorContext) (render: FunctionRender) = fun functionPath ->
    Ast.Member(
        Name.Case.valueOrModified render.Name,
        (ParameterRenderArray.prerenderPatterns genCache functionPath render.Parameters 
        |> _.Default
        |> Ast.TuplePat
        |> Ast.ParenPat),
        Exprs.jsUndefined,
        returnType =
            (render.ReturnType
            |> TypeRefRender.toWidget genCache functionPath)
        )

let renderStaticMember (genCache: GeneratorContext) (render: FunctionRender) = fun functionPath ->
    renderMember genCache render functionPath |> _.toStatic()