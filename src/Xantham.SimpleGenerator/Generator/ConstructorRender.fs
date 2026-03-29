module Xantham.SimpleGenerator.Generator.ConstructorRender

open Fabulous.AST
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getConstructorPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructor>) = fun keyConstructorParentPath ->
    genCache.pathResolver.Prerenderer keyConstructorParentPath (Constructor.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructor>) =
    let constructorKey = Constructor.toMasterKey key
    let parametersFn =
        Constructor.parameters key
        |> ParameterRender.prerender genCache
    fun keyConstructorParentPath ->
        let path = genCache.pathResolver.Prerenderer keyConstructorParentPath constructorKey
        { ConstructorRender.Parameters = parametersFn path }

let renderConstructor (genCache: GeneratorContext) (prerender: ConstructorRender) = fun constructorPath ->
    Ast.Constructor(
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache constructorPath
        |> _.Default
        |> Ast.TuplePat
        |> Ast.ParenPat
        , Exprs.jsUndefined
    )   .attribute(Attributes.emitConstructor)

let renderConstructorAndOverloads (genCache: GeneratorContext) (prerender: ConstructorRender) = fun constructorPath ->
    let prerenderParameters =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache constructorPath
    let parameters, paramObjectAttr =
        prerenderParameters.TryParamObjectValueAttributeTupleOrDefault
    match paramObjectAttr with
    | ValueSome attr ->
        [|
            Ast.Constructor(
                parameters
                |> Ast.TuplePat
                |> Ast.ParenPat
                , Exprs.jsUndefined
            )   .attributes([
                Attributes.emitConstructor
                attr
            ])
            Ast.Constructor(
                prerenderParameters.Default
                |> Ast.TuplePat
                |> Ast.ParenPat
                , Exprs.jsUndefined
                ).attribute(Attributes.emitConstructor)
        |]
    | ValueNone ->
        [|
            Ast.Constructor(
                prerenderParameters.Default
                |> Ast.TuplePat
                |> Ast.ParenPat
                , Exprs.jsUndefined
                ).attribute(Attributes.emitConstructor)
        |]
        
