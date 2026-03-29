module Xantham.SimpleGenerator.Generator.ConstructSignatureRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getConstructorPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructSignature>) =
    let masterKey = ConstructSignature.toMasterKey key
    fun constructorParentPath ->
        genCache.pathResolver.Prerenderer constructorParentPath masterKey

/// <summary>
/// Prerenders a construct signature
/// </summary>
let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructSignature>) =
    let parametersFn =
        ConstructSignature.parameters key
        |> ParameterRender.prerender genCache
    let masterKey = ConstructSignature.toMasterKey key |> PatternContext.value
    let returnTypeFn = GeneratorContext.getTypeRef genCache masterKey
    fun constructorParentPath ->
        let path = getConstructorPath genCache key constructorParentPath
        {
            ConstructSignatureRender.Parameters = parametersFn path
            ReturnType = returnTypeFn path
        }

let renderMember (genCache: GeneratorContext) (prerender: ConstructSignatureRender) = fun constructSignaturePath ->
    let parameters, paramObjectAttr  =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache constructSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    Ast.Member(
        "Create",
        parameters,
        Exprs.jsUndefined,
        TypeRefRender.toWidget genCache.ctx constructSignaturePath prerender.ReturnType
        )
    |> _.attributes(attributes {
        Attributes.emitConstructor
        paramObjectAttr
    })
    |> _.toStatic()
let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructSignature>) = fun constructSignatureParentPath ->
    prerender genCache key constructSignatureParentPath
    |> renderMember genCache 
    |> funApply (getConstructorPath genCache key constructSignatureParentPath)

let renderMemberAndOverloads (genCache: GeneratorContext) (prerender: ConstructSignatureRender) = fun constructSignaturePath ->
    let parameters =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache constructSignaturePath
    match parameters with
    | { ParamObject = ValueSome { ParamObjectPrerenders = parameters; Index = idx } } ->
        [|
            Utils.renderMemberWithAttributes
                true
                parameters
                (TypeRefRender.toWidget genCache.ctx constructSignaturePath prerender.ReturnType)
                [ Attributes.paramObject idx
                  Attributes.emitConstructor ]
                {| Name = Source "Create" |}
            Utils.renderMemberWithAttributes
                true
                (Array.truncate idx parameters)
                (TypeRefRender.toWidget genCache.ctx constructSignaturePath prerender.ReturnType)
                [ Attributes.emitConstructor ]
                {| Name = Source "Create" |}
        |]
    | { Default = parameters } ->
        Utils.renderMemberWithAttributes
            true
            parameters
            (TypeRefRender.toWidget genCache.ctx constructSignaturePath prerender.ReturnType)
            [ Attributes.emitConstructor ]
            {| Name = Source "Create" |}
        |> Array.singleton
let renderMemberAndOverloadsFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructSignature>) = fun constructSignatureParentPath ->
    prerender genCache key constructSignatureParentPath 
    |> renderMemberAndOverloads genCache
    <| getConstructorPath genCache key constructSignatureParentPath
        
let renderAbstract (genCache: GeneratorContext) (prerender: ConstructSignatureRender) = fun constructSignaturePath ->
    let parameters, paramObjectAttr =
        prerender.Parameters
        |> ParameterRenderArray.prerenderTypes genCache constructSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    Ast.AbstractMember(
        "Create",
        parameters,
        TypeRefRender.toWidget genCache.ctx constructSignaturePath prerender.ReturnType
        )
    |> _.attributes(attributes {
        Attributes.emitConstructor
        paramObjectAttr
    })
    |> _.toStatic()
let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyConstructSignature>) = fun constructSignatureParentPath ->
    prerender genCache key constructSignatureParentPath
    |> renderAbstract genCache
    <| getConstructorPath genCache key constructSignatureParentPath
