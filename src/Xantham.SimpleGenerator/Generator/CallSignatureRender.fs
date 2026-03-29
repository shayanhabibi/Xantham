module Xantham.SimpleGenerator.Generator.CallSignatureRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

/// <summary>
/// Prerenders a call signature
/// </summary>
let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyCallSignature>) =
    let memoisedParameterFn = CallSignature.parameters key |> ParameterRender.prerender genCache
    let memoisedReturnTypeFn = CallSignature.type' key |> PatternContext.value
    let returnTypeFn = GeneratorContext.getTypeRef genCache memoisedReturnTypeFn
    fun callSignatureParent ->
        {
            CallSignatureRender.Parameters = memoisedParameterFn callSignatureParent
            ReturnType = returnTypeFn callSignatureParent
        }

/// <summary>
/// Renders a call signature as a member. The member is named <c>Invoke</c>.
/// </summary>
let renderMember (genCache: GeneratorContext) (prerender: CallSignatureRender) = fun callSignaturePath ->
    let parameters, paramObjectAttr =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache callSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    Ast.Member(
        "_.Invoke",
        parameters,
        Exprs.jsUndefined,
        TypeRefRender.toWidget genCache.ctx callSignaturePath prerender.ReturnType
    ).attributes(attributes {
        Attributes.emit "$0(...$1)"
        paramObjectAttr
    })

let getCallSignaturePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyCallSignature>) =
    let masterKey = CallSignature.toMasterKey key
    fun parentPath ->
        genCache.pathResolver.Prerenderer parentPath masterKey

let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyCallSignature>) = fun callSignatureParent ->
    prerender genCache key callSignatureParent
    |> renderMember genCache
    |> funApply (getCallSignaturePath genCache key callSignatureParent)
    
let renderMemberAndOverloads (genCache: GeneratorContext) (prerender: CallSignatureRender) = fun callSignaturePath ->
    let parameters =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache callSignaturePath
    match parameters with
    | { ParamObject = ValueSome { ParamObjectPrerenders = parameters; Index = idx } } ->
        Utils.renderMemberWithParamIdxAndOverload
            false
            idx
            parameters
            (TypeRefRender.toWidget genCache.ctx callSignaturePath prerender.ReturnType )
            {| Name = Source "Invoke" |}
    | { Default = parameters } ->
        Utils.renderMemberWithAttributes
            false
            parameters
            (TypeRefRender.toWidget genCache.ctx callSignaturePath prerender.ReturnType)
            Seq.empty
            {| Name = Source "Invoke" |}
        |> Array.singleton
let renderMemberAndOverloadsFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyCallSignature>) = fun callSignatureParent ->
    prerender genCache key callSignatureParent
    |> renderMemberAndOverloads genCache
    |> funApply (getCallSignaturePath genCache key callSignatureParent)

let renderAbstract (genCache: GeneratorContext) (prerender: CallSignatureRender) = fun callSignaturePath ->
    let parameters, paramObjectAttr =
        prerender.Parameters
        |> ParameterRenderArray.prerenderTypes genCache callSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    Ast.AbstractMember("Invoke", parameters, TypeRefRender.toWidget genCache.ctx callSignaturePath prerender.ReturnType)
    |> _.attributes(attributes {
        Attributes.emit "$0(...$1)"
        paramObjectAttr
    })
let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyCallSignature>) = fun callSignatureParent ->
    prerender genCache key callSignatureParent 
    |> renderAbstract genCache
    |> funApply (getCallSignaturePath genCache key callSignatureParent)

let toTypeRef (genCache: GeneratorContext) (prerender: CallSignatureRender) = fun callSignaturePath ->
    Ast.Funs(
        prerender.Parameters
        |> ParameterRenderArray.prerenderNamelessTypes genCache callSignaturePath
        |> _.Default,
        prerender.ReturnType
        |> TypeRefRender.toWidget genCache callSignaturePath
    )
    |> TypeRefRender.create false
