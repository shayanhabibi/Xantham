module Xantham.SimpleGenerator.Generator.IndexSignatureRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getIndexSignaturePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexSignature>) = fun indexParentPath ->
    genCache.pathResolver.Prerenderer indexParentPath (IndexSignature.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexSignature>) =
    let masterKey = IndexSignature.toMasterKey key
    let typeMasterKey = key.Value.Type
    let parametersFn =
        IndexSignature.parameters key
        |> ParameterRender.prerender genCache
    let isReadOnly = key.Value.IsReadonly
    let returnTypeFn = GeneratorContext.getTypeRef genCache typeMasterKey
    fun indexParentPath ->
        let path = genCache.pathResolver.Prerenderer indexParentPath masterKey
        {
            ReturnType = returnTypeFn path
            Parameters = parametersFn path
            IsReadOnly = isReadOnly
        }
let renderMember (genCache: GeneratorContext) (prerender: IndexSignatureRender) = fun indexSignaturePath ->
    let parameters, paramObjectAttr =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache indexSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    (* How to correctly do setter with Item signatures outside abstract members? *)
    // match prerender with
    // | { ReadOnly = true } ->
    //     Ast.Member("_.Item", parameters, Exprs.jsUndefined, prerender.ReturnType)
    //     |> _.attributes([
    //         Attributes.erase
    //     ])
    // | _ ->
    //     Ast.Member(
    //         "_.Item",
    //         getter = Ast.Getter(
    //             Ast.ParenPat(Ast.TuplePat(parameters)),
    //             Exprs.jsUndefined,
    //             prerender.ReturnType
    //             )
    //         // todo - setter?
    //         )
    Ast.Member("_.Item", parameters, Exprs.jsUndefined, TypeRefRender.toWidget genCache.ctx indexSignaturePath prerender.ReturnType)
    |> _.attributes([
        Attributes.emitIndexer
        match paramObjectAttr with
        | ValueSome attr -> attr
        | _ -> ()
    ])
let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexSignature>) = fun indexSignatureParentPath ->
    prerender genCache key indexSignatureParentPath 
    |> renderMember genCache
    <| getIndexSignaturePath genCache key indexSignatureParentPath
let renderMemberAndOverloads (genCache: GeneratorContext) (prerender: IndexSignatureRender) = fun indexSignaturePath ->
    let parameters =
        prerender.Parameters
        |> ParameterRenderArray.prerenderPatterns genCache indexSignaturePath
    match parameters with
    | { ParamObject = ValueSome { ParamObjectPrerenders = parameters; Index = idx } } ->
        [|
            Utils.renderMemberWithAttributes
                true
                parameters
                (TypeRefRender.toWidget genCache.ctx indexSignaturePath prerender.ReturnType)
                [ Attributes.paramObject idx
                  Attributes.emitIndexer ]
                {| Name = Source "Item" |}
            Utils.renderMemberWithAttributes
                true
                (Array.truncate idx parameters)
                (TypeRefRender.toWidget genCache.ctx indexSignaturePath prerender.ReturnType)
                [ Attributes.emitIndexer ]
                {| Name = Source "Item" |}
        |]
    | { Default = parameters } ->
        Utils.renderMemberWithAttributes
            true
            parameters
            (TypeRefRender.toWidget genCache.ctx indexSignaturePath prerender.ReturnType)
            [ Attributes.emitIndexer ]
            {| Name = Source "Item" |}
        |> Array.singleton
let renderMemberAndOverloadsFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexSignature>) = fun indexSignatureParentPath ->
    prerender genCache key indexSignatureParentPath 
    |> renderMemberAndOverloads genCache
    <| getIndexSignaturePath genCache key indexSignatureParentPath
let renderAbstract (genCache: GeneratorContext) (prerender: IndexSignatureRender) = fun indexSignaturePath ->
    let parameters, paramObjectAttr =
        prerender.Parameters
        |> ParameterRenderArray.prerenderTypes genCache indexSignaturePath
        |> _.TryParamObjectValueAttributeTupleOrDefault
    Ast.AbstractMember("Item", parameters, TypeRefRender.toWidget genCache.ctx indexSignaturePath prerender.ReturnType)
    |> _.attributes([
        Attributes.emitIndexer
        match paramObjectAttr with
        | ValueSome attr -> attr
        | _ -> ()
    ])
let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexSignature>) = fun indexSignatureParentPath ->
    prerender genCache key indexSignatureParentPath
    |> renderAbstract genCache
    <| getIndexSignaturePath genCache key indexSignatureParentPath
