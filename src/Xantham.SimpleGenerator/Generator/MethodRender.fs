module Xantham.SimpleGenerator.Generator.MethodRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getMethodPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) = fun methodParentPath ->
    genCache.pathResolver.Prerenderer methodParentPath (Method.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) =
    let isStatic, isOptional =
        Method.isStatic key,
        Method.isOptional key
    match key with
    | Method.Name (Value name)
     & Method.Parameters parameters
     & Method.Type (Value returnType) ->
        let parameterFn = ParameterRender.prerender genCache parameters
        let returnTypeFn = GeneratorContext.getTypeRef genCache returnType
        let casedName = Name.Camel.create name
        fun methodParentPath ->
            let methodPath = getMethodPath genCache key methodParentPath
            {
                IsStatic = isStatic
                IsOptional = isOptional
                Name = casedName
                Parameters = parameterFn methodPath
                ReturnType = returnTypeFn methodPath
            }

let private render (genCache: GeneratorContext) (methodPath: KeyPathKind) (prerender: MethodRender) =
    let parameterPrerenderer =
        (
            if prerender.IsOptional
            then ParameterRenderArray.prerenderNamelessTypes
            else ParameterRenderArray.prerenderTypes
        ) genCache methodPath

    let parameterPrerender = parameterPrerenderer prerender.Parameters
    let returnType = TypeRefRender.toWidget genCache.ctx methodPath prerender.ReturnType
    let attributes = attributes {
        compiledName prerender.Name
    }
    let abstractMember =
        Ast.AbstractMember(
            Name.Case.valueOrModified prerender.Name,
            parameterPrerender.Default,
            returnType
            )
        |> Utils.AbstractMember.attributesIfNotEmpty attributes
        |> Utils.AbstractMember.toStaticIfTrue prerender.IsStatic
    match parameterPrerender with
    | { ParamObject = ValueNone } ->
        abstractMember, seq {  }
    | { ParamObject = ValueSome _ } as _ ->
        abstractMember,
        seq {
            let parameters, paramObjectAttribute =
                ParameterRenderArray.prerenderPatterns genCache methodPath prerender.Parameters
                |> _.ParamObjectValueAttributeTuple
            Utils.renderMemberWithAttributes
                prerender.IsStatic
                parameters
                returnType
                [
                    Attributes.erase
                    paramObjectAttribute
                ]
                prerender
        }
            
/// <summary>
/// Renders a method as an abstract member.
/// </summary>
/// <param name="genCache"></param>
/// <param name="key"></param>
/// <param name="methodParent"></param>
let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) =
    let masterKey = Method.toMasterKey key
    fun methodParent ->
        prerender genCache key methodParent
        |> render genCache (genCache.pathResolver.Prerenderer methodParent masterKey)
        |> fst

let renderAbstract genCache prerender= fun methodPath ->  render genCache methodPath prerender |> fst
    
/// <summary>
/// Renders a method as an abstract member and all overloads.
/// </summary>
/// <remarks>
/// The overloads are returned as an array in a tuple.
/// The overloads are rendered as member extensions.
/// </remarks>
let renderAbstractOverloads (genCache: GeneratorContext) (prerender: MethodRender) = fun methodPath ->
    let abstractMember, overloadMembers = render genCache methodPath prerender
    {| AbstractMember = abstractMember; OverloadMembers = overloadMembers |> Seq.toArray |}

let renderAbstractOverloadsFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) = fun methodParentPath ->
    prerender genCache key methodParentPath
    |> renderAbstractOverloads genCache
    |> funApply (getMethodPath genCache key methodParentPath)
        
/// <summary>
/// Renders a method as a member.
/// </summary>
let renderMember (genCache: GeneratorContext) (prerender: MethodRender) =
    let renderedName = Utils.renderMemberName prerender
    fun methodPath ->
        let parameters, paramIdxAttribute =
            ParameterRenderArray.prerenderPatterns genCache methodPath prerender.Parameters 
            |> _.TryParamObjectValueAttributeTupleOrDefault
        Ast.Member(
            renderedName,
            Ast.ParenPat(Ast.TuplePat parameters),
            Exprs.jsUndefined,
            TypeRefRender.toWidget genCache.ctx methodPath prerender.ReturnType
            )
        |> Utils.Member.attributesIfNotEmpty (attributes {
            Attributes.erase
            compiledName prerender.Name
            paramIdxAttribute
        })
        |> Utils.Member.toStaticIfTrue prerender.IsStatic
    
let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) = fun methodParentPath ->
    prerender genCache key methodParentPath
    |> renderMember genCache
    |> funApply (getMethodPath genCache key methodParentPath)
/// <summary>
/// Renders a method as a member and all overloads.
/// </summary>
let renderMemberAndOverloads (genCache: GeneratorContext) (prerender: MethodRender) = fun methodPath ->
    let prerenderedParameters = ParameterRenderArray.prerenderPatterns genCache methodPath prerender.Parameters
    let returnType = TypeRefRender.toWidget genCache methodPath prerender.ReturnType
    // Renders a member with the given parameters and attributes
    let render = fun paras attr ->
        Utils.renderMemberWithAttributes
            prerender.IsStatic
            paras
            returnType
            [
                yield Attributes.erase
                match attr with
                | ValueSome attr -> yield attr
                | _ -> ()
            ]
            prerender
        
    match prerenderedParameters with
    | { Default = parameters; ParamObject = ValueNone } ->
        render parameters ValueNone
        |> Array.singleton
    | { Default = defaultParameters; ParamObject = ValueSome _ } as paramObjectPrerender ->
        let paramObjectOverloadParameters, paramObjectAttribute =
            paramObjectPrerender.ParamObjectValueAttributeTuple
        [|
            render paramObjectOverloadParameters (ValueSome paramObjectAttribute)
            render defaultParameters ValueNone
            match paramObjectPrerender.TryOptionless with
            | ValueSome optionlessParameters ->
                render optionlessParameters ValueNone
            | _ -> ()
        |]
let renderMemberAndOverloadsFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyMethod>) = fun methodParentPath ->
    prerender genCache key methodParentPath 
    |> renderMemberAndOverloads genCache
    |> funApply (getMethodPath genCache key methodParentPath)

let toTypeRef (genCache: GeneratorContext) (prerender: MethodRender) = fun methodPath ->
    Ast.Funs(
        prerender.Parameters
        |> ParameterRenderArray.prerenderNamelessTypes genCache methodPath
        |> _.Default
        ,
        prerender.ReturnType
        |> TypeRefRender.toWidget genCache methodPath
    )
    |> TypeRefRender.create prerender.IsOptional
