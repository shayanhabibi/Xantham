namespace Xantham.SimpleGenerator.Generator

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns


[<AutoOpen>]
module ParameterArrayPrerenderExtensions =
    type ParameterArrayPrerenderOverloads<'T>  with
        member this.ParamObjectValues =
            let paramObject = this.ParamObject.Value
            Array.append
                this.Default[.. paramObject.Index - 1]
                paramObject.ParamObjectPrerenders
        member inline this.ParamObjectAttribute =
            Attributes.paramObject this.ParamObject.Value.Index
        member inline this.ParamObjectValueAttributeTuple =
            this.ParamObjectValues, this.ParamObjectAttribute
        member this.TryParamObjectValues =
            match this.ParamObject with
            | ValueSome { ParamObjectPrerenders = paramObjectPrerenders; Index = index } ->
                Array.append
                    this.Default[.. index - 1]
                    paramObjectPrerenders
                |> ValueSome
            | _ -> ValueNone
        member inline this.TryParamObjectAttribute =
            match this.ParamObject with
            | ValueSome { Index = index } ->
                Attributes.paramObject index
                |> ValueSome
            | _ -> ValueNone
        member this.TryParamObjectValueAttributeTuple =
            match this.TryParamObjectValues with
            | ValueSome paramObjectValues ->
                (paramObjectValues, this.ParamObjectAttribute)
                |> ValueSome
            | _ -> ValueNone
        member this.TryOptionless =
            match this.Optionless with
            | ValueSome idx when idx < this.Default.Length - 1 ->
                this.Default[0.. idx]
                |> ValueSome
            | _ ->
                ValueNone
        member this.TryParamObjectValueAttributeTupleOrDefault =
            match this.TryParamObjectAttribute with
            | ValueSome paramObjectAttribute ->
                this.ParamObjectValues, ValueSome paramObjectAttribute
            | _ ->
                this.Default, ValueNone
            

module ParameterRender =
    /// <summary>
    /// Represents a unit parameter.
    /// </summary>
    let unitParameter = {
        Name = Name.Source null |> unbox<Name<Case.camel>>
        Type = Prelude.TypeRefRenders.unit
        IsOptional = false
        IsParamArray = false
    }
    
    /// <summary>
    /// Renders a parameter as a pattern widget.
    /// </summary>
    /// <remarks>
    /// If the name of the parameter is null, then the parameter is rendered as a unit parameter.
    /// </remarks>
    /// <param name="ctx"></param>
    /// <param name="parameterParent">
    /// The path to the method, function, or whatever the parent of the parameter is.
    /// This is used to resolve the nested types of the parameter.
    /// </param>   
    let renderAsPattern (ctx: GeneratorContext): KeyPathKind -> ParameterRender -> _ = fun parameterParent -> function
        | param when param.Name = unitParameter.Name && not (param.IsOptional || param.IsParamArray) ->
            Ast.UnitPat()
        | param ->
            let makeName: ParameterRender -> _ =
                Utils.renderParameterName
                >> Ast.Constant
            let path = KeyPath.appendQualifierFromNamedRender ctx.ctx param parameterParent
            Ast.ParameterPat(
                makeName param,
                TypeRefRender.toWidget ctx path param.Type
            )
            |> Utils.Pattern.attributesIfNotEmpty [
                if param.IsParamArray then Attributes.paramArray
            ]
            
    /// <summary>
    /// Renders a parameter as a type widget using named parameter widgets.
    /// </summary>
    /// <remarks>
    /// If the name of the parameter is null, then the parameter is rendered as a unit parameter.
    /// </remarks>
    /// <param name="ctx"></param>
    /// <param name="parameterParentPath">
    /// The path to the method, function, or whatever the parent of the parameter is.
    /// This is used to resolve the nested types of the parameter.
    /// </param>  
    let renderAsType ctx parameterParentPath : ParameterRender -> _ = function
        | param when param.Name = unitParameter.Name && not (param.IsOptional || param.IsParamArray) ->
            Ast.Unit()
        | param ->
            let makeName = Utils.renderParameterName
            let path = KeyPath.appendQualifierFromNamedRender ctx.ctx param parameterParentPath
            let renderType =
                (if param.IsOptional then
                    TypeRefRender.toWidgetNoOption 
                else TypeRefRender.toWidget) ctx.ctx path param.Type
            Ast.SignatureParameter(
                makeName param,
                renderType
                )
            |> Utils.SignatureParameter.attributesIfNotEmpty [
                if param.IsParamArray then Attributes.paramArray
            ]
            
    /// <summary>
    /// Renders a parameter as a type widget without the name.
    /// </summary>
    /// <remarks>
    /// If the name of the parameter is null, then the parameter is rendered as a unit parameter.
    /// </remarks>
    /// <param name="ctx"></param>
    /// <param name="path">
    /// The path to the method, function, or whatever the parent of the parameter is.
    /// This is used to resolve the nested types of the parameter.
    /// </param>  
    let renderAsNamelessType ctx path: ParameterRender -> _ = function
        | param when param.Name = unitParameter.Name && not (param.IsOptional || param.IsParamArray) ->
            Ast.Unit()
        | param ->
            param.Type
            |> TypeRefRender.toWidget ctx.ctx path
            |> Utils.SignatureParameter.attributesIfNotEmpty [
                if param.IsParamArray then Attributes.paramArray
            ]

    /// <summary>
    /// INTERNAL<br/>
    /// The parameter is optional if it is a optional parameter, or the type it references is optional.
    /// </summary>
    let private isTypeOptional = function
        | Parameter.IsOptional | Parameter.Type (Patterns.OptionalUnion _) -> true
        | _ -> false
        
    /// <summary>
    /// INTERNAL<br/>
    /// The property is optional if it is a optional property, or the type it references is optional.
    /// </summary>
    let private isPropTypeOptional = function
        | Property.IsOptional | Property.Type (Patterns.OptionalUnion _) -> true
        | _ -> false
    
    let private getPathOfParameterKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyParameter>) = fun parameterPathParent ->
        key
        |> Parameter.toMasterKey
        |> genCache.pathResolver.Prerenderer parameterPathParent
        
    /// Creates a parameter prerender computation for a single key parameter.
    /// Resolution of the prerender computation requires the parent path of the parameter.
    let prerenderKeyParameter (genCache: GeneratorContext) (key: PatternContextHolder<KeyParameter>) =
        let typeKey = Parameter.type' key
        let isOptional = isTypeOptional key
        let name = Parameter.name key |> PatternContext.value |> Name.Camel.create
        let isSpread = Parameter.isSpread key
        let masterKey = Parameter.toMasterKey key
        let typeFn = GeneratorContext.getTypeRef genCache typeKey.Value
        fun parameterParentPath ->
            // The parameter path is the path of the parameter appended to the parent path.
            let parameterPath = genCache.pathResolver.Prerenderer parameterParentPath masterKey
            // The type reference for the parameter is resolved using the parameter path
            {
                ParameterRender.Name = name
                Type = typeFn parameterPath
                IsOptional = isOptional
                IsParamArray = isSpread
            }
    
    
    let private getPathOfPropertyKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>) =
        let masterKey = Property.toMasterKey key
        fun propertyPathParent ->
            masterKey
            |> genCache.pathResolver.Prerenderer propertyPathParent
        
    
    let private renderTypeForParameterKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyParameter>) =
        let typeKey = Parameter.type' key |> PatternContext.value
        GeneratorContext.getTypeRef genCache typeKey

    let private renderTypeForPropertyKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>) = 
        let typeKey = Property.type' key |> PatternContext.value
        GeneratorContext.getTypeRef genCache typeKey

    let private prerenderPropertyAsParam (genCache: GeneratorContext) (value: PatternContextHolder<KeyProperty>) =
        let isOptional = isPropTypeOptional value
        let masterKey = Property.toMasterKey value
        let name = Property.name value |> PatternContext.value |> Name.Camel.create
        fun parameterPath ->
            {
                ParameterRender.Name = name
                Type =
                    genCache.pathResolver.Prerenderer parameterPath masterKey
                    |> renderTypeForPropertyKey genCache value
                IsOptional = isOptional
                IsParamArray = false
            }
        
    
    /// <summary>
    /// </summary>
    /// <param name="genCache"></param>
    /// <param name="keys"></param>
    let prerender (genCache: GeneratorContext) (keys: PatternContextHolder<KeyParameter array>) = 
        let standardParameterRender = prerenderKeyParameter genCache
        let canParameteriseAtIndex idx members =
            let keyArray = keys.Value
            
            // If the previous parameter isnt optional, then we can inline the parameter irregardless
            let previousIdxIsOptional =
                if idx = 0 then true else
                keyArray
                |> Array.item (idx - 1)
                |> _.IsOptional
                
            not previousIdxIsOptional
            ||  // otherwise we check if the first member is optional, which would indicate the rest are too
                let firstMemberIsOptional =
                    members
                    |> PatternContext.mapc (fun ctx ->
                        Array.head
                        >> PatternContext.prepare ctx
                        >> function
                        | MemberKey.Property Property.IsOptional -> true
                        | _ -> false
                        )
                    |> PatternContext.value
                firstMemberIsOptional    
            // || // otherwise we check if all members are optional-like
            //     let allMembersAreOptionalLike =
            //         members
            //         |> PatternContext.Array.cforall (function
            //             | MemberKey.Property (
            //                 Property.IsOptional
            //                  | Property.Type (Patterns.OptionalUnion _)
            //                 ) -> true
            //             | _ -> false
            //         )
            //     allMembersAreOptionalLike
                
        fun parametersParent ->
            let keyParamArrayFoldFunc
                    (lastIdx: int)
                    (acc: _ array, hasParamObj: bool)
                    (idx: int)
                    (keyParam: PatternContextHolder<KeyParameter>) =
                match keyParam with
                // short path
                | _ when idx <> lastIdx || Parameter.isSpread keyParam ->
                    Array.appendOne (standardParameterRender keyParam parametersParent) acc, hasParamObj
                // long path; unlikely; paramobject inliner
                | Parameter.VisitationFlag VisitationFlags.IsMaybeParamObject & Parameter.NotSpread
                    & Parameter.Type (MasterKey.Members members | Patterns.OptionalUnion (MasterKey.Members members))
                    when members |> PatternContext.Array.cforall MemberKey.isProperty
                        && canParameteriseAtIndex idx members ->
                    let parameterPath = getPathOfParameterKey genCache keyParam parametersParent
                    members
                    |> PatternContext.Array.cmap (function
                        | MemberKey.Property prop -> prerenderPropertyAsParam genCache prop parameterPath
                        | _ -> failwith "Unreachable"
                        )
                    |> PatternContext.value
                    |> fun pats -> Array.append acc pats, true
                // long path; likely; long path fallback to shortpath render func
                | _ -> Array.appendOne (standardParameterRender keyParam parametersParent) acc, hasParamObj

            match keys with
            | Array.Length 0 ->
                ParameterRenderArray.Standard [| unitParameter |]
            | Array.Length length ->
                let lastIdx = length - 1
                keys
                |> PatternContext.Array.Indexed.cfold (keyParamArrayFoldFunc lastIdx) ([||], false)
                |> PatternContext.value
                |> function
                | pats, false ->
                    ParameterRenderArray.Standard pats
                | pats, _ when lastIdx = 0 ->
                    let standardParamRender =
                        keys
                        |> PatternContext.Array.item lastIdx
                        |> PatternContext.cmap standardParameterRender
                    ParameterRenderArray.ParamObject(
                        [| standardParamRender parametersParent |],
                        pats,
                        lastIdx
                        )
                | pats, _ ->
                    let standardParamRenders =
                        pats[0..lastIdx - 1]
                        |> Array.appendOne (
                            keys
                            |> PatternContext.Array.item lastIdx
                            |> PatternContext.cmap standardParameterRender
                            |> funApply parametersParent
                            )
                    let paramObjRenders = pats[lastIdx ..]
                    ParameterRenderArray.ParamObject (standardParamRenders, paramObjRenders, lastIdx)

    let toParameterArrayPrender = function
        | ParameterRenderArray.Standard parameters ->
            {
                Default = parameters
                ParamObject = ValueNone
                Optionless = ValueNone
            }
        | ParameterRenderArray.ParamObject (parameters, paramObjectRenders, idx) ->
            {
                Default = parameters
                ParamObject = ValueSome { ParamObjectPrerenders = paramObjectRenders; Index = idx }
                Optionless =
                    parameters
                    |> Array.tryFindIndex _.IsOptional
                    |> ValueOption.ofOption
            }
    
    let inline private prerenderMappingImpl
        (fn: GeneratorContext -> KeyPathKind -> ParameterRender -> WidgetBuilder<'T>)
        (genCache: GeneratorContext)
        (keys: PatternContextHolder<KeyParameter array>)
         = 
        prerender genCache keys
        >> function
        | ParameterRenderArray.Standard pats ->
            fun (parametersParent: KeyPathKind) ->
                let standardParamRenders =
                    pats
                    |> Array.map (fn genCache parametersParent)
                {
                    Default = standardParamRenders
                    ParamObject = ValueNone
                    Optionless = ValueNone
                }
        | ParameterRenderArray.ParamObject (pats, paramObjectRenders, idx) ->
            let firstNonOption = pats |> Array.tryFindIndex _.IsOptional |> ValueOption.ofOption
            fun (parametersParent: KeyPathKind) ->
                let standardParamRenders =
                    pats |> Array.map (fn genCache parametersParent)
                let paramObjectRenders =
                    paramObjectRenders |> Array.map (fn genCache parametersParent)
                {
                    Default = standardParamRenders
                    ParamObject = ValueSome { ParamObjectPrerenders = paramObjectRenders; Index = idx }
                    Optionless = firstNonOption
                }
    let prerenderPatterns genCache keys =
        prerenderMappingImpl renderAsPattern genCache keys

    let prerenderTypes genCache keyPath =
        prerenderMappingImpl renderAsType genCache keyPath
    
    let prerenderNamelessTypes genCache keyPath =
        prerenderMappingImpl renderAsNamelessType genCache keyPath
module ParameterArrayPrerender =
    let map<'T, 'U> (fn: 'T -> 'U) (prerender: ParameterArrayPrerenderOverloads<'T>): ParameterArrayPrerenderOverloads<'U> =
        {
            Default = prerender.Default |> Array.map fn
            ParamObject =
                prerender.ParamObject
                |> ValueOption.map (fun paramObject ->
                    {
                        ParamObjectPrerenders = paramObject.ParamObjectPrerenders |> Array.map fn
                        Index = paramObject.Index
                    }
                )
            Optionless = prerender.Optionless
        }
    let inline mapc<'T, 'U> (fn: GeneratorContext -> KeyPathKind -> 'T -> 'U) (genCache: GeneratorContext) (keyPath: KeyPathKind) (prerender: ParameterArrayPrerenderOverloads<'T>): ParameterArrayPrerenderOverloads<'U> =
        map (fn genCache keyPath) prerender
module ParameterRenderArray =
    let toParameterArrayPrerender = function
        | ParameterRenderArray.Standard parameters ->
            {
                Default = parameters
                ParamObject = ValueNone
                Optionless = ValueNone
            }
        | ParameterRenderArray.ParamObject (parameters, paramObjectRenders, idx) ->
            {
                Default = parameters
                ParamObject = ValueSome { ParamObjectPrerenders = paramObjectRenders; Index = idx }
                Optionless = parameters |> Array.tryFindIndex _.IsOptional |> ValueOption.ofOption
            }
    
    let inline private prerenderMappingImpl 
        (fn: GeneratorContext -> KeyPathKind -> ParameterRender -> WidgetBuilder<'T>)
        (genCache: GeneratorContext)
        (keyPath: KeyPathKind)=
            toParameterArrayPrerender
            >> ParameterArrayPrerender.mapc fn genCache keyPath
    
    let prerenderPatterns genCache parametersParent =
        prerenderMappingImpl ParameterRender.renderAsPattern genCache parametersParent

    let prerenderTypes genCache parametersParent =
        prerenderMappingImpl ParameterRender.renderAsType genCache parametersParent

    let prerenderNamelessTypes genCache parametersParent =
        prerenderMappingImpl ParameterRender.renderAsNamelessType genCache parametersParent