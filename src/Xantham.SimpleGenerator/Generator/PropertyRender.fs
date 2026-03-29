module Xantham.SimpleGenerator.Generator.PropertyRender

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getPropertyPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>) = fun propertyParentPath ->
    genCache.pathResolver.Prerenderer propertyParentPath (Property.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>): _ -> PropertyRender =
    let propertyMasterKey = Property.toMasterKey key
    let keyType, name, accessor, isStatic, isOptional, isPrivate =
        match key with
        | Property.Name (Value name)
         & Property.Accessor (Value accessor)
         & Property.Type typ ->
            typ,
            name,
            accessor,
            Property.isStatic key,
            Property.isOptional key || typ |> Patterns.isOptionalUnion |> ValueOption.isSome,
            Property.isPrivate key
    
    match keyType with
    | MasterKey.KeyType.TypeLiteral (TypeLiteral.IsCallMethod callMethod)
    | Patterns.OptionalUnion (
        MasterKey.KeyType.TypeLiteral (TypeLiteral.IsCallMethod callMethod)
        ) ->
        let callSigFn = CallSignatureRender.prerender genCache callMethod
        let casedName = Name.Camel.create name
        fun propertyParentPath ->
            let propertyPath = genCache.pathResolver.Prerenderer  propertyParentPath propertyMasterKey
            {
                Name = casedName
                CallSignatureRender = callSigFn propertyPath
                IsOptional = isOptional 
                IsStatic = isStatic
                Accessor = accessor
                IsPrivate = isPrivate
            } |> PropertyRender.Method
    | Value masterKey ->
        let typeFn = GeneratorContext.getTypeRef genCache masterKey
        let casedName = Name.Camel.create name
        fun propertyParentPath ->
            let propertyPath = genCache.pathResolver.Prerenderer  propertyParentPath propertyMasterKey
            {
                Name = casedName
                Type = typeFn propertyPath 
                Accessor = accessor
                IsOptional = isOptional 
                IsStatic = isStatic
                IsPrivate = isPrivate
            } |> PropertyRender.Default
        
let renderDefaultName = Name.Case.valueOrModified >> sprintf "_.%s"
let renderStaticName = Name.Case.valueOrModified >> sprintf "%s"
let renderAbstractName: Name<Case.camel> -> string = Name.Case.valueOrModified

let inline renderName (value: ^T when ^T : (member IsStatic: bool) and ^T: (member Name: Name<Case.camel>)) =
    if value.IsStatic then renderStaticName value.Name else renderDefaultName value.Name
    
let renderMember (genCache: GeneratorContext) (prerender: PropertyRender) = fun propertyPath ->
    match prerender with
    | PropertyRender.Default propertyDefaultRender ->
        let name = renderName propertyDefaultRender
        let renderTypeOptional =
            if propertyDefaultRender.Type.Nullable || propertyDefaultRender.IsOptional then
                fun path -> TypeRefRender.toWidgetNoOption genCache.ctx path propertyDefaultRender.Type |> Types.option
            else fun path -> TypeRefRender.toWidgetNoOption genCache.ctx path propertyDefaultRender.Type
        match propertyDefaultRender.Accessor with
        | TsAccessor.ReadOnly ->
            let attributes = attributes {
                emitPropertyOrErase propertyDefaultRender.Name
            }
            Ast.Member(
                name,
                Exprs.jsUndefined,
                renderTypeOptional propertyPath 
                )
            |> Utils.Member.attributesIfNotEmpty attributes
        | TsAccessor.WriteOnly ->
            let parameter = Ast.ParameterPat("value", renderTypeOptional propertyPath) |> Ast.ParenPat
            let attributes = attributes {
                emitPropertyOrErase propertyDefaultRender.Name
            }
            Ast.Member(
                name,
                Ast.Setter(parameter, Exprs.unit)
                |> Utils.Setter.attributesIfNotEmpty attributes
                )
            |> Utils.Member.attributesIfNotEmpty attributes
        | TsAccessor.ReadWrite ->
            let parameter = Ast.ParameterPat("value", renderTypeOptional propertyPath ) |> Ast.ParenPat
            let attributes = attributes {
                emitPropertyOrErase propertyDefaultRender.Name
            }
            Ast.Member(
                name,
                Ast.Getter(
                    Exprs.jsUndefined,
                    renderTypeOptional propertyPath 
                    ) |> Utils.Getter.attributesIfNotEmpty attributes
                ,
                Ast.Setter(
                    parameter,
                    Exprs.unit
                    ) |> Utils.Setter.attributesIfNotEmpty attributes
                )
            |> Utils.Member.attributesIfNotEmpty attributes
        |> Utils.Member.toStaticIfTrue propertyDefaultRender.IsStatic
        |> Utils.Member.toPrivateIfTrue propertyDefaultRender.IsPrivate
    | PropertyRender.Method propertyMethodRender ->
        let parameters, paramObjectAttr =
            propertyMethodRender.CallSignatureRender.Parameters
            |> ParameterRenderArray.prerenderPatterns genCache propertyPath
            |> _.TryParamObjectValueAttributeTupleOrDefault
        let returnType = propertyMethodRender.CallSignatureRender.ReturnType
        let name = renderName propertyMethodRender
        let attributes = attributes {
            compiledNameOrErase propertyMethodRender.Name
            paramObjectAttr
        }
        Ast.Member(
            name,
            Ast.ParenPat(Ast.TuplePat(parameters)),
            Exprs.jsUndefined,
            TypeRefRender.toWidget genCache.ctx propertyPath returnType
            )
        |> Utils.Member.attributesIfNotEmpty attributes
        |> Utils.Member.toStaticIfTrue propertyMethodRender.IsStatic
        |> Utils.Member.toPrivateIfTrue propertyMethodRender.IsPrivate
let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>) = fun propertyParentPath ->
    prerender genCache key propertyParentPath
    |> renderMember genCache
    |> funApply (getPropertyPath genCache key propertyParentPath)

let renderAbstract (genCache: GeneratorContext) (render: PropertyRender) = fun propertyPath ->
    match render with
    | PropertyRender.Default propertyDefaultRender ->
        let name = renderAbstractName propertyDefaultRender.Name
        let renderTypeOptional =
            if propertyDefaultRender.Type.Nullable || propertyDefaultRender.IsOptional then
                TypeRefRender.toWidgetNoOption genCache.ctx propertyPath propertyDefaultRender.Type |> Types.option
            else TypeRefRender.toWidgetNoOption genCache.ctx propertyPath propertyDefaultRender.Type
        // let attributes = [
        //     if Name.Case.isModified propertyDefaultRender.Name then
        //         Name.Case.valueOrSource propertyDefaultRender.Name
        //         |> Attributes.compiledName
        // ]
        let attributes = attributes {
            compiledName propertyDefaultRender.Name
        }
        
        Ast.AbstractMember(
            name,
            renderTypeOptional,
            hasSetter = not propertyDefaultRender.Accessor.IsReadOnly,
            hasGetter = not propertyDefaultRender.Accessor.IsWriteOnly
            )
        |> Utils.AbstractMember.attributesIfNotEmpty attributes
        |> Utils.AbstractMember.toStaticIfTrue propertyDefaultRender.IsStatic
    | PropertyRender.Method propertyMethodRender ->
        let parameters, paramObjectAttr =
            propertyMethodRender.CallSignatureRender.Parameters
            |> ParameterRenderArray.prerenderTypes genCache propertyPath
            |> _.TryParamObjectValueAttributeTupleOrDefault
        let returnType = propertyMethodRender.CallSignatureRender.ReturnType
        // let attributes = [
        //     if Name.Case.isModified propertyMethodRender.Name then
        //         Name.Case.valueOrSource propertyMethodRender.Name
        //         |> Attributes.compiledName
        //     if paramObjectAttr.IsSome then paramObjectAttr.Value
        // ]
        let attributes = attributes {
            compiledName propertyMethodRender.Name
            paramObjectAttr
        }
        Ast.AbstractMember(
            renderAbstractName propertyMethodRender.Name,
            parameters,
            TypeRefRender.toWidget genCache.ctx propertyPath returnType,
            isTupled = true,
            hasGetter = not propertyMethodRender.Accessor.IsWriteOnly,
            hasSetter = not propertyMethodRender.Accessor.IsReadOnly
            )
        |> Utils.AbstractMember.attributesIfNotEmpty attributes
        |> Utils.AbstractMember.toStaticIfTrue propertyMethodRender.IsStatic
let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyProperty>) = fun propertyParentPath ->
    prerender genCache key propertyParentPath
    |> renderAbstract genCache
    |> funApply (getPropertyPath genCache key propertyParentPath)

let toTypeRef (genCache: GeneratorContext) (prerender: PropertyRender) =
    match prerender with
    | PropertyRender.Default { Type = typ } -> fun _ -> typ
    | PropertyRender.Method { CallSignatureRender = callSignatureRender } ->
        CallSignatureRender.toTypeRef genCache callSignatureRender