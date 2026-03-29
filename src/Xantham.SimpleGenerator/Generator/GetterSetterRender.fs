namespace Xantham.SimpleGenerator.Generator

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

module GetterSetterPrerender =
    let inline private makeValueParameter (widget: WidgetBuilder<Type>) =
        Ast.ParameterPat("value", widget)
    let inline private wrapInParen (widget: WidgetBuilder<Pattern>) = Ast.ParenPat(widget)
    
    let private makeGetter genCache (prerender: GetterSetterRender) = fun path ->
        Ast.Getter(
            Ast.UnitPat(),
            Exprs.jsUndefined,
            TypeRefRender.toWidget genCache.ctx path prerender.Type
            ).attribute(Attributes.erase)
        
    let private makeSetter genCache (prerender: GetterSetterRender) = fun path ->
        Ast.Setter(
            TypeRefRender.toWidget genCache.ctx path prerender.Type
            |> makeValueParameter
            |> wrapInParen
            , Exprs.unit
            ).attribute(Attributes.erase)
    
    let renderMember (genCache: GeneratorContext) (prerender: GetSetRender) = fun getterSetterPath ->
        let name = Utils.renderMemberName prerender
        let getOrSetter =
            match prerender with
            | GetSetRender.Getter getterPrerender -> makeGetter genCache getterPrerender getterSetterPath 
            | GetSetRender.Setter setterPrerender -> makeSetter genCache setterPrerender getterSetterPath
        Ast.Member(name, getOrSetter).attributes([
            if Name.Case.isModified prerender.Name then
                Name.Case.valueOrSource prerender.Name
                |> Attributes.compiledName
            else Attributes.erase
        ])
        |> if prerender.IsPrivate then _.toPrivate() else id
        |> if prerender.IsStatic then _.toStatic() else id
        
    let renderAbstract (genCache: GeneratorContext) (prerender: GetSetRender) = fun getterSetterPath ->
        let name = Name.Case.valueOrModified prerender.Name
        Ast.AbstractMember(
            name,
            TypeRefRender.toWidget genCache.ctx getterSetterPath prerender.Type,
            hasSetter = prerender.IsSetter,
            hasGetter = prerender.IsGetter
            )
        |> _.attributes([
            if Name.Case.isModified prerender.Name then
                Name.Case.valueOrSource prerender.Name
                |> Attributes.compiledName
        ])
        |> if prerender.IsStatic then _.toStatic() else id

module GetAccessorPrerender =
    let getGetterPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyGetAccessor>) = fun getterSetterParentPath ->
        genCache.pathResolver.Prerenderer getterSetterParentPath (GetAccessor.toMasterKey key)
    let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyGetAccessor>) =
        match key with
        | GetAccessor.Name (Value name)
         & GetAccessor.Type (Value masterKey) ->
            
        let isStatic = GetAccessor.isStatic key
        let isPrivate = GetAccessor.isPrivate key
        let getterMasterKey = GetAccessor.toMasterKey key
        let typeFn = GeneratorContext.getTypeRef genCache masterKey
        let casedName = Name.Camel.create name
        fun getterSetterParentPath ->
            let getterSetterPath = genCache.pathResolver.Prerenderer getterSetterParentPath getterMasterKey
            GetSetRender.Getter {
                Name = casedName
                Type = typeFn getterSetterPath 
                IsStatic = isStatic
                IsPrivate = isPrivate
            }
    let renderMember genCache key = fun getterSetterPath -> GetterSetterPrerender.renderMember genCache key getterSetterPath
    let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyGetAccessor>) = fun getterSetterParentPath ->
        prerender genCache key getterSetterParentPath
        |> renderMember genCache
        <| getGetterPath genCache key getterSetterParentPath
    let renderAbstract genCache key= fun getterSetterPath -> GetterSetterPrerender.renderAbstract genCache key getterSetterPath
    let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeyGetAccessor>) = fun getterSetterParentPath ->
        prerender genCache key getterSetterParentPath
        |> renderAbstract genCache
        <| getGetterPath genCache key getterSetterParentPath

module SetAccessorPrerender =
    let getSetterPath (genCache: GeneratorContext) (key: PatternContextHolder<KeySetAccessor>) = fun setterParentPath ->
        genCache.pathResolver.Prerenderer setterParentPath (SetAccessor.toMasterKey key)
    let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeySetAccessor>) =
        match key with
        | SetAccessor.Name (Value name)
         & SetAccessor.Type (Value masterKey) ->
        
        let isStatic = SetAccessor.isStatic key
        let isPrivate = SetAccessor.isPrivate key
        let setterMasterKey = SetAccessor.toMasterKey key
        let typeFn = GeneratorContext.getTypeRef genCache masterKey
        let casedName = Name.Camel.create name
        fun setterParentPath ->
            let setterPath = genCache.pathResolver.Prerenderer setterParentPath setterMasterKey
            GetSetRender.Setter {
                Name = casedName
                Type = typeFn setterPath 
                IsStatic = isStatic
                IsPrivate = isPrivate
            }
    let renderMember (genCache: GeneratorContext) prerender = fun setterPath -> GetterSetterPrerender.renderMember genCache prerender setterPath
    let renderMemberFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeySetAccessor>) = fun setterParentPath ->
        prerender genCache key setterParentPath
        |> renderMember genCache
        <| getSetterPath genCache key setterParentPath
    let renderAbstract (genCache: GeneratorContext) prerender = fun setterPath -> GetterSetterPrerender.renderAbstract genCache prerender setterPath
    let renderAbstractFromKey (genCache: GeneratorContext) (key: PatternContextHolder<KeySetAccessor>) = fun setterParentPath ->
        prerender genCache key setterParentPath
        |> renderAbstract genCache
        <| getSetterPath genCache key setterParentPath

module GetterSetterRender =
    let toTypeRef (genCache: GeneratorContext) (prerender: GetSetRender) =
        match prerender with
        | GetSetRender.Getter { Type = typRef } ->
            fun gettsetterPath ->
                typRef
                |> TypeRefRender.toWidgetNoOption genCache gettsetterPath
                |> TypeRefRender.create typRef.Nullable
        | GetSetRender.Setter { Type = typRef } ->
            fun gettersetterPath ->
                Ast.Funs((typRef |> TypeRefRender.toWidgetNoOption genCache gettersetterPath), Types.unit)
                |> TypeRefRender.create typRef.Nullable
        