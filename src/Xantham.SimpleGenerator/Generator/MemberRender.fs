module Xantham.SimpleGenerator.Generator.MemberRender

open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<MemberKey>) =
    match key with
    | MemberKey.Property prop ->
        PropertyRender.prerender genCache prop 
        >> MemberRender.Property
    | MemberKey.Method method ->
        MethodRender.prerender genCache method 
        >> MemberRender.Method
    | MemberKey.CallSignature signature ->
        CallSignatureRender.prerender genCache signature 
        >> MemberRender.CallSignature
    | MemberKey.IndexSignature signature ->
        IndexSignatureRender.prerender genCache signature 
        >> MemberRender.IndexSignature
    | MemberKey.ConstructSignature signature ->
        ConstructSignatureRender.prerender genCache signature 
        >> MemberRender.ConstructSignature
    | MemberKey.GetAccessor getter ->
        GetAccessorPrerender.prerender genCache getter 
        >> MemberRender.GetSet
    | MemberKey.SetAccessor setter ->
        SetAccessorPrerender.prerender genCache setter 
        >> MemberRender.GetSet

let renderAbstract (genCache: GeneratorContext) (render: MemberRender) =
    match render with
    | MemberRender.Property (PropertyRender.Default { Name = name } | PropertyRender.Method { Name = name } as propertyRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> PropertyRender.renderAbstract genCache propertyRender
    | MemberRender.GetSet (GetSetRender.Getter { Name = name } | GetSetRender.Setter { Name = name } as setRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> GetterSetterPrerender.renderAbstract genCache setRender
    | MemberRender.Method methodRender ->
        KeyPath.appendQualifierFromNamedRender genCache.ctx methodRender
        >> MethodRender.renderAbstract genCache methodRender
    | MemberRender.IndexSignature indexSignatureRender ->
        IndexSignatureRender.renderAbstract genCache indexSignatureRender
    | MemberRender.CallSignature callSignatureRender ->
        CallSignatureRender.renderAbstract genCache callSignatureRender
    | MemberRender.ConstructSignature constructSignatureRender ->
        ConstructSignatureRender.renderAbstract genCache constructSignatureRender

let renderMember (genCache: GeneratorContext) (render: MemberRender) =
    match render with
    | MemberRender.Property (PropertyRender.Default { Name = name } | PropertyRender.Method { Name = name } as propertyRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> PropertyRender.renderMember genCache propertyRender
    | MemberRender.GetSet (GetSetRender.Getter { Name = name } | GetSetRender.Setter { Name = name } as setRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> GetterSetterPrerender.renderMember genCache setRender
    | MemberRender.Method methodRender ->
        KeyPath.appendQualifierFromNamedRender genCache.ctx methodRender
        >> MethodRender.renderMember genCache methodRender
    | MemberRender.IndexSignature indexSignatureRender ->
        IndexSignatureRender.renderMember genCache indexSignatureRender
    | MemberRender.CallSignature callSignatureRender ->
        CallSignatureRender.renderMember genCache callSignatureRender
    | MemberRender.ConstructSignature constructSignatureRender ->
        ConstructSignatureRender.renderMember genCache constructSignatureRender

let renderMemberAndOverloads (genCache: GeneratorContext) (render: MemberRender) = 
    match render with
    | MemberRender.Property (PropertyRender.Default { Name = name } | PropertyRender.Method { Name = name } as propertyRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> PropertyRender.renderMember genCache propertyRender
        >> Array.singleton
    | MemberRender.GetSet (GetSetRender.Getter { Name = name } | GetSetRender.Setter { Name = name } as setRender) ->
        KeyPath.appendQualifierWithName genCache.ctx name
        >> GetterSetterPrerender.renderMember genCache setRender
        >> Array.singleton
    | MemberRender.Method methodRender ->
        KeyPath.appendQualifierFromNamedRender genCache.ctx methodRender
        >> MethodRender.renderMemberAndOverloads genCache methodRender
    | MemberRender.IndexSignature indexSignatureRender ->
        IndexSignatureRender.renderMemberAndOverloads genCache indexSignatureRender
    | MemberRender.CallSignature callSignatureRender ->
        CallSignatureRender.renderMemberAndOverloads genCache callSignatureRender
    | MemberRender.ConstructSignature constructSignatureRender ->
        ConstructSignatureRender.renderMemberAndOverloads genCache constructSignatureRender

let toTypeRef (genCache: GeneratorContext) (render: MemberRender) =
    match render with
    | Property propertyRender -> PropertyRender.toTypeRef genCache propertyRender
    | Method methodRender -> MethodRender.toTypeRef genCache methodRender
    | MemberRender.CallSignature callSignatureRender -> CallSignatureRender.toTypeRef genCache callSignatureRender
    | GetSet setRender -> GetterSetterRender.toTypeRef genCache setRender
    | ConstructSignature constructSignatureRender -> failwith "todo"
    | IndexSignature indexSignatureRender -> failwith "todo"
