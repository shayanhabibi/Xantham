module Xantham.SimpleGenerator.Generator.TypeReferenceRender

open Fabulous.AST
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getTypeReferencePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeReference>) = fun typeReferenceParentPath ->
    genCache.pathResolver.Prerenderer typeReferenceParentPath (TypeReference.toMasterKey key)
    
let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeReference>) =
    let typeReferenceType =
        TypeReference.type' key
        |> PatternContext.value
    let typeArguments =
        TypeReference.typeArguments key
        |> PatternContext.value
        |> Array.map (GeneratorContext.getTypeRef genCache)
    let typeRefMasterKey = TypeReference.toMasterKey key
    let typeFn = GeneratorContext.getTypeRef genCache typeReferenceType
    fun typeReferenceParentPath ->
        let path = genCache.pathResolver.Prerenderer typeReferenceParentPath typeRefMasterKey
        {
            TypeReferenceRender.Type = typeFn path 
            TypeArguments =
                typeArguments
                |> Array.mapApply path
        }

let toWidget (genCache: GeneratorContext) (value: TypeReferenceRender) = fun typeReferencePath ->
    match value with
    | { TypeArguments = [||]; Type = typ } ->
        TypeRefRender.toWidgetNoOption genCache typeReferencePath typ
    | { TypeArguments = typeArguments; Type = typ } ->
        Ast.AppPrefix(
            TypeRefRender.toWidgetNoOption genCache typeReferencePath typ,
            typeArguments
            |> Array.map (TypeRefRender.toWidgetNoOption genCache typeReferencePath)
        )
    |> if value.Type.Nullable then Types.option else id