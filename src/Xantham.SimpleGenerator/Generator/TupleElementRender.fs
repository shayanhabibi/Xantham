module Xantham.SimpleGenerator.Generator.TupleElementRender

open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

[<Struct>]
type TupleElementPrerender = {
    Name: Name<Case.camel> voption
    Type: TypeRefRender
    IsOptional: bool
    IsRest: bool
    IsVariadic: bool
}

let getTupleElementPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTupleElement>) = fun tupleElementParentPath ->
    genCache.pathResolver.Prerenderer tupleElementParentPath (TupleElement.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTupleElement>) =
    let name = TupleElement.name key |> ValueOption.map (PatternContext.value >> Name.Camel.create)
    let typeFn =
        TupleElement.type' key
        |> PatternContext.value
        |> GeneratorContext.getTypeRef genCache

    let isOptional = TupleElement.isOptional key
    let isRest = TupleElement.isRest key
    let isVariadic = TupleElement.isVariadic key
    let masterKey = TupleElement.toMasterKey key
    fun tupleElementParentPath ->
        let path = genCache.pathResolver.Prerenderer tupleElementParentPath masterKey
        {
            TupleElementPrerender.Name = name
            Type = typeFn path
            IsOptional = isOptional
            IsRest = isRest
            IsVariadic = isVariadic
        }