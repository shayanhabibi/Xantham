module Xantham.SimpleGenerator.Generator.TupleRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

[<Struct>]
type TuplePrerender = {
    Elements: TupleElementRender.TupleElementPrerender array
    MinRequired: int
    FixedLength: int
    IsReadOnly: bool
}

let getTuplePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTuple>) = fun tupleParentPath ->
    genCache.pathResolver.Prerenderer tupleParentPath (Tuple.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTuple>) =
    let elementFns =
        Tuple.elements key
        |> PatternContext.Array.cmap (TupleElementRender.prerender genCache)
        |> PatternContext.value
    let minRequired = Tuple.minRequired key
    let fixedLength = Tuple.fixedLength key
    let isReadOnly = Tuple.isReadOnly key
    let masterKey = Tuple.toMasterKey key
    fun tupleParentPath ->
        let path = genCache.pathResolver.Prerenderer tupleParentPath masterKey
        {
            TuplePrerender.Elements = Array.mapApply path elementFns
            MinRequired = minRequired
            FixedLength = fixedLength
            IsReadOnly = isReadOnly
        }
    

let toTupleRender (prerender: TuplePrerender) =
    if
        prerender.Elements
        |> Array.forall (function
            { IsOptional = false; IsVariadic = false } -> true
            | _ -> false)
    then
        {
            FixedTupleRender.Types =
                prerender.Elements
                |> Array.map (function
                    { Name = name
                      Type = typeRender
                      IsRest = isRest } -> {
                        Name = name
                        Type = typeRender
                        IsRest = isRest
                    }
                    )
        }
        |> TupleRender.Fixed
    else
        let optional,required =
            prerender.Elements
            |> Array.partition _.IsOptional
        {
            OptionalTupleRender.Fixed =
                required
                |> Array.map (function
                    { Name = name
                      Type = typeRender
                      IsRest = isRest } -> {
                        Name = name
                        Type = typeRender
                        IsRest = isRest
                    }
                    )
            Optional =
                optional
                |> Array.map (function
                    { Name = name
                      Type = typeRender
                      IsRest = isRest } -> {
                        Name = name
                        Type = typeRender
                        IsRest = isRest
                    }
                    )
        }
        |> TupleRender.Optional

let toTypes (genCache: GeneratorContext) (render: TupleRender) = fun (tuplePath: KeyPathKind) ->
    match render with
    | Fixed fixedTupleRender ->
        fixedTupleRender.Types
        |> Array.map (fun ele -> false, ele)
    | Optional { Optional = optional; Fixed = fixedElements } ->
        optional
        |> Array.map (fun ele -> true, ele)
        |> Array.append (
            fixedElements
            |> Array.map (fun ele -> false, ele)
            )
    |> Array.map (function
        | true, { Type = typeRender } ->
            TypeRefRender.toWidgetNoOption genCache tuplePath typeRender
            |> Types.option
        | false, { Type = typeRender } ->
            TypeRefRender.toWidgetNoOption genCache tuplePath typeRender
        )
let toTypeDefnAbbrev (genCache: GeneratorContext) (render: TupleRender) = fun (tuplePath: KeyPathKind) ->
    let name =
        tuplePath.Value.Qualifiers
        |> Array.last
        |> Dictionary.Flip.item genCache.ctx.cache.nameKeys
        |> Name.Pascal.create
    let tupleElementTypes = toTypes genCache render tuplePath
    Ast.Abbrev(
        Name.Case.valueOrModified name,
        Ast.Tuple(tupleElementTypes)
        )
