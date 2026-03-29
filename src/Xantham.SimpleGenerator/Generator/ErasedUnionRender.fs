module Xantham.SimpleGenerator.Generator.ErasedUnionRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let prerenderFromTypeArray (genCache: GeneratorContext) (types: PatternContextHolder<MasterKey[]>) =
    let typesFn =
        types
        |> PatternContext.value
        |> Array.map (GeneratorContext.getTypeRef genCache)
    fun erasedUnionParentPath ->
        { ErasedUnionRender.Types = Array.mapApply erasedUnionParentPath typesFn }

let prerenderFromConditional (genCache: GeneratorContext) (conditional: PatternContextHolder<KeyConditional>) = 
    let trueTypeFn = GeneratorContext.getTypeRef genCache conditional.Value.True
    let falseTypeFn = GeneratorContext.getTypeRef genCache conditional.Value.False
    fun conditionalParentPath ->
        let path = genCache.pathResolver.Prerenderer conditionalParentPath (Conditional.toMasterKey conditional)
        { ErasedUnionRender.Types = [| trueTypeFn path; falseTypeFn path |] }

let toWidget (genCache: GeneratorContext) (value: ErasedUnionRender) = fun erasedUnionPath ->
    value.Types
    |> Array.map (TypeRefRender.toWidgetNoOption genCache erasedUnionPath)
    |> Types.union
    |> if value.Types |> Array.exists _.Nullable then Types.option else id

let prerenderFromUnion (genCache: GeneratorContext) (union: PatternContextHolder<KeyUnion>) = fun unionParentPath ->
    let unionPath = genCache.pathResolver.Prerenderer unionParentPath (Union.toMasterKey union)
    let types =
        Union.types union
    let nullable =
        types
        |> PatternContext.Array.cexists (function MasterKey.IsNullish -> true | _ -> false)
    let typesWithoutNulls =
        if not nullable then types else
        types
        |> PatternContext.Array.cfilter (function MasterKey.IsNullish -> false | _ -> true)
    { ErasedUnionRender.Types =
        typesWithoutNulls.Value
        |> Array.map (GeneratorContext.getTypeRef genCache >> funApply unionPath )
        // if contained a nullable then we'll force the first typeref to be nullable
        // so that rendering the type to a widget will register the option type.
        |> if nullable then
            Array.mapi (function
                | 0 -> function
                    | { Nullable = false } as typeRefRender -> { typeRefRender with Nullable = true }
                    | typeRefRender -> typeRefRender
                | _ -> id)
           else id }

let renderAbbrev (genCache: GeneratorContext) (name: Name) (render: ErasedUnionRender) = fun erasedUnionPath ->
    Ast.Abbrev(
        Name.valueOrModified name,
        toWidget genCache render erasedUnionPath 
        )