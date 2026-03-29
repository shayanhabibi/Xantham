module Xantham.SimpleGenerator.Generator.TypeParameterRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getTypeParameterPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeParameter>) = fun typeParameterParentPath ->
    genCache.pathResolver.Prerenderer typeParameterParentPath (TypeParameter.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeParameter>) =
    let name =
        TypeParameter.name key
        |> PatternContext.value
        |> Name.Typar.create
    let constraintVoption =
        TypeParameter.constraint' key
        |> ValueOption.map (PatternContext.value >> GeneratorContext.getTypeRef genCache)
    let defaultVoption =
        try
        TypeParameter.default' key |> ValueOption.map (PatternContext.value >> GeneratorContext.getTypeRef genCache)
        with _ -> ValueNone
    let typeParamMasterKey = TypeParameter.toMasterKey key
    fun typeParameterParentPath ->
        let path = genCache.pathResolver.Prerenderer typeParameterParentPath typeParamMasterKey
        {
            TypeParameterRender.Name = name
            Constraint =
                constraintVoption
                |> ValueOption.mapApply path
            Default =
                try
                defaultVoption
                |> ValueOption.mapApply path
                with _ -> ValueNone
        }

let toTyparDecl (genCache: GeneratorContext) (value: TypeParameterRender) = fun typeParameterPath ->
    match value.Constraint with
    | ValueSome constraint' ->
        Ast.TyparDecl(
            value.Name
            |> Name.Case.valueOrModified,
            [
                Ast.WithSubType(
                Ast.SubtypeOf(
                    Name.Case.valueOrModified value.Name,
                    constraint'
                    |> TypeRefRender.toWidgetNoOption genCache typeParameterPath
                )
                    )
            ]
            )
    | ValueNone ->
        Ast.TyparDecl(value.Name |> Name.Case.valueOrModified)

let toTyparDeclWithNoConstraint (value: TypeParameterRender) =
    Ast.TyparDecl(value.Name |> Name.Case.valueOrModified)

let toWidget (value: TypeParameterRender) =
    value.Name
    |> Name.Case.valueOrModified
    |> Ast.LongIdent