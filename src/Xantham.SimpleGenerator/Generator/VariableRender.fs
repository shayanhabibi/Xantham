module Xantham.SimpleGenerator.Generator.VariableRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getVariablePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyVariable>) = fun variableParentPath ->
    genCache.pathResolver.Prerenderer variableParentPath (Variable.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyVariable>) =
    let name = Variable.name key |> PatternContext.value |> Name.Camel.create
    let type' = Variable.type' key |> PatternContext.value
    let masterKey = Variable.toMasterKey key
    let typeFn = GeneratorContext.getTypeRef genCache type'
    fun variableParentPath ->
        let variablePath = genCache.pathResolver.Prerenderer variableParentPath masterKey
        {
            VariableRender.Name = name
            Type = typeFn variablePath
        }

let renderTypeRef (genCache: GeneratorContext) (value: VariableRender) = fun variablePath ->
    value.Type
    |> TypeRefRender.toWidget genCache variablePath

let renderAbstract (genCache: GeneratorContext) (value: VariableRender) = fun variablePath ->
    Ast.AbstractMember(
        Name.Case.valueOrModified value.Name,
        renderTypeRef genCache value variablePath,
        hasGetter = true,
        hasSetter = true
    )   .toStatic() // variables are static members of a module

let renderMember (genCache: GeneratorContext) (value: VariableRender) = fun variablePath ->
    Ast.Member(
        Name.Case.valueOrModified value.Name,
        Exprs.jsUndefined,
        returnType = renderTypeRef genCache value variablePath
    )
        .toStatic()
        .toMutable()
        .attributes([
            Attributes.erase
        ])

let renderLetBinding (genCache: GeneratorContext) (value: VariableRender) = fun variablePath ->
    Ast.Value(
        Name.Case.valueOrModified value.Name,
        Exprs.jsUndefined,
        returnType = renderTypeRef genCache value variablePath
    )   .toMutable()