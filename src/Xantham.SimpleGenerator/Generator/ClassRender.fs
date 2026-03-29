module Xantham.SimpleGenerator.Generator.ClassRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getClassPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyClass>) = fun classParentPath ->
    genCache.pathResolver.Prerenderer classParentPath (Class.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyClass>) =
    let name =
        Class.name key
        |> PatternContext.value
        |> Name.Pascal.create
    let heritageFns =
        Class.heritage key
        |> PatternContext.Array.cmap (TypeReferenceRender.prerender genCache)
        |> PatternContext.value
    let memberFns =
        Class.members key
        |> PatternContext.Array.cmap (MemberRender.prerender genCache)
        |> PatternContext.value
    let constructors =
        Class.constructors key
        |> PatternContext.Array.cmap (ConstructorRender.prerender genCache)
        |> PatternContext.value
    let typeParameters =
        Class.typeParameters key
        |> ValueOption.map (
            PatternContext.Array.cmap (TypeParameterRender.prerender genCache)
            >> PatternContext.value
            )
        |> ValueOption.defaultValue [||]
    let classMemberKey = Class.toMasterKey key
    fun classParentPath ->
        let classPath = genCache.pathResolver.Prerenderer classParentPath classMemberKey
        {
            ClassRender.Name = name
            Heritage = Array.mapApply classPath heritageFns 
            Members = Array.mapApply classPath memberFns
            Constructors = Array.mapApply classPath constructors
            TypeParameters = Array.mapApply classPath typeParameters
            Extensions = [||] // todo - ??
        }

let renderTypeDefn (genCache: GeneratorContext) (render: ClassRender) = fun classPath ->
    Ast.ClassEnd(
        Name.Case.valueOrModified render.Name,
        Ast.Constructor().toPrivate()
        ) {
        yield!
            render.Heritage
            |> Array.map (TypeReferenceRender.toWidget genCache >> funApply classPath >> Ast.Inherit)
        yield!
            render.Members
            |> Array.collect (MemberRender.renderMemberAndOverloads genCache >> funApply classPath)
        yield!
            render.Extensions
            |> Array.collect (MemberRender.renderMemberAndOverloads genCache >> funApply classPath)
        yield!
            render.Constructors
            |> Array.collect (ConstructorRender.renderConstructorAndOverloads genCache >> funApply classPath)
    }
    |> Utils.TypeDefn.withTyparsIfNotEmpty (
            render.TypeParameters
            |> Array.map (TypeParameterRender.toTyparDecl genCache >> funApply classPath)
            |> Array.toList
        )