module Xantham.SimpleGenerator.Generator.InterfaceRender

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getInterfacePath (genCache: GeneratorContext) (key: PatternContextHolder<KeyInterface>) = fun interfaceParentPath ->
    genCache.pathResolver.Prerenderer interfaceParentPath (Interface.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyInterface>) =
    let name = Interface.name key |> PatternContext.value
    let heritageFns =
        Interface.heritage key
        |> PatternContext.Array.cmap (TypeReferenceRender.prerender genCache)
        |> PatternContext.value
    let memberFns =
        Interface.members key
        |> PatternContext.map (
            Array.except (
                Interface.heritage key
                |> PatternContext.Array.cchoose (TypeReference.toMasterKey >> MasterKey.members >> ValueOption.map PatternContext.value >> ValueOption.toOption)
                |> PatternContext.Array.collect id
                |> PatternContext.value
                )
            )
        |> PatternContext.Array.cmap (MemberRender.prerender genCache)
        |> PatternContext.value
    let typeParamFns =
        Interface.typeParameters key
        |> ValueOption.map (
            PatternContext.Array.cmap (TypeParameterRender.prerender genCache)
            >> PatternContext.value
            )
        |> ValueOption.defaultValue [||]
    let interfaceMasterKey = Interface.toMasterKey key
    let casedName = Name.Pascal.create name
    fun interfaceParentPath ->
        let path = genCache.pathResolver.Prerenderer interfaceParentPath interfaceMasterKey
        let inline resolve fns = Array.map (fun fn -> fn path) fns
        {
            InterfaceRender.Name = casedName
            Heritage = resolve heritageFns
            Members = resolve memberFns
            TypeParameters = resolve typeParamFns
            Extensions = [||] // todo - ??
        }

type private IntersectionFolder = {
    Interfaces: KeyInterface array
    TypeLiterals: KeyTypeLiteral array
    TypeReferences: KeyTypeReference array
    Others: MasterBuilder array
}

let prerenderFromIntersection (genCache: GeneratorContext) (key: PatternContextHolder<KeyIntersection>) =
    let inline addInterface acc (Value interfaceKey) = { acc with Interfaces = Array.appendOne interfaceKey acc.Interfaces }
    let inline addTypeLiteral acc (Value typeLiteralKey) = { acc with TypeLiterals = Array.appendOne typeLiteralKey acc.TypeLiterals }
    let inline addOther acc (MasterKey.MasterBuilder (Value otherKey)) = { acc with Others = Array.appendOne otherKey acc.Others }
    let inline addReference acc (Value typeReferenceKey) = { acc with TypeReferences = Array.appendOne typeReferenceKey acc.TypeReferences }
    let foldedTypes =
        key
        |> Intersection.types
        |> PatternContext.Array.cfold (fun state -> function
            | MasterKey.KeyType.Interface interfaceKey ->
                interfaceKey |> addInterface state
            | MasterKey.KeyType.TypeLiteral typeLiteralKey ->
                typeLiteralKey |> addTypeLiteral state
            | MasterKey.KeyType.TypeReference typeReferenceKey ->
                typeReferenceKey |> addReference state
            | other -> other |> addOther state
            ) { Interfaces = [||]; TypeLiterals = [||]; TypeReferences = [||]; Others = [||] }
    let heritageFns =
        foldedTypes
        |> PatternContext.map _.Interfaces
        |> PatternContext.Array.cbind Interface.toMasterKey
        |> PatternContext.Array.map (GeneratorContext.getTypeRef genCache)
        |> PatternContext.value
    let typeReferenceHeritageFns =
        foldedTypes
        |> PatternContext.map _.TypeReferences
        |> PatternContext.Array.cmap (TypeReferenceRender.prerender genCache)
        |> PatternContext.value
    let memberCollectionFns =
        foldedTypes
        |> PatternContext.map _.TypeLiterals
        |> PatternContext.Array.ccollect (TypeLiteral.members >> PatternContext.value)
        |> PatternContext.Array.cmap (MemberRender.prerender genCache)
    let intersectionMasterKey =
        key
        |> PatternContext.map (
            genCache.ctx.createIntersectionKey
            >> MasterBuilder.Intersection
            >> genCache.ctx.createMasterKey
            )
    fun intersectionParentPath ->
        let path = genCache.pathResolver.Prerenderer intersectionParentPath intersectionMasterKey
        let casedName =
            KeyPath.popQualifier path.Value
            |> snd
            |> ValueOption.defaultValue Prelude.Name.typarTransientKey
            |> genCache.ctx.Item
            |> Name.Pascal.create
        {
            InterfaceRender.Name = casedName
            Heritage =
                heritageFns
                |> Array.map (fun fn ->
                    { TypeReferenceRender.Type = fn path
                      TypeArguments = [||] }
                    )
                |> Array.append (
                    Array.mapApply path typeReferenceHeritageFns
                    )
            Members =
                memberCollectionFns.Value
                |> Array.mapApply path
            TypeParameters = [||]
            Extensions = [||]
        }


let renderInterfaceTypeDefn (genCache: GeneratorContext) (render: InterfaceRender) =
    let renderEmptyInterface _ = Ast.InterfaceEnd(Name.Case.valueOrModified render.Name) {}
    let typeParams interfacePath =
        render.TypeParameters
        |> Array.map (TypeParameterRender.toTyparDecl genCache >> funApply interfacePath)
        |> Array.toList
    let attributes = [
        Attributes.``interface``
        Attributes.allowNullLiteral
    ]
    match render with
    | { Heritage = [||]; Members = [||]; Extensions = [||] } ->
        fun interfacePath ->
            renderEmptyInterface interfacePath
            |> Utils.TypeDefn.withTyparsIfNotEmpty (typeParams interfacePath)
            |> Utils.TypeDefn.attributesIfNotEmpty attributes
    | { Heritage = heritage; Members = members; Extensions = extensions } ->
        fun interfacePath ->
            Ast.TypeDefn( Name.Case.valueOrModified render.Name ) {
                for heritageRef in heritage do
                    TypeReferenceRender.toWidget genCache heritageRef interfacePath
                    |> Ast.Inherit
                for memberRender in members do
                    MemberRender.renderAbstract genCache memberRender interfacePath
                for extension in extensions do
                    MemberRender.renderMember genCache extension interfacePath
            }
            |> Utils.TypeDefn.withTyparsIfNotEmpty (typeParams interfacePath)
            |> Utils.TypeDefn.attributesIfNotEmpty attributes
        