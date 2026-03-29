module Xantham.SimpleGenerator.Generator.TypeLiteralRender

open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getTypeLiteralPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeLiteral>) = fun typeLiteralParentPath ->
    genCache.pathResolver.Prerenderer typeLiteralParentPath (TypeLiteral.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyTypeLiteral>) =
    let typeLiteralKey = TypeLiteral.toMasterKey key
    let prepdMembers =
        TypeLiteral.members key
        |> PatternContext.Array.cmap (MemberRender.prerender genCache)
        |> PatternContext.value
    
    fun typeLiteralParentPath ->
        let path = genCache.pathResolver.Prerenderer typeLiteralParentPath typeLiteralKey
        { Members = prepdMembers |> Array.mapApply path }

let collectMembersAsAbstracts (genCache: GeneratorContext) (render: TypeLiteralRender) = fun typeLiteralPath ->
    render.Members
    |> Array.map (MemberRender.renderAbstract genCache >> funApply typeLiteralPath)

let collectMembersAndOverloads (genCache: GeneratorContext) (render: TypeLiteralRender) = fun typeLiteralPath ->
    render.Members
    |> Array.collect (MemberRender.renderMemberAndOverloads genCache >> funApply typeLiteralPath)

let collectMembers (genCache: GeneratorContext) (render: TypeLiteralRender) = fun typeLiteralPath ->
    render.Members
    |> Array.map (MemberRender.renderMember genCache >> funApply typeLiteralPath)