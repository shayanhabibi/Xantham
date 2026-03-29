module Xantham.SimpleGenerator.Generator.IndexAccessRender

open Fabulous.AST
open Xantham
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getIndexAccessPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexAccess>) = fun indexAccessParentPath ->
    genCache.pathResolver.Prerenderer indexAccessParentPath (IndexAccess.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndexAccess>) =
    let indexAccessKey = IndexAccess.toMasterKey key
    let getPath parent = genCache.pathResolver.Prerenderer parent indexAccessKey
    let indexAccessObjectKey = IndexAccess.object key |> PatternContext.value
    let indexAccessIndexKey = IndexAccess.index key |> PatternContext.value
    match key with
    | IndexAccess.ToMemberKey memberKey ->
        let memberFn = MemberRender.prerender genCache memberKey
        let typeFn = GeneratorContext.getTypeRef genCache indexAccessObjectKey
        fun indexAccessParentPath ->
            let path = getPath indexAccessParentPath
            {
                IndexAccessResolved.Member = 
                    memberFn path
                    |> Array.singleton
                Type = typeFn path 
            }
            |> IndexAccessRender.Resolved
    | IndexAccess.Index (
        MasterKey.AssociatedLiterals (
            literals
            & Array.Length length
            )
        ) when length = 1 ->
        let literalKeys =
            literals
            |> PatternContext.Array.item 0
            |> PatternContext.cmap LiteralKey.toLiteral
            |> PatternContext.value
            |> function
                | TsLiteral.String value -> value
                | _ -> failwith "Invalid literal value for index type"
        let indexTypeFn = GeneratorContext.getTypeRef genCache indexAccessIndexKey
        fun indexAccessParentPath ->
            let path = getPath indexAccessParentPath
            {
                IndexType = indexTypeFn path
                IndexAccessIndexResolved.Index = literalKeys
            }
            |> IndexAccessRender.Index
    | IndexAccess.Index (
        MasterKey.AssociatedLiterals literals
        ) & IndexAccess.Object (
        MasterKey.Members members
        ) ->
        let literalKeys =
            literals
            |> PatternContext.Array.cbind LiteralKey.toLiteral
            |> PatternContext.Array.cchoose (function Literal.String value -> Some value | _ -> None)
        let membersFns =
            members
            |> PatternContext.Array.cfilter MemberKey.hasName
            |> PatternContext.Array.cfilter (function
                | MemberKey.HasName value when literalKeys.Value |> Array.contains value.Value -> true
                | _ -> false
                )
            |> PatternContext.Array.cmap (MemberRender.prerender genCache)
            |> PatternContext.value
        let indexedObject =
            indexAccessObjectKey
            |> GeneratorContext.getTypeRef genCache
            // TODO double check
        // let members =
        //     fun path ->
        //         let path = getPath path
        //         membersFns |> Array.mapApply path
        //         |> Array.map (MemberRender.toTypeRef genCache)
        //         |> Array.mapApply path
        //         |> fun memberRefs ->
        //             {
        //                 ErasedUnionRender.Types = memberRefs
        //             }
        //         |> ErasedUnionRender.toWidget genCache
        //         |> funApply path
        //         |> TypeRefRender.create false
        fun path ->
            let path = getPath path
            {
                IndexAccessResolved.Member =
                    membersFns
                    |> Array.mapApply path
                Type = indexedObject path
                    
            }
            |> IndexAccessRender.Resolved
    | IndexAccess.Object (
        MasterKey.Members _
        ) ->
        let objectFn = GeneratorContext.getTypeRef genCache indexAccessObjectKey
        fun indexAccessParentPath ->
            let path = getPath indexAccessParentPath
            { IndexAccessObjectResolved.Object = objectFn path }
            |> IndexAccessRender.Object
    | _ ->
        let objectFn = GeneratorContext.getTypeRef genCache indexAccessObjectKey
        let indexFn = GeneratorContext.getTypeRef genCache indexAccessIndexKey
        fun indexAccessParentPath ->
            let path = getPath indexAccessParentPath
            {
                IndexAccessRenderUnresolved.Object = objectFn path
                Index = indexFn path
            }
            |> IndexAccessRender.Unresolved

let toTypeRef (genCache: GeneratorContext) (value: IndexAccessRender) = fun indexAccessPath ->
    match value with
    | IndexAccessRender.Resolved { Member = [| memb |] } ->
        memb
        |> MemberRender.toTypeRef genCache
        |> funApply indexAccessPath
    | IndexAccessRender.Resolved { Member = [||]; Type = typeRef } 
    | IndexAccessRender.Object { Object = typeRef } ->
        {
            TypeReferenceRender.Type =
                Prelude.TypeRefRenders.proptypelock
            TypeArguments = [|
                typeRef
            |]
        }
        |> TypeReferenceRender.toWidget genCache <| indexAccessPath
        |> TypeRefRender.create false
    | IndexAccessRender.Resolved { Member = members } ->
        {
            ErasedUnionRender.Types =
                members
                |> Array.map (MemberRender.toTypeRef genCache >> funApply indexAccessPath)
        }
        |> ErasedUnionRender.toWidget genCache
        |> funApply indexAccessPath
        |> TypeRefRender.create false
    | IndexAccessRender.Index indexAccessIndexResolved ->
        {
            TypeReferenceRender.Type =
                Prelude.TypeRefRenders.keyof
            TypeArguments = [| indexAccessIndexResolved.IndexType |]
        }
        |> TypeReferenceRender.toWidget genCache <| indexAccessPath
        |> TypeRefRender.create false
    | IndexAccessRender.Unresolved indexAccessRenderUnresolved ->
        // It seems unlikely that in practice, we are able to reasonably resolve the
        // index in any way, so it is more appropriate to render a `proptypelock`.
        {
            TypeReferenceRender.Type =
                Prelude.TypeRefRenders.proptypelock
            TypeArguments = [| indexAccessRenderUnresolved.Object |]
        }
        |> TypeReferenceRender.toWidget genCache <| indexAccessPath
        |> TypeRefRender.create false
        