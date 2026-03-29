module Xantham.SimpleGenerator.Generator.IndexRender

open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let getIndexPath (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndex>) = fun indexParentPath ->
    genCache.pathResolver.Prerenderer indexParentPath (Index.toMasterKey key)

let prerender (genCache: GeneratorContext) (key: PatternContextHolder<KeyIndex>) =
    let masterKey = Index.toMasterKey key
    match key with
    | Index.Members (members & Array.Length length) & Index.MembersToLiterals literals when length > 0 ->
        let memberFns =
            members
            |> PatternContext.Array.cmap (MemberRender.prerender genCache)
            |> PatternContext.value
        let memberLiterals =
            literals
            |> PatternContext.Array.cchoose (function
                | Literal.String value -> Some value 
                | _ -> None)
            |> PatternContext.value
        fun indexParentPath ->
            let path = genCache.pathResolver.Prerenderer indexParentPath masterKey
            {
                IndexResolved.Members = Array.mapApply path memberFns
                MemberLiterals = memberLiterals
            }
            |> IndexRender.Resolved
    | Index.Type (Value typeKey) ->
        fun indexParentPath ->
            let path = genCache.pathResolver.Prerenderer indexParentPath masterKey
            { IndexUnresolved.Object = GeneratorContext.getTypeRef genCache typeKey path }
            |> IndexRender.Unresolved

let tryToTypeRender (genCache: GeneratorContext) (value: IndexRender) = fun (indexPath: KeyPathKind) ->
    let name =
        indexPath.Value.Qualifiers
        |> Array.tryLast
        |> Option.map genCache.ctx.Item
        |> Option.defaultValue "Index"
        |> Name.Pascal.create
    match value with
    | IndexRender.Resolved indexResolved ->
        TypeRender.LiteralUnion {
            Name = name
            Cases =
                indexResolved.MemberLiterals
                |> PatternContext.prepare genCache.ctx
                |> PatternContext.Array.cmap (function Name.ToLiteral value -> EnumCaseRender.prerenderFromLiteral value)
                |> PatternContext.value
        }
        |> Ok
    | IndexRender.Unresolved indexUnresolved ->
        {
            TypeReferenceRender.Type = Prelude.TypeRefRenders.keyof
            TypeReferenceRender.TypeArguments = [| indexUnresolved.Object |]
        }
        |> Error