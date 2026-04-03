module Xantham.Fable.Reading.Dispatcher

open Xantham.Fable
open Xantham.Fable.Types

let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.Value with
    // noop
    | Ignore _ -> ()
    // ====== Member-Level Dispatch ========
    // Already dispatched - noop
    | XanTagKind.MemberDeclaration _ when tryGetOrRegisterMemberStore ctx tag |> Option.isNone -> ()
    // Dispatch
    | XanTagKind.MemberDeclaration memberDeclaration ->
        MemberDeclaration.dispatch ctx tag memberDeclaration
    // ====== Type-Level Dispatch ========
    // Already dispatched - noop
    | _ when tryGetOrRegisterStore ctx tag |> Option.isNone -> ()
    | Patterns.XanTagKind.CanBeExported when tryGetOrRegisterExportedStore ctx tag |> Option.isNone -> ()
    | XanTagKind.ModulesAndExports modulesAndExports ->
        ModulesAndExports.dispatch ctx tag modulesAndExports
    | XanTagKind.Type typeFlagPrimary ->
        TypeFlagPrimary.dispatch ctx tag typeFlagPrimary
    | XanTagKind.TypeDeclaration typeDeclaration ->
        TypeDeclaration.dispatch ctx tag typeDeclaration
    | XanTagKind.TypeNode typeNode ->
        TypeNode.dispatch ctx tag typeNode
    | XanTagKind.JSDocTag jsDocTags ->
        JSDocTags.read ctx tag jsDocTags |> unbox
    | XanTagKind.LiteralTokenNode literalTokenNodes ->
        LiteralTokenNode.dispatch ctx tag literalTokenNodes
