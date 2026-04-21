module Xantham.Fable.Reading.Dispatcher

open Xantham.Fable
open Xantham.Fable.Types

let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.Value with
    // noop
    | Ignore _ -> ()
    // ====== Member-Level Dispatch ========
    // Already dispatched - noop
    | XanTagKind.MemberDeclaration _ when tryGetOrRegisterMemberStore ctx tag |> Option.isNone ->
        XanthamTag.debugLocationAndCommentAndForget "Dispatcher.dispatch | MemberDeclaration" "Already dispatched" tag
    // Dispatch
    | XanTagKind.MemberDeclaration memberDeclaration ->
        MemberDeclaration.dispatch ctx tag memberDeclaration
    // ====== Type-Level Dispatch ========
    // Already dispatched - noop
    | _ when tryGetOrRegisterStore ctx tag |> Option.isNone -> 
        XanthamTag.debugLocationAndCommentAndForget "Dispatcher.dispatch | _" "TypeStore has been registered already" tag
    | Patterns.XanTagKind.CanBeExported when tryGetOrRegisterExportedStore ctx tag |> Option.isNone ->
        XanthamTag.debugLocationAndCommentAndForget "Dispatcher.dispatch | CanBeExported" "ExportStore has been registered already" tag
    | XanTagKind.ModulesAndExports modulesAndExports ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | ModulesAndExports" tag
        ModulesAndExports.dispatch ctx tag modulesAndExports
    | XanTagKind.Type typeFlagPrimary ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | Type" tag
        TypeFlagPrimary.dispatch ctx tag typeFlagPrimary
    | XanTagKind.TypeDeclaration typeDeclaration ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | TypeDeclaration" tag
        TypeDeclaration.dispatch ctx tag typeDeclaration
    | XanTagKind.TypeNode typeNode ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | TypeNode" tag
        TypeNode.dispatch ctx tag typeNode
    | XanTagKind.JSDocTag jsDocTags ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | JSDocTag" tag
        JSDocTags.read ctx tag jsDocTags |> unbox
    | XanTagKind.LiteralTokenNode literalTokenNodes ->
        XanthamTag.debugLocationAndForget "Dispatcher.dispatch | LiteralTokenNode" tag
        LiteralTokenNode.dispatch ctx tag literalTokenNodes
