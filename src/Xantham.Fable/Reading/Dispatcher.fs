module Xantham.Fable.Reading.Dispatcher

open Xantham.Fable
open Xantham.Fable.Types

let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) =
    // Writes to log when building in debug mode
    tag.trace (fun logger traceId ->
        logger.logft
            "[%i{traceId}] Preparing dispatch for %s{identityKey} of: %s{tagKind}"
            traceId (tag.IdentityKey.ToString()) (tag.Value.ToString())
        )
    let debugMessage = sprintf "Dispatching tag of kind %s" >> tag.doDebugMessage
    match tag.Value with
    // noop
    | Ignore _ -> ()
    // ====== Member-Level Dispatch ========
    // Already dispatched - noop
    | XanTagKind.MemberDeclaration _ when tryGetOrRegisterMemberStore ctx tag |> Option.isNone ->
        tag.doDebugMessage "Dispatch early return: MemberStore has been registered already"
    // Dispatch
    | XanTagKind.MemberDeclaration memberDeclaration ->
        debugMessage "MemberDeclaration" 
        MemberDeclaration.dispatch ctx tag memberDeclaration
    // ====== Type-Level Dispatch ========
    // Already dispatched - noop
    | _ when tryGetOrRegisterStore ctx tag |> Option.isNone -> 
        tag.doDebugMessage "Dispatch early return: TypeStore has been registered already"
    | Patterns.XanTagKind.CanBeExported when tryGetOrRegisterExportedStore ctx tag |> Option.isNone ->
        tag.doDebugMessage "Dispatch early return: ExportStore has been registered already"
    | XanTagKind.ModulesAndExports modulesAndExports ->
        debugMessage "ModulesAndExports"
        ModulesAndExports.dispatch ctx tag modulesAndExports
    | XanTagKind.Type typeFlagPrimary ->
        debugMessage "TypeFlagPrimary"
        TypeFlagPrimary.dispatch ctx tag typeFlagPrimary
    | XanTagKind.TypeDeclaration typeDeclaration ->
        nameof XanTagKind.TypeDeclaration |> debugMessage
        TypeDeclaration.dispatch ctx tag typeDeclaration
    | XanTagKind.TypeNode typeNode ->
        nameof XanTagKind.TypeNode |> debugMessage
        TypeNode.dispatch ctx tag typeNode
    | XanTagKind.JSDocTag jsDocTags ->
        nameof XanTagKind.JSDocTag |> debugMessage
        JSDocTags.read ctx tag jsDocTags |> unbox
    | XanTagKind.LiteralTokenNode literalTokenNodes ->
        nameof XanTagKind.LiteralTokenNode |> debugMessage
        LiteralTokenNode.dispatch ctx tag literalTokenNodes
