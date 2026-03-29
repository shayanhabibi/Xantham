module Xantham.Fable.Reading.Dispatcher

open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer
open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Xantham.Fable.Types.Signal

let inline setAstSignal (tag: XanthamTag) astValue =
    tag
    |> GuardedData.AstNodeBuilder.getOrSetWith (fun () -> Signal.pending<STsAstNodeBuilder>())
    |> Signal.fill astValue

let dispatch (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.Value with
    | Ignore -> ()
    | _ when tryGetOrRegisterStore ctx tag |> Option.isNone -> ()
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
