module Xantham.Fable.Reading.JSDocTags

open System.Text.RegularExpressions
open Fable.Core
open TypeScript
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Tracer

let inline private getText mapping (tagComment: Option<U2<string, ResizeArray<Ts.JSDocComment>>>) =
    tagComment
    |> Option.bind (fun comment -> ts.getTextOfJSDocComment comment)
    |> Option.map mapping

// Credit to MangelMaxine for the original implementation of this function
let read (reader: TypeScriptReader) (xanTag: XanthamTag) (tag: JSDocTags) =
    let inline getArrText tag map =
        match getText tag map with
        | Some text -> [| text |]
        | _ -> [||]
    if xanTag.Documentation.IsNone then
        xanTag.Documentation <-
        match tag with
        | JSDocTags.ParameterTag jsDocParameterTag ->
            let name = NameHelpers.getName jsDocParameterTag.name
            let content = jsDocParameterTag.comment |> getText id
            [| TsComment.Param(name, content) |]
        | JSDocTags.ReturnTag jsDocReturnTag -> getArrText TsComment.Returns jsDocReturnTag.comment 
        | JSDocTags.DeprecatedTag jsDocDeprecatedTag -> 
            [| TsComment.Deprecated(getText id jsDocDeprecatedTag.comment) |]
        | JSDocTags.ThrowsTag jsDocThrowsTag -> getArrText TsComment.Throws jsDocThrowsTag.comment
        | JSDocTags.DocTag jsDocTag ->
            match NameHelpers.getName jsDocTag.tagName with
            | "example" -> getArrText TsComment.Example jsDocTag.comment
            | "remarks" -> getArrText TsComment.Remarks jsDocTag.comment 
            | "defaultValue" -> getArrText TsComment.DefaultValue jsDocTag.comment 
            | "typeParam" ->
                    match getText id jsDocTag.comment with
                    | Some text ->
                        let regex =
                            Regex(
                                "\s*(?<type>[^-\s]*)\s*-\s*(?<description>.*)",
                                RegexOptions.Singleline
                            )
                        let m = regex.Match text
                        if m.Success then
                            let content = 
                                if m.Groups["description"].Success then Some m.Groups["description"].Value else None
                            [| TsComment.TypeParam(m.Groups["type"].Value, content) |]
                        else 
                            $"Invalid typeParam tag format: {text}" |> reader.warnings.Add
                            [||]
                    | _ -> [||]
            | _ -> [||]
        | JSDocTags.UnknownTag _ -> [||]
        
// Credit to MangelMaxine for the original implementation of this function
let readSummary (reader: TypeScriptReader) (xanTag: XanthamTag) =
    if xanTag.SummaryDocumentation.IsNone then
        match xanTag.Guard.Value with
        | IdentityKey.Symbol sym | IdentityKey.AliasSymbol sym ->
            sym.getDocumentationComment(Some reader.checker) |> Some
        | _ -> None
        |> unbox
        |> ts.displayPartsToString
        |> String.splitLines
        |> Array.filter (System.String.IsNullOrWhiteSpace >> not)
        |> function
            | [||] -> ()
            | arr ->
                xanTag.SummaryDocumentation <- TsComment.Summary (arr |> Array.toList)

let readDocsForTag (ctx: TypeScriptReader) (xanTag: XanthamTag) =
    let inline getJSDocTagsForNode (node: Ts.Node) =
        ts.getJSDocTags node
        |> _.AsArray
        |> Array.collect (fun node ->
            let jsDocTag = JSDocTags.Create node
            let xanthamTag = ctx.CreateXanthamTag node |> fst |> _.Value
            read ctx xanthamTag jsDocTag
            xanthamTag.Documentation
            |> ValueOption.defaultValue [||]
            )
    readSummary ctx xanTag
    if xanTag.Documentation.IsSome then () else
    match xanTag.Value.UnderlyingValue with
    | Choice2Of2 node ->
        xanTag.Documentation <- getJSDocTagsForNode node
    | Choice1Of2 _ when xanTag.IdentityKey.IsAliasSymbol || xanTag.IdentityKey.IsSymbol ->
        let symb = match xanTag.IdentityKey with | IdentityKey.Symbol sym -> sym | IdentityKey.AliasSymbol sym -> sym | _ -> failwith "unreachable"
        symb.declarations
        |> Option.bind (_.AsArray >> Array.tryHead)
        |> Option.iter (getJSDocTagsForNode >> fun docs -> xanTag.Documentation <- docs)
    | _ -> ()




let resolveDocsForTag (ctx: TypeScriptReader) (xanTag: XanthamTag) =
    readDocsForTag ctx xanTag
    [
        if xanTag.SummaryDocumentation.IsSome then yield xanTag.SummaryDocumentation.Value
        if xanTag.Documentation.IsSome then yield! xanTag.Documentation.Value
    ]