module Xantham.SimpleGenerator.Generator.DocumentationRender

open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham
open Xantham.Decoder

module Documentation =
    type private Comments = {
        Summary: string list
        Remarks: string list
        Parameters: (string * string option) list
        Deprecated: string option
        DefaultValue: string list
        Example: string list
        TypeParameters: (string * string option) list
        Throws: string list 
        Returns: string list
    }
    let render (documentation: TsComment list) =
        let inline ifNotEmptyThenTag tag = function
            | [] -> []
            | content ->
                [
                    $"<{tag}>"
                    yield! content
                    $"</{tag}>"
                ]
        let comments =
            documentation
            |> List.rev
            |> List.fold (fun acc -> function
                | TsComment.Summary summary ->
                    { acc with Summary = (summary |> List.rev) @ acc.Summary }
                | TsComment.Returns s ->
                    { acc with Returns = s :: acc.Returns }
                | TsComment.Param(name, content) ->
                    { acc with Parameters = (Name.create name |> Name.valueOrModified, content) :: acc.Parameters }
                | TsComment.Deprecated stringOption ->
                    { acc with Deprecated = stringOption }
                | TsComment.Remarks s ->
                    { acc with Remarks = s :: acc.Remarks }
                | TsComment.DefaultValue s ->
                    { acc with DefaultValue = s :: acc.DefaultValue }
                | TsComment.Example s ->
                    { acc with Example = s :: acc.Example }
                | TsComment.TypeParam(typeName, content) ->
                    { acc with TypeParameters = (typeName, content) :: acc.TypeParameters }
                | TsComment.Throws s ->
                    { acc with Throws = s :: acc.Throws }
            ) { Summary = []
                Remarks = []
                Parameters = []
                Deprecated = None
                DefaultValue = []
                Example = []
                TypeParameters = []
                Throws = []
                Returns = [] }
            |> fun summary ->
                {
                    summary with
                        Summary = summary.Summary |> List.rev
                        Remarks = summary.Remarks |> List.rev
                        Parameters = summary.Parameters |> List.rev
                        DefaultValue = summary.DefaultValue |> List.rev
                        Example = summary.Example |> List.rev
                        TypeParameters = summary.TypeParameters |> List.rev
                        Throws = summary.Throws |> List.rev
                        Returns = summary.Returns |> List.rev
                }
        [
            yield!
                comments.Summary
                |> ifNotEmptyThenTag "summary"
            yield!
                comments.Remarks
                |> ifNotEmptyThenTag "remarks"
            yield!
                comments.Example
                |> ifNotEmptyThenTag "example"
            yield!
                comments.Parameters
                |> List.collect (function
                    | name, None ->
                        "<param name=\"" + name + "\"/>"
                        |> List.singleton
                    | name, Some content ->
                        [
                            "<param name=\"" + name + "\">"
                            yield! content.Split('\n')
                            "</param>"
                        ]
                    )
            match comments.Deprecated with
            | Some (Null | "") -> "<deprecated/>"
            | Some value -> "<deprecated>" + value + "</deprecated>"
            | None -> ()
        ]
        |> function
            | [] -> ValueNone
            | docs ->
                docs
                |> Ast.XmlDocs
                |> ValueSome
