module Xantham.Generator.Shape.LiteralUnions

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

let private dedupeUnionCases (cases: FsUnionCase list) =
    List.map2
        (fun (case: FsUnionCase) name -> { case with Name = name })
        cases
        (uniqueCaseNames (cases |> List.map _.Name))

let private dedupeEnumCases (cases: (string * int) list) =
    List.map2 (fun (_, value) name -> name, value) cases (uniqueCaseNames (cases |> List.map fst))

/// Declarations for named literal unions: StringEnum DUs with `CompiledName` per case, mixed
/// unions carrying `CompiledValue` cases (D12), all-integer unions as F# enums - including
/// reassembled TS enums, whose members name their cases (§4.7, §4.2).
let classifyLiteralUnions: Pass<ShapeModel> =
    {
        Name = "classify-literal-unions"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let decls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
                                let _, remaining = splitNullish model facts

                                let literals =
                                    remaining
                                    |> List.choose (fun id ->
                                        Map.tryFind id model.Types
                                        |> Option.bind (fun m -> literalOf m |> Option.map (fun l -> m, l)))

                                if
                                    literals.Length < remaining.Length
                                    || literals.Length < 2
                                    || isBooleanPair model remaining
                                then
                                    None
                                else

                                    let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                                    let intCase =
                                        function
                                        | LitNumber value when
                                            System.Double.IsInteger value && abs value < 2147483648.0
                                            ->
                                            Some(int value)
                                        | _ -> None

                                    let allInts =
                                        literals |> List.forall (fun (_, literal) -> (intCase literal).IsSome)

                                    if allInts then
                                        // Numeric enum territory: member symbols (a real TS enum) name
                                        // the cases; bare numeric literal unions derive them.
                                        let cases =
                                            literals
                                            |> List.map (fun (m, literal) ->
                                                let caseName =
                                                    match m.SymbolName with
                                                    | Some symbolName when not (isSyntheticName symbolName) ->
                                                        Naming.pascalSegment symbolName
                                                    | _ ->
                                                        match literal with
                                                        | LitNumber value -> Naming.enumCaseOfNumber value
                                                        | _ -> "Case"

                                                caseName, (intCase literal).Value)

                                        Some(
                                            FsEnum
                                                {
                                                    Name = name
                                                    Docs = ""
                                                    Tags = []
                                                    Order = order
                                                    Cases = dedupeEnumCases cases
                                                }
                                        )
                                    else
                                        let cases =
                                            literals
                                            |> List.map (fun (m, literal) ->
                                                let caseName =
                                                    match m.SymbolName with
                                                    | Some symbolName when not (isSyntheticName symbolName) ->
                                                        Naming.pascalSegment symbolName
                                                    | _ ->
                                                        match literal with
                                                        | LitString text -> Naming.enumCaseOfString text
                                                        | LitNumber value -> Naming.enumCaseOfNumber value
                                                        | LitBool true -> "True"
                                                        | LitBool false -> "False"

                                                match literal with
                                                | LitString text ->
                                                    {
                                                        Name = caseName
                                                        CompiledName = (if text = caseName then None else Some text)
                                                        CompiledValue = None
                                                    }
                                                | literal ->
                                                    findings <-
                                                        findings
                                                        @ [
                                                            Finding.make
                                                                name
                                                                ClassifyLiteralUnions.NonStringLiteralCase
                                                        ]

                                                    {
                                                        Name = caseName
                                                        CompiledName = None
                                                        CompiledValue = Some literal
                                                    })

                                        Some(
                                            FsStringEnum
                                                {
                                                    Name = name
                                                    Docs = ""
                                                    Tags = []
                                                    Order = order
                                                    Cases = dedupeUnionCases cases
                                                }
                                        )
                            | _ -> None)

                    let model =
                        { model with
                            Decls = model.Decls @ decls
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
