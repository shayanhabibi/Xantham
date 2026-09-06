module Xantham.Generator.Shape.TaggedUnions

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Declarations for the unions the checker proves are discriminated (D4, §4.5(2)): an F# DU of
/// one payload per case, erased by Fable back to the object - `Circle(radius = 2.0)` becomes
/// `{ kind: "circle", radius: 2 }`. An arm that is not plain data is left to `shape-aliases`.
/// Arms sharing a tag value fold into one case carrying the members they agree on; a fold
/// leaving a single case leaves the union to `shape-aliases` too.
let detectTaggedUnions: Pass<ShapeModel> =
    {
        Name = "detect-tagged-unions"
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
                                let nullish, _ = splitNullish model facts

                                // A nullable tagged union would have to drop its `null` case to fit
                                // the DU, so it stays an abbreviation and keeps the `option`.
                                if not (List.isEmpty nullish) then
                                    None
                                else

                                    match taggedUnionShape model facts with
                                    | Untagged -> None
                                    | TagCollides(tag, value) ->
                                        findings <-
                                            findings
                                            @ [ Finding.make name (DetectTaggedUnions.TagValueShared(tag, value)) ]

                                        None
                                    | Discriminated(tag, tagged, folded) ->
                                        if not (tagged |> List.forall (fst >> isTaggedCaseData tag)) then
                                            findings <-
                                                findings
                                                @ [ Finding.make name (DetectTaggedUnions.ArmNotPlainData tag) ]

                                            None
                                        else

                                            let caseNames =
                                                tagged |> List.map (snd >> Naming.enumCaseOfString) |> uniqueCaseNames

                                            let cases =
                                                List.map2
                                                    (fun (arm, value) caseName ->
                                                        let fields =
                                                            taggedCaseFields tag arm
                                                            |> List.map (fun m ->
                                                                let reference, refFindings =
                                                                    typeRef
                                                                        ctx
                                                                        model
                                                                        None
                                                                        $"{name}.{caseName}.{m.Symbol.Name}"
                                                                        m.TypeId

                                                                findings <- findings @ refFindings

                                                                {
                                                                    Name = Naming.memberName m.Symbol.Name
                                                                    Type = optionalRef m.Optional reference
                                                                })

                                                        {
                                                            Name = caseName
                                                            CompiledName =
                                                                (if value = caseName then None else Some value)
                                                            Fields = fields
                                                        })
                                                    tagged
                                                    caseNames

                                            findings <-
                                                findings
                                                @ [
                                                    for value in folded ->
                                                        Finding.make
                                                            name
                                                            (DetectTaggedUnions.ArmsMergedOnSharedTag(tag, value))
                                                ]
                                                @ [ Finding.make name (DetectTaggedUnions.TaggedUnion tag) ]

                                            Some(
                                                FsTaggedUnion
                                                    {
                                                        Name = name
                                                        Docs = ""
                                                        Tags = []
                                                        Order =
                                                            Map.tryFind typeId model.DeclOrders
                                                            |> Option.defaultValue None
                                                        Tag = tag
                                                        Cases = cases
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
