module Xantham.Generator.Shape.TaggedUnions

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Declarations for the unions the checker proves are discriminated (D4, §4.5(2)): an F# DU of
/// one payload per case, erased by Fable back to the object - `Circle(radius = 2.0)` becomes
/// `{ kind: "circle", radius: 2 }`. An arm that is not plain data is left to `shape-aliases`.
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
                                    | None -> None
                                    | Some(tag, tagged) ->
                                        // Fable writes the discriminant itself, so the tag property is
                                        // not a field; everything else on the arm is.
                                        let fieldsOf (arm: TypeFacts) =
                                            arm.Members
                                            |> List.filter (fun m ->
                                                m.Symbol.Name <> tag && not (isSymbolKeyed m.Symbol.Name))

                                        let isPlainData (arm: TypeFacts) =
                                            arm.CallSignatures.IsEmpty
                                            && arm.ConstructSignatures.IsEmpty
                                            && (fieldsOf arm
                                                |> List.forall (fun m ->
                                                    not (hasAny SymbolFlags.Method m.Symbol.Flags)))
                                            && (fieldsOf arm).Length <= TaggedCaseFieldBudget

                                        if not (tagged |> List.forall (fst >> isPlainData)) then
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
                                                            fieldsOf arm
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
                                                findings @ [ Finding.make name (DetectTaggedUnions.TaggedUnion tag) ]

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
