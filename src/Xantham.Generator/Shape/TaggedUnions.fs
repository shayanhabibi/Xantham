module Xantham.Generator.Shape.TaggedUnions

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Declarations for the unions the checker proves are discriminated (D4, §4.5(2)): an F# DU
/// carrying one payload per case, tagged so Fable erases it straight back to the underlying
/// object. Runs after `classify-literal-unions` because the two are disjoint - a union of
/// literals has no members to carry a discriminant - and before `shape-aliases`, which would
/// otherwise abbreviate the same name structurally.
///
/// Each case carries the arm's own properties as case fields, because that is what Fable's
/// erasure actually writes: `Circle(radius = 2.0)` becomes `{ kind: "circle", radius: 2 }`. An
/// arm that is not plain data has no such form - a method would have to arrive as a delegate
/// field, which reads back as a value rather than a callable member - so a union with one is
/// left to `shape-aliases`, where it stays an erased union over the arm types.
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
