module Xantham.Generator.Shape.TaggedUnions

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Declarations for the unions the checker proves are discriminated (D4, §4.5(2)): an F# DU of
/// one payload per case, erased by Fable back to the object - `Circle(radius = 2.0)` becomes
/// `{ kind: "circle", radius: 2 }`. A named arm, and an arm more than its case's fields carry, is
/// a case of one `[<InlineInterface>]` field, its interface: `Ping of Ping` compiles as the `Ping`
/// value with the tag set. An arm with a call or construct signature leaves the union to
/// `shape-aliases`.
/// Each arm is a case of its own; arms sharing a tag value are told apart by each case's
/// `[<CaseTest>]`, and arms no test separates leave the union to `shape-aliases`. The DU binds the alias's type
/// parameters and those it reads from the scope it was written in (§4.9): a payload of type `T`
/// reads `'T`. An application of a generic alias reads its declaration.
let detectTaggedUnions: Pass<ShapeModel> =
    {
        Name = "detect-tagged-unions"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    // The alias's own comment, which the DU is the only declaration to carry.
                    let exportDocs =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            Map.tryFind export.Symbol.SymbolId model.ExportTypes
                            |> Option.bind _.Declared
                            |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                        |> Map.ofList

                    let decls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.filter (fun (typeId, _) -> not (Map.containsKey typeId model.AliasApplications))
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
                                    | Discriminated(tag, tagged, shared) ->
                                        if
                                            not (tagged |> List.forall (fun arm -> isTaggedCase model tag arm.Facts))
                                        then
                                            findings <-
                                                findings
                                                @ [ Finding.make name (DetectTaggedUnions.ArmNotPlainData tag) ]

                                            None
                                        else

                                            let typeParameters, scope, parameterFindings =
                                                declTypeParams ctx model name facts

                                            findings <- findings @ parameterFindings
                                            let scoped = { model with TypeVars = scope }

                                            let caseNames = taggedCaseNamesOf tagged

                                            let cases =
                                                List.map2
                                                    (fun (tagged: TaggedArm) caseName ->
                                                        let arm = tagged.Facts
                                                        let value = tagged.Value
                                                        let symbol = $"{name}.{caseName}"

                                                        // The arm's interface, where the case carries
                                                        // it whole. A plain-data arm read through no
                                                        // declared name keeps its fields.
                                                        let inlined =
                                                            if not (caseInlinesArm model tag arm) then
                                                                None
                                                            else
                                                                match
                                                                    typeRef ctx scoped None symbol arm.Response.TypeId
                                                                with
                                                                | (FsNamed armName | FsApp(armName, _)) as reference,
                                                                  refFindings -> Some(armName, reference, refFindings)
                                                                | _ when isTaggedCaseData tag arm -> None
                                                                | reference, refFindings ->
                                                                    Some(
                                                                        typeSpelling reference,
                                                                        reference,
                                                                        refFindings
                                                                    )

                                                        if Option.isSome inlined then
                                                            let armName, reference, refFindings = Option.get inlined

                                                            findings <-
                                                                findings
                                                                @ refFindings
                                                                @ [
                                                                    Finding.make
                                                                        symbol
                                                                        (DetectTaggedUnions.CaseInlinesInterface
                                                                            armName)
                                                                ]

                                                            {
                                                                Name = caseName
                                                                CompiledName =
                                                                    (if value = caseName then None else Some value)
                                                                CaseTest = tagged.Test
                                                                Inlines = true
                                                                Fields =
                                                                    [
                                                                        {
                                                                            Name = ""
                                                                            Docs = ""
                                                                            Tags = []
                                                                            Type = reference
                                                                            Literal = None
                                                                        }
                                                                    ]
                                                                Create = None
                                                            }
                                                        else

                                                            let fields =
                                                                taggedCaseFields tag arm
                                                                |> List.map (fun m ->
                                                                    let reference, refFindings =
                                                                        typeRef
                                                                            ctx
                                                                            scoped
                                                                            None
                                                                            $"{name}.{caseName}.{m.Symbol.Name}"
                                                                            m.TypeId

                                                                    findings <- findings @ refFindings

                                                                    // A boolean literal field is written too: it can be
                                                                    // what separates arms sharing a tag value.
                                                                    let booleanLiteral () =
                                                                        match Map.tryFind m.TypeId model.Types with
                                                                        | Some facts when
                                                                            not m.Optional
                                                                            && flag TypeFlags.BooleanLiteral facts
                                                                            ->
                                                                            literalOf facts
                                                                        | _ -> None

                                                                    let literal =
                                                                        literalMemberValue model m m.TypeId
                                                                        |> Option.orElseWith booleanLiteral
                                                                        |> Option.filter (fun _ ->
                                                                            reference = FsFloat
                                                                            || reference = FsString
                                                                            || reference = FsBool)

                                                                    {
                                                                        Name = Naming.memberName m.Symbol.Name
                                                                        Docs = m.Docs
                                                                        Tags = m.Tags
                                                                        Type = optionalRef m.Optional reference
                                                                        Literal = literal
                                                                    })

                                                            let optionalOrLiteral =
                                                                fields
                                                                |> List.exists (fun field ->
                                                                    field.Literal.IsSome
                                                                    || (match field.Type with
                                                                        | FsOption _ -> true
                                                                        | _ -> false))

                                                            let createName = $"Create{caseName}"

                                                            let create =
                                                                if not optionalOrLiteral then
                                                                    None
                                                                elif List.contains createName caseNames then
                                                                    findings <-
                                                                        findings
                                                                        @ [
                                                                            Finding.make
                                                                                symbol
                                                                                (DetectTaggedUnions
                                                                                    .CaseCreateNameTaken
                                                                                    createName)
                                                                        ]

                                                                    None
                                                                else
                                                                    findings <-
                                                                        findings
                                                                        @ [
                                                                            Finding.make
                                                                                symbol
                                                                                (DetectTaggedUnions
                                                                                    .CaseCreateSynthesized
                                                                                    createName)
                                                                        ]

                                                                    Some createName


                                                            {
                                                                Name = caseName
                                                                CompiledName =
                                                                    (if value = caseName then None else Some value)
                                                                CaseTest = tagged.Test
                                                                Inlines = false
                                                                Fields = fields
                                                                Create = create
                                                            })
                                                    tagged
                                                    caseNames

                                            findings <-
                                                findings
                                                @ [
                                                    for value in shared ->
                                                        Finding.make
                                                            name
                                                            (DetectTaggedUnions.ArmsSeparatedByCaseTest(tag, value))
                                                ]
                                                @ [ Finding.make name (DetectTaggedUnions.TaggedUnion tag) ]

                                            let docs, tags =
                                                Map.tryFind typeId exportDocs |> Option.defaultValue ("", [])

                                            Some(
                                                FsTaggedUnion
                                                    {
                                                        Name = name
                                                        Docs = docs
                                                        Tags = tags
                                                        Order =
                                                            Map.tryFind typeId model.DeclOrders
                                                            |> Option.defaultValue None
                                                        TypeParameters = typeParameters
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
