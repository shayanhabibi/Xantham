module Xantham.Generator.Shape.ErasedObjectUnions

open Xantham.Generator
open Xantham.Generator.Shape.Spec

let private sameSignature model (left: ResolvedSignature) (right: ResolvedSignature) =
    let bindings =
        if left.TypeParameters.Length = right.TypeParameters.Length then
            List.zip left.TypeParameters right.TypeParameters |> Set.ofList
        else
            Set.empty

    let same = sameShapeUnder bindings model

    let constraintsAgree =
        left.TypeParameters.Length = right.TypeParameters.Length
        && List.forall2
            (fun leftId rightId ->
                match Map.tryFind leftId model.Types, Map.tryFind rightId model.Types with
                | Some leftParam, Some rightParam ->
                    match leftParam.Constraint, rightParam.Constraint with
                    | None, None -> true
                    | Some leftBound, Some rightBound -> same leftBound rightBound
                    | _ -> false
                | _ -> false)
            left.TypeParameters
            right.TypeParameters

    left.HasRest = right.HasRest
    && constraintsAgree
    && left.Parameters.Length = right.Parameters.Length
    && same left.ReturnTypeId right.ReturnTypeId
    && List.forall2
        (fun (a: ResolvedMember) (b: ResolvedMember) -> a.Optional = b.Optional && same a.TypeId b.TypeId)
        left.Parameters
        right.Parameters

/// Identity-preserving unions with one typed arm and runtime predicate per case.
let detectErasedObjectUnions: Pass<ShapeModel> =
    {
        Name = "detect-erased-object-unions"
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
                            | Some facts when not (Map.containsKey typeId model.AliasApplications) ->
                                match erasedObjectUnionShape model facts with
                                | None -> None
                                | Some arms ->
                                    let typeParameters, scope, parameterFindings = declTypeParams ctx model name facts
                                    findings <- findings @ parameterFindings
                                    let scoped = { model with TypeVars = scope }

                                    let names =
                                        arms
                                        |> List.map (fun (arm, test) ->
                                            match arm.SymbolName, test with
                                            | Some symbol, _ when
                                                not (isAnonymousArm arm) && not (isSyntheticName symbol)
                                                ->
                                                Naming.enumCaseOfString (string symbol)
                                            | _, IsDefined property -> Naming.enumCaseOfString property
                                            | _ -> "Case")

                                    let names = uniqueCaseNames ("Invoke" :: names) |> List.tail

                                    let cases =
                                        List.map2
                                            (fun (arm: TypeFacts, test) caseName ->
                                                let payload, payloadFindings =
                                                    typeRef
                                                        ctx
                                                        scoped
                                                        None
                                                        (name + "." + caseName)
                                                        arm.Response.TypeId

                                                findings <- findings @ payloadFindings

                                                {
                                                    Name = caseName
                                                    Payload = payload
                                                    Test = test
                                                })
                                            arms
                                            names

                                    let shared =
                                        (fst arms.Head).CallSignatures
                                        |> List.filter (fun signature ->
                                            arms
                                            |> List.forall (fun (arm, _) ->
                                                arm.CallSignatures |> List.exists (sameSignature model signature)))

                                    let invokes =
                                        shared
                                        |> List.map (fun signature ->
                                            let parameters, arguments, returns, signatureFindings =
                                                shapeSignature ctx scoped (Some name) (name + ".Invoke") signature

                                            findings <- findings @ signatureFindings

                                            {
                                                Docs = ""
                                                Tags = []
                                                TypeParameters = parameters
                                                Parameters = arguments
                                                Return = returns
                                            })

                                    let docs, tags =
                                        model.Harvest.Exports
                                        |> List.tryFind (fun export ->
                                            Map.tryFind export.Symbol.SymbolId model.ExportTypes
                                            |> Option.bind _.Declared
                                            |> Option.contains typeId)
                                        |> Option.map (fun export -> export.Docs, export.Tags)
                                        |> Option.defaultValue ("", [])

                                    findings <-
                                        findings
                                        @ [ Finding.make name (DetectErasedObjectUnions.TypedArms arms.Length) ]

                                    Some(
                                        FsErasedObjectUnion
                                            {
                                                Name = name
                                                Docs = docs
                                                Tags = tags
                                                Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                                TypeParameters = typeParameters
                                                Cases = cases
                                                Invokes = invokes
                                            }
                                    )
                            | _ -> None)

                    let model =
                        { model with
                            Decls = model.Decls @ decls
                        }

                    return
                        if findings.IsEmpty then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
