/// Tier 3 - Shape: `expand-union-arms`, the deferred half of D4 (§4.5). A member whose
/// parameter is an erased union gains one overload per arm, *beside* the union member rather
/// than instead of it, so a value already held at union type still passes.
///
/// Runs after `order-declarations`, which is the earliest point an export is an `FsExports`
/// container with the name its findings are reported under; before that the exports are a flat
/// `ExportMembers` list with no container name to key on.
///
/// Runs after `dedupe-overloads`, which is the opposite of the intuitive order and deliberate:
/// expanding first would have dedupe absorb synthesized arms and report `DO001` for them,
/// conflating "TypeScript declared overloads F# cannot separate" with "this pass invented a
/// clashing signature". Expanding second leaves the collision check here, against a settled
/// signature set.
///
/// Disabled by default. Arm overloads make `f(!^ x)` ambiguous (FS0041) at every existing call
/// site, so enabling it is a consumer's migration to opt into.
module Xantham.Generator.Shape.UnionArms

open Xantham.Generator
open Xantham.Generator.Shape.Spec

/// What a member's parameter list looks like to .NET overload resolution: optionality, rest-ness
/// and the abbreviation-expanded type of each parameter. Return types are absent because
/// resolution ignores them.
let private signatureKey (abbrevs: Map<string, FsTypeRef>) (parameters: FsParam list) =
    parameters
    |> List.map (fun p -> p.Optional, p.Rest, expandAbbreviations abbrevs Set.empty p.Type)

/// The sole parameter of a member that reads as an erased union, with its arms.
///
/// An optional union parameter is not a candidate: expanding it would have to choose between
/// dropping the optionality and wrapping each arm, and neither is the signature the consumer
/// asked for. A member with more than one union parameter is what `policy: "linear"` is for.
let private soleUnionParameter (abbrevs: Map<string, FsTypeRef>) (parameters: FsParam list) =
    let unions =
        parameters
        |> List.indexed
        |> List.choose (fun (index, p) ->
            if p.Optional then
                None
            else
                match expandAbbreviations abbrevs Set.empty p.Type with
                | FsErasedUnion arms -> Some(index, p, arms)
                | _ -> None)

    match unions with
    | [ single ] -> Some single
    | _ -> None

let expandUnionArms: Pass<ShapeModel> =
    {
        Name = "expand-union-arms"
        Run =
            fun ctx model ->
                async {
                    let config = ctx.Config.UnionArmOverloads

                    if not config.Enabled then
                        return Advanced model
                    else

                        let abbrevs = abbreviations model.Decls
                        let mutable findings = []

                        /// One export member's expansion: the members it becomes, in render order.
                        /// The union member always survives and always comes first.
                        let expandMember (owner: string) (taken: Set<string * _>) (owned: OwnedExportMember) =
                            let export = owned.Member

                            match export.Body with
                            | ExportFunction(parameters, returns) ->
                                match soleUnionParameter abbrevs parameters with
                                | None -> [ owned ]
                                | Some(index, parameter, arms) ->
                                    let site = $"{owner}.{export.Name}"

                                    let armKeys =
                                        arms
                                        |> List.map (fun arm ->
                                            parameters
                                            |> List.mapi (fun i p -> if i = index then { p with Type = arm } else p)
                                            |> signatureKey abbrevs)

                                    if arms.Length > config.MaxArms then
                                        findings <-
                                            findings
                                            @ [
                                                Finding.make
                                                    site
                                                    (ExpandUnionArms.ArmCountExceedsCap(
                                                        parameter.Name,
                                                        arms.Length,
                                                        config.MaxArms
                                                    ))
                                            ]

                                        [ owned ]
                                    elif (List.distinct armKeys).Length <> armKeys.Length then
                                        // A partial arm set would be an API whose shape depends on
                                        // which arms happened to survive, so the member declines whole.
                                        findings <-
                                            findings
                                            @ [
                                                Finding.make
                                                    site
                                                    (ExpandUnionArms.ArmsCollapseToOneSignature parameter.Name)
                                            ]

                                        [ owned ]
                                    elif armKeys |> List.exists (fun key -> Set.contains (export.Name, key) taken) then
                                        findings <-
                                            findings
                                            @ [
                                                Finding.make
                                                    site
                                                    (ExpandUnionArms.ArmOverloadCollides(parameter.Name, export.Name))
                                            ]

                                        [ owned ]
                                    else
                                        findings <-
                                            findings
                                            @ [
                                                Finding.make
                                                    site
                                                    (ExpandUnionArms.ArmOverloadsSynthesized(
                                                        parameter.Name,
                                                        arms.Length
                                                    ))
                                            ]

                                        let synthesized =
                                            arms
                                            |> List.map (fun arm ->
                                                let armParameters =
                                                    parameters
                                                    |> List.mapi (fun i p ->
                                                        if i = index then { p with Type = arm } else p)

                                                { owned with
                                                    Member =
                                                        { export with
                                                            Body = ExportFunction(armParameters, returns)
                                                        }
                                                })

                                        owned :: synthesized
                            | _ -> [ owned ]

                        let expandContainer (container: FsExportContainer) =
                            // Every signature the container already has, so a synthesized arm that
                            // clashes with a TypeScript-declared overload is refused rather than
                            // emitted. Built once, before expansion, and not added to: two members
                            // expanding into each other's space is the collision this catches.
                            let taken =
                                container.Members
                                |> List.choose (fun owned ->
                                    match owned.Member.Body with
                                    | ExportFunction(parameters, _)
                                    | ExportConstructor(parameters, _) ->
                                        Some(owned.Member.Name, signatureKey abbrevs parameters)
                                    | ExportValue _ -> None)
                                |> Set.ofList

                            { container with
                                Members = container.Members |> List.collect (expandMember container.Name taken)
                            }

                        let decls =
                            model.Decls
                            |> List.map (function
                                | FsExports container -> FsExports(expandContainer container)
                                | other -> other)

                        let model = { model with Decls = decls }

                        return
                            if List.isEmpty findings then
                                Advanced model
                            else
                                Degraded(model, findings)
                }
    }
