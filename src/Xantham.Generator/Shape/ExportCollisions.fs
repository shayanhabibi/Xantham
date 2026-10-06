module Xantham.Generator.Shape.ExportCollisions

open Xantham.Generator
open Xantham.Generator.Shape.Spec

/// How two candidates under one exported name relate once their compiled signatures are read.
type private Relation =
    /// Same source declaration, same signature ordinal, same binding: one export seen twice.
    | Occurrence
    /// Different declarations that map to one F# signature, return included.
    | Collapsed
    /// One compiled parameter signature, different returns.
    | ReturnOnly
    /// Signatures that one call selects alike (`CompiledSignature.ambiguousCall`).
    | AmbiguousCall
    /// Distinct compiled signatures the compiler keeps apart.
    | Distinct

/// Every candidate an owner's exports produced stays callable: repeated occurrences consolidate,
/// overloads with one compiled parameter signature and different returns read as one member
/// returning the union of those returns, and a candidate the compiler could not tell from an
/// earlier sibling takes a numbered name. No candidate is dropped.
let resolveExportCollisions: Pass<ShapeModel> =
    {
        Name = "resolve-export-collisions"
        Run =
            fun _ model ->
                async {
                    let mutable findings: Finding list = []
                    let emit finding = findings <- findings @ [ finding ]

                    let abbrevs = abbreviations model.Decls
                    let compiled = CompiledSignature.compiled abbrevs
                    let parameterKey = CompiledSignature.parameterKey abbrevs

                    let bodyParts (m: FsExportMember) =
                        match m.Body with
                        | ExportFunction(parameters, returns) -> Some("fn", parameters, returns)
                        | ExportConstructor(parameters, returns) -> Some("new", parameters, returns)
                        | ExportValue _ -> None

                    let returnOf (m: FsExportMember) =
                        match m.Body with
                        | ExportFunction(_, returns)
                        | ExportConstructor(_, returns)
                        | ExportValue returns -> returns

                    let withReturn (returns: FsTypeRef) (m: FsExportMember) =
                        { m with
                            Body =
                                match m.Body with
                                | ExportFunction(parameters, _) -> ExportFunction(parameters, returns)
                                | ExportConstructor(parameters, _) -> ExportConstructor(parameters, returns)
                                | ExportValue _ -> ExportValue returns
                        }

                    /// The distinct arms of a union over every candidate's return, flattened and
                    /// in candidate order.
                    let unionOf (typeParameters: FsTypeParam list) (returns: FsTypeRef list) =
                        returns
                        |> List.collect (function
                            | FsErasedUnion arms -> arms
                            | other -> [ other ])
                        |> List.distinctBy (compiled typeParameters)

                    let ambiguousCall = CompiledSignature.ambiguousCall abbrevs

                    let relate (earlier: OwnedExportMember) (later: OwnedExportMember) : Relation =
                        let a = earlier.Member
                        let b = later.Member

                        let sameProvenance =
                            earlier.SourceSymbolId = later.SourceSymbolId
                            && earlier.SignatureOrdinal = later.SignatureOrdinal
                            && earlier.ExportName = later.ExportName
                            && a.Binding = b.Binding
                            && a.Settable = b.Settable
                            && a.TypeParameters = b.TypeParameters
                            && a.Body = b.Body

                        if sameProvenance then
                            Occurrence
                        else
                            match bodyParts a, bodyParts b with
                            | Some(kindA, parametersA, returnsA), Some(kindB, parametersB, returnsB) when kindA = kindB ->
                                let keyA = parameterKey a.TypeParameters parametersA
                                let keyB = parameterKey b.TypeParameters parametersB

                                if keyA = keyB then
                                    if compiled a.TypeParameters returnsA = compiled b.TypeParameters returnsB then
                                        Collapsed
                                    else
                                        ReturnOnly
                                elif ambiguousCall (a.TypeParameters, parametersA) (b.TypeParameters, parametersB) then
                                    AmbiguousCall
                                else
                                    Distinct
                            | None, None -> Collapsed
                            | _ -> AmbiguousCall

                    let resolveContainer (container: FsExportContainer) =
                        let owner = container.Name
                        let symbol (name: string) = $"{owner}.{name}"

                        // Names a rename may not take: every original member, and the
                        // accessors a property compiles to.
                        let mutable taken =
                            container.Members
                            |> List.collect (fun owned ->
                                match owned.Member.Body with
                                | ExportValue _ ->
                                    [ owned.Member.Name; $"get_{owned.Member.Name}"; $"set_{owned.Member.Name}" ]
                                | _ -> [ owned.Member.Name ])
                            |> Set.ofList

                        // Each rename, keyed by the name it replaced.
                        let mutable renamedFrom: Map<string, string> = Map.empty

                        let allocate (name: string) =
                            let candidate =
                                Seq.initInfinite (fun i -> $"{name}_Overload{i + 2}")
                                |> Seq.find (fun candidate -> not (Set.contains candidate taken))

                            taken <- Set.add candidate taken
                            renamedFrom <- Map.add candidate name renamedFrom
                            candidate

                        // Candidates fold left in harvest order: each one is placed against the
                        // members already settled under its name.
                        let settle (settled: OwnedExportMember list) (candidate: OwnedExportMember) =
                            let name = candidate.Member.Name
                            let siblings = settled |> List.filter (fun s -> s.Member.Name = name)

                            // Members this pass renamed from `name`: each accepts a merge, and
                            // its new name sits outside `name`'s call forms.
                            let renamedSiblings =
                                settled
                                |> List.filter (fun s -> Map.tryFind s.Member.Name renamedFrom = Some name)

                            // Merging into a sibling takes precedence over renaming away from
                            // one.
                            let related =
                                let merges (_, relation) =
                                    relation <> Distinct && relation <> AmbiguousCall

                                let relations =
                                    siblings |> List.map (fun sibling -> sibling, relate sibling candidate)

                                relations
                                |> List.tryFind merges
                                |> Option.orElse (
                                    renamedSiblings
                                    |> List.map (fun sibling -> sibling, relate sibling candidate)
                                    |> List.tryFind merges
                                )
                                |> Option.orElse (
                                    relations |> List.tryFind (fun (_, relation) -> relation = AmbiguousCall)
                                )

                            match related with
                            | None -> settled @ [ candidate ]
                            | Some(sibling, Occurrence) ->
                                emit (
                                    Finding.make
                                        (symbol sibling.Member.Name)
                                        (DedupeOverloads.ExportOccurrenceConsolidated(owner, candidate.ExportName, 2))
                                )

                                settled
                                |> List.map (fun s ->
                                    if obj.ReferenceEquals(s, sibling) then
                                        { s with
                                            Member =
                                                { s.Member with
                                                    Docs =
                                                        if
                                                            s.Member.Docs = candidate.Member.Docs
                                                            || candidate.Member.Docs = ""
                                                        then
                                                            s.Member.Docs
                                                        elif s.Member.Docs = "" then
                                                            candidate.Member.Docs
                                                        else
                                                            s.Member.Docs + "\n" + candidate.Member.Docs
                                                    Tags =
                                                        s.Member.Tags
                                                        @ (candidate.Member.Tags |> List.except s.Member.Tags)
                                                }
                                        }
                                    else
                                        s)
                            | Some(sibling, Collapsed) ->
                                emit (
                                    Finding.make
                                        (symbol sibling.Member.Name)
                                        (DedupeOverloads.ExportDeclarationsConsolidated(owner, candidate.ExportName, 2))
                                )

                                settled
                            | Some(sibling, ReturnOnly) ->
                                let arms =
                                    unionOf
                                        sibling.Member.TypeParameters
                                        [ returnOf sibling.Member; returnOf candidate.Member ]

                                emit (
                                    Finding.make
                                        (symbol sibling.Member.Name)
                                        (DedupeOverloads.ExportReturnTypesUnioned(
                                            owner,
                                            candidate.ExportName,
                                            arms |> List.map typeSpelling |> String.concat ", "
                                        ))
                                )

                                settled
                                |> List.map (fun s ->
                                    if obj.ReferenceEquals(s, sibling) then
                                        { s with
                                            Member = withReturn (FsErasedUnion arms) s.Member
                                        }
                                    else
                                        s)
                            | Some(sibling, AmbiguousCall) ->
                                let renamed = allocate name

                                let reason =
                                    match sibling.Member.Body, candidate.Member.Body with
                                    | ExportValue _, _
                                    | _, ExportValue _ -> "member-kind conflict"
                                    | _ -> "ambiguous call form"

                                emit (
                                    Finding.make
                                        (symbol renamed)
                                        (DedupeOverloads.ExportMemberRenamed(
                                            owner,
                                            candidate.ExportName,
                                            renamed,
                                            reason
                                        ))
                                )

                                settled
                                @ [
                                    { candidate with
                                        Member = { candidate.Member with Name = renamed }
                                    }
                                ]
                            | Some(_, Distinct) -> settled @ [ candidate ]

                        FsExports
                            { container with
                                Members = container.Members |> List.fold settle []
                            }

                    let model =
                        { model with
                            Decls =
                                model.Decls
                                |> List.map (function
                                    | FsExports container -> resolveContainer container
                                    | other -> other)
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
