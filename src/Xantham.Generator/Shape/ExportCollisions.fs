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
    /// A shorter parameter list that is a prefix of a longer one whose tail is optional or rest:
    /// a call supplying the prefix alone selects either.
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

                    let exportContainers, others =
                        model.Decls
                        |> List.partitionWith (function
                            | FsExports container -> Choice1Of2 container
                            | other -> Choice2Of2 other)

                    let abbrevs =
                        model.Decls
                        |> List.choose (function
                            | FsAbbrev decl -> Some(decl.Name, decl.Target)
                            | _ -> None)
                        |> Map.ofList

                    /// The reference with abbreviations expanded, the way the compiler compares
                    /// it. A cycle stops at its first repeated name.
                    let rec normalize (visited: Set<string>) (reference: FsTypeRef) : FsTypeRef =
                        let recur = normalize visited

                        match reference with
                        | FsNamed name when Map.containsKey name abbrevs && not (Set.contains name visited) ->
                            normalize (Set.add name visited) abbrevs[name]
                        | FsOption inner -> FsOption(recur inner)
                        | FsArray element -> FsArray(recur element)
                        | FsTuple components -> FsTuple(List.map recur components)
                        | FsErasedUnion arms -> FsErasedUnion(List.map recur arms)
                        | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
                        | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
                        | FsApp(name, args) -> FsApp(name, List.map recur args)
                        | FsBranded(primitive, measure) -> FsBranded(recur primitive, measure)
                        | other -> other

                    /// A reference with the signature's own type variables renamed by position:
                    /// .NET overload resolution does not see a type parameter's name.
                    let rec renameTypeVars (rename: Map<string, string>) (reference: FsTypeRef) : FsTypeRef =
                        let recur = renameTypeVars rename

                        match reference with
                        | FsTypeVar name -> FsTypeVar(rename |> Map.tryFind name |> Option.defaultValue name)
                        | FsOption inner -> FsOption(recur inner)
                        | FsArray element -> FsArray(recur element)
                        | FsTuple components -> FsTuple(List.map recur components)
                        | FsErasedUnion arms -> FsErasedUnion(List.map recur arms)
                        | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
                        | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
                        | FsApp(name, args) -> FsApp(name, List.map recur args)
                        | FsBranded(primitive, measure) -> FsBranded(recur primitive, measure)
                        | other -> other

                    let compiled (typeParameters: FsTypeParam list) (reference: FsTypeRef) =
                        let rename = typeParameters |> List.mapi (fun i p -> p.Name, $"T{i}") |> Map.ofList
                        normalize Set.empty (renameTypeVars rename reference)

                    /// The compiled parameter signature: optionality, rest and type per position,
                    /// plus generic arity.
                    let parameterKey (typeParameters: FsTypeParam list) (parameters: FsParam list) =
                        typeParameters.Length,
                        parameters
                        |> List.map (fun p -> p.Optional, p.Rest, compiled typeParameters p.Type)

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

                    /// Whether `shorter`'s parameters are a prefix of `longer`'s and the rest of
                    /// `longer` may be omitted at the call.
                    let ambiguousPrefix
                        (shorter: FsTypeParam list * FsParam list)
                        (longer: FsTypeParam list * FsParam list)
                        =
                        let shorterTypes, shorterParams = shorter
                        let longerTypes, longerParams = longer

                        shorterParams.Length < longerParams.Length
                        && shorterTypes.Length = longerTypes.Length
                        && List.forall2
                            (fun (a: FsParam) (b: FsParam) ->
                                a.Rest = b.Rest && compiled shorterTypes a.Type = compiled longerTypes b.Type)
                            shorterParams
                            (List.take shorterParams.Length longerParams)
                        && (longerParams
                            |> List.skip shorterParams.Length
                            |> List.forall (fun p -> p.Optional || p.Rest))

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
                                elif
                                    ambiguousPrefix (a.TypeParameters, parametersA) (b.TypeParameters, parametersB)
                                    || ambiguousPrefix (b.TypeParameters, parametersB) (a.TypeParameters, parametersA)
                                then
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

                        let allocate (name: string) =
                            let candidate =
                                Seq.initInfinite (fun i -> $"{name}_Overload{i + 2}")
                                |> Seq.find (fun candidate -> not (Set.contains candidate taken))

                            taken <- Set.add candidate taken
                            candidate

                        // Candidates fold left in harvest order: each one is placed against the
                        // members already settled under its name.
                        let settle (settled: OwnedExportMember list) (candidate: OwnedExportMember) =
                            let name = candidate.Member.Name
                            let siblings = settled |> List.filter (fun s -> s.Member.Name = name)

                            let related =
                                siblings
                                |> List.map (fun sibling -> sibling, relate sibling candidate)
                                |> List.tryFind (fun (_, relation) -> relation <> Distinct)

                            match related with
                            | None -> settled @ [ candidate ]
                            | Some(sibling, Occurrence) ->
                                emit (
                                    Finding.make
                                        (symbol name)
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
                            | Some(_, Collapsed) ->
                                emit (
                                    Finding.make
                                        (symbol name)
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
                                        (symbol name)
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
                            Decls = others @ (exportContainers |> List.map resolveContainer)
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
