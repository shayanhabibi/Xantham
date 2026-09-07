module Xantham.Generator.Shape.FreeTypeParams

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The type-parameter ids a declaration reads without binding, in first-use order (§4.9). A
/// signature's own parameters are bound inside it, and another named declaration binds its own,
/// so the walk stops there; a hoisted anonymous declaration is walked into.
let private freeTypeParams (model: ShapeModel) (root: int) : int list =
    let mutable found = []
    let mutable visited = Set.empty

    let rec go (bound: Set<int>) (typeId: int) =
        if not (Set.contains typeId visited) then
            visited <- Set.add typeId visited

            match Map.tryFind typeId model.Types with
            | None -> ()
            | Some facts ->
                if flag TypeFlags.Any facts || flag TypeFlags.Unknown facts then
                    ()
                elif flag TypeFlags.TypeParameter facts then
                    if
                        facts.Response.IsThisType <> ValueSome true
                        && not (Set.contains typeId bound)
                        && not (List.contains typeId found)
                    then
                        found <- found @ [ typeId ]
                elif
                    typeId <> root
                    && Map.containsKey typeId model.DeclNames
                    && (facts.SymbolName |> Option.exists (isSyntheticName >> not))
                then
                    // A declaration of its own: it binds what it declares. Only an
                    // instantiation carries arguments worth reading; the declared form's
                    // arguments are its own parameters.
                    if (ownArguments facts).IsEmpty then
                        for argument in facts.TypeArguments do
                            go bound argument
                else
                    for m in facts.Members do
                        go bound m.TypeId

                    for info in facts.IndexInfos do
                        go bound info.KeyTypeId
                        go bound info.ValueTypeId

                    for signature in facts.CallSignatures @ facts.ConstructSignatures do
                        let inner =
                            signature.TypeParameters |> List.fold (fun set id -> Set.add id set) bound

                        for p in signature.Parameters do
                            go inner p.TypeId

                        go inner signature.ReturnTypeId

                    for id in
                        facts.UnionMembers
                        @ facts.IntersectionMembers
                        @ facts.TypeArguments
                        @ facts.BaseTypes
                        @ facts.AliasTypeArguments do
                        go bound id

                    if flag TypeFlags.Index facts then
                        facts.Response.Target |> ValueOption.iter (go bound)

    match Map.tryFind root model.Types with
    | Some facts -> go (Set.ofList (declParamIds facts)) root
    | None -> ()

    found

/// Reuses an anonymous declaration under a complete substitution of its free parameters.
/// Caller constraints remain on the caller's parameters.
let private reuseAnonymousApplications (model: ShapeModel) =
    let anonymous instantiated (facts: TypeFacts) =
        let flags = facts.Response.ObjectFlags |> ValueOption.defaultValue ObjectFlags.None

        flags.HasFlag ObjectFlags.Anonymous
        && flags.HasFlag ObjectFlags.Instantiated = instantiated
        && not facts.Declarations.IsEmpty
        && not facts.Members.IsEmpty

    let declarations =
        model.DeclNames
        |> Map.toList
        |> List.choose (fun (id, _) ->
            let facts = model.Types[id]

            if anonymous false facts && not (freeParamsOf model id).IsEmpty then
                Some(facts.Declarations, id)
            else
                None)
        |> List.groupBy fst
        |> List.choose (fun (handles, entries) ->
            match entries with
            | [ _, id ] -> Some(handles, id)
            | _ -> None)
        |> Map.ofList

    let arguments declared instance =
        let parameters = freeParamsOf model declared
        let mutable substitution = Map.empty
        let mutable visited = Set.empty

        let pairwise compare left right =
            List.length left = List.length right && List.forall2 compare left right

        let rec same left right =
            if List.contains left parameters then
                match Map.tryFind right model.Types with
                | Some facts when flag TypeFlags.TypeParameter facts ->
                    match Map.tryFind left substitution with
                    | Some previous -> previous = right
                    | None ->
                        substitution <- Map.add left right substitution
                        true
                | _ -> false
            elif left = right || Set.contains (left, right) visited then
                true
            else
                visited <- Set.add (left, right) visited

                match Map.tryFind left model.Types, Map.tryFind right model.Types with
                | Some original, Some applied when original.Response.Flags = applied.Response.Flags ->
                    if
                        original.Response.Target.IsSome
                        && original.Response.Target = applied.Response.Target
                        && not original.TypeArguments.IsEmpty
                    then
                        pairwise same original.TypeArguments applied.TypeArguments
                    elif flag TypeFlags.Union original then
                        pairwise same original.UnionMembers applied.UnionMembers
                    elif flag TypeFlags.Intersection original then
                        pairwise same original.IntersectionMembers applied.IntersectionMembers
                    elif
                        flag TypeFlags.Object original
                        && not original.Declarations.IsEmpty
                        && original.Declarations = applied.Declarations
                    then
                        pairwise members original.Members applied.Members
                        && pairwise
                            (fun (left: ResolvedIndex) (right: ResolvedIndex) ->
                                left.IsReadonly = right.IsReadonly
                                && same left.KeyTypeId right.KeyTypeId
                                && same left.ValueTypeId right.ValueTypeId)
                            original.IndexInfos
                            applied.IndexInfos
                        && pairwise signatures original.CallSignatures applied.CallSignatures
                        && pairwise signatures original.ConstructSignatures applied.ConstructSignatures
                        && pairwise same original.BaseTypes applied.BaseTypes
                    else
                        false
                | _ -> false

        and members (left: ResolvedMember) (right: ResolvedMember) =
            left.Symbol.Name = right.Symbol.Name
            && left.Optional = right.Optional
            && left.ReadOnly = right.ReadOnly
            && same left.TypeId right.TypeId

        and signatures (left: ResolvedSignature) (right: ResolvedSignature) =
            left.TypeParameters = right.TypeParameters
            && left.HasRest = right.HasRest
            && left.IsAbstract = right.IsAbstract
            && pairwise members left.Parameters right.Parameters
            && same left.ReturnTypeId right.ReturnTypeId

        if
            same declared instance
            && parameters |> List.forall (fun id -> Map.containsKey id substitution)
        then
            Some(parameters |> List.map (fun id -> substitution[id]))
        else
            None

    model.DeclNames
    |> Map.fold
        (fun current id _ ->
            let facts = model.Types[id]

            match Map.tryFind facts.Declarations declarations with
            | Some declared when anonymous true facts ->
                match arguments declared id with
                | Some arguments ->
                    { current with
                        DeclNames = Map.add id model.DeclNames[declared] current.DeclNames
                        DeclParams = Map.add id arguments current.DeclParams
                        AliasApplications = Map.add id declared current.AliasApplications
                    }
                | None -> current
            | _ -> current)
        model

/// Declares each hoisted object type over the type parameters it reads from the scope it was
/// written in (§4.9, `DeclParams`). `each<T, U>(props: { items: T[]; render: (item: T) => U })`
/// declares `EachProps<'T, 'U>`, and the parameter position applies them back.
let bindFreeTypeParams: Pass<ShapeModel> =
    Pass.pure' "bind-free-type-params" (fun ctx model ->
        let bound =
            model.DeclNames
            |> Map.toList
            |> List.sortBy fst
            |> List.choose (fun (typeId, _) ->
                match Map.tryFind typeId model.Types with
                | Some facts when
                    (flag TypeFlags.Object facts
                     && GeneratorConfig.disposition ctx.Config facts.Origin = Ship
                     && (arrayElement model facts).IsNone
                     && not (isTuple facts))
                    || isFlattenable model facts
                    ->
                    let own = declParamIds facts

                    match
                        freeTypeParams model typeId
                        |> List.filter (fun id -> not (List.contains id own))
                    with
                    | [] -> None
                    | free -> Some(typeId, free)
                | _ -> None)
            |> Map.ofList

        // An entry `synthesize-anonymous` set itself wins: an erased alias application carries
        // the arguments recovered from its operands, which are what a reference has to apply -
        // not whatever type parameters its expansion happens to read free.
        { model with
            DeclParams =
                model.DeclParams
                |> Map.fold (fun kept typeId arguments -> Map.add typeId arguments kept) bound
        }
        |> reuseAnonymousApplications)
