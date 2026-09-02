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
                if flag TypeFlags.TypeParameter facts then
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
                     && not (isTuple facts)
                     && not (isPureCallback facts))
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
        })
