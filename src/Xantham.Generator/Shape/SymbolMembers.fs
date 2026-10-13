module Xantham.Generator.Shape.SymbolMembers

open Xantham.Generator
open Xantham.Generator.Shape.Spec

// ---------------------------------------------------------------------------------------------
// Well-known symbol members (W4). `shape-interfaces` writes `[Symbol.iterator]` as an inherited
// `seq` beside the member itself; this pass settles, bases first, which of the two each
// declaration keeps. A declaration whose base already iterates keeps the base's sequence alone,
// the one `seq` instantiation F# admits per interface (FS3360).
// ---------------------------------------------------------------------------------------------

/// `reference` with each bound type variable replaced.
let rec private substitute (bindings: Map<string, FsTypeRef>) (reference: FsTypeRef) : FsTypeRef =
    let go = substitute bindings

    match reference with
    | FsTypeVar name -> Map.tryFind name bindings |> Option.defaultValue reference
    | FsOption inner -> FsOption(go inner)
    | FsArray inner -> FsArray(go inner)
    | FsTuple items -> FsTuple(List.map go items)
    | FsErasedUnion items -> FsErasedUnion(List.map go items)
    | FsDelegate(arguments, returns) -> FsDelegate(List.map go arguments, go returns)
    | FsFunc(argument, returns) -> FsFunc(go argument, go returns)
    | FsApp(name, arguments) -> FsApp(name, List.map go arguments)
    | FsBranded(primitive, measure) -> FsBranded(go primitive, measure)
    | FsBool
    | FsString
    | FsFloat
    | FsBigInt
    | FsUnit
    | FsObj
    | FsNamed _ -> reference

let private isIterator (m: FsMember) =
    match m with
    | FsSymbolKeyed("iterator", _) -> true
    | _ -> false

/// The symbols a declaration's members are keyed by, in member order.
let private symbolsOf (members: FsMember list) =
    members
    |> List.choose (function
        | FsSymbolKeyed(symbol, inner) -> Some(symbol, inner)
        | _ -> None)
    |> List.distinctBy fst

/// Settles every interface's `seq` inherit against its bases, and reports each well-known
/// symbol member a declaration keeps. The findings raised while shaping a `[Symbol.iterator]`
/// are reported with the `seq` or the member they describe. An `[<AbstractClass>]` entrypoint
/// keeps neither, so a consumer subclass gains no abstract slot to implement.
let settleSymbolMembers: Pass<ShapeModel> =
    {
        Name = "settle-symbol-members"
        Run =
            fun _ model ->
                async {
                    let interfaces =
                        model.Decls
                        |> List.choose (function
                            | FsInterface decl -> Some(decl.Name, decl)
                            | _ -> None)
                        |> Map.ofList

                    // Per declaration: whether it keeps its own `seq`, and every element it
                    // iterates once settled, inherited ones included.
                    let mutable settled: Map<string, bool * FsTypeRef list> = Map.empty

                    let rec settle (visiting: Set<string>) (decl: FsInterfaceDecl) : bool * FsTypeRef list =
                        match Map.tryFind decl.Name settled with
                        | Some result -> result
                        | None when decl.Entrypoint.IsSome -> false, []
                        | None ->
                            let visiting = Set.add decl.Name visiting

                            let inherited =
                                decl.Inherits
                                |> List.collect (fun reference ->
                                    let name, arguments =
                                        match reference with
                                        | FsNamed name -> Some name, []
                                        | FsApp(name, arguments) -> Some name, arguments
                                        | _ -> None, []

                                    match name |> Option.bind (fun name -> Map.tryFind name interfaces) with
                                    | Some baseDecl when not (Set.contains baseDecl.Name visiting) ->
                                        let bindings =
                                            List.zip
                                                (baseDecl.TypeParameters
                                                 |> List.truncate arguments.Length
                                                 |> List.map _.Name)
                                                (arguments |> List.truncate baseDecl.TypeParameters.Length)
                                            |> Map.ofList

                                        settle visiting baseDecl |> snd |> List.map (substitute bindings)
                                    | _ -> [])
                                |> List.distinct

                            let result =
                                match decl.Inherits |> List.tryPick sequenceElement with
                                | Some element when inherited.IsEmpty -> true, [ element ]
                                | Some _
                                | None -> false, inherited

                            settled <- Map.add decl.Name result settled
                            result

                    let mutable findings = []
                    let emit finding = findings <- findings @ [ finding ]

                    let decls =
                        model.Decls
                        |> List.map (function
                            | FsInterface decl when decl.Entrypoint.IsSome ->
                                for symbol, _ in symbolsOf decl.Members do
                                    emit (Finding.make $"{decl.Name}.__@{symbol}" Members.SymbolKeyedMemberDropped)

                                FsInterface
                                    { decl with
                                        Inherits = decl.Inherits |> List.filter (sequenceElement >> Option.isNone)
                                        Members =
                                            decl.Members
                                            |> List.filter (function
                                                | FsSymbolKeyed _ -> false
                                                | _ -> true)
                                    }
                            | FsInterface decl ->
                                let keeps, elements = settle Set.empty decl
                                let own = decl.Inherits |> List.tryPick sequenceElement

                                let inherits =
                                    if keeps then
                                        decl.Inherits
                                    else
                                        decl.Inherits |> List.filter (sequenceElement >> Option.isNone)

                                // The member stays where no `seq` covers it: no own element, or an
                                // own element that differs from an inherited one.
                                let covered =
                                    match own with
                                    | Some element -> keeps || elements |> List.forall ((=) element)
                                    | None -> false

                                let members =
                                    if covered then
                                        decl.Members |> List.filter (isIterator >> not)
                                    else
                                        decl.Members

                                match own with
                                | Some element when keeps ->
                                    emit (
                                        Finding.make
                                            $"{decl.Name}.[Symbol.iterator]"
                                            (Members.SymbolIteratorAsSeq(typeSpelling element))
                                    )
                                // A narrowed element whose iterator member `drop-inherited` removed
                                // reads at the base's element alone.
                                | Some _ when not covered && not (members |> List.exists isIterator) ->
                                    emit (Finding.make $"{decl.Name}.__@iterator" Members.SymbolKeyedMemberDropped)
                                | _ -> ()

                                let elementFindings, memberFindings =
                                    Map.tryFind decl.Name model.IteratorFindings |> Option.defaultValue ([], [])

                                if keeps then
                                    for finding in elementFindings do
                                        emit finding

                                if members |> List.exists isIterator then
                                    for finding in memberFindings do
                                        emit finding

                                for symbol, inner in symbolsOf members do
                                    let name =
                                        match inner with
                                        | FsProperty p -> p.Name
                                        | FsMethod m -> m.Name
                                        | _ -> symbolMemberName symbol

                                    emit (
                                        Finding.make
                                            $"{decl.Name}.[Symbol.{symbol}]"
                                            (Members.SymbolKeyedMemberAsEmit(symbol, name))
                                    )

                                FsInterface
                                    { decl with
                                        Inherits = inherits
                                        Members = members
                                    }
                            | other -> other)

                    let model =
                        { model with
                            Decls = decls
                            IteratorFindings = Map.empty
                        }

                    return
                        if findings.IsEmpty then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
