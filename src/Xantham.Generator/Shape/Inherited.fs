module Xantham.Generator.Shape.Inherited

open Xantham.Generator

/// A reference with its bound type variables replaced.
let rec internal substitute (bindings: Map<string, FsTypeRef>) (reference: FsTypeRef) : FsTypeRef =
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
    | _ -> reference

let rec private substituteMember bindings member_ =
    // Comparison-only binder names are disjoint from emitted F# identifiers.
    let binder index = "\u0000method" + string index

    let scoped parameters =
        parameters
        |> List.mapi (fun index (parameter: FsTypeParam) -> parameter.Name, FsTypeVar(binder index))
        |> List.fold (fun scope (name, reference) -> Map.add name reference scope) bindings

    let parameter scope (parameter: FsParam) =
        { parameter with
            Type = substitute scope parameter.Type
        }

    let typeParameters scope parameters =
        parameters
        |> List.mapi (fun index (parameter: FsTypeParam) ->
            { parameter with
                Name = binder index
                Constraint = Option.map (substitute scope) parameter.Constraint
            })

    let invoke (member_: FsConstructorMember) =
        let scope = scoped member_.TypeParameters

        { member_ with
            TypeParameters = typeParameters scope member_.TypeParameters
            Parameters = List.map (parameter scope) member_.Parameters
            Return = substitute scope member_.Return
        }

    match member_ with
    | FsProperty member_ ->
        FsProperty
            { member_ with
                Type = substitute bindings member_.Type
            }
    | FsMethod member_ ->
        let scope = scoped member_.TypeParameters

        FsMethod
            { member_ with
                TypeParameters = typeParameters scope member_.TypeParameters
                Parameters = List.map (parameter scope) member_.Parameters
                Return = substitute scope member_.Return
            }
    | FsIndexer member_ ->
        FsIndexer
            { member_ with
                Key = substitute bindings member_.Key
                Value = substitute bindings member_.Value
            }
    | FsConstructor member_ -> FsConstructor(invoke member_)
    | FsInvoke member_ -> FsInvoke(invoke member_)
    | FsSymbolKeyed(symbol, inner) -> FsSymbolKeyed(symbol, substituteMember bindings inner)
    | FsLiteralKeyed(target, literals, inner) -> FsLiteralKeyed(target, literals, substituteMember bindings inner)

// ---------------------------------------------------------------------------------------------
// Inherited members: a member a base already declares at the same signature is written once, on
// the base. A member redeclared at a different signature stays on the derived interface.
// ---------------------------------------------------------------------------------------------

/// A member with its documentation cleared, so two declarations compare on signature alone.
let rec private signature (m: FsMember) : FsMember =
    match substituteMember Map.empty m with
    | FsProperty p -> FsProperty { p with Docs = ""; Tags = [] }
    | FsMethod m -> FsMethod { m with Docs = ""; Tags = [] }
    | FsIndexer _ -> m
    | FsConstructor c -> FsConstructor { c with Docs = ""; Tags = [] }
    | FsInvoke c -> FsInvoke { c with Docs = ""; Tags = [] }
    | FsSymbolKeyed(symbol, inner) -> FsSymbolKeyed(symbol, signature inner)
    | FsLiteralKeyed(target, literals, inner) -> FsLiteralKeyed(target, literals, signature inner)

/// The declared name a base reference resolves to, where the base is one this run declares.
let private baseName (reference: FsTypeRef) =
    match reference with
    | FsNamed name
    | FsApp(name, _) -> Some name
    | _ -> None

/// Base references and members instantiated in the derived declaration's parameter scope.
let private inheritedContracts (interfaces: Map<string, FsInterfaceDecl>) owner bases =
    let rec collect visited reference =
        match baseName reference with
        | Some name when not (Set.contains name visited) ->
            match Map.tryFind name interfaces with
            | Some baseDecl ->
                let arguments =
                    match reference with
                    | FsApp(_, arguments) -> arguments
                    | _ -> []

                if arguments.Length <> baseDecl.TypeParameters.Length then
                    []
                else
                    let bindings =
                        List.zip (baseDecl.TypeParameters |> List.map _.Name) arguments |> Map.ofList

                    let members = baseDecl.Members |> List.map (substituteMember bindings >> signature)

                    (reference, members)
                    :: (baseDecl.Inherits
                        |> List.map (substitute bindings)
                        |> List.collect (collect (Set.add name visited)))
            | None -> []
        | _ -> []

    bases |> List.collect (collect (Set.singleton owner))

/// Drops from every interface each member a base declares at the same signature. Bases are
/// read as shaped before this pass, so a member dropped from a base by this same pass is still
/// reachable further up the chain.
let dropInherited: Pass<ShapeModel> =
    {
        Name = "drop-inherited"
        Run =
            fun _ model ->
                async {
                    let interfaces =
                        model.Decls
                        |> List.choose (function
                            | FsInterface decl -> Some(decl.Name, decl)
                            | _ -> None)
                        |> Map.ofList

                    let decls =
                        model.Decls
                        |> List.map (function
                            | FsInterface decl when not decl.Inherits.IsEmpty ->
                                let inherited =
                                    inheritedContracts interfaces decl.Name decl.Inherits
                                    |> List.collect snd
                                    |> Set.ofList

                                let bases =
                                    if decl.Callable then
                                        decl.Inherits
                                        |> List.distinct
                                        |> List.filter (fun reference ->
                                            not (
                                                decl.Inherits
                                                |> List.exists (fun other ->
                                                    other <> reference
                                                    && (inheritedContracts interfaces decl.Name [ other ]
                                                        |> List.exists (fun (ancestor, _) -> ancestor = reference)))
                                            ))
                                    else
                                        decl.Inherits

                                FsInterface
                                    { decl with
                                        Inherits = bases
                                        Members =
                                            decl.Members
                                            |> List.filter (fun m -> not (Set.contains (signature m) inherited))
                                    }
                            | other -> other)

                    return Advanced { model with Decls = decls }
                }
    }
