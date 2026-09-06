module Xantham.Generator.Shape.Inherited

open Xantham.Generator

// ---------------------------------------------------------------------------------------------
// Inherited members: a member a base already declares at the same signature is written once, on
// the base. A member redeclared at a different signature stays on the derived interface.
// ---------------------------------------------------------------------------------------------

/// A member with its documentation cleared, so two declarations compare on signature alone.
let private signature (m: FsMember) : FsMember =
    match m with
    | FsProperty p -> FsProperty { p with Docs = ""; Tags = [] }
    | FsMethod m -> FsMethod { m with Docs = ""; Tags = [] }
    | FsIndexer _ -> m
    | FsConstructor c -> FsConstructor { c with Docs = ""; Tags = [] }
    | FsInvoke c -> FsInvoke { c with Docs = ""; Tags = [] }

/// The declared name a base reference resolves to, where the base is one this run declares.
let private baseName (reference: FsTypeRef) =
    match reference with
    | FsNamed name
    | FsApp(name, _) -> Some name
    | _ -> None

/// Every member signature reachable through a declaration's bases, transitively.
let private inheritedSignatures (interfaces: Map<string, FsInterfaceDecl>) (decl: FsInterfaceDecl) =
    let rec collect (visited: Set<string>) (acc: Set<FsMember>) (bases: FsTypeRef list) =
        bases
        |> List.fold
            (fun (visited, acc) reference ->
                match baseName reference with
                | Some name when not (Set.contains name visited) ->
                    match Map.tryFind name interfaces with
                    | Some baseDecl ->
                        let acc = baseDecl.Members |> List.fold (fun acc m -> Set.add (signature m) acc) acc
                        collect (Set.add name visited) acc baseDecl.Inherits
                    | None -> Set.add name visited, acc
                | _ -> visited, acc)
            (visited, acc)

    collect (Set.singleton decl.Name) Set.empty decl.Inherits |> snd

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
                                let inherited = inheritedSignatures interfaces decl

                                FsInterface
                                    { decl with
                                        Members =
                                            decl.Members
                                            |> List.filter (fun m -> not (Set.contains (signature m) inherited))
                                    }
                            | other -> other)

                    return Advanced { model with Decls = decls }
                }
    }
