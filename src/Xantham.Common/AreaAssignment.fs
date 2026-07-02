namespace Xantham

// ─────────────────────────────────────────────────────────────────────────────
// AREA ASSIGNMENT — every IR type key mapped to its owning library (Phase 1,
// docs/PLAN.md). Pure and total over the ENCODED result:
//
//   authority order (a declaration belongs to the package that DECLARES it):
//     1. FQN source-path provenance  -> recipe package role   (re-exports never move ownership)
//     2. EntryExports seeding        -> the entry's lib       (fills FQN-less ambient/entry decls)
//     3. LibEsExports                -> LibEs                 (lib.es substitution territory)
//     4. reverse-reference fixpoint  -> synthetics inherit their referencers' owner;
//                                       referenced from >=2 libs -> SharedRoot
//
// The walk mirrors src/Xantham.Fable/Read.fs applyRemap (every TypeKey-bearing
// position) — if a field is added there, it must be added here.
// ─────────────────────────────────────────────────────────────────────────────

open Xantham.Schema

/// The computed owner of a type key.
type AreaOwner =
    /// Emitted inside this library's compilation unit.
    | OwnedBy of lib: string
    /// Referenced from two or more libraries: emitted in the dependency-order root unit.
    | SharedRoot
    /// lib.es/lib.dom ambient surface: LibEsSubstitution territory, never a unit member.
    | LibEs
    /// A dependency under erase-with-advisory / out-of-scope / no-surface policy:
    /// not emitted; references degrade with an advisory at emission.
    | ErasedDependency of package: string

type AreaAssignmentResult = {
    Owners: Map<TypeKey, AreaOwner>
    /// Keys unreachable by reference from ANY owned root — dead minted keys the
    /// encoder emitted but nothing uses (dedup/merge re-keyed their would-be
    /// referencers; the decoder's compress prunes them). Never emitted; totality
    /// is Owners + Dead = all keys, and owned nodes never reference dead keys
    /// (both invariants pinned by the integration suite). Encoder-side cleanup
    /// (don't emit unreferenced minted keys) is a backlog item, not load-bearing.
    Dead: TypeKey list
}

module AreaAssignment =

    // ── reference walk (mirror of Read.fs applyRemap coverage) ────────────────
    let private keysOfTypeRef (r: TsTypeReference) =
        r.Type :: r.TypeArguments @ Option.toList r.ResolvedType
    let private keysOfParam (p: TsParameter) = [ p.Type ]
    let private keysOfInlinedTypar ((key, tp): InlinedTsTypeParameter) =
        key :: Option.toList tp.Constraint @ Option.toList tp.Default
    let private keysOfMember (m: TsMember) =
        match m with
        | TsMember.Method m ->
            m.ToList() |> List.collect (fun v ->
                v.Type :: (v.Parameters |> List.collect keysOfParam) @ (v.TypeParameters |> List.collect keysOfInlinedTypar))
        | TsMember.Property p -> [ p.Type ]
        | TsMember.GetAccessor g -> [ g.Type ]
        | TsMember.SetAccessor s -> [ s.ArgumentType ]
        | TsMember.CallSignature c ->
            c.ToList() |> List.collect (fun v ->
                v.Type :: (v.Parameters |> List.collect keysOfParam) @ (v.TypeParameters |> List.collect keysOfInlinedTypar))
        | TsMember.IndexSignature i -> i.Type :: (i.Parameters |> List.collect keysOfParam)
        | TsMember.ConstructSignature c ->
            c.ToList() |> List.collect (fun v ->
                v.Type :: (v.Parameters |> List.collect keysOfParam) @ (v.TypeParameters |> List.collect keysOfInlinedTypar))

    let rec keysOfType (typ: TsType) : TypeKey list =
        match typ with
        | TsType.GlobalThis
        | TsType.Primitive _
        | TsType.Literal _ -> []
        | TsType.Union (TsTypeUnion members) -> members
        | TsType.Intersection (TsTypeIntersection members) -> members
        | TsType.Conditional c -> [ c.Check; c.Extends; c.True; c.False ]
        | TsType.Interface i ->
            (i.Members |> List.collect keysOfMember)
            @ (i.TypeParameters |> List.collect keysOfInlinedTypar)
            @ (i.Heritage.Extends |> List.collect keysOfTypeRef)
        | TsType.Class c ->
            (c.Members |> List.collect keysOfMember)
            @ (c.TypeParameters |> List.collect keysOfInlinedTypar)
            @ (c.Constructors |> List.collect (fun ctor -> ctor.Parameters |> List.collect keysOfParam))
            @ (c.Heritage.Implements |> Option.toList |> List.collect keysOfTypeRef)
            @ (c.Heritage.Extends |> List.collect keysOfTypeRef)
        | TsType.Enum e -> e.Members |> List.map _.Parent
        | TsType.EnumCase ec -> [ ec.Parent ]
        | TsType.IndexedAccess ia -> [ ia.Object; ia.Index ]
        | TsType.TypeReference r -> keysOfTypeRef r
        | TsType.Array t
        | TsType.ReadOnly t -> keysOfType t
        | TsType.TypeParameter tp -> Option.toList tp.Constraint @ Option.toList tp.Default
        | TsType.Tuple t ->
            t.Types |> List.collect (function
                | TsTupleElement.Variadic key -> [ key ]
                | TsTupleElement.Fixed e -> [ e.Type ]
                | TsTupleElement.FixedLabeled(_, e) -> [ e.Type ])
        | TsType.Index i -> [ i.Type ]
        | TsType.Predicate p -> [ p.Type ]
        | TsType.TypeLiteral tl -> tl.Members |> List.collect keysOfMember
        | TsType.TemplateLiteral t -> t.Types
        | TsType.Optional r -> keysOfTypeRef r
        | TsType.Substitution s -> [ s.Base; s.Constraint ]
        | TsType.TypeQuery q -> [ q.Type ]

    let rec keysOfExport (export: TsExportDeclaration) : TypeKey list =
        match export with
        | TsExportDeclaration.Variable v -> [ v.Type ]
        | TsExportDeclaration.Interface i ->
            (i.Members |> List.collect keysOfMember)
            @ (i.TypeParameters |> List.collect keysOfInlinedTypar)
            @ (i.Heritage.Extends |> List.collect keysOfTypeRef)
        | TsExportDeclaration.TypeAlias a ->
            a.Type :: (a.TypeParameters |> List.collect keysOfInlinedTypar)
        | TsExportDeclaration.Class c ->
            (c.Members |> List.collect keysOfMember)
            @ (c.TypeParameters |> List.collect keysOfInlinedTypar)
            @ (c.Constructors |> List.collect (fun ctor -> ctor.Parameters |> List.collect keysOfParam))
            @ (c.Heritage.Implements |> Option.toList |> List.collect keysOfTypeRef)
            @ (c.Heritage.Extends |> List.collect keysOfTypeRef)
        | TsExportDeclaration.Enum e -> e.Members |> List.map _.Parent
        | TsExportDeclaration.Module m -> m.Exports |> List.collect keysOfExport
        | TsExportDeclaration.Function f ->
            f.ToList() |> List.collect (fun v ->
                v.Type :: v.SignatureKey :: (v.Parameters |> List.collect keysOfParam) @ (v.TypeParameters |> List.collect keysOfInlinedTypar))

    // ── provenance: the declaring source path, when the node carries one ──────
    // DECLARATION nodes only. TypeQuery is deliberately excluded: its FQN names the
    // QUERIED TARGET, not the querying context — using it assigned reference nodes
    // to the referenced package (measured: 587 impossible Zod->Mcp edges). Reference-
    // sided nodes inherit their owner from their referencers via the fixpoint.
    let private fqnOfType (typ: TsType) : string list option =
        match typ with
        | TsType.Interface i -> Some i.FullyQualifiedName
        | TsType.Class c -> Some c.FullyQualifiedName
        | TsType.Enum e -> Some e.FullyQualifiedName
        | TsType.EnumCase ec -> Some ec.FullyQualifiedName
        | _ -> None

    let rec private fqnOfExport (export: TsExportDeclaration) : string list option =
        match export with
        | TsExportDeclaration.Variable v -> Some v.FullyQualifiedName
        | TsExportDeclaration.Interface i -> Some i.FullyQualifiedName
        | TsExportDeclaration.TypeAlias a -> Some a.FullyQualifiedName
        | TsExportDeclaration.Class c -> Some c.FullyQualifiedName
        | TsExportDeclaration.Enum e -> Some e.FullyQualifiedName
        | TsExportDeclaration.Module m -> Some m.FullyQualifiedName
        | TsExportDeclaration.Function f -> Some f.ValueOrHead.FullyQualifiedName

    /// The owner a recipe assigns to a PACKAGE, if it declares one.
    let private ownerOfPackage (recipe: Recipe) (package: string) : AreaOwner option =
        match recipe.RoleOf package with
        | Some(Choice1Of2 entry) -> entry.Lib |> Option.map OwnedBy
        | Some(Choice2Of2 dep) ->
            match dep.Policy with
            | BindTouchpoints -> None // touchpoint deps must be [[entry]]-declared with a lib
            | EraseWithAdvisory | OpaqueHandle | OutOfScope | NoSurface -> Some(ErasedDependency dep.Package)
        | None -> None

    /// Combine the owners of a key's referencers into the key's own owner.
    /// Documented rule: one lib -> that lib; two libs (or any SharedRoot referencer)
    /// -> SharedRoot; libs win over erased/LibEs (emission needs the key); erased-only
    /// -> erased (same package) or SharedRoot (mixed); LibEs-only -> LibEs (the lib.es
    /// closure is substitution territory with the rest of the lib.es surface).
    let private combineOwners (owners: AreaOwner list) : AreaOwner option =
        let owners = owners |> List.distinct
        let libs = owners |> List.choose (function OwnedBy l -> Some l | _ -> None)
        let erased = owners |> List.choose (function ErasedDependency p -> Some p | _ -> None)
        let hasLibEs = owners |> List.contains LibEs
        match libs, owners |> List.contains SharedRoot, erased with
        | [], false, [] -> if hasLibEs then Some LibEs else None
        | [ lib ], false, _ -> Some(OwnedBy lib)
        | [], false, [ pkg ] -> Some(ErasedDependency pkg)
        | [], false, _ -> Some SharedRoot
        | _ -> Some SharedRoot

    /// Compute the assignment. `packageOfPath` maps a source-path FQN part to its
    /// npm package (injected: Common stays free of path/regex concerns).
    let assign
        (packageOfPath: string -> string option)
        (recipe: Recipe)
        (result: EncodedResult)
        : AreaAssignmentResult =

        let packageOfFqn (fqn: string list) : string option =
            fqn |> List.tryPick packageOfPath

        let owners = System.Collections.Generic.Dictionary<TypeKey, AreaOwner>()

        // pass 1 — declaring-package provenance (never overwritten)
        for KeyValue(key, typ) in result.Types do
            fqnOfType typ
            |> Option.bind packageOfFqn
            |> Option.bind (ownerOfPackage recipe)
            |> Option.iter (fun owner -> owners[key] <- owner)
        for KeyValue(key, export) in result.ExportedDeclarations do
            if not (owners.ContainsKey key) then
                fqnOfExport export
                |> Option.bind packageOfFqn
                |> Option.bind (ownerOfPackage recipe)
                |> Option.iter (fun owner -> owners[key] <- owner)

        // pass 2 — entry seeding for FQN-less declarations (ambient roots, entry files)
        for KeyValue(entryFile, keys) in result.EntryExports do
            match packageOfPath entryFile |> Option.bind (ownerOfPackage recipe) with
            | Some owner ->
                for key in keys do
                    if not (owners.ContainsKey key) then owners[key] <- owner
            | None -> ()

        // pass 3 — lib.es surface
        for key in result.LibEsExports do
            if not (owners.ContainsKey key) then owners[key] <- LibEs

        // pass 4 — reverse-reference fixpoint for synthetics and transitively-reached
        // types. MONOTONE LATTICE, not first-assignment-wins: passes 1–3 are
        // authoritative (declarations belong to their package, frozen); fixpoint-
        // DERIVED owners are recomputed every sweep and only climb the lattice
        // (None -> LibEs/Erased/OwnedBy -> SharedRoot), so a key first seen from one
        // lib upgrades to SharedRoot when a second lib's evidence arrives — assignment
        // is order-independent (measured defect: 587 impossible Zod->Mcp edges from
        // first-wins ownership of shared instantiation literals).
        let referencers = System.Collections.Generic.Dictionary<TypeKey, ResizeArray<TypeKey>>()
        let addEdge fromKey toKey =
            match referencers.TryGetValue toKey with
            | true, list -> list.Add fromKey
            | _ -> referencers[toKey] <- ResizeArray [ fromKey ]
        for KeyValue(key, typ) in result.Types do
            for referenced in keysOfType typ do addEdge key referenced
        for KeyValue(key, export) in result.ExportedDeclarations do
            for referenced in keysOfExport export do addEdge key referenced

        let allKeys =
            Seq.append result.Types.Keys result.ExportedDeclarations.Keys
            |> Seq.distinct
            |> Seq.toArray

        let derived = System.Collections.Generic.Dictionary<TypeKey, AreaOwner>()
        let ownerOf key =
            match owners.TryGetValue key with
            | true, o -> Some o
            | _ ->
                match derived.TryGetValue key with
                | true, o -> Some o
                | _ -> None
        let mutable changed = true
        while changed do
            changed <- false
            for key in allKeys do
                if not (owners.ContainsKey key) then
                    match referencers.TryGetValue key with
                    | true, froms ->
                        let combined =
                            froms |> Seq.choose ownerOf |> Seq.toList |> combineOwners
                        match combined, derived.TryGetValue key with
                        | Some owner, (false, _) ->
                            derived[key] <- owner
                            changed <- true
                        | Some owner, (true, previous) when owner <> previous ->
                            derived[key] <- owner
                            changed <- true
                        | _ -> ()
                    | _ -> ()
        for KeyValue(key, owner) in derived do
            owners[key] <- owner

        {
            Owners = owners |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
            // By fixpoint construction: still-unowned keys have no owned referencer
            // chain from any root — unreachable, therefore dead.
            Dead = allKeys |> Array.filter (owners.ContainsKey >> not) |> Array.toList
        }
