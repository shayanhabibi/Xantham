module Xantham.Generator.Shape.Arity

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

// ---------------------------------------------------------------------------------------------
// Arity repair: the two ways a shaped declaration can still be un-writable F#.
// ---------------------------------------------------------------------------------------------

/// Every type reference inside a reference, rebuilt through `f` (applied outside-in, so a
/// widening stops the descent). Written once because both repairs below rewrite references in
/// place, and a hand-rolled traversal per repair is how a new `FsTypeRef` case gets silently
/// skipped by one of them.
let rec private mapRef (f: FsTypeRef -> FsTypeRef) (reference: FsTypeRef) : FsTypeRef =
    let recur = mapRef f

    match f reference with
    | FsOption inner -> FsOption(recur inner)
    | FsArray element -> FsArray(recur element)
    | FsTuple elements -> FsTuple(elements |> List.map recur)
    | FsErasedUnion arms -> FsErasedUnion(arms |> List.map recur)
    | FsDelegate(arguments, returns) -> FsDelegate(arguments |> List.map recur, recur returns)
    | FsApp(name, arguments) -> FsApp(name, arguments |> List.map recur)
    | other -> other

/// The type variables a reference mentions.
/// Every type reference in a declaration, rebuilt through `f`.
let private mapDeclRefs (f: FsTypeRef -> FsTypeRef) (decl: FsDecl) : FsDecl =
    let reference = mapRef f
    let parameter (p: FsParam) = { p with Type = reference p.Type }

    let typeParam (p: FsTypeParam) =
        { p with
            Constraint = p.Constraint |> Option.map reference
        }

    let declMember =
        function
        | FsProperty p -> FsProperty { p with Type = reference p.Type }
        | FsIndexer i ->
            FsIndexer
                { i with
                    Key = reference i.Key
                    Value = reference i.Value
                }
        | FsMethod m ->
            FsMethod
                { m with
                    Parameters = m.Parameters |> List.map parameter
                    Return = reference m.Return
                }
        | FsConstructor c ->
            FsConstructor
                { c with
                    Parameters = c.Parameters |> List.map parameter
                    Return = reference c.Return
                }

    match decl with
    | FsInterface d ->
        FsInterface
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Inherits = d.Inherits |> List.map reference
                Members = d.Members |> List.map declMember
                CreateOverloads = d.CreateOverloads |> List.map (List.map parameter)
            }
    | FsAbbrev d ->
        FsAbbrev
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Target = reference d.Target
            }
    | FsPhantom d ->
        FsPhantom
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Carrier = reference d.Carrier
            }
    | FsMeasure d ->
        FsMeasure
            { d with
                Primitive = reference d.Primitive
            }
    | FsTaggedUnion d ->
        FsTaggedUnion
            { d with
                Cases =
                    d.Cases
                    |> List.map (fun case ->
                        { case with
                            Fields =
                                case.Fields
                                |> List.map (fun field ->
                                    { field with
                                        Type = reference field.Type
                                    })
                        })
            }
    | FsExports members ->
        FsExports(
            members
            |> List.map (fun m ->
                { m with
                    Body =
                        match m.Body with
                        | ExportFunction(parameters, returns) ->
                            ExportFunction(parameters |> List.map parameter, reference returns)
                        | ExportValue returns -> ExportValue(reference returns)
                        | ExportConstructor(parameters, returns) ->
                            ExportConstructor(parameters |> List.map parameter, reference returns)
                })
        )
    | FsStringEnum _
    | FsEnum _ -> decl

/// The name a declaration is written under, for the two repairs that work by name.
let private declName =
    function
    | FsInterface d -> Some d.Name
    | FsAbbrev d -> Some d.Name
    | FsPhantom d -> Some d.Name
    | FsMeasure d -> Some d.Name
    | FsTaggedUnion d -> Some d.Name
    | FsStringEnum d -> Some d.Name
    | FsEnum d -> Some d.Name
    | FsExports _ -> None

/// The repair itself, as a plain function: the model it produces plus what it had to widen.
let private repaired (model: ShapeModel) =
    let mutable findings = []

    // Only abbreviations can hit FS0035; an interface may leave a parameter unused.
    let dropped =
        model.Decls
        |> List.choose (function
            | FsAbbrev decl when not decl.TypeParameters.IsEmpty ->
                let used = typeVarsOf decl.Target

                if decl.TypeParameters |> List.forall (fun p -> Set.contains p.Name used) then
                    None
                else
                    Some decl.Name
            | _ -> None)
        |> Set.ofList

    for name in dropped do
        findings <- findings @ [ Finding.make name RepairArity.GenericAliasDropped ]

    let surviving =
        model.Decls
        |> List.filter (fun decl ->
            match declName decl with
            | Some name -> not (Set.contains name dropped)
            | None -> true)

    // Arity by name, over the survivors only - a dropped alias must not look applicable.
    let arity =
        surviving
        |> List.choose (function
            | FsInterface d -> Some(d.Name, d.TypeParameters.Length)
            | FsAbbrev d -> Some(d.Name, d.TypeParameters.Length)
            | FsPhantom d -> Some(d.Name, d.TypeParameters.Length)
            | _ -> None)
        |> Map.ofList

    let decls =
        surviving
        |> List.map (fun decl ->
            let owner = declName decl |> Option.defaultValue "Exports"

            let widen (kind: RepairArity) =
                findings <- findings @ [ Finding.make owner kind ]
                FsObj

            // FS0252: a settable property must have a settable type, and `unit` is not one.
            // The type is right - a `never`-typed brand or an `undefined` slot holds no
            // value - so only the setter goes, and the member still reads.
            let demoteUnitSetters (decl: FsDecl) =
                match decl with
                | FsInterface d ->
                    FsInterface
                        { d with
                            Members =
                                d.Members
                                |> List.map (function
                                    | FsProperty p when not p.ReadOnly && p.Type = FsUnit ->
                                        findings <-
                                            findings @ [ Finding.make owner (RepairArity.ReadWithoutWrite p.Name) ]

                                        FsProperty { p with ReadOnly = true }
                                    | other -> other)
                            // And no Create parameter either: there is no value to pass,
                            // and writing the key as `undefined` is not what the author
                            // declared. The property still reads on the result.
                            CreateOverloads = d.CreateOverloads |> List.map (List.filter (fun p -> p.Type <> FsUnit))
                        }
                | other -> other

            // FS0887: `inherit obj` is not an interface type. A base the widening above just
            // took the name off has nothing left to inherit, and the widening owns that loss
            // already, so the edge goes rather than the generated file failing to compile.
            let dropWidenedInherits (decl: FsDecl) =
                match decl with
                | FsInterface d ->
                    FsInterface
                        { d with
                            Inherits =
                                d.Inherits
                                |> List.filter (function
                                    | FsNamed _
                                    | FsApp _ -> true
                                    | _ -> false)
                        }
                | other -> other

            decl
            |> demoteUnitSetters
            |> mapDeclRefs (fun reference ->
                match reference with
                | FsNamed name when Set.contains name dropped -> widen (RepairArity.ReferenceToDroppedAlias name)
                | FsApp(name, _) when Set.contains name dropped -> widen (RepairArity.ReferenceToDroppedAlias name)
                | FsNamed name when Map.tryFind name arity |> Option.exists (fun n -> n > 0) ->
                    widen (RepairArity.GenericWithoutArguments name)
                | FsApp(name, arguments) when Map.tryFind name arity |> Option.exists (fun n -> n <> arguments.Length) ->
                    widen (RepairArity.ArityMismatch(name, arguments.Length, arity[name]))
                | other -> other)
            |> dropWidenedInherits)

    { model with Decls = decls }, findings

/// The last two ways a shaped model still fails to be F#, both repaired by widening - the type
/// exists, but this position cannot say which instantiation of it, and `obj` is what that means.
///
/// *A generic abbreviation whose target does not mention its parameters* is FS0035: F# has no
/// unused type variables in an abbreviation. It arises when the right side widened -
/// `type Params<'P> = obj` after `P`'s only use dropped to `obj`. Dropping the parameter instead
/// would silently change the alias's arity at every application, so the declaration goes and its
/// references widen.
///
/// *A generic declaration named bare* is FS0033: `PagesFunctionContext` needs three arguments,
/// and a member of some *other* declaration has no names to write for them. §4.9 already widens
/// an out-of-scope type *variable* to `obj` for the same reason; this is that rule one level up,
/// at the declaration head.
///
/// Runs after `order-declarations`, which is what folds the export members into an `FsExports`
/// declaration: references written there need the same repair as any other.
let repairArity: Pass<ShapeModel> =
    {
        Name = "repair-arity"
        Run =
            fun _ model ->
                async {
                    let model, findings = repaired model

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
