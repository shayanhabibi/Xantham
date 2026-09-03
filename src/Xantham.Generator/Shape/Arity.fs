module Xantham.Generator.Shape.Arity

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

// ---------------------------------------------------------------------------------------------
// Arity repair: the ways a shaped declaration can still be un-writable F#.
// ---------------------------------------------------------------------------------------------

/// Every type reference inside a reference, rebuilt through `f`, applied outside-in so a
/// widening stops the descent.
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
                Entrypoint =
                    d.Entrypoint
                    |> Option.map (fun entrypoint ->
                        { entrypoint with
                            Parameters = entrypoint.Parameters |> List.map parameter
                        })
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
    let unused (decl: FsAbbrevDecl) =
        let used = typeVarsOf decl.Target

        not decl.TypeParameters.IsEmpty
        && decl.TypeParameters |> List.exists (fun p -> not (Set.contains p.Name used))

    // FS0037: F# rejects a parameter list naming one variable twice, at every arity, so the
    // phantom repairs nothing there and the declaration goes.
    let writableHead (decl: FsAbbrevDecl) =
        let names = decl.TypeParameters |> List.map _.Name
        (List.distinct names).Length = names.Length

    let phantomed, dropped =
        model.Decls
        |> List.choose (function
            | FsAbbrev decl when unused decl -> Some(decl.Name, writableHead decl)
            | _ -> None)
        |> List.partition snd
        |> fun (kept, lost) -> Set.ofList (List.map fst kept), Set.ofList (List.map fst lost)

    for name in phantomed do
        findings <- findings @ [ Finding.make name (RepairArity.AliasKeptAsPhantom name) ]

    for name in dropped do
        findings <- findings @ [ Finding.make name RepairArity.GenericAliasDropped ]

    // The declaration is rewritten as `shape-aliases`' erased phantom (§4.10), which admits the
    // type variable an abbreviation may not: the head keeps every parameter, the resolved target
    // becomes the private case's carrier, and an application of the name finds the arity it was
    // written with.
    let declared =
        model.Decls
        |> List.choose (function
            | FsAbbrev decl when Set.contains decl.Name phantomed ->
                Some(
                    FsPhantom
                        {
                            Name = decl.Name
                            Docs = decl.Docs
                            Tags = decl.Tags
                            Order = decl.Order
                            TypeParameters = decl.TypeParameters
                            Carrier = decl.Target
                        }
                )
            | decl ->
                match declName decl with
                | Some name when Set.contains name dropped -> None
                | _ -> Some decl)

    // Arity by name, read after the rewrite: a phantom answers for the alias it replaced.
    let arity =
        declared
        |> List.choose (function
            | FsInterface d -> Some(d.Name, d.TypeParameters.Length)
            | FsAbbrev d -> Some(d.Name, d.TypeParameters.Length)
            | FsPhantom d -> Some(d.Name, d.TypeParameters.Length)
            | _ -> None)
        |> Map.ofList

    let decls =
        declared
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

/// The ways a shaped model still fails to be F#, repaired in place. FS0035: a generic
/// abbreviation whose target leaves a parameter unused (`type Params<'P> = obj`) is rewritten as
/// an erased phantom, which keeps the name and the arity; where the head itself is unwritable the
/// declaration goes and its references widen. FS0033: a generic declaration named bare, and an
/// application at an arity the head does not declare, widen to `obj`. FS0252: a settable property
/// of type `unit` is demoted to read-only.
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
