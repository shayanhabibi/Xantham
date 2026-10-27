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
    | FsFunc(argument, returns) -> FsFunc(recur argument, recur returns)
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

    let rec declMember =
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
        | FsInvoke c ->
            FsInvoke
                { c with
                    Parameters = c.Parameters |> List.map parameter
                    Return = reference c.Return
                }
        | FsSymbolKeyed(symbol, inner) -> FsSymbolKeyed(symbol, declMember inner)
        | FsStringKeyed(key, inner) -> FsStringKeyed(key, declMember inner)
        | FsLiteralKeyed(target, literals, inner) -> FsLiteralKeyed(target, literals, declMember inner)

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
                CreateOverloads =
                    d.CreateOverloads
                    |> List.map (fun overload ->
                        { overload with
                            Parameters = List.map parameter overload.Parameters
                        })
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
    | FsDelegateType d ->
        FsDelegateType
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Parameters = d.Parameters |> List.map (fun p -> { p with Type = reference p.Type })
                Return = reference d.Return
            }
    | FsMeasure d ->
        FsMeasure
            { d with
                Primitive = reference d.Primitive
            }
    | FsErasedObjectUnion d ->
        FsErasedObjectUnion
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
                Cases =
                    d.Cases
                    |> List.map (fun case ->
                        { case with
                            Payload = reference case.Payload
                        })
                Invokes =
                    d.Invokes
                    |> List.map (fun invoke ->
                        { invoke with
                            TypeParameters = invoke.TypeParameters |> List.map typeParam
                            Parameters = invoke.Parameters |> List.map parameter
                            Return = reference invoke.Return
                        })
            }
    | FsTaggedUnion d ->
        FsTaggedUnion
            { d with
                TypeParameters = d.TypeParameters |> List.map typeParam
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
    | FsExports container ->
        FsExports
            { container with
                Members =
                    container.Members
                    |> List.map (fun owned ->
                        { owned with
                            Member =
                                { owned.Member with
                                    Body =
                                        match owned.Member.Body with
                                        | ExportFunction(parameters, returns) ->
                                            ExportFunction(parameters |> List.map parameter, reference returns)
                                        | ExportValue returns -> ExportValue(reference returns)
                                        | ExportConstructor(parameters, returns) ->
                                            ExportConstructor(parameters |> List.map parameter, reference returns)
                                }
                        })
            }
    | FsStringEnum _
    | FsEnum _ -> decl

/// The name a declaration is written under, for the two repairs that work by name.
let private declName =
    function
    | FsInterface d -> Some d.Name
    | FsAbbrev d -> Some d.Name
    | FsDelegateType d -> Some d.Name
    | FsPhantom d -> Some d.Name
    | FsMeasure d -> Some d.Name
    | FsTaggedUnion d -> Some d.Name
    | FsErasedObjectUnion d -> Some d.Name
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

    let headTypeParameters =
        function
        | FsInterface d -> Some d.TypeParameters
        | FsAbbrev d -> Some d.TypeParameters
        | FsDelegateType d -> Some d.TypeParameters
        | FsPhantom d -> Some d.TypeParameters
        | FsTaggedUnion d -> Some d.TypeParameters
        | FsErasedObjectUnion d -> Some d.TypeParameters
        | _ -> None

    let unwritableHead decl =
        headTypeParameters decl
        |> Option.map (fun parameters ->
            let names = parameters |> List.map _.Name
            (List.distinct names).Length <> names.Length)
        |> Option.defaultValue false

    let unwritable =
        model.Decls
        |> List.choose (fun decl ->
            match declName decl with
            | Some name when
                not (Set.contains name phantomed)
                && not (Set.contains name dropped)
                && unwritableHead decl
                ->
                Some name
            | _ -> None)
        |> Set.ofList

    for name in unwritable do
        findings <-
            findings
            @ [ Finding.make name (RepairArity.DuplicateTypeParameterUnwritable name) ]

    let dropped = Set.union dropped unwritable

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
            | FsDelegateType d -> Some(d.Name, d.TypeParameters.Length)
            | FsPhantom d -> Some(d.Name, d.TypeParameters.Length)
            | FsTaggedUnion d -> Some(d.Name, d.TypeParameters.Length)
            | FsErasedObjectUnion d -> Some(d.Name, d.TypeParameters.Length)
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
                            CreateOverloads =
                                d.CreateOverloads
                                |> List.map (fun overload ->
                                    { overload with
                                        Parameters = List.filter (fun p -> p.Type <> FsUnit) overload.Parameters
                                    })
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
                // A package free to declare `Record` takes the unqualified name; the support
                // package's declaration of the same name is still reachable, so the reference
                // is qualified rather than widened.
                | FsApp(name, arguments) when
                    Naming.SupportBindings.shadows name
                    && Map.tryFind name arity |> Option.exists (fun n -> n <> arguments.Length)
                    ->
                    FsApp(Naming.SupportBindings.qualify name, arguments)
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

/// Collapses erased unions containing a local nongeneric abbreviation for `obj`.
let internal normalizeObjUnions: Pass<ShapeModel> =
    {
        Name = "normalize-obj-unions"
        Run =
            fun _ model ->
                async {
                    let aliases =
                        model.Decls
                        |> List.choose (function
                            | FsAbbrev decl when decl.TypeParameters.IsEmpty -> Some(decl.Name, decl.Target)
                            | _ -> None)
                        |> Map.ofList

                    let rec admitsObj visited =
                        function
                        | FsObj -> true
                        | FsNamed name when not (Set.contains name visited) ->
                            Map.tryFind name aliases |> Option.exists (admitsObj (Set.add name visited))
                        | FsErasedUnion arms -> List.exists (admitsObj visited) arms
                        | _ -> false

                    let mutable findings = []

                    let decls =
                        model.Decls
                        |> List.map (fun decl ->
                            let owner = declName decl |> Option.defaultValue "Exports"

                            decl
                            |> mapDeclRefs (function
                                | FsErasedUnion arms when List.exists (admitsObj Set.empty) arms ->
                                    findings <- findings @ [ Finding.make owner TypeReference.UnionWithObjArm ]
                                    FsObj
                                | other -> other))

                    let model = { model with Decls = decls }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, List.distinct findings)
                }
    }

/// The type parameters each named declaration writes on its head.
let private heads (decls: FsDecl list) =
    decls
    |> List.choose (fun decl ->
        match decl with
        | FsInterface d -> Some(d.Name, d.TypeParameters)
        | FsAbbrev d -> Some(d.Name, d.TypeParameters)
        | FsPhantom d -> Some(d.Name, d.TypeParameters)
        | FsDelegateType d -> Some(d.Name, d.TypeParameters)
        | FsTaggedUnion d -> Some(d.Name, d.TypeParameters)
        | FsErasedObjectUnion d -> Some(d.Name, d.TypeParameters)
        | _ -> None)
    |> Map.ofList

/// Every application in a declaration, each with the type parameters in scope where it is
/// written: the declaration's own, and a generic member's on top of them.
let private applications (decl: FsDecl) : (Map<string, FsTypeRef option> * string * FsTypeRef list) list =
    let bind (scope: Map<string, FsTypeRef option>) (parameters: FsTypeParam list) =
        parameters
        |> List.fold (fun scope parameter -> Map.add parameter.Name parameter.Constraint scope) scope

    let rec walk scope reference =
        [
            match reference with
            | FsApp(name, arguments) ->
                yield scope, name, arguments
                yield! arguments |> List.collect (walk scope)
            | FsOption inner
            | FsArray inner -> yield! walk scope inner
            | FsTuple items
            | FsErasedUnion items -> yield! items |> List.collect (walk scope)
            | FsDelegate(arguments, returns) -> yield! (returns :: arguments) |> List.collect (walk scope)
            | FsFunc(argument, returns) -> yield! [ argument; returns ] |> List.collect (walk scope)
            | _ -> ()
        ]

    let parameters scope (items: FsParam list) =
        items |> List.collect (fun parameter -> walk scope parameter.Type)

    let bounds scope (items: FsTypeParam list) =
        items
        |> List.collect (fun parameter -> parameter.Constraint |> Option.toList |> List.collect (walk scope))

    let rec declMember scope =
        function
        | FsProperty p -> walk scope p.Type
        | FsIndexer i -> walk scope i.Key @ walk scope i.Value
        | FsMethod m ->
            let scope = bind scope m.TypeParameters

            bounds scope m.TypeParameters
            @ parameters scope m.Parameters
            @ walk scope m.Return
        | FsConstructor c
        | FsInvoke c ->
            let scope = bind scope c.TypeParameters

            bounds scope c.TypeParameters
            @ parameters scope c.Parameters
            @ walk scope c.Return
        | FsSymbolKeyed(_, inner)
        | FsStringKeyed(_, inner)
        | FsLiteralKeyed(_, _, inner) -> declMember scope inner

    let exportMember scope (m: FsExportMember) =
        let scope = bind scope m.TypeParameters

        bounds scope m.TypeParameters
        @ match m.Body with
          | ExportFunction(items, returns)
          | ExportConstructor(items, returns) -> parameters scope items @ walk scope returns
          | ExportValue returns -> walk scope returns

    match decl with
    | FsInterface d ->
        let scope = bind Map.empty d.TypeParameters

        bounds scope d.TypeParameters
        @ (d.Inherits |> List.collect (walk scope))
        @ (d.Members |> List.collect (declMember scope))
        @ (d.Statics |> List.collect (exportMember scope))
        @ (d.CreateOverloads
           |> List.collect (fun overload -> parameters scope overload.Parameters))
        @ (d.Entrypoint
           |> Option.toList
           |> List.collect (fun entrypoint -> parameters scope entrypoint.Parameters))
    | FsAbbrev d ->
        let scope = bind Map.empty d.TypeParameters
        bounds scope d.TypeParameters @ walk scope d.Target
    | FsPhantom d ->
        let scope = bind Map.empty d.TypeParameters
        bounds scope d.TypeParameters @ walk scope d.Carrier
    | FsDelegateType d ->
        let scope = bind Map.empty d.TypeParameters

        bounds scope d.TypeParameters
        @ (d.Parameters |> List.collect (fun parameter -> walk scope parameter.Type))
        @ walk scope d.Return
    | FsErasedObjectUnion d ->
        let scope = bind Map.empty d.TypeParameters

        bounds scope d.TypeParameters
        @ (d.Cases |> List.collect (fun case -> walk scope case.Payload))
        @ (d.Invokes |> List.collect (FsInvoke >> declMember scope))
    | FsTaggedUnion d ->
        let scope = bind Map.empty d.TypeParameters

        bounds scope d.TypeParameters
        @ (d.Cases
           |> List.collect (fun case -> case.Fields |> List.collect (fun field -> walk scope field.Type)))
    | FsExports container ->
        container.Members
        |> List.collect (fun owned -> exportMember Map.empty owned.Member)
    | FsMeasure _
    | FsStringEnum _
    | FsEnum _ -> []

/// A head's bound with the head's own variables replaced by an application's arguments.
let rec private substituted (bindings: Map<string, FsTypeRef>) (reference: FsTypeRef) =
    let recur = substituted bindings

    match reference with
    | FsTypeVar name -> Map.tryFind name bindings |> Option.defaultValue reference
    | FsOption inner -> FsOption(recur inner)
    | FsArray inner -> FsArray(recur inner)
    | FsTuple items -> FsTuple(List.map recur items)
    | FsErasedUnion items -> FsErasedUnion(List.map recur items)
    | FsDelegate(arguments, returns) -> FsDelegate(List.map recur arguments, recur returns)
    | FsFunc(argument, returns) -> FsFunc(recur argument, recur returns)
    | FsApp(name, arguments) -> FsApp(name, List.map recur arguments)
    | other -> other

/// TypeScript admits a variable at a bounded slot by covariance; F# admits it only where the
/// variable's own bound is the slot's. A head whose bound some application supplies with an
/// unbounded variable, or with one bounded by another instantiation of the same generic, drops
/// the bound (`TP012`), until every application in the run meets the heads it applies.
let repairConstraints: Pass<ShapeModel> =
    {
        Name = "repair-constraints"
        Run =
            fun _ model ->
                async {
                    let applied =
                        model.Decls
                        |> List.collect (fun decl ->
                            let owner = declName decl |> Option.defaultValue "Exports"
                            applications decl |> List.map (fun application -> owner, application))

                    let unmet (heads: Map<string, FsTypeParam list>) =
                        applied
                        |> List.collect (fun (owner, (scope, name, arguments)) ->
                            match Map.tryFind name heads with
                            | Some parameters when parameters.Length = arguments.Length ->
                                let bindings = List.zip (parameters |> List.map _.Name) arguments |> Map.ofList

                                List.zip parameters arguments
                                |> List.indexed
                                |> List.choose (fun (index, (parameter, argument)) ->
                                    match parameter.Constraint, argument with
                                    | Some bound, FsTypeVar variable when Map.containsKey variable scope ->
                                        match scope[variable], substituted bindings bound with
                                        | None, _ -> Some(name, index, bound, owner)
                                        | Some(FsApp(head, stated)), FsApp(target, required) when
                                            head = target && stated <> required
                                            ->
                                            Some(name, index, bound, owner)
                                        | _ -> None
                                    | _ -> None)
                            | _ -> [])

                    let rec settle (heads: Map<string, FsTypeParam list>) dropped =
                        match unmet heads |> List.distinctBy (fun (name, index, _, _) -> name, index) with
                        | [] -> heads, dropped
                        | found ->
                            let heads =
                                found
                                |> List.fold
                                    (fun (heads: Map<string, FsTypeParam list>) (name, index, _, _) ->
                                        heads
                                        |> Map.add
                                            name
                                            (heads[name]
                                             |> List.mapi (fun at parameter ->
                                                 if at = index then
                                                     { parameter with Constraint = None }
                                                 else
                                                     parameter)))
                                    heads

                            settle heads (dropped @ found)

                    let heads, dropped = settle (heads model.Decls) []

                    if List.isEmpty dropped then
                        return Advanced model
                    else
                        let retyped (name: string) (parameters: FsTypeParam list) =
                            Map.tryFind name heads |> Option.defaultValue parameters

                        let decls =
                            model.Decls
                            |> List.map (fun decl ->
                                match decl with
                                | FsInterface d ->
                                    FsInterface
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | FsAbbrev d ->
                                    FsAbbrev
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | FsPhantom d ->
                                    FsPhantom
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | FsDelegateType d ->
                                    FsDelegateType
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | FsTaggedUnion d ->
                                    FsTaggedUnion
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | FsErasedObjectUnion d ->
                                    FsErasedObjectUnion
                                        { d with
                                            TypeParameters = retyped d.Name d.TypeParameters
                                        }
                                | other -> other)

                        let findings =
                            dropped
                            |> List.map (fun (name, index, bound, owner) ->
                                let parameter = heads[name][index]

                                Finding.make
                                    name
                                    (TypeParameters.ConstraintUnmetByReference(
                                        parameter.Name,
                                        typeSpelling bound,
                                        owner
                                    )))

                        return Degraded({ model with Decls = decls }, findings)
                }
    }
