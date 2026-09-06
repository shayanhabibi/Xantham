module Xantham.Generator.Shape.Interfaces

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// What `shape-interfaces` declares under a name: an object shape with members, or an
/// intersection of object types flattened into one (§4.6). Not an array, a tuple, or a named
/// instantiation. An index signature counts: `interface Bag { [key: string]: number }`.
let private declaresInterface (model: ShapeModel) (facts: TypeFacts) =
    (flag TypeFlags.Object facts
     // A constructor object is shape even with no properties of its own: `interface F { new
     // (): X }` has no members and one construct signature, and becomes an interface of one
     // `Create` (§4.4).
     && not (
         facts.Members.IsEmpty
         && facts.IndexInfos.IsEmpty
         && facts.ConstructSignatures.IsEmpty
     )
     // A pure index signature the checker gave no symbol of its own - reached through a
     // type alias, or written inline - resolves through `objectRef` as a `Record`/
     // `ReadonlyRecord` reference instead (TR059). `interface Bag { [key: string]:
     // number }` keeps its own name: its own symbol carries it past this exclusion.
     && not (isAnonymousIndexSignature model facts)
     && (arrayElement model facts).IsNone
     && not (isTuple facts)
     && (instantiationOf model facts).IsNone)
    || isFlattenable model facts

/// Whether one declaration name already reaches another through the `inherit` edges emitted so
/// far. F# refuses cyclic inheritance (FS0954), and an F# name is not a type id, so two ids
/// hash-consed onto one name can close a loop the source never wrote.
let private reaches (graph: Map<string, string list>) (from: string) (target: string) =
    let rec walk seen name =
        name = target
        || (not (Set.contains name seen)
            && (Map.tryFind name graph
                |> Option.defaultValue []
                |> List.exists (walk (Set.add name seen))))

    walk Set.empty from

/// F# interfaces for every named object type with members: exported interfaces, class instance
/// sides, and synthesized anonymous shapes. Inherited members are declared in full, and a base
/// this run declares is `inherit`ed beside them (§4.4); any other base is flattened alone.
let shapeInterfaces: Pass<ShapeModel> =
    {
        Name = "shape-interfaces"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let fallbackDocs =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            Map.tryFind export.Symbol.Id model.ExportTypes
                            |> Option.bind _.Declared
                            |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                        |> Map.ofList

                    // A union whose arms agree on every member except which ones they declare
                    // `never` (wave fourteen item 4): folded into one interface named after the
                    // union itself, with the exclusive members optional and one `Create`
                    // overload per arm. Declined pairs are reported and otherwise untouched -
                    // each arm keeps minting its own interface, as today.
                    let exclusiveUnions =
                        model.DeclNames
                        |> Map.toList
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) ->
                                match exclusiveArmShape model facts with
                                | NotExclusiveArms -> None
                                | ExclusiveArmsFold(shared, arms) -> Some(typeId, name, Choice1Of2(shared, arms))
                                | ExclusiveArmsDecline arms -> Some(typeId, name, Choice2Of2 arms)
                            | _ -> None)

                    let foldedArmIds =
                        exclusiveUnions
                        |> List.collect (fun (_, _, outcome) ->
                            match outcome with
                            | Choice1Of2(_, arms) -> arms |> List.map (fun a -> a.Facts.Response.Id)
                            | Choice2Of2 _ -> [])
                        |> Set.ofList

                    let declNames =
                        model.DeclNames |> Map.filter (fun id _ -> not (Set.contains id foldedArmIds))

                    // The names this pass declares, known ahead of the declarations: what a
                    // flattened intersection may inherit.
                    let interfaceNames =
                        declNames
                        |> Map.toList
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when declaresInterface model facts -> Some name
                            | _ -> None)
                        |> Set.ofList

                    // The `inherit` edges emitted so far, accumulated in declaration order. A
                    // cycle is closed by whichever edge is added last, so refusing an edge the
                    // graph can already walk back from is enough to keep the whole graph acyclic.
                    let mutable inheritGraph: Map<string, string list> = Map.empty

                    // A name is declared once. `synthesize-anonymous` hash-conses an erased alias
                    // application onto the declaration it applies, so two ids can deliberately
                    // carry one name: the smaller is the declared form and the larger is a
                    // reference site. Everything else it names is unique by construction.
                    let mutable declaredOnce = Set.empty

                    // The instance side of each exported class, which is what decides whether a
                    // declaration's optional methods are lifecycle hooks (§4.4).
                    let classSides = exportedClassSides model

                    let decls =
                        declNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.collect (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when declaresInterface model facts && not (Set.contains name declaredOnce) ->
                                declaredOnce <- Set.add name declaredOnce
                                let typeParameters, scope, parameterFindings = declTypeParams ctx model name facts

                                findings <- findings @ parameterFindings

                                // §4.4's heritage rule and §4.6's is-a relation through one
                                // gate: a declared base and an intersection operand are both a
                                // type this declaration *is*. Members are declared in full too.
                                let inheritable (operandId: int) =
                                    match typeRef ctx { model with TypeVars = scope } None name operandId with
                                    | (FsNamed operand | FsApp(operand, _)) as reference, refFindings ->
                                        if not (Set.contains operand interfaceNames) then
                                            Error(ShapeInterfaces.BaseNotDeclaredHere operand)
                                        elif reaches inheritGraph operand name then
                                            Error(ShapeInterfaces.BaseWouldCycle operand)
                                        else
                                            Ok(operand, reference, refFindings)
                                    | _ -> Error ShapeInterfaces.BaseMembersFlattened

                                // The optional methods of a declaration `shape-classes` writes as
                                // an `[<AbstractClass>]` - an ambient module's exported class,
                                // abstract or based, rendered under an `inherit` line this run
                                // omits. Each is a lifecycle hook and becomes an interface a
                                // subclass opts into; an optional method anywhere else stays an
                                // option property.
                                let hooks =
                                    let entrypoint =
                                        match Map.tryFind typeId classSides with
                                        | Some(export, valueFacts) ->
                                            isEntrypoint export valueFacts.ConstructSignatures facts.BaseTypes
                                        | None -> false

                                    let inheritsSomething () =
                                        (if flag TypeFlags.Intersection facts then
                                             facts.IntersectionMembers
                                         else
                                             [])
                                        @ facts.BaseTypes
                                        |> List.exists (fun operandId ->
                                            match inheritable operandId with
                                            | Ok _ -> true
                                            | Error _ -> false)

                                    if entrypoint && not (inheritsSomething ()) then
                                        facts.Members
                                        |> List.filter (isOptionalHook model)
                                        |> List.map _.Symbol.Name
                                        |> Set.ofList
                                    else
                                        Set.empty

                                // Members are shaped under the declaration's own parameters, so a
                                // `T` in a member position names the variable rather than widening.
                                let members, memberFindings =
                                    shapeMembers ctx { model with TypeVars = scope } hooks name facts

                                findings <- findings @ memberFindings

                                let hookDecls =
                                    facts.Members
                                    |> List.filter (fun m -> Set.contains m.Symbol.Name hooks)
                                    |> List.map (fun m ->
                                        let decl, hookFindings =
                                            shapeHook
                                                ctx
                                                { model with TypeVars = scope }
                                                name
                                                typeParameters
                                                (Map.tryFind typeId model.DeclOrders |> Option.defaultValue None)
                                                m

                                        findings <- findings @ hookFindings
                                        decl)

                                let mutable inherits = []

                                // `record` is what separates the two callers: a declared base
                                // says per base what became of it, where an intersection says
                                // `IntersectionFlattened` once for the whole operand list.
                                let admit (record: bool) (operandId: int) =
                                    match inheritable operandId with
                                    | Ok(operand, reference, refFindings) ->
                                        if not (List.exists (fun (taken, _) -> taken = operand) inherits) then
                                            findings <- findings @ refFindings
                                            inherits <- inherits @ [ operand, reference ]

                                            if record then
                                                findings <-
                                                    findings
                                                    @ [ Finding.make name (ShapeInterfaces.BaseInherited operand) ]
                                    | Error kind ->
                                        if record then
                                            findings <- findings @ [ Finding.make name kind ]

                                if flag TypeFlags.Intersection facts then
                                    for operandId in facts.IntersectionMembers do
                                        admit false operandId

                                for baseId in facts.BaseTypes do
                                    admit true baseId

                                inheritGraph <- Map.add name (inherits |> List.map fst) inheritGraph

                                if isConstructorObject facts then
                                    findings <-
                                        findings
                                        @ [
                                            Finding.make
                                                name
                                                (ShapeInterfaces.ConstructorObjectDeclared
                                                    facts.ConstructSignatures.Length)
                                        ]

                                // A hybrid's call signatures reach through an `Invoke` member (§4.4's
                                // counterpart for the *call* side): `[<Emit("$0($1...)")>]` applies the
                                // receiver to the arguments, so `x.Invoke(a)` is `x(a)`. A member the
                                // interface already declares under that exact name would collide with
                                // it, so that one case keeps the loss honest instead.
                                let invokeCollides =
                                    members
                                    |> List.exists (function
                                        | FsProperty p -> p.Name = "Invoke"
                                        | FsMethod m -> m.Name = "Invoke"
                                        | FsIndexer _
                                        | FsConstructor _
                                        | FsInvoke _ -> false)

                                let invokes =
                                    if invokeCollides then
                                        []
                                    else
                                        facts.CallSignatures
                                        |> List.map (fun signature ->
                                            let typeParameters, parameters, returns, signatureFindings =
                                                shapeSignature
                                                    ctx
                                                    { model with TypeVars = scope }
                                                    (Some name)
                                                    $"{name}.Invoke"
                                                    signature

                                            findings <- findings @ signatureFindings

                                            FsInvoke
                                                {
                                                    Docs = ""
                                                    Tags = []
                                                    TypeParameters = typeParameters
                                                    Parameters = parameters
                                                    Return = returns
                                                })

                                if not facts.CallSignatures.IsEmpty then
                                    if invokeCollides then
                                        findings <-
                                            findings @ [ Finding.make name ShapeInterfaces.HybridLosesCallSignatures ]
                                    else
                                        findings <-
                                            findings
                                            @ [
                                                Finding.make
                                                    name
                                                    (ShapeInterfaces.HybridCallSignaturesAsInvoke
                                                        facts.CallSignatures.Length)
                                            ]

                                if flag TypeFlags.Intersection facts then
                                    findings <-
                                        findings
                                        @ [
                                            Finding.make
                                                name
                                                (ShapeInterfaces.IntersectionFlattened facts.IntersectionMembers.Length)
                                        ]

                                let docs, tags = Map.tryFind typeId fallbackDocs |> Option.defaultValue ("", [])

                                FsInterface
                                    {
                                        Name = name
                                        Docs = docs
                                        Tags = tags
                                        Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                        TypeParameters = typeParameters
                                        Inherits = inherits |> List.map snd
                                        Members = members @ invokes
                                        Entrypoint = None
                                        CreateOverloads = []
                                        Statics = []
                                    }
                                :: hookDecls
                            | _ -> [])

                    // Every arm's own `Create` overload carries only its own members and the
                    // shared ones - never a `never`-typed placeholder for the other arm's
                    // exclusive member - so a folded pair's overloads carry no `unit` parameter.
                    let buildParam (owner: string) (m: ResolvedMember) =
                        let reference, refFindings = typeRef ctx model None owner m.TypeId

                        {
                            Name = Naming.memberName m.Symbol.Name
                            Optional = m.Optional
                            Rest = false
                            Type = reference
                        },
                        refFindings

                    let buildMember (owner: string) (optional: bool) (m: ResolvedMember) =
                        let reference, refFindings = typeRef ctx model None owner m.TypeId

                        FsProperty
                            {
                                Name = Naming.memberName m.Symbol.Name
                                Docs = m.Docs
                                Tags = m.Tags
                                ReadOnly = m.ReadOnly
                                Type = optionalRef optional reference
                            },
                        refFindings

                    let foldedDecls, foldedFindings =
                        exclusiveUnions
                        |> List.map (fun (typeId, name, outcome) ->
                            match outcome with
                            | Choice2Of2 arms ->
                                [], [ Finding.make name (TypeReference.ExclusiveArmsNotFoldable arms) ]
                            | Choice1Of2(shared, arms) ->
                                let sharedMembers, sharedFindings =
                                    shared |> List.map (buildMember name false) |> List.unzip

                                let exclusiveMembers, exclusiveFindings =
                                    arms
                                    |> List.collect (fun a -> a.Own)
                                    |> List.map (buildMember name true)
                                    |> List.unzip

                                let overloads, overloadFindings =
                                    arms
                                    |> List.map (fun arm ->
                                        let sharedParams, sharedParamFindings =
                                            shared |> List.map (buildParam name) |> List.unzip

                                        let ownParams, ownParamFindings =
                                            arm.Own |> List.map (buildParam name) |> List.unzip

                                        let required, optional =
                                            (sharedParams @ ownParams) |> List.partition (fun p -> not p.Optional)

                                        required @ optional,
                                        List.concat sharedParamFindings @ List.concat ownParamFindings)
                                    |> List.unzip

                                let docs, tags = Map.tryFind typeId fallbackDocs |> Option.defaultValue ("", [])
                                let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                                [
                                    FsInterface
                                        {
                                            Name = name
                                            Docs = docs
                                            Tags = tags
                                            Order = order
                                            TypeParameters = []
                                            Inherits = []
                                            Members = sharedMembers @ exclusiveMembers
                                            Entrypoint = None
                                            CreateOverloads = overloads
                                            Statics = []
                                        }
                                ],
                                (List.concat sharedFindings
                                 @ List.concat exclusiveFindings
                                 @ List.concat overloadFindings
                                 @ [ Finding.make name (TypeReference.ExclusiveArmsFolded arms.Length) ]))
                        |> List.unzip
                        |> fun (ds, fs) -> List.concat ds, List.concat fs

                    let findings = findings @ foldedFindings

                    let model =
                        { model with
                            Decls = model.Decls @ decls @ foldedDecls
                            DeclNames = declNames
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
