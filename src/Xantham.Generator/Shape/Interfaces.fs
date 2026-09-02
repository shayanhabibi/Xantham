module Xantham.Generator.Shape.Interfaces

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// F# interfaces for every named object type with members: exported interfaces and class
/// instance sides alike, plus the synthesized anonymous shapes. Heritage is both `inherit`ed
/// and flattened: the checker's property list already includes the inherited members, so they
/// are declared here in full, and a base this run also declares as an interface is `inherit`ed
/// beside them (§4.4) so that the derived type upcasts to it. A base that has no F# name here,
/// names something this run does not declare, or would close a cycle stays flattened alone,
/// with a finding saying which of the three it is.
/// What `shape-interfaces` declares under a name: an object shape with members - not an array,
/// a tuple, or a named instantiation (`type StringBox = Box<string>`, an abbreviation of the
/// application rather than a second copy of the expansion, which `shape-aliases` writes) - or
/// an intersection of object types flattened into one interface (§4.6). An index signature is
/// shape too: `interface Bag { [key: string]: number }` has no properties at all, and without
/// that it would reach `shape-aliases` looking empty and abbreviate to obj (§4.10).
let private declaresInterface (model: ShapeModel) (facts: TypeFacts) =
    (flag TypeFlags.Object facts
     // A constructor object is shape even with no properties of its own: `interface F { new
     // (): X }` has no members and one construct signature, and is an interface of one
     // `Create`. Without this it would reach `shape-aliases` looking empty and abbreviate to
     // `obj` (§4.4), the same trap an index signature falls into just below.
     && not (
         facts.Members.IsEmpty
         && facts.IndexInfos.IsEmpty
         && facts.ConstructSignatures.IsEmpty
     )
     && (arrayElement facts).IsNone
     && not (isTuple facts)
     && (instantiationOf model facts).IsNone)
    || isFlattenable model facts

/// Whether one declaration name already reaches another through the `inherit` edges emitted so
/// far. F# refuses a cyclic inheritance relation outright (FS0954), and while TypeScript admits
/// no cyclic heritage, an F# name is not a type id - a declaration reached twice, or two ids
/// hash-consed onto one name, can close a loop the source never wrote. The walk carries its own
/// visited set so an already-broken graph cannot make it diverge.
let private reaches (graph: Map<string, string list>) (from: string) (target: string) =
    let rec walk seen name =
        name = target
        || (not (Set.contains name seen)
            && (Map.tryFind name graph
                |> Option.defaultValue []
                |> List.exists (walk (Set.add name seen))))

    walk Set.empty from

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

                    // The names this pass declares, known ahead of the declarations: what a
                    // flattened intersection may inherit.
                    let interfaceNames =
                        model.DeclNames
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

                    let decls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when declaresInterface model facts ->
                                let typeParameters, scope, parameterFindings = declTypeParams ctx model name facts

                                findings <- findings @ parameterFindings

                                // Members are shaped under the declaration's own parameters, so a
                                // `T` in a member position names the variable rather than widening.
                                let members, memberFindings =
                                    shapeMembers ctx { model with TypeVars = scope } name facts

                                findings <- findings @ memberFindings

                                // §4.4's heritage rule and §4.6's is-a relation, through one
                                // gate: a declared base and an intersection operand are both a
                                // type this declaration *is*, and F# can say so exactly when
                                // this run declares that type as an interface. The members are
                                // still declared here in full - F# admits the redeclaration,
                                // and it is what keeps `Create` and the member list exact when
                                // a sibling base or operand is not inheritable (a lib binding,
                                // a callable, an anonymous shape folded in).
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

                                if not facts.CallSignatures.IsEmpty then
                                    findings <-
                                        findings @ [ Finding.make name ShapeInterfaces.HybridLosesCallSignatures ]

                                if flag TypeFlags.Intersection facts then
                                    findings <-
                                        findings
                                        @ [
                                            Finding.make
                                                name
                                                (ShapeInterfaces.IntersectionFlattened facts.IntersectionMembers.Length)
                                        ]

                                let docs, tags = Map.tryFind typeId fallbackDocs |> Option.defaultValue ("", [])

                                Some(
                                    FsInterface
                                        {
                                            Name = name
                                            Docs = docs
                                            Tags = tags
                                            Order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                            TypeParameters = typeParameters
                                            Inherits = inherits |> List.map snd
                                            Members = members
                                            CreateOverloads = []
                                            Statics = []
                                        }
                                )
                            | _ -> None)

                    let model =
                        { model with
                            Decls = model.Decls @ decls
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
