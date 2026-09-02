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

                    // A name is declared once. `synthesize-anonymous` hash-conses an erased alias
                    // application onto the declaration it applies, so two ids can deliberately
                    // carry one name: the smaller is the declared form and the larger is a
                    // reference site. Everything else it names is unique by construction.
                    let mutable declaredOnce = Set.empty

                    let decls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.choose (fun (typeId, name) ->
                            match Map.tryFind typeId model.Types with
                            | Some facts when declaresInterface model facts && not (Set.contains name declaredOnce) ->
                                declaredOnce <- Set.add name declaredOnce
                                let typeParameters, scope, parameterFindings = declTypeParams ctx model name facts

                                findings <- findings @ parameterFindings

                                // Members are shaped under the declaration's own parameters, so a
                                // `T` in a member position names the variable rather than widening.
                                let members, memberFindings =
                                    shapeMembers ctx { model with TypeVars = scope } name facts

                                findings <- findings @ memberFindings

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
