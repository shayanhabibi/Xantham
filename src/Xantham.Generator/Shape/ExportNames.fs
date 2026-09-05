module Xantham.Generator.Shape.ExportNames

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type the first in harvest order names it and `shape-aliases` abbreviates the rest.
///
/// Two exports of two *different* types under one name are two declarations, and F# admits one
/// name per declaration. Where TypeScript separates them by the namespace one of them is
/// written in, so does F#: `@cloudflare/workers-types` declares `WorkflowSleepDuration` twice,
/// once in global scope and once in `CloudflareWorkersModule`, and the second reads
/// `CloudflareWorkersModule.WorkflowSleepDuration` inside a module of that name. A pair with no
/// namespace between them keeps the numeric suffix `shape-anonymous` uses for the same clash.
let nameExports: Pass<ShapeModel> =
    {
        Name = "name-exports"
        Run =
            fun ctx model ->
                async {
                    let fallback = defaultExportName ctx

                    let claim (taken: Set<string>) (preferred: string) =
                        if not (Set.contains preferred taken) then
                            preferred
                        else
                            Seq.initInfinite (fun i -> $"{preferred}{i + 2}")
                            |> Seq.find (fun candidate -> not (Set.contains candidate taken))

                    /// The module name an export nests under, where its symbol is written inside
                    /// a namespace this run names.
                    let namespaceOf (export: HarvestedExport) =
                        export.Symbol.Parent
                        |> ValueOption.toOption
                        |> Option.bind (fun parent -> Map.tryFind parent model.Harvest.Namespaces)
                        |> Option.map Naming.pascalSegment

                    // The claim every export makes, in harvest order, read before any of them is
                    // granted. A contested name is visible only from the whole list, and the
                    // namespaced declaration is as often the first claimant as the second - it is
                    // the one with somewhere else to go either way.
                    let claimants =
                        model.Harvest.Exports
                        |> List.fold
                            (fun (claimants, seen) export ->
                                if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                                    claimants, seen
                                else
                                    match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                                    | Some typeId when
                                        not (Map.containsKey typeId model.DeclNames) && not (Set.contains typeId seen)
                                        ->
                                        claimants
                                        @ [ typeId, export.Order, fsName fallback export, namespaceOf export ],
                                        Set.add typeId seen
                                    | _ -> claimants, seen)
                            ([], Set.empty)
                        |> fst

                    let declared = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList

                    let contested =
                        claimants
                        |> List.countBy (fun (_, _, preferred, _) -> preferred)
                        |> List.filter (fun (preferred, count) -> count > 1 || Set.contains preferred declared)
                        |> List.map fst
                        |> Set.ofList

                    let names, orders, _, findings =
                        claimants
                        |> List.fold
                            (fun (names, orders, taken, findings) (typeId, order, preferred, owner) ->
                                let wanted =
                                    match owner with
                                    | Some ns when Set.contains preferred contested -> nestUnder ns preferred
                                    | _ -> preferred

                                let name = claim taken wanted

                                Map.add typeId name names,
                                Map.add typeId order orders,
                                Set.add name taken,
                                if name.Contains "." then
                                    findings @ [ Finding.make name (SynthesizeAnonymous.NameNestedUnderOwner name) ]
                                else
                                    findings)
                            (model.DeclNames, model.DeclOrders, declared, [])

                    let model =
                        { model with
                            DeclNames = names
                            DeclOrders = orders
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
