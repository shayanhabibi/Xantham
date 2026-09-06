module Xantham.Generator.Shape.ExportNames

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The export that owns each declared type's generated definition. Keep harvest order within
/// one group; across shipped groups, prefer the defining symbol over an alias of it.
let declarationExports (ctx: Context) (model: ShapeModel) =
    model.Harvest.Exports
    |> List.choose (fun export ->
        if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
            None
        else
            Map.tryFind export.Symbol.Id model.ExportTypes
            |> Option.bind _.Declared
            |> Option.map (fun typeId -> typeId, export))
    |> List.groupBy fst
    |> List.map (fun (typeId, candidates) ->
        let exports = candidates |> List.map snd
        let first = List.head exports
        let firstOrigin = Grouping.classify ctx.PackageDir (ValueSome first.Symbol)

        let defining =
            exports
            |> List.tryFind (fun export ->
                let origin = Grouping.classify ctx.PackageDir (ValueSome export.Symbol)

                origin <> firstOrigin
                && GeneratorConfig.disposition ctx.Config origin = Ship
                && (Map.tryFind typeId model.Types
                    |> Option.exists (fun facts -> facts.Response.Symbol = ValueSome export.Symbol.Id)))
            |> Option.defaultValue first

        typeId, defining)

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type the first in harvest order names it and `shape-aliases` abbreviates the rest. Across
/// shipped groups, the defining symbol takes precedence so an entry alias cannot make the
/// shared declaration depend on its consumer.
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
                        declarationExports ctx model
                        |> List.filter (fun (typeId, _) -> not (Map.containsKey typeId model.DeclNames))
                        |> List.map (fun (typeId, export) ->
                            typeId, export.Order, fsName fallback export, namespaceOf export)

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
