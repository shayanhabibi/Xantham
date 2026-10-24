module Xantham.Generator.Shape.ExportNames

open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The export that owns each declared type's generated definition. Across shipped groups, the
/// defining symbol takes precedence over an alias of it; within one group, the export whose
/// owner path is shallowest, ordinal owner specifier and then harvest order breaking ties.
let declarationExports (ctx: Context) (model: ShapeModel) =
    let hasEntryOwner = ExportLayout.hasEntryOwner model

    model.Harvest.Exports
    |> List.choose (fun export ->
        if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
            None
        else
            Map.tryFind export.Symbol.SymbolId model.ExportTypes
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
                    |> Option.exists (fun facts -> facts.Response.SymbolId = ValueSome export.Symbol.SymbolId)))
            |> Option.defaultValue (
                exports
                |> List.indexed
                |> List.minBy (fun (index, export) ->
                    let owner = ExportLayout.ownerOf model.RuntimePackage export.Origin

                    ExportLayout.depthOf model.RuntimePackage hasEntryOwner owner,
                    ExportLayout.ownerSpecifier owner,
                    export.ExportName <> export.Symbol.Name,
                    index)
                |> snd
            )

        typeId, defining)

/// Names every type-like export before anything refers to one, so later passes see references
/// as `FsNamed` instead of expansions. Keys are type ids; when two exports share a declared
/// type, `declarationExports` names it under the defining symbol across shipped groups, or
/// otherwise under the shallowest owner path (ordinal owner specifier, then harvest order,
/// breaking ties), and `shape-aliases` abbreviates the rest.
///
/// Source namespace members always retain their namespace module path. Two declarations
/// competing for the same full path keep distinct names through a numeric suffix.
let nameExports: Pass<ShapeModel> =
    {
        Name = "name-exports"
        Run =
            fun ctx model ->
                async {
                    let fallback = defaultExportName ctx
                    let pathOf = ExportLayout.declPath (ExportLayout.modulePaths model) model

                    let claim (taken: Set<string>) (preferred: string) =
                        if not (Set.contains preferred taken) then
                            preferred
                        else
                            Seq.initInfinite (fun i -> $"{preferred}{i + 2}")
                            |> Seq.find (fun candidate -> not (Set.contains candidate taken))

                    /// The module name an export nests under, where its symbol is written inside
                    /// a namespace this run names.
                    let namespaceOf (export: HarvestedExport) =
                        namespacePath model.Harvest.Namespaces export.DeclaringNamespace

                    /// Where a declaration was harvested from, in the words a manifest reads.
                    let originOf (export: HarvestedExport) =
                        match export.Origin with
                        | FromModule -> "the entry module"
                        | FromGlobal -> "global scope"
                        | FromAmbientModule specifier -> $"ambient module \"{specifier / uom<importSpecifier>}\""

                    // The claim every export makes, in harvest order, read before any of them is
                    // granted. A contested name is visible only from the whole list, and the
                    // namespaced declaration is as often the first claimant as the second - it is
                    // the one with somewhere else to go either way.
                    let exportsById = declarationExports ctx model |> Map.ofList

                    let claimants =
                        declarationExports ctx model
                        |> List.filter (fun (typeId, _) -> not (Map.containsKey typeId model.DeclNames))
                        |> List.map (fun (typeId, export) ->
                            typeId,
                            export.Order,
                            Naming.typeNameSegment (fsName fallback export),
                            namespaceOf export,
                            originOf export,
                            pathOf export)

                    let declared = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList

                    /// Public-owner path followed by the source namespace and declaration name.
                    let wantedOf preferred owner path =
                        path @ [ underNamespace owner preferred ] |> String.concat "."

                    let sourceOf typeId preferred =
                        Map.tryFind typeId exportsById
                        |> Option.map (fsName fallback)
                        |> Option.defaultValue preferred

                    // A sanitised name yields to a declaration spelling it verbatim at the same
                    // path: `$ZodType` reads `ZodType2` beside an exported `ZodType`, whichever
                    // is harvested first.
                    let verbatim =
                        claimants
                        |> List.filter (fun (typeId, _, preferred, _, _, _) -> sourceOf typeId preferred = preferred)
                        |> List.map (fun (_, _, preferred, owner, _, path) -> wantedOf preferred owner path)
                        |> Set.ofList

                    let names, orders, _, findings =
                        claimants
                        |> List.fold
                            (fun (names, orders, taken, findings) (typeId, order, preferred, owner, origin, path) ->
                                let wanted = wantedOf preferred owner path
                                let source = sourceOf typeId preferred
                                let sanitised = source <> preferred

                                let name =
                                    if sanitised then
                                        claim (Set.union taken verbatim) wanted
                                    else
                                        claim taken wanted

                                let findings =
                                    if sanitised then
                                        findings
                                        @ [
                                            Finding.make
                                                name
                                                (SynthesizeAnonymous.NameSanitisedForIdentifier(source, name))
                                        ]
                                    else
                                        findings

                                let findings =
                                    if name <> wanted then
                                        findings
                                        @ [ Finding.make name (NameExports.TypeNameSuffixed(wanted, name, origin)) ]
                                    else
                                        findings

                                Map.add typeId name names, Map.add typeId order orders, Set.add name taken, findings)
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
