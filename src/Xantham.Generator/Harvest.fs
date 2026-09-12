/// Tier 1 - Harvest: the Wire-driven inventory of what the author exported. No mapping
/// decisions live here; the tier's invariant is that every export of the entry module appears
/// exactly once, aliases followed to their origin.
module Xantham.Generator.Harvest

open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// Where the entry file's whole in-scope environment resolved, grouped and counted, for a run
/// where none of it classified as the entry package (reading `Grouping.classify`'s existing
/// groups, not adding one). Most populous group first, so a reference chain dominated by one
/// group names that group up front.
let private elsewhere (packageDir: string<dirPath>) (symbols: SymbolResponse[]) =
    symbols
    |> Array.countBy (fun symbol -> Grouping.classify packageDir (ValueSome symbol))
    |> Array.sortByDescending snd
    |> Array.map (fun (origin, count) ->
        let label =
            GeneratorConfig.groupKey origin
            |> Option.map (fun name -> name / uom<npmDependency>)
            |> Option.defaultValue "unclassified declarations"

        $"{count} in {label}")
    |> String.concat ", "

/// `path` spelled relative to `packageDir` with forward separators, so a finding that carries a
/// file reads the same on every machine. A path outside the package keeps its own spelling.
let internal underPackage (packageDir: string) (path: string) =
    let relative = System.IO.Path.GetRelativePath(packageDir, path)

    if relative.StartsWith ".." then
        path.Replace('\\', '/')
    else
        relative.Replace('\\', '/')

/// The namespace symbols among `symbols`, by id, for the declarations written inside them to
/// nest under. An ambient module declaration is a namespace symbol whose name is its quoted
/// specifier (`"cloudflare:workers"`), which heads no F# module, so the map holds only names a
/// declaration can take a path segment from.
let private namespacesAmong (symbols: SymbolResponse seq) =
    symbols
    |> Seq.filter (fun symbol -> hasAny SymbolFlags.Module symbol.Flags && Naming.isWritableTypeName symbol.Name)
    |> Seq.map (fun symbol -> symbol.SymbolId, symbol.SymbolName)
    |> Map.ofSeq

/// The entry module's exports, each followed through `getAliasedSymbol` to its origin so that
/// re-exports and default-export aliases land on the declaring symbol.
///
/// A global type library (`@cloudflare/workers-types`, `@types/*` that declare no module) has
/// no module symbol at all. That is not an error and not an escape: the pass advances with
/// nothing, and `harvest-globals` picks the file's ambient declarations up instead.
let harvestExports: Pass<HarvestModel> =
    {
        Name = "harvest-exports"
        Run =
            fun ctx model ->
                async {
                    let! moduleSymbol =
                        ctx.Session.getSymbolOfSourceFile (DocumentIdentifier.FileName(ctx.EntryFile / uom<_>))

                    match moduleSymbol with
                    | ValueNone -> return Advanced model
                    | ValueSome moduleSymbol ->
                        let! exports = ctx.Session.getExportsOfModule moduleSymbol.Id
                        let exports = exports |> ValueOption.defaultValue [||]

                        // Fan out the alias-following freely - the mailbox coalesces it - but
                        // Async.Parallel's result order is input order, so the fold is deterministic.
                        let! resolved =
                            exports
                            |> Array.map (fun export ->
                                async {
                                    if export.Flags.HasFlag SymbolFlags.Alias then
                                        let! origin = ctx.Session.getAliasedSymbol export.Id
                                        return export.Name, origin
                                    else
                                        return export.Name, export
                                })
                            |> Async.Parallel

                        let valueExport = ExportProvenance.reader ctx

                        let! harvested =
                            resolved
                            |> Array.sortBy fst
                            |> Array.map (fun (name, origin) ->
                                async {
                                    let! hasValueExport = valueExport moduleSymbol name

                                    return
                                        {
                                            ExportName = name
                                            Symbol = origin
                                            HasValueExport = hasValueExport
                                            Docs = ""
                                            Tags = []
                                            Origin = FromModule
                                            Order = Grouping.declOrder origin.Declarations
                                        }
                                })
                            |> Async.Sequential

                        // A namespace the entry file declares without exporting is still the
                        // owner of the types an exported signature reaches through it, so the
                        // scope at the top of the file is asked for rather than the export list.
                        let! inScope =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName(ctx.EntryFile / uom<_>),
                                position = 0
                            )

                        let namespaces =
                            inScope
                            |> Array.filter (fun symbol ->
                                Grouping.classify ctx.PackageDir (ValueSome symbol) = EntryPackage)
                            |> Array.append (resolved |> Array.map snd)
                            |> namespacesAmong

                        return
                            Advanced
                                { model with
                                    Exports = harvested |> Array.toList
                                    Namespaces = namespaces
                                }
                }
    }

/// One export of an ambient module, followed through `getAliasedSymbol` so that
/// `export { _connect as connect }` lands on the declaring symbol under the exported name.
let private followAlias (ctx: Context) (export: SymbolResponse) =
    async {
        if export.Flags.HasFlag SymbolFlags.Alias then
            let! origin = ctx.Session.getAliasedSymbol export.Id
            return export.Name, origin
        else
            return export.Name, export
    }

/// One ambient module declaration, harvested: its exports under the specifier they import
/// from, the findings the declaration raises, and the symbol id of the namespace it re-exports
/// where it is written `export = Namespace`.
///
/// `getExportsOfModule` resolves `export =` in place, so the exports of `cloudflare:workers` are
/// the members of `CloudflareWorkersModule`, and `getParentOfSymbol` over any of them returns
/// that namespace. Such a namespace is the module's body, reachable through the specifier
/// alone; a `[<Global>]` binding to it reads `undefined`.
let private harvestAmbientModule (ctx: Context) (moduleSymbol: SymbolResponse) =
    async {
        let specifier = moduleSymbol.Name.Trim '"'

        if specifier.Contains "*" then
            return
                [],
                [
                    Finding.make moduleSymbol.Name (HarvestGlobals.AmbientModuleWildcard specifier)
                ],
                None
        else
            let! exports = ctx.Session.getExportsOfModule moduleSymbol.Id
            let exports = exports |> ValueOption.defaultValue [||]

            if exports.Length = 0 then
                return [], [ Finding.make moduleSymbol.Name HarvestGlobals.AmbientModuleDropped ], None
            else
                let! resolved = exports |> Array.map (followAlias ctx) |> Async.Parallel
                let! parent = ctx.Session.getParentOfSymbol exports[0].Id

                let body =
                    parent
                    |> ValueOption.filter (fun (p: SymbolResponse) -> p.Name <> moduleSymbol.Name)
                    |> ValueOption.map (fun p -> p.Id, p.Name)
                    |> ValueOption.toOption

                let valueExport = ExportProvenance.reader ctx

                let! harvested =
                    resolved
                    |> Array.sortBy fst
                    |> Array.map (fun (name, origin) ->
                        async {
                            let! hasValueExport = valueExport moduleSymbol name

                            return
                                {
                                    ExportName = name
                                    Symbol = origin
                                    HasValueExport = hasValueExport
                                    Docs = ""
                                    Tags = []
                                    Origin = FromAmbientModule(specifier * uom<importSpecifier>)
                                    Order = Grouping.declOrder origin.Declarations
                                }
                        })
                    |> Async.Sequential

                let findings =
                    [
                        Finding.make
                            moduleSymbol.Name
                            (HarvestGlobals.AmbientModuleHarvested(specifier, harvested.Length))

                        match body with
                        | Some(_, name) -> Finding.make name (HarvestGlobals.NamespaceIsModuleBody(name, specifier))
                        | None -> ()
                    ]

                return harvested |> Array.toList, findings, body |> Option.map fst
    }

/// The entry package's ambient global declarations, for a package that declares no module at
/// all. Asking the checker for the symbols in scope at the top of the entry file returns the
/// whole global environment - three thousand names for `@cloudflare/workers-types`, two
/// thirds of them `lib.dom.d.ts` - so the result is filtered to the symbols the package
/// itself declares, by the same O7 placement the resolve tier groups types with.
///
/// An ambient module declaration (`declare module "cloudflare:email"`) arrives here too, under
/// a symbol name that is its quoted specifier. Its exports are harvested under
/// `FromAmbientModule`: the types are declared beside the package's globals, and the values
/// carry the specifier's own import.
///
/// For `@types/node`, collapses `node:X` and bare `X` ambient-module specifiers onto one
/// module, spelled `node:X`: every builtin's exports bind `[<Import(name, "node:X")>]`,
/// and a builtin split across both spellings nests under one F# module. Members from both
/// spellings are carried under the collapsed specifier; a divergence between the two
/// spellings' export sets additionally raises a finding.
let private collapseNodeAliases (ctx: Context) (exports: HarvestedExport list) =
    if ctx.PackageName / uom<npmDependency> <> "@types/node" then
        exports, []
    else
        let bare (specifier: string) =
            if specifier.StartsWith "node:" then
                specifier.Substring 5
            else
                specifier

        let findings =
            exports
            |> List.choose (fun export ->
                match export.Origin with
                | FromAmbientModule specifier -> Some(specifier / uom<importSpecifier>, export.ExportName)
                | _ -> None)
            |> List.groupBy (fst >> bare)
            |> List.choose (fun (name, occurrences) ->
                let spellings = occurrences |> List.map fst |> List.distinct

                let exportsOf spelling =
                    occurrences
                    |> List.filter (fun (specifier, _) -> specifier = spelling)
                    |> List.map snd
                    |> Set.ofList

                match spellings with
                | [ _ ] -> None
                | _ when spellings |> List.map exportsOf |> List.distinct |> List.length = 1 -> None
                | _ -> Some(Finding.make name (HarvestGlobals.AmbientModuleAliasDivergent(name, spellings))))

        let collapsed =
            exports
            |> List.map (fun export ->
                match export.Origin with
                | FromAmbientModule specifier ->
                    { export with
                        Origin =
                            FromAmbientModule($"node:{bare (specifier / uom<importSpecifier>)}" * uom<importSpecifier>)
                    }
                | _ -> export)

        collapsed, findings

/// Runs only when `harvest-exports` found nothing: a package with a module symbol may also
/// augment the global scope, and folding those globals into its exports would emit names the
/// package does not export.
let harvestGlobals: Pass<HarvestModel> =
    {
        Name = "harvest-globals"
        Run =
            fun ctx model ->
                async {
                    if not (List.isEmpty model.Exports) then
                        return Advanced model
                    else
                        // Types and values both: a global library is mostly interfaces and aliases,
                        // but `declare function`/`declare var` are exactly what needs `[<Global>]`.
                        let! symbols =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Type ||| SymbolFlags.Value,
                                file = DocumentIdentifier.FileName(ctx.EntryFile / uom<declFile>),
                                position = 0
                            )

                        // A run that ships the compiler-lib group (`xantham.json`'s
                        // `"typescript/lib": "ship"`) harvests its declarations alongside the
                        // entry package's own: the shape tier already renders such a name in
                        // full rather than through `libBinding` (`Shape/Spec.fs`), so a
                        // declaration this pass withholds never reaches anything downstream that
                        // could act on it.
                        let shipsCompilerLib = GeneratorConfig.disposition ctx.Config CompilerLib = Ship

                        let admits origin =
                            origin = EntryPackage || (origin = CompilerLib && shipsCompilerLib)

                        let ours =
                            symbols
                            |> Array.filter (fun symbol -> admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))

                        // A name a `lib.*.d.ts` declaration precedes classifies as the compiler
                        // lib (`Grouping.classify` reads only the first declaration), so such a
                        // symbol never reaches `ours` above, unless `shipsCompilerLib` admitted it
                        // there already. This still finds the rest: any of the symbol's
                        // declarations sitting under the package directory is this package's own
                        // contribution to the merge, lost to `Exports` all the same.
                        let shadowedByLib =
                            symbols
                            |> Array.filter (fun symbol ->
                                not (admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))
                                && Grouping.declaresUnderPackage ctx.PackageDir symbol)
                            |> Array.length

                        // A namespace of types alone is neither a type nor a value, so
                        // `TailStream` arrives under `Module` and nowhere else. Its members
                        // reach the shape tier through the types that refer to them, and the
                        // namespace is what separates two declarations of one name.
                        let! declared =
                            ctx.Session.getSymbolsInScope (
                                SymbolFlags.Module,
                                file = DocumentIdentifier.FileName(ctx.EntryFile / uom<declFile>),
                                position = 0
                            )

                        let namespaces =
                            declared
                            |> Array.filter (fun symbol -> admits (Grouping.classify ctx.PackageDir (ValueSome symbol)))
                            |> namespacesAmong

                        // An ambient module declaration is a global-scope symbol whose name is
                        // its quoted specifier. Its exports are harvested under that specifier;
                        // the specifier itself heads no declaration.
                        let writable, unwritable =
                            ours |> Array.partition (fun symbol -> Naming.isWritableTypeName symbol.Name)

                        let modules, unnameable =
                            unwritable |> Array.partition (fun symbol -> symbol.Name.StartsWith "\"")

                        let! fromModules =
                            modules
                            |> Array.sortBy _.Name
                            |> Array.map (harvestAmbientModule ctx)
                            |> Async.Parallel

                        let moduleBodies =
                            fromModules |> Array.choose (fun (_, _, body) -> body) |> Set.ofArray

                        let findings =
                            [
                                for symbol in unnameable do
                                    Finding.make symbol.Name HarvestGlobals.UnwritableGlobalDropped

                                for _, moduleFindings, _ in fromModules do
                                    yield! moduleFindings
                            ]

                        let harvested =
                            [
                                for symbol in writable do
                                    if not (Set.contains symbol.Id moduleBodies) then
                                        {
                                            ExportName = symbol.Name
                                            Symbol = symbol
                                            HasValueExport = hasAny SymbolFlags.Value symbol.Flags
                                            Docs = ""
                                            Tags = []
                                            Origin = FromGlobal
                                            Order = Grouping.declOrder symbol.Declarations
                                        }

                                for exports, _, _ in fromModules do
                                    yield! exports
                            ]

                        let harvested, aliasFindings = collapseNodeAliases ctx harvested
                        let findings = findings @ aliasFindings

                        if List.isEmpty harvested && List.isEmpty findings then
                            return
                                Degraded(
                                    { model with
                                        ShadowedByLib = shadowedByLib
                                    },
                                    [
                                        Finding.make
                                            "<module>"
                                            (HarvestGlobals.NothingHarvested(
                                                underPackage
                                                    (ctx.PackageDir / uom<dirPath>)
                                                    (ctx.EntryFile / uom<declFile>),
                                                symbols.Length,
                                                elsewhere ctx.PackageDir symbols
                                            ))
                                    ]
                                )
                        else
                            let model =
                                { model with
                                    Exports = harvested
                                    Namespaces = namespaces
                                    ShadowedByLib = shadowedByLib
                                }

                            return
                                if List.isEmpty findings then
                                    Advanced model
                                else
                                    Degraded(model, findings)
                }
    }

/// Documentation for every harvested export, from the checker rather than the syntax tree so
/// merged declarations already read as one.
let harvestDocs: Pass<HarvestModel> =
    {
        Name = "harvest-docs"
        Run =
            fun ctx model ->
                async {
                    let! documented =
                        model.Exports
                        |> List.map (fun export ->
                            async {
                                let! docs = ctx.Session.getDocumentationComment export.Symbol.Id
                                let! tags = ctx.Session.getJsDocTags export.Symbol.Id

                                return
                                    { export with
                                        Docs = docs
                                        Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                                    }
                            })
                        |> Async.Parallel

                    return
                        Advanced
                            { model with
                                Exports = Array.toList documented
                            }
                }
    }

/// Entry declarations first, then package-relative dependency and compiler sources.
/// Node index and export name break ties; exports without declarations sort last.
let orderExports: Pass<HarvestModel> =
    Pass.pure' "harvest-order" (fun ctx model ->
        { model with
            Exports =
                model.Exports
                |> List.sortBy (fun export ->
                    (match export.Order with
                     | Some order -> Grouping.sourceOrderKey ctx.PackageDir (order.File / uom<node>), order.NodeIndex
                     | None -> (2, "", ""), (System.Int32.MaxValue * uom<Measure.nodeId>)),
                    export.ExportName)
        })

/// The tier's pass list, in execution order.
let passes: Pass<HarvestModel> list =
    [ harvestExports; harvestGlobals; harvestDocs; orderExports ]
