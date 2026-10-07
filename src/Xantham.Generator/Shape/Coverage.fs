module Xantham.Generator.Shape.Coverage

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The no-silent-drops check: every harvested export either appears in the declarations or is
/// the subject of a finding this pass adds. Passes that drop already say so, so overlap is
/// possible - this is the safety net, not the reporter of record.
let auditCoverage: Pass<ShapeModel> =
    {
        Name = "audit-coverage"
        Run =
            fun ctx model ->
                async {
                    let generated =
                        model.Decls
                        |> List.collect (function
                            | FsInterface decl -> [ decl.Name ]
                            | FsStringEnum decl -> [ decl.Name ]
                            | FsTaggedUnion decl -> [ decl.Name ]
                            | FsEnum decl -> [ decl.Name ]
                            | FsAbbrev decl -> [ decl.Name ]
                            | FsDelegateType decl -> [ decl.Name ]
                            | FsPhantom decl -> [ decl.Name ]
                            | FsMeasure decl -> [ decl.Name ]
                            | FsExports container -> container.Members |> List.map (fun owned -> owned.Member.Name))
                        |> Set.ofList

                    let name = fsName (defaultExportName ctx)

                    // The fully qualified name assigned by `name-exports`, which every other
                    // pass's finding also reports: `Store.SetStoreFunction` for a subpath export
                    // nested under its module. A value export falls back to its bare export
                    // name, `DeclNames` covering type exports only.
                    let qualifiedName (export: HarvestedExport) =
                        model.ExportTypes
                        |> Map.tryFind export.Symbol.SymbolId
                        |> Option.bind _.Declared
                        |> Option.bind (fun typeId -> Map.tryFind typeId model.DeclNames)
                        |> Option.defaultValue (name export)

                    // An export is represented by a declaration carrying its name, by a declaration nested
                    // under a module of its name (a TS namespace), or by a declaration whose final segment is
                    // its name (a specifier-scoped or namespace-contested type), or by a declaration
                    // carrying the name assigned by `name-exports` (`$Shape` declared `Shape`).
                    let represented (export: HarvestedExport) =
                        let exported = name export

                        Set.contains exported generated
                        || Set.contains (qualifiedName export) generated
                        || generated
                           |> Set.exists (fun declared ->
                               declared.StartsWith(exported + ".") || declared.EndsWith("." + exported))

                    // The parent symbols of every harvested export, identifying which namespaces
                    // have harvested members.
                    let namespacesWithMembers =
                        model.Harvest.Exports
                        |> List.choose (fun export -> export.Symbol.ParentSymbolId |> ValueOption.toOption)
                        |> Set.ofList

                    // A namespace export whose declared members all omit the `export` keyword -
                    // `SolidStore.Unwrappable` in `solid-js`. Harvest yields it empty, so its type
                    // and value surface is empty and a declaration would represent nothing.
                    let opaqueNamespace (export: HarvestedExport) =
                        hasAny SymbolFlags.Module export.Symbol.Flags
                        && not (hasAny SymbolFlags.Type export.Symbol.Flags)
                        && not export.HasValueExport
                        && not (Set.contains export.Symbol.SymbolId namespacesWithMembers)

                    let missing =
                        model.Harvest.Exports
                        |> List.filter (fun export ->
                            export.HasValueExport
                            || hasAny (SymbolFlags.Type ||| SymbolFlags.Module) export.Symbol.Flags)
                        |> List.filter (opaqueNamespace >> not)
                        |> List.filter (represented >> not)
                        |> List.map (fun export ->
                            Finding.make (qualifiedName export) AuditCoverage.ExportNotRepresented)

                    return
                        if List.isEmpty missing then
                            Advanced model
                        else
                            Degraded(model, missing)
                }
    }
