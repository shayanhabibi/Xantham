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

                    // A type export's own declaration carries the fully qualified name
                    // `name-exports` assigned it - `Store.SetStoreFunction` for a subpath export
                    // nested under its module - which is the name every other pass's finding
                    // names it by. Falls back to the bare export name for a value export, which
                    // `name-exports` does not assign a `DeclNames` entry to.
                    let qualifiedName (export: HarvestedExport) =
                        model.ExportTypes
                        |> Map.tryFind export.Symbol.SymbolId
                        |> Option.bind _.Declared
                        |> Option.bind (fun typeId -> Map.tryFind typeId model.DeclNames)
                        |> Option.defaultValue (name export)

                    // An export is represented by a declaration carrying its name, by a declaration nested
                    // under a module of its name (a TS namespace), or by a declaration whose final segment is
                    // its name (a specifier-scoped or namespace-contested type).
                    let represented (export: HarvestedExport) =
                        let exported = name export

                        Set.contains exported generated
                        || generated
                           |> Set.exists (fun declared ->
                               declared.StartsWith(exported + ".") || declared.EndsWith("." + exported))

                    // The parent symbols of every harvested export, for telling a namespace with
                    // harvested members apart from one with none.
                    let namespacesWithMembers =
                        model.Harvest.Exports
                        |> List.choose (fun export -> export.Symbol.ParentSymbolId |> ValueOption.toOption)
                        |> Set.ofList

                    // A namespace export whose own declared members carry none of them the `export`
                    // keyword - `SolidStore.Unwrappable` in `solid-js` - reaches harvest with no
                    // member of its own, and holds no type or value surface a declaration could ever
                    // carry on its behalf.
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
