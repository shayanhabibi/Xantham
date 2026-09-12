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

                    // An export is represented by a declaration carrying its name, by a declaration nested
                    // under a module of its name (a TS namespace), or by a declaration whose final segment is
                    // its name (a specifier-scoped or namespace-contested type).
                    let represented (export: HarvestedExport) =
                        let exported = name export

                        Set.contains exported generated
                        || generated
                           |> Set.exists (fun declared ->
                               declared.StartsWith(exported + ".") || declared.EndsWith("." + exported))

                    let missing =
                        model.Harvest.Exports
                        |> List.filter (fun export ->
                            export.HasValueExport
                            || hasAny (SymbolFlags.Type ||| SymbolFlags.Module) export.Symbol.Flags)
                        |> List.filter (represented >> not)
                        |> List.map (fun export -> Finding.make (name export) AuditCoverage.ExportNotRepresented)

                    return
                        if List.isEmpty missing then
                            Advanced model
                        else
                            Degraded(model, missing)
                }
    }
