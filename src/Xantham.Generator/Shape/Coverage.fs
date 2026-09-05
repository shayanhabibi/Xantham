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
                            | FsPhantom decl -> [ decl.Name ]
                            | FsMeasure decl -> [ decl.Name ]
                            | FsExports members -> members |> List.map _.Name)
                        |> Set.ofList

                    let name = fsName (defaultExportName ctx)

                    // A namespace arrives as the module the declarations written inside it nest
                    // in, so a name under it stands for the export where no declaration carries
                    // the export's own name.
                    let represented (export: HarvestedExport) =
                        let exported = name export

                        Set.contains exported generated
                        || generated |> Set.exists (fun declared -> declared.StartsWith(exported + "."))

                    let missing =
                        model.Harvest.Exports
                        |> List.filter (represented >> not)
                        |> List.map (fun export -> Finding.make (name export) AuditCoverage.ExportNotRepresented)

                    return
                        if List.isEmpty missing then
                            Advanced model
                        else
                            Degraded(model, missing)
                }
    }
