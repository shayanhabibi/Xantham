module Xantham.Generator.Shape.Ordering

open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// The nested-module path a dotted declaration name renders under - `[]` for a root name.
let private modulePath (name: string) =
    let segments = name.Split '.'
    if segments.Length <= 1 then [] else List.ofArray segments[.. segments.Length - 2]

/// A declaration's name, for ordering purposes: the `Exports` container's own name stands in
/// for a type declaration's.
let private nameFor =
    function
    | FsInterface decl -> decl.Name
    | FsStringEnum decl -> decl.Name
    | FsTaggedUnion decl -> decl.Name
    | FsEnum decl -> decl.Name
    | FsAbbrev decl -> decl.Name
    | FsDelegateType decl -> decl.Name
    | FsPhantom decl -> decl.Name
    | FsMeasure decl -> decl.Name
    | FsExports container -> container.Name

/// Root declarations first, then every nested module in alphabetical order of its path,
/// preserving the order already fixed within each.
let private byModulePath decls =
    decls |> List.sortBy (nameFor >> modulePath >> fun path -> (List.isEmpty path |> not), path)

/// Fixes the output order the renderer will follow verbatim: declarations in source order with
/// name as the tiebreak, then the `Exports` type - its members in harvest order - last; root
/// declarations before every specifier module, each module alphabetical by its path.
let orderDeclarations: Pass<ShapeModel> =
    Pass.pure' "order-declarations" (fun ctx model ->
        let orderKey (order: DeclOrder option) (name: string) =
            (match order with
             | Some order -> Grouping.sourceOrderKey ctx.PackageDir (order.File / uom<node>), order.NodeIndex
             | None -> (2, "", ""), (System.Int32.MaxValue * uom<nodeId>)),
            name

        let declarationNames, decls =
            model.Decls
            |> List.sortBy (function
                | FsInterface decl -> orderKey decl.Order decl.Name
                | FsStringEnum decl -> orderKey decl.Order decl.Name
                | FsTaggedUnion decl -> orderKey decl.Order decl.Name
                | FsEnum decl -> orderKey decl.Order decl.Name
                | FsAbbrev decl -> orderKey decl.Order decl.Name
                | FsDelegateType decl -> orderKey decl.Order decl.Name
                | FsPhantom decl -> orderKey decl.Order decl.Name
                | FsMeasure decl -> orderKey decl.Order decl.Name
                | FsExports _ -> ((2, "", ""), (System.Int32.MaxValue * uom<nodeId>)), "￿")
            |> List.map (function
                | FsInterface decl as declWrap -> decl.Name, declWrap
                | FsStringEnum decl as declWrap -> decl.Name, declWrap
                | FsTaggedUnion decl as declWrap -> decl.Name, declWrap
                | FsEnum decl as declWrap -> decl.Name, declWrap
                | FsAbbrev decl as declWrap -> decl.Name, declWrap
                | FsDelegateType decl as declWrap -> decl.Name, declWrap
                | FsPhantom decl as declWrap -> decl.Name, declWrap
                | FsMeasure decl as declWrap -> decl.Name, declWrap
                | FsExports decl as declWrap -> "", declWrap)
            |> List.unzip

        let exports =
            model.ExportMembers
            |> List.sortBy (fun owned -> owned.HarvestIndex, owned.Member.Name)

        let containers = ExportLayout.containersFor model (exports |> List.map _.Owner)

        let exportDecls =
            exports
            |> List.groupBy _.Owner
            |> List.choose (function
                | _, [] -> None
                | owner, exports ->
                    Some
                    <| FsExports
                        {
                            Name =
                                containers
                                |> Map.tryFind owner
                                |> Option.defaultWith (fun () -> ExportLayout.containerName declarationNames owner [])
                            FsExportContainer.Owner = owner
                            Members = exports
                        })
            |> List.sortBy (function
                | FsExports container -> container.Name
                | _ -> "")

        { model with
            Decls = byModulePath (decls @ exportDecls)
            ExportMembers = []
        })
