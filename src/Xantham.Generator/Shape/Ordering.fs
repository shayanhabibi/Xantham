module Xantham.Generator.Shape.Ordering

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Fixes the output order the renderer will follow verbatim: declarations in source order with
/// name as the tiebreak, then the `Exports` type - its members in harvest order - last.
let orderDeclarations: Pass<ShapeModel> =
    Pass.pure' "order-declarations" (fun _ model ->
        let orderKey (order: DeclOrder option) (name: string) =
            (match order with
             | Some order -> order.File, order.NodeIndex
             | None -> "￿", System.Int32.MaxValue),
            name

        let decls =
            model.Decls
            |> List.sortBy (function
                | FsInterface decl -> orderKey decl.Order decl.Name
                | FsStringEnum decl -> orderKey decl.Order decl.Name
                | FsTaggedUnion decl -> orderKey decl.Order decl.Name
                | FsEnum decl -> orderKey decl.Order decl.Name
                | FsAbbrev decl -> orderKey decl.Order decl.Name
                | FsPhantom decl -> orderKey decl.Order decl.Name
                | FsMeasure decl -> orderKey decl.Order decl.Name
                | FsExports _ -> ("￿", System.Int32.MaxValue), "￿")

        let exports =
            model.ExportMembers
            |> List.sortBy (fun (index, m) -> index, m.Name)
            |> List.map snd

        { model with
            Decls =
                match exports with
                | [] -> decls
                | exports -> decls @ [ FsExports exports ]
            ExportMembers = []
        })
