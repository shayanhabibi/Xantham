module Xantham.Generator.Shape.ExportCollisions

open System
open System.Security.Cryptography
open System.Text
open Xantham.Generator
open Xantham.Generator.Measure

let resolveExportCollisions: Pass<ShapeModel> = {
    Name = "resolve-export-collisions"
    Run = fun ctx model -> async {
        let mutable findings: Finding list = []
        let exportContainers, others =
            model.Decls
            |> List.partitionWith (function
                | FsExports fsExportContainer -> Choice1Of2 fsExportContainer
                | o -> Choice2Of2 o
                )

        let abbrevs =
            model.Decls
            |> List.choose (function
                | FsAbbrev decl -> Some(decl.Name, decl.Target)
                | _ -> None)
            |> Map.ofList
            
        /// The reference with abbreviations expanded, so `TargetsParam` and
        /// `DOMTargetsParam` (both `obj`) compare equal the way the compiler sees them.
        let rec normalize (visited: Set<string>) (reference: FsTypeRef) : FsTypeRef =
            match reference with
            | FsNamed name when Map.containsKey name abbrevs && not (Set.contains name visited) ->
                normalize (Set.add name visited) abbrevs[name]
            | FsOption inner -> FsOption(normalize visited inner)
            | FsArray element -> FsArray(normalize visited element)
            | FsDelegate(args, ret) ->
                FsDelegate(args |> List.map (normalize visited), normalize visited ret)
            | FsFunc(argument, ret) -> FsFunc(normalize visited argument, normalize visited ret)
            | other -> other

        /// A reference with its own signature's type variables renamed by declaration
        /// order, so `<A extends T>(value: A): A` and `<B extends T>(value: B): B`
        /// compare equal once their dropped constraints leave both as `'T0 -> 'T0` -
        /// .NET overload resolution does not see a type parameter's name.
        let rec renameTypeVars (rename: Map<string, string>) (reference: FsTypeRef) : FsTypeRef =
            let recur = renameTypeVars rename

            match reference with
            | FsTypeVar name -> FsTypeVar(rename |> Map.tryFind name |> Option.defaultValue name)
            | FsOption inner -> FsOption(recur inner)
            | FsArray element -> FsArray(recur element)
            | FsTuple components -> FsTuple(List.map recur components)
            | FsErasedUnion arms -> FsErasedUnion(List.map recur arms)
            | FsDelegate(args, ret) -> FsDelegate(List.map recur args, recur ret)
            | FsFunc(argument, ret) -> FsFunc(recur argument, recur ret)
            | FsApp(name, args) -> FsApp(name, List.map recur args)
            | FsBranded(primitive, measure) -> FsBranded(recur primitive, measure)
            | other -> other

        let signatureKey (typeParameters: FsTypeParam list) (parameters: FsParam list) =
            let rename = typeParameters |> List.mapi (fun i p -> p.Name, $"T{i}") |> Map.ofList

            parameters
            |> List.map (fun p -> p.Optional, p.Rest, normalize Set.empty (renameTypeVars rename p.Type))

        let exportContainers =
            exportContainers
            |> List.map (fun exportContainer ->
                let mutable seenExports = Set.empty
                let exportMembers =
                    exportContainer.Members
                    |> List.filter (fun { OwnedExportMember.Member = m } ->
                        let key, dropped =
                            match m.Body with
                            | ExportFunction(parameters, _) ->
                                Some("fn", signatureKey [] parameters),
                                DedupeOverloads.ExportFunctionOverloadDropped
                            | ExportConstructor(parameters, _) ->
                                Some("new", signatureKey [] parameters), DedupeOverloads.OverloadDropped
                            | ExportValue _ -> None, DedupeOverloads.OverloadDropped

                        match key with
                        | None -> true
                        | Some key ->
                            let key = (m.Name, key).ToString()

                            if Set.contains key seenExports then
                                findings <- findings @ [ Finding.make m.Name dropped ]

                                false
                            else
                                seenExports <- Set.add key seenExports
                                true)
                FsExports { exportContainer with Members = exportMembers }
                )
        
        let model = { model with Decls = others @ exportContainers }
        
        return if List.isEmpty findings then Advanced model else Degraded(model, findings)
    }
}
