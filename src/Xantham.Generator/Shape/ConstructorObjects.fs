module Xantham.Generator.Shape.ConstructorObjects

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Names the *constructor objects* (§4.4) reached at a reference position - `typeof Request` in
/// a member type - as an interface `RequestConstructor` whose `Create` members are its construct
/// signatures. A class export's own value type is left out; `shape-classes` already emits it.
let nameConstructorObjects: Pass<ShapeModel> =
    Pass.pure' "name-constructor-objects" (fun ctx model ->
        let fallback = defaultExportName ctx

        let exportNames =
            model.Harvest.Exports
            |> List.fold
                (fun found export ->
                    if hasAny SymbolFlags.Class export.Symbol.Flags then
                        found
                    else
                        match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Value with
                        | Some typeId when not (Map.containsKey typeId found) ->
                            Map.add typeId (fsName fallback export) found
                        | _ -> found)
                Map.empty

        let mutable names = model.DeclNames
        let mutable orders = model.DeclOrders
        let mutable taken = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList
        let mutable visited = Set.empty
        let mutable claimed = []

        let claim (preferred: string) typeId order =
            let unique =
                if not (Set.contains preferred taken) then
                    preferred
                else
                    Seq.initInfinite (fun i -> $"{preferred}{i + 2}")
                    |> Seq.find (fun candidate -> not (Set.contains candidate taken))

            names <- Map.add typeId unique names
            orders <- Map.add typeId order orders
            taken <- Set.add unique taken
            claimed <- claimed @ [ typeId, unique ]

        /// The name to declare a constructor object under: the export it is the value of, the
        /// name the author gave it, the instance side it constructs, or the path that reached
        /// it - in that order of how much the reader will recognize.
        let preferredName (path: string) (facts: TypeFacts) =
            let instanceName () =
                facts.ConstructSignatures
                |> List.tryPick (fun signature ->
                    Map.tryFind signature.ReturnTypeId names
                    |> Option.orElseWith (fun () ->
                        match Map.tryFind signature.ReturnTypeId model.Types with
                        | Some returns ->
                            match returns.Response.Target with
                            | ValueSome target -> Map.tryFind target names
                            | ValueNone -> None
                        | None -> None))

            // `pascalSegment` splits on dots, so only a source name passes through it. An
            // instance side's name and the path are F# names already, and dotted where the
            // shape nests under its owner.
            let stem =
                Map.tryFind facts.Response.Id exportNames
                |> Option.orElseWith (fun () -> facts.SymbolName |> Option.filter (isSyntheticName >> not))
                |> Option.map Naming.pascalSegment
                |> Option.orElseWith instanceName
                |> Option.defaultValue path

            // The lib spells its own static sides `ErrorConstructor` already; doubling the
            // suffix would only make the name harder to match against the `.d.ts`.
            if stem.EndsWith "Constructor" then
                stem
            else
                $"{stem}Constructor"

        /// A shape this run is entitled to declare: the entry package's own, or an anonymous
        /// one, which belongs to whatever transformed it (D6). A referenced group's is that
        /// group's to declare; an identity-only one carries no signatures.
        let declarable (facts: TypeFacts) =
            GeneratorConfig.disposition ctx.Config facts.Origin = Ship
            || facts.SymbolName |> Option.forall isSyntheticName

        /// The reference positions a declaration reads.
        let positions (facts: TypeFacts) =
            [
                for m in facts.Members do
                    if not (isSymbolKeyed m.Symbol.Name) then
                        Naming.pascalSegment m.Symbol.Name, m.TypeId
                for info in facts.IndexInfos do
                    "Item", info.KeyTypeId
                    "Item", info.ValueTypeId
                for signature in facts.CallSignatures @ facts.ConstructSignatures do
                    for p in signature.Parameters do
                        Naming.pascalSegment p.Symbol.Name, p.TypeId

                    "Result", signature.ReturnTypeId
                for memberId in facts.UnionMembers do
                    "", memberId
                for memberId in facts.IntersectionMembers do
                    "", memberId
                for argument in facts.TypeArguments do
                    "Item", argument
            ]

        // An unnamed shape is expanded into whatever reads it, so its positions are the reading
        // declaration's too and the descent continues through it. A named one stops the descent:
        // it is a root of its own.
        let rec descend (path: string) order (typeId: int) =
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited

                match Map.tryFind typeId model.Types with
                | None -> ()
                | Some facts ->
                    for segment, target in positions facts do
                        consider (if segment = "" then path else nestUnder path segment) order target

        and consider path order typeId =
            match Map.tryFind typeId model.Types with
            | Some facts when isConstructorObject facts ->
                if declarable facts && not (Map.containsKey typeId names) then
                    claim (preferredName path facts) typeId order
            | Some _ when not (Map.containsKey typeId names) -> descend path order typeId
            | _ -> ()

        // A non-class export's own value type is a root of its own: `declare const Pair: { new
        // (): P }` is referenced from nowhere else, and `Exports.Pair` is exactly the position
        // that wants the name.
        for export in model.Harvest.Exports do
            match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Value with
            | None -> ()
            | Some typeId ->
                let path = Naming.pascalSegment (fsName fallback export)

                if hasAny SymbolFlags.Class export.Symbol.Flags then
                    // A class's own static side stays `shape-classes`'s, but a static *of type*
                    // `typeof Other` reads through this pass like any other member position.
                    descend path export.Order typeId
                else
                    consider path export.Order typeId

        let exportRoots = claimed

        let mutable queue = (model.DeclNames |> Map.toList |> List.sortBy fst) @ exportRoots

        while not queue.IsEmpty do
            claimed <- []

            for typeId, name in queue do
                descend name (Map.tryFind typeId orders |> Option.defaultValue None) typeId

            queue <- claimed

        { model with
            DeclNames = names
            DeclOrders = orders
        })
