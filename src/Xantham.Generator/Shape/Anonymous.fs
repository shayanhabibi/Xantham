module Xantham.Generator.Shape.Anonymous

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Walks the type graph reachable from the exports in deterministic order and names what needs
/// a declaration but has none (§4.4, hash-consed by type id). Non-exported named entry types
/// keep their name; the anonymous rest take path-derived ones, with a numeric suffix on clash.
let synthesizeAnonymous: Pass<ShapeModel> =
    Pass.pure' "synthesize-anonymous" (fun ctx model ->
        let mutable names = model.DeclNames
        let mutable orders = model.DeclOrders
        let mutable taken = model.DeclNames |> Map.toList |> List.map snd |> Set.ofList
        let mutable visited = Set.empty

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

        /// A literal union worth a declaration: at least two non-nullish members, all literal,
        /// not just `true | false`, and no already-named union with the same member set.
        let isLiteralUnion (facts: TypeFacts) =
            let _, remaining = splitNullish model facts

            remaining.Length > 1
            && remaining
               |> List.forall (fun id ->
                   match Map.tryFind id model.Types with
                   | Some m -> (literalOf m).IsSome
                   | None -> false)
            && not (isBooleanPair model remaining)
            && (namedUnionByMembers { model with DeclNames = names } remaining).IsNone

        let needsName (facts: TypeFacts) =
            if Map.containsKey facts.Response.Id names then
                false
            elif flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
                isLiteralUnion facts
            elif flag TypeFlags.Object facts then
                // Entry-group object shapes with members become interfaces; callbacks stay
                // inline as delegates, arrays as arrays, tuples as F# tuples (D7). An anonymous
                // shape belongs to the entry package whatever file its node sits in (D6).
                (GeneratorConfig.disposition ctx.Config facts.Origin = Ship
                 || facts.SymbolName |> Option.forall isSyntheticName)
                && not (isPureCallback facts)
                && (arrayElement facts).IsNone
                && not (isTuple facts)
                && facts.ConstructSignatures.IsEmpty
                // An index signature is shape too: `Record<string, boolean>` has no members
                // and one index signature, and is an interface of one `Item`.
                && not (facts.Members.IsEmpty && facts.IndexInfos.IsEmpty)
                // An instantiation of a generic this run declares is written as an
                // application (§4.9). Naming it would declare the expansion a second time
                // under a made-up name and lose the tie to the generic it came from.
                && (instantiationOf { model with DeclNames = names } facts).IsNone
            elif flag TypeFlags.Intersection facts then
                // An intersection of object types is one flattened interface (§4.6): the
                // resolve tier read its members off the intersection itself, so it names and
                // declares like any anonymous shape. An operand-only intersection widens.
                isFlattenable model facts
            else
                false

        let rec walk (path: string) (order: DeclOrder option) (typeId: int) =
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited

                match Map.tryFind typeId model.Types with
                | None -> ()
                | Some facts ->
                    // The generic declaration behind an instantiation is named ahead of it, so
                    // `Ready<T>` reached only through `Resource<T> = Ready<T> | ...` declares
                    // `Ready<'T>` once and instantiations are applications of it (§4.9).
                    match facts.Response.Target with
                    | ValueSome target when target <> typeId && Map.containsKey target model.Types ->
                        walk path order target
                    | _ -> ()

                    if needsName facts then
                        let preferred =
                            match facts.SymbolName with
                            | Some name when not (isSyntheticName name) -> Naming.pascalSegment name
                            | _ -> path

                        claim preferred typeId order

                    // Recurse in the shape the declaration will read: members, signatures,
                    // union members, then structural identity (element, arguments, bases).
                    let named = Map.tryFind typeId names

                    let into segment =
                        (named |> Option.defaultValue path) + segment

                    // An instantiation of a named declaration reads only its arguments: its
                    // members are the declaration's, substituted, and shaping happens there.
                    if (instantiationOf { model with DeclNames = names } facts).IsSome then
                        for argument in facts.TypeArguments do
                            walk (into "Item") order argument
                    else

                        // A tuple reads only its components and an array only its element, so
                        // `length`, the numeric indices and `Array<T>`'s lib members claim no
                        // names. A symbol-keyed member is dropped at render, so nor does its type.
                        if not (isTuple facts) && (arrayElement facts).IsNone then
                            for m in facts.Members do
                                if not (isSymbolKeyed m.Symbol.Name) then
                                    walk (into (Naming.pascalSegment m.Symbol.Name)) order m.TypeId

                        // An index signature's value is shape the declaration reads too:
                        // `Record<string, A & B>` reaches its intersection nowhere else.
                        for info in facts.IndexInfos do
                            walk (into "Item") order info.ValueTypeId

                        let signatures = facts.CallSignatures @ facts.ConstructSignatures

                        for signature in signatures do
                            for p in signature.Parameters do
                                walk (into (Naming.pascalSegment p.Symbol.Name)) order p.TypeId

                            walk (into "Result") order signature.ReturnTypeId

                        for memberId in facts.UnionMembers do
                            walk (named |> Option.defaultValue path) order memberId

                        for argument in facts.TypeArguments do
                            walk (into "Item") order argument

                        for baseId in facts.BaseTypes do
                            walk (into "Base") order baseId

        let fallback = defaultExportName ctx

        for export in model.Harvest.Exports do
            let root = Naming.pascalSegment (fsName fallback export)

            match Map.tryFind export.Symbol.Id model.ExportTypes with
            | Some ids ->
                for typeId in [ yield! Option.toList ids.Declared; yield! Option.toList ids.Value ] do
                    walk root export.Order typeId
            | None -> ()

        { model with
            DeclNames = names
            DeclOrders = orders
        })
