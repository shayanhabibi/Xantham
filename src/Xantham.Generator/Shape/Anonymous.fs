module Xantham.Generator.Shape.Anonymous

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// Walks the type graph reachable from the exports in deterministic order and names what needs
/// a declaration but has none: anonymous entry-group object types with members, and literal
/// unions (hash-consing by type id, §4.4). Named non-exported entry types keep their own name;
/// path-derived names cover the anonymous rest; collisions take a numeric suffix in visit
/// order.
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
                // inline as delegates, arrays as arrays, tuples as F# tuples (D7). Constructor
                // objects (a class's static side) get their constructors on `Exports`, not a
                // declaration.
                // An anonymous shape is the entry package's whatever file its node sits in:
                // `Record<string, boolean>` is written in `lib.es5.d.ts`, but what it stands
                // for is this package's operand, transformed (D6) - the resolve tier already
                // reads it by content, and the disposition of the lib is not its to inherit.
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
                // declares like any anonymous shape. A brand is a measure, named elsewhere;
                // an intersection with no members (a type-parameter operand) has nothing to
                // declare and widens at the reference.
                isFlattenable model facts
            else
                false

        let rec walk (path: string) (order: DeclOrder option) (typeId: int) =
            if not (Set.contains typeId visited) then
                visited <- Set.add typeId visited

                match Map.tryFind typeId model.Types with
                | None -> ()
                | Some facts ->
                    // The generic declaration behind an instantiation is named ahead of it,
                    // so that `Ready<T>` reached only through `Resource<T> = Ready<T> | ...`
                    // declares `Ready<'T>` once and every instantiation is written as an
                    // application of it (§4.9) - never a second copy of the expansion under
                    // a made-up name.
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
                    // Walking them would claim names for substituted anonymous member types
                    // that nothing then references.
                    if (instantiationOf { model with DeclNames = names } facts).IsSome then
                        for argument in facts.TypeArguments do
                            walk (into "Item") order argument
                    else

                        // A tuple declaration reads only its components, so its members - `length`
                        // and the numeric indices - are not part of the shape and must not claim
                        // names: `[number, number?]` would otherwise declare its own `1 | 2` length
                        // as an enum nothing references. An array reads only its element, for the
                        // same reason: `Array<T>`'s own members are the lib's, and the anonymous
                        // shape behind its `[Symbol.unscopables]` is nothing a declaration reads.
                        // A symbol-keyed member is dropped at render (unrepresentable), so its
                        // type is not shape either - and its name carries a session-specific id.
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
