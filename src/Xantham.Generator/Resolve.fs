/// Tier 2 - Resolve: the type table. Breadth-first from the harvested exports' types, batched
/// per generation frontier through the mailbox, memoized on `TypeResponse.Id`. The tier's
/// invariant is closure: every type id a `TypeFacts` refers to is in the table or recorded in
/// `NotFollowed` with its reason.
module Xantham.Generator.Resolve

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Generations the breadth-first walk follows before recording the rest as deliberately not
/// followed. The frontier doubles as the cycle boundary - ids already derived are never
/// re-entered - so this bounds runaway utility-type expansion, not recursion.
[<Literal>]
let private FollowDepth = 12

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// Whether a type's own symbol is declared outside the package being generated. Such types -
/// the standard library, dependencies - widen to `obj` in phase A, so deriving their members
/// would be wire traffic nothing reads; worse, one `RegExp` reaches most of the standard
/// library transitively. Anonymous shapes (no symbol, or no declaration) always derive.
let private declaredOutside (ctx: Context) (symbol: SymbolResponse voption) =
    match symbol |> ValueOption.bind (fun s -> Harvest.declOrder s.Declarations |> ValueOption.ofOption) with
    | ValueNone -> false
    | ValueSome order ->
        let normalize (path: string) = path.Replace('\\', '/').TrimEnd '/'

        not (
            (normalize order.File)
                .StartsWith(normalize ctx.PackageDir + "/", System.StringComparison.OrdinalIgnoreCase)
        )

/// The type ids each export resolves to: the declared type for type-like symbols, the value
/// type for value-like ones, both for symbols that are both (a class). The responses land in
/// the table shallow; `resolveTypeTable` derives them.
let resolveExportTypes: Pass<ResolveModel> =
    { Name = "resolve-export-types"
      Run =
        fun ctx model ->
            async {
                let! resolved =
                    model.Harvest.Exports
                    |> List.map (fun export ->
                        async {
                            let! declared =
                                if hasAny SymbolFlags.Type export.Symbol.Flags then
                                    async {
                                        let! ty = ctx.Session.getDeclaredTypeOfSymbol export.Symbol.Id
                                        return Some ty
                                    }
                                else
                                    async.Return None

                            let! value =
                                if hasAny SymbolFlags.Value export.Symbol.Flags then
                                    async {
                                        let! ty = ctx.Session.getTypeOfSymbol export.Symbol.Id
                                        return Some ty
                                    }
                                else
                                    async.Return None

                            return export.Symbol.Id, declared, value
                        })
                    |> Async.Parallel

                let exportTypes =
                    resolved
                    |> Array.fold
                        (fun map (symbolId, declared, value) ->
                            Map.add
                                symbolId
                                { Declared = declared |> Option.map _.Id
                                  Value = value |> Option.map _.Id }
                                map)
                        model.ExportTypes

                let types =
                    resolved
                    |> Array.collect (fun (_, declared, value) ->
                        [| yield! Option.toArray declared; yield! Option.toArray value |])
                    |> Array.fold (fun map ty -> Map.add ty.Id (TypeFacts.shallow ty) map) model.Types

                return
                    Advanced
                        { model with
                            ExportTypes = exportTypes
                            Types = types }
            } }

/// Derives one type's facts and reports the responses it discovered, for the next frontier.
let private deriveFacts (ctx: Context) (ty: TypeResponse) : Async<TypeFacts * TypeResponse list> =
    async {
        let has flag = ty.Flags.HasFlag(flag: TypeFlags)

        // `boolean` is a union of `true | false` wearing the Boolean flag; it maps as a
        // primitive, so its members are not worth a round trip.
        if has TypeFlags.Union && not (has TypeFlags.Boolean) then
            let! members = ctx.Session.getTypesOfType ty.Id
            let members = members |> ValueOption.map Array.toList |> ValueOption.defaultValue []

            return
                { TypeFacts.shallow ty with
                    UnionMembers = members |> List.map _.Id },
                members
        elif has TypeFlags.Object then
            let! symbol = ctx.Session.getSymbolOfType ty.Id

            if declaredOutside ctx symbol then
                // Kept shallow on purpose: the shape tier widens references to it, and the
                // symbol name kept here is what makes that finding legible.
                return
                    { TypeFacts.shallow ty with
                        SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption },
                    []
            else

            let! properties = ctx.Session.getPropertiesOfType ty.Id
            let properties = properties |> ValueOption.defaultValue [||]

            let resolveMember readOnlyRelevant (property: SymbolResponse) =
                async {
                    let! propertyType = ctx.Session.getTypeOfSymbol property.Id
                    let! docs = ctx.Session.getDocumentationComment property.Id
                    let! tags = ctx.Session.getJsDocTags property.Id

                    // `CheckFlags.Readonly` only marks transient symbols; a declared
                    // `readonly` modifier is the checker's to see, so ask it.
                    let! readOnly =
                        if readOnlyRelevant then
                            ctx.Session.isReadonlySymbol property.Id
                        else
                            async.Return false

                    return
                        { Symbol = property
                          Docs = docs
                          Tags = tags |> ValueOption.map Array.toList |> ValueOption.defaultValue []
                          Optional =
                            property.Flags.HasFlag SymbolFlags.Optional
                            || property.CheckFlags.HasFlag CheckFlags.OptionalParameter
                          ReadOnly = readOnly
                          TypeId = propertyType.Id },
                        propertyType
                }

            let! members = properties |> Array.map (resolveMember true) |> Async.Parallel
            let! signatures = ctx.Session.getSignaturesOfType (ty.Id, SignatureKind.Call)

            let! signatureFacts =
                signatures
                |> Array.map (fun signature ->
                    async {
                        let! parameters = ctx.Session.getParametersOfSignature signature.Id
                        let parameters = parameters |> ValueOption.defaultValue [||]
                        let! parameterFacts = parameters |> Array.map (resolveMember false) |> Async.Parallel
                        let! returnType = ctx.Session.getReturnTypeOfSignature signature.Id

                        return
                            { Parameters = parameterFacts |> Array.map fst |> Array.toList
                              ReturnTypeId = returnType.Id },
                            [ yield! parameterFacts |> Array.map snd; returnType ]
                    })
                |> Async.Parallel

            let discovered =
                [ yield! members |> Array.map snd
                  yield! signatureFacts |> Array.collect (snd >> List.toArray) ]

            return
                { Response = ty
                  SymbolName = symbol |> ValueOption.map _.Name |> ValueOption.toOption
                  Members = members |> Array.map fst |> Array.toList
                  CallSignatures = signatureFacts |> Array.map fst |> Array.toList
                  UnionMembers = [] },
                discovered
        else
            return TypeFacts.shallow ty, []
    }

/// Builds the closed type table: derive the current frontier (sorted by id, so the fold is
/// deterministic whatever order answers arrive in), collect what derivation discovered, and
/// recurse until the frontier is exhausted or the depth cutoff records the remainder.
let resolveTypeTable: Pass<ResolveModel> =
    { Name = "resolve-type-table"
      Run =
        fun ctx model ->
            async {
                let rec walk table derived notFollowed findings frontier depth =
                    async {
                        let fresh =
                            frontier
                            |> List.distinctBy (fun (ty: TypeResponse) -> ty.Id)
                            |> List.filter (fun ty -> not (Set.contains ty.Id derived))
                            |> List.sortBy _.Id

                        match fresh with
                        | [] -> return table, notFollowed, findings
                        | fresh when depth > FollowDepth ->
                            let notFollowed =
                                fresh
                                |> List.fold
                                    (fun map ty -> Map.add ty.Id $"beyond the depth cutoff ({FollowDepth})" map)
                                    notFollowed

                            let findings =
                                findings
                                @ [ for ty in fresh ->
                                        Finding.make
                                            Widened
                                            $"type#{ty.Id}"
                                            $"not resolved: beyond the depth cutoff ({FollowDepth})" ]

                            return table, notFollowed, findings
                        | fresh ->
                            let! results = fresh |> List.map (deriveFacts ctx) |> Async.Parallel

                            let table =
                                results
                                |> Array.fold (fun map (facts, _) -> Map.add facts.Response.Id facts map) table

                            let derived = fresh |> List.fold (fun set ty -> Set.add ty.Id set) derived
                            let discovered = results |> Array.toList |> List.collect snd
                            return! walk table derived notFollowed findings discovered (depth + 1)
                    }

                let seeds = model.Types |> Map.toList |> List.map (fun (_, facts) -> facts.Response)
                let! table, notFollowed, findings = walk model.Types Set.empty model.NotFollowed [] seeds 0

                let model =
                    { model with
                        Types = table
                        NotFollowed = notFollowed }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, findings)
            } }

/// The tier's pass list, in execution order.
let passes: Pass<ResolveModel> list = [ resolveExportTypes; resolveTypeTable ]
