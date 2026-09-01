/// Tier 3 - Shape: the mapping document executed, minimally. Phase A of
/// `docs/plans/generator-architecture.md` covers interfaces, functions, primitives and
/// `option`; everything richer widens to `obj` with a finding, so the fidelity manifest - not
/// silence - says what the skeleton does not do yet. Every pass here is pure.
module Xantham.Generator.Shape

open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

let private hasAny (mask: SymbolFlags) (flags: SymbolFlags) = uint32 (flags &&& mask) <> 0u

/// The fallback name for a default export - `Naming.defaultExport` over the entry package.
let defaultExportName (ctx: Context) = Naming.defaultExport ctx.PackageName

/// The F# name a harvested export generates under: the exported name; a default export takes
/// its declaring symbol's name - except `export default function name` binds the symbol itself
/// as `default`, which falls back to the package-derived name.
let fsName (fallback: string) (export: HarvestedExport) =
    if export.ExportName <> "default" then export.ExportName
    elif export.Symbol.Name = "default" || export.Symbol.Name.StartsWith "__" then fallback
    else export.Symbol.Name

/// The F# type written at a reference position, with the findings any widening produces.
/// Order of the flag tests matters twice: literals before their base primitives, and `boolean`
/// (a union wearing the Boolean flag) before the union case.
let rec typeRef (ctx: Context) (model: ShapeModel) (owner: string) (typeId: int) : FsTypeRef * Finding list =
    match Map.tryFind typeId model.Types with
    | None ->
        match Map.tryFind typeId model.NotFollowed with
        | Some reason -> FsObj, [ Finding.make Widened owner $"type not resolved ({reason}); widened to obj" ]
        | None -> FsObj, [ Finding.make Escape owner $"type#{typeId} missing from the type table; widened to obj" ]
    | Some facts ->
        let has flag = facts.Response.Flags.HasFlag(flag: TypeFlags)

        if has TypeFlags.Boolean || has TypeFlags.BooleanLiteral then
            FsBool, []
        elif has TypeFlags.StringLiteral then
            FsString, [ Finding.make Widened owner "string literal type widened to string (literal unions are phase B)" ]
        elif has TypeFlags.NumberLiteral then
            FsFloat, [ Finding.make Widened owner "numeric literal type widened to float (literal unions are phase B)" ]
        elif has TypeFlags.String then
            FsString, []
        elif has TypeFlags.Number then
            FsFloat, []
        elif has TypeFlags.Void || has TypeFlags.Undefined || has TypeFlags.Never then
            FsUnit, []
        elif has TypeFlags.Any then
            FsObj, [ Finding.make Escape owner "any maps to obj" ]
        elif has TypeFlags.Unknown then
            FsObj, [ Finding.make Widened owner "unknown maps to obj (D8)" ]
        elif has TypeFlags.Union then
            unionRef ctx model owner facts
        elif has TypeFlags.Object then
            let named =
                [ facts.Response.AliasSymbol; facts.Response.Symbol ]
                |> List.tryPick (fun symbol ->
                    symbol
                    |> ValueOption.toOption
                    |> Option.bind (fun id -> Map.tryFind id model.DeclNames))

            match named with
            | Some name -> FsNamed name, []
            | None ->
                match GeneratorConfig.disposition ctx.Config facts.Origin, facts.SymbolName with
                | Reference, Some typeName ->
                    // The O7 contract: a `ship` run of this group produces exactly this name.
                    FsNamed $"{Naming.groupModule ctx.PackageName facts.Origin}.{typeName}", []
                | Reference, None ->
                    FsObj,
                    [ Finding.make Widened owner "anonymous type in a referenced group cannot be templated; widened to obj" ]
                | (Ship | Widen), _ ->
                    let shown = facts.SymbolName |> Option.defaultValue "an anonymous object type"
                    FsObj, [ Finding.make Widened owner $"{shown} is not among the generated declarations; widened to obj" ]
        else
            FsObj, [ Finding.make Widened owner $"type flags {facts.Response.Flags} not mapped in phase A; widened to obj" ]

/// A union hoists its `null`/`undefined` members into `option` (D1); what remains must be a
/// single type for the skeleton to keep it, otherwise the union widens to `obj` (D4 union
/// classification is phase C).
and private unionRef (ctx: Context) (model: ShapeModel) (owner: string) (facts: TypeFacts) : FsTypeRef * Finding list =
    let nullish typeId =
        match Map.tryFind typeId model.Types with
        | Some memberFacts ->
            memberFacts.Response.Flags.HasFlag TypeFlags.Undefined
            || memberFacts.Response.Flags.HasFlag TypeFlags.Null
            || memberFacts.Response.Flags.HasFlag TypeFlags.Void
        | None -> false

    let hoisted, remaining = facts.UnionMembers |> List.partition nullish

    match remaining with
    | [ single ] ->
        let inner, findings = typeRef ctx model owner single

        if List.isEmpty hoisted then
            inner, findings
        else
            // Never nest: an already-optional inner stays one level, per the D1 note on
            // Fable's erased option being unsound when nested.
            let wrapped =
                match inner with
                | FsOption _ -> inner
                | inner -> FsOption inner

            wrapped, Finding.make Ergonomic owner "null/undefined union members hoisted to option" :: findings
    | [] -> FsUnit, [ Finding.make Widened owner "union of only null/undefined members maps to unit" ]
    | _ ->
        let baseRef, findings =
            FsObj, [ Finding.make Widened owner "union with several non-null members widened to obj (unions are phase C)" ]

        if List.isEmpty hoisted then
            baseRef, findings
        else
            FsOption baseRef, findings

/// An optional member or parameter reads as `option`, one level deep however the optionality
/// arrived (a `?` marker, an `undefined` union member, or both).
let private optionalRef (optional: bool) (reference: FsTypeRef) =
    match optional, reference with
    | false, reference -> reference
    | true, FsOption _ -> reference
    | true, reference -> FsOption reference

/// Names every type-like export before anything refers to one, so later passes see alias
/// references as `FsNamed` instead of expanding them.
let nameExports: Pass<ShapeModel> =
    Pass.pure' "name-exports" (fun ctx model ->
        { model with
            DeclNames =
                model.Harvest.Exports
                |> List.filter (fun export -> hasAny SymbolFlags.Type export.Symbol.Flags)
                |> List.map (fun export -> export.Symbol.Id, fsName (defaultExportName ctx) export)
                |> Map.ofList })

/// F# interfaces from type-like exports whose declared type is a plain object shape. Callable
/// or unresolved shapes are dropped with an Escape finding - phase B territory.
let shapeInterfaces: Pass<ShapeModel> =
    { Name = "shape-interfaces"
      Run =
        fun ctx model ->
            async {
                let mutable findings = []

                let emit finding = findings <- finding :: findings

                let decls =
                    model.Harvest.Exports
                    |> List.choose (fun export ->
                        if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                            None
                        else
                            let name = fsName (defaultExportName ctx) export

                            let declared =
                                Map.tryFind export.Symbol.Id model.ExportTypes
                                |> Option.bind (_.Declared >> ValueOption.toOption)
                                |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                            match declared with
                            | None ->
                                emit (Finding.make Escape name "no declared type in the table; export dropped")
                                None
                            | Some facts when not facts.CallSignatures.IsEmpty ->
                                emit (Finding.make Escape name "callable type not shaped in phase A; export dropped")
                                None
                            | Some facts ->
                                let members =
                                    facts.Members
                                    |> List.map (fun m ->
                                        let owner = $"{name}.{m.Symbol.Name}"
                                        let reference, refFindings = typeRef ctx model owner m.TypeId
                                        refFindings |> List.iter emit

                                        if m.Optional then
                                            emit (Finding.make Ergonomic owner "optional member reads as option")

                                        { Name = m.Symbol.Name
                                          Docs = m.Docs
                                          Tags = m.Tags
                                          ReadOnly = m.ReadOnly
                                          Type = optionalRef m.Optional reference })

                                Some(
                                    FsInterface
                                        { Name = name
                                          Docs = export.Docs
                                          Tags = export.Tags
                                          Order = export.Order
                                          Members = members }
                                ))

                let model = { model with Decls = model.Decls @ decls }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, List.rev findings)
            } }

/// The `Exports` erased type from value-like exports with call signatures: one member per
/// exported function, bound by name or as the default import. Overloads collapse to the first
/// signature with a finding (overloads are phase B).
let shapeExports: Pass<ShapeModel> =
    { Name = "shape-exports"
      Run =
        fun ctx model ->
            async {
                let mutable findings = []

                let emit finding = findings <- finding :: findings

                let members =
                    model.Harvest.Exports
                    |> List.choose (fun export ->
                        if not (hasAny SymbolFlags.Value export.Symbol.Flags) then
                            None
                        else
                            let name = fsName (defaultExportName ctx) export

                            let valueFacts =
                                Map.tryFind export.Symbol.Id model.ExportTypes
                                |> Option.bind (_.Value >> ValueOption.toOption)
                                |> Option.bind (fun typeId -> Map.tryFind typeId model.Types)

                            match valueFacts with
                            | None ->
                                emit (Finding.make Escape name "no value type in the table; export dropped")
                                None
                            | Some facts ->
                                match facts.CallSignatures with
                                | [] ->
                                    emit (
                                        Finding.make
                                            Escape
                                            name
                                            "value export without call signatures not shaped in phase A; dropped"
                                    )

                                    None
                                | signature :: rest ->
                                    if not rest.IsEmpty then
                                        emit (
                                            Finding.make
                                                Widened
                                                name
                                                $"{rest.Length + 1} overloads collapsed to the first signature (overloads are phase B)"
                                        )

                                    let parameters =
                                        signature.Parameters
                                        |> List.map (fun p ->
                                            let owner = $"{name}({p.Symbol.Name})"
                                            let reference, refFindings = typeRef ctx model owner p.TypeId
                                            refFindings |> List.iter emit

                                            // The wire does not flag optional parameters on their
                                            // symbols, so a parameter whose type admits `undefined`
                                            // (already hoisted to option by `typeRef`) is optional
                                            // too - D1 collapses the distinction anyway.
                                            let optional =
                                                p.Optional
                                                || (match reference with
                                                    | FsOption _ -> true
                                                    | _ -> false)

                                            if p.Optional then
                                                emit (Finding.make Ergonomic owner "optional parameter reads as option")

                                            { Name = p.Symbol.Name
                                              Optional = optional
                                              Type = optionalRef optional reference })

                                    let returnRef, returnFindings = typeRef ctx model $"{name}()" signature.ReturnTypeId
                                    returnFindings |> List.iter emit

                                    Some
                                        { Name = name
                                          Docs = export.Docs
                                          Tags = export.Tags
                                          Binding =
                                            if export.ExportName = "default" then
                                                ImportDefault
                                            else
                                                ImportNamed export.ExportName
                                          Parameters = parameters
                                          Return = returnRef })

                let model =
                    match members with
                    | [] -> model
                    | members -> { model with Decls = model.Decls @ [ FsExports members ] }

                return
                    if List.isEmpty findings then
                        Advanced model
                    else
                        Degraded(model, List.rev findings)
            } }

/// Fixes the output order the renderer will follow verbatim: interfaces in source order, the
/// `Exports` type last.
let orderDeclarations: Pass<ShapeModel> =
    Pass.pure' "order-declarations" (fun _ model ->
        { model with
            Decls =
                model.Decls
                |> List.sortBy (function
                    | FsInterface decl ->
                        0,
                        (match decl.Order with
                         | Some order -> order.File, order.NodeIndex
                         | None -> "￿", System.Int32.MaxValue),
                        decl.Name
                    | FsExports _ -> 1, ("", 0), "") })

/// The no-silent-drops check: every harvested export either appears in the declarations or is
/// the subject of a finding this pass adds. Passes that drop already say so, so overlap is
/// possible - this is the safety net, not the reporter of record.
let auditCoverage: Pass<ShapeModel> =
    { Name = "audit-coverage"
      Run =
        fun ctx model ->
            async {
                let generated =
                    model.Decls
                    |> List.collect (function
                        | FsInterface decl -> [ decl.Name ]
                        | FsExports members -> members |> List.map _.Name)
                    |> Set.ofList

                let name = fsName (defaultExportName ctx)

                let missing =
                    model.Harvest.Exports
                    |> List.filter (fun export -> not (Set.contains (name export) generated))
                    |> List.map (fun export ->
                        Finding.make Escape (name export) "export not represented in the generated output")

                return
                    if List.isEmpty missing then
                        Advanced model
                    else
                        Degraded(model, missing)
            } }

/// The tier's pass list, in execution order.
let passes: Pass<ShapeModel> list =
    [ nameExports; shapeInterfaces; shapeExports; orderDeclarations; auditCoverage ]
