module Xantham.Generator.Shape.Aliases

open Xantham.Generator
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator.Shape.Spec

/// `typeRef` with the type's own naming suppressed, for the right side of an abbreviation.
/// Declared unions with the same member set may only be matched at a *smaller* type id, so
/// alias chains strictly decrease; the smallest twin widens structurally instead.
let private typeRefIgnoringSelf
    (ctx: Context)
    (model: ShapeModel)
    (name: string)
    (facts: TypeFacts)
    : FsTypeRef * Finding list =
    let largerTwins =
        if flag TypeFlags.Union facts && not (flag TypeFlags.Boolean facts) then
            let wanted = nonNullishMemberSet model facts

            model.DeclNames
            |> Map.toList
            |> List.choose (fun (typeId, _) ->
                if typeId <= facts.Response.Id then
                    None
                else
                    match Map.tryFind typeId model.Types with
                    | Some candidate when flag TypeFlags.Union candidate && not (flag TypeFlags.Boolean candidate) ->
                        if nonNullishMemberSet model candidate = wanted then
                            Some typeId
                        else
                            None
                    | _ -> None)
        else
            []

    let unnamed =
        { model with
            DeclNames =
                largerTwins
                |> List.fold (fun names id -> Map.remove id names) (Map.remove facts.Response.Id model.DeclNames)
        }

    typeRef ctx unnamed None name facts.Response.Id

/// Abbreviations for the named types no earlier pass declared: aliases to primitives, arrays,
/// other named types, or whatever `typeRef` widens them to. Also covers a second export of an
/// already-named type.
let shapeAliases: Pass<ShapeModel> =
    {
        Name = "shape-aliases"
        Run =
            fun ctx model ->
                async {
                    let mutable findings = []

                    let declaredNames =
                        model.Decls
                        |> List.collect (function
                            | FsInterface decl -> [ decl.Name ]
                            | FsStringEnum decl -> [ decl.Name ]
                            | FsTaggedUnion decl -> [ decl.Name ]
                            | FsEnum decl -> [ decl.Name ]
                            | FsAbbrev decl -> [ decl.Name ]
                            | FsPhantom decl -> [ decl.Name ]
                            | FsMeasure decl -> [ decl.Name ]
                            | FsExports _ -> [])
                        |> Set.ofList

                    let fallback = defaultExportName ctx

                    // An abbreviation that stands in for an export - `type StringBox = Box<string>`
                    // reaches here rather than `shape-interfaces` - still carries that export's
                    // documentation; it is the only declaration the reader will see for it.
                    let exportDocs =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            Map.tryFind export.Symbol.Id model.ExportTypes
                            |> Option.bind _.Declared
                            |> Option.map (fun typeId -> typeId, (export.Docs, export.Tags)))
                        |> Map.ofList

                    // A generic declaration cannot be named bare on the right of an abbreviation -
                    // F# demands the full arity - so an alias to one repeats its parameters and
                    // applies them straight through: `type Alias<'T> = Primary<'T>`.
                    let parametersOf =
                        model.Decls
                        |> List.choose (function
                            | FsInterface decl -> Some(decl.Name, decl.TypeParameters)
                            | FsAbbrev decl -> Some(decl.Name, decl.TypeParameters)
                            | FsPhantom decl -> Some(decl.Name, decl.TypeParameters)
                            | _ -> None)
                        |> Map.ofList

                    // A second type-like export of an already-named type abbreviates to it.
                    let aliasDecls =
                        model.Harvest.Exports
                        |> List.choose (fun export ->
                            if not (hasAny SymbolFlags.Type export.Symbol.Flags) then
                                None
                            else
                                let name = fsName fallback export

                                match Map.tryFind export.Symbol.Id model.ExportTypes |> Option.bind _.Declared with
                                | Some typeId ->
                                    match Map.tryFind typeId model.DeclNames with
                                    | Some primary when primary <> name ->
                                        let typeParameters = Map.tryFind primary parametersOf |> Option.defaultValue []

                                        let target =
                                            if typeParameters.IsEmpty then
                                                FsNamed primary
                                            else
                                                FsApp(primary, typeParameters |> List.map (_.Name >> FsTypeVar))

                                        Some(
                                            FsAbbrev
                                                {
                                                    Name = name
                                                    Docs = export.Docs
                                                    Tags = export.Tags
                                                    Order = export.Order
                                                    TypeParameters = typeParameters
                                                    Target = target
                                                }
                                        )
                                    | _ -> None
                                | None -> None)

                    let remainingDecls =
                        model.DeclNames
                        |> Map.toList
                        |> List.sortBy fst
                        |> List.choose (fun (typeId, name) ->
                            if Set.contains name declaredNames then
                                None
                            else
                                match Map.tryFind typeId model.Types with
                                | Some facts ->
                                    // A branding intersection is a name and nothing else in F#:
                                    // a unit of measure, spelled at the uses as `string<Name>`
                                    // rather than declared as an abbreviation (§4.6, D11).
                                    let brand = brandedPrimitive model facts

                                    if brand.IsSome then
                                        findings <- findings @ [ Finding.make name ShapeAliases.BrandAsMeasure ]

                                        Some(
                                            FsMeasure
                                                {
                                                    Name = name
                                                    Docs =
                                                        Map.tryFind typeId exportDocs
                                                        |> Option.defaultValue ("", [])
                                                        |> fst
                                                    Tags =
                                                        Map.tryFind typeId exportDocs
                                                        |> Option.defaultValue ("", [])
                                                        |> snd
                                                    Order =
                                                        Map.tryFind typeId model.DeclOrders |> Option.defaultValue None
                                                    Primitive = brand.Value
                                                }
                                        )
                                    else

                                        let typeParameters, scope, parameterFindings =
                                            declTypeParams ctx model name facts

                                        let scoped = { model with TypeVars = scope }

                                        // The named cases earlier passes handle; what reaches here is
                                        // referable without a declaration of its own.
                                        let reference, refFindings =
                                            match arrayElement facts with
                                            | Some element ->
                                                let inner, innerFindings = typeRef ctx scoped None name element
                                                FsArray inner, innerFindings
                                            | None ->
                                                match Map.tryFind facts.Response.Id model.DeclNames with
                                                | Some primary when primary <> name -> FsNamed primary, []
                                                | _ -> typeRefIgnoringSelf ctx scoped name facts

                                        findings <- findings @ parameterFindings @ refFindings

                                        let docs, tags = Map.tryFind typeId exportDocs |> Option.defaultValue ("", [])

                                        let order = Map.tryFind typeId model.DeclOrders |> Option.defaultValue None

                                        // A right side that does not mention its parameters is a
                                        // type-level computation the checker could not finish
                                        // (`DeepPartial<T>`); a phantom keeps name and arity.
                                        if
                                            not typeParameters.IsEmpty
                                            && typeParameters
                                               |> List.forall (fun p ->
                                                   not (Set.contains p.Name (typeVarsOf reference)))
                                        then
                                            findings <-
                                                findings @ [ Finding.make name ShapeAliases.PhantomComputation ]

                                            Some(
                                                FsPhantom
                                                    {
                                                        Name = name
                                                        Docs = docs
                                                        Tags = tags
                                                        Order = order
                                                        TypeParameters = typeParameters
                                                        Carrier =
                                                            if
                                                                flag TypeFlags.TemplateLiteral facts
                                                                || flag TypeFlags.StringMapping facts
                                                            then
                                                                FsString
                                                            else
                                                                FsObj
                                                    }
                                            )
                                        else
                                            Some(
                                                FsAbbrev
                                                    {
                                                        Name = name
                                                        Docs = docs
                                                        Tags = tags
                                                        Order = order
                                                        TypeParameters = typeParameters
                                                        Target = reference
                                                    }
                                            )
                                | None -> None)

                    let model =
                        { model with
                            Decls = model.Decls @ aliasDecls @ remainingDecls
                        }

                    return
                        if List.isEmpty findings then
                            Advanced model
                        else
                            Degraded(model, findings)
                }
    }
