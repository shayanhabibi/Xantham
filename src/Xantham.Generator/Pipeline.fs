/// The pipeline: per tier, a plain fold over that tier's pass list; between tiers, total
/// transition functions (decision O1 - accumulating records, so reading an artifact that does
/// not exist yet is a compile error, not a runtime one). Passes run strictly sequentially
/// (decision O3); concurrency lives inside passes, where the mailbox batches it.
module Xantham.Generator.Pipeline

open System
open System.IO
open Xantham.TypeScript.Wire

/// Folds a tier: each pass advances the model, findings are stamped with the pass that made
/// them, and the tier hands back both.
let runTier (ctx: Context) (passes: Pass<'Model> list) (model: 'Model) : Async<'Model * Finding list> =
    let rec go model findings passes =
        async {
            match passes with
            | [] -> return model, findings
            | (pass: Pass<'Model>) :: rest ->
                match! pass.Run ctx model with
                | Advanced model -> return! go model findings rest
                | Degraded(model, fresh) ->
                    let stamped = fresh |> List.map (fun finding -> { finding with Pass = pass.Name })
                    return! go model (findings @ stamped) rest
        }

    go model [] passes

/// Harvest -> Resolve: the harvest is carried, the tables start empty.
let toResolve (harvest: HarvestModel) : ResolveModel =
    {
        Harvest = harvest
        ExportTypes = Map.empty
        Types = Map.empty
        NotFollowed = Map.empty
    }

/// Resolve -> Shape: everything resolved is carried, the declarations start empty.
let toShape (resolve: ResolveModel) : ShapeModel =
    {
        Harvest = resolve.Harvest
        ExportTypes = resolve.ExportTypes
        Types = resolve.Types
        NotFollowed = resolve.NotFollowed
        DeclNames = Map.empty
        DeclOrders = Map.empty
        DeclParams = Map.empty
        ExportMembers = []
        TypeVars = Map.empty
        KeyVars = Map.empty
        Decls = []
    }

/// The generated module's name: the config override, or the entry package's name under the
/// O7 naming contract (`@scope/pkg-name` -> `Scope.PkgName`).
let moduleName (ctx: Context) =
    match ctx.Config.ModuleName with
    | Some name -> name
    | None -> Naming.packageModule ctx.PackageName

/// The group each generated declaration belongs to (O7), read off the type the shape tier named
/// it from. A name carried by two type ids takes the smaller id's group.
let private declOrigins (shape: ShapeModel) : Map<string, PackageId> =
    shape.DeclNames
    |> Map.toList
    |> List.sortBy fst
    |> List.fold
        (fun origins (typeId, name) ->
            match Map.tryFind name origins, Map.tryFind typeId shape.Types with
            | None, Some facts -> Map.add name facts.Origin origins
            | _ -> origins)
        Map.empty

/// The group a declaration is written into: its own where that group ships, the entry package's
/// otherwise. An anonymous shape belongs to the entry package whatever file its node sits in
/// (D6).
let private emittingGroup (ctx: Context) (origin: PackageId) =
    match origin with
    | EntryPackage
    | Unclassified -> EntryPackage
    | origin when GeneratorConfig.disposition ctx.Config origin = Ship -> origin
    | _ -> EntryPackage

/// The modules a run writes: the entry package's, plus one per shipped group a declaration
/// reached.
let groupModules (ctx: Context) (shape: ShapeModel) : Render.GroupModule list =
    let origins = declOrigins shape

    let groupOf decl =
        Render.declName decl
        |> Option.bind (fun name -> Map.tryFind name origins)
        |> Option.defaultValue Unclassified
        |> emittingGroup ctx

    let placed = shape.Decls |> List.groupBy groupOf |> Map.ofList

    let moduleOf (origin: PackageId) : Render.GroupModule =
        let decls = placed |> Map.tryFind origin |> Option.defaultValue []

        match GeneratorConfig.groupKey origin with
        | None ->
            {
                Group = ctx.PackageName
                IsEntry = true
                Module = moduleName ctx
                RuntimePackage = GeneratorConfig.runtimePackage ctx.Config ctx.PackageName
                Decls = decls
            }
        | Some key ->
            {
                Group = key
                IsEntry = false
                Module = Naming.groupModule ctx.PackageName origin
                RuntimePackage = GeneratorConfig.derivedRuntimePackage key
                Decls = decls
            }

    let shipped =
        placed
        |> Map.toList
        |> List.map fst
        |> List.filter (fun origin -> origin <> EntryPackage)
        |> List.map moduleOf

    moduleOf EntryPackage :: shipped

/// Shape -> Render: declarations plus every finding of every earlier tier.
let toRender (ctx: Context) (shape: ShapeModel) (findings: Finding list) : RenderModel =
    {
        ModuleName = moduleName ctx
        PackageName = ctx.PackageName
        RuntimePackage = GeneratorConfig.runtimePackage ctx.Config ctx.PackageName
        PackageDir = ctx.PackageDir
        Decls = shape.Decls
        Findings = findings
        Files = []
    }

/// Runs the whole pipeline against a package directory and returns the rendered model without
/// touching the output directory - what tests diff against goldens.
let generate (config: GeneratorConfig) (packageDir: string) : Async<RenderModel> =
    async {
        let! mailbox, ctx = Bootstrap.start config packageDir
        use _ = mailbox :> IDisposable

        let! harvest, harvestFindings = runTier ctx Harvest.passes HarvestModel.Empty
        let! resolve, resolveFindings = runTier ctx Resolve.passes (toResolve harvest)
        let! shape, shapeFindings = runTier ctx Shape.Passes.passes (toShape resolve)

        let render = toRender ctx shape (harvestFindings @ resolveFindings @ shapeFindings)

        // The two halves of the render tier run separately so the manifest reports what group
        // emission found: a pass reads the findings the model carries, not the ones the fold
        // is still accumulating.
        let! sourced, sourceFindings =
            runTier ctx [ Render.renderSources (groupModules ctx shape) ] render

        let! rendered, manifestFindings =
            runTier
                ctx
                [ Render.renderManifest ]
                { sourced with
                    Findings = sourced.Findings @ sourceFindings
                }

        return
            { rendered with
                Findings = rendered.Findings @ manifestFindings
            }
    }

let private utf8NoBom = Text.UTF8Encoding false

/// Runs the pipeline and writes the rendered files into `outDir`, creating it and the `groups/`
/// directory a shipped group is written under if needed.
let run (config: GeneratorConfig) (packageDir: string) (outDir: string) : Async<RunReport> =
    async {
        let! rendered = generate config packageDir
        Directory.CreateDirectory outDir |> ignore

        for name, content in rendered.Files do
            let path = Path.Combine(outDir, name)
            Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
            File.WriteAllText(path, content, utf8NoBom)

        return
            {
                ModuleName = rendered.ModuleName
                OutputFiles = rendered.Files |> List.map fst
                Findings = rendered.Findings
                Counts = Render.counts (Render.symbolTiers rendered)
            }
    }
