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
    { Harvest = harvest
      ExportTypes = Map.empty
      Types = Map.empty
      NotFollowed = Map.empty }

/// Resolve -> Shape: everything resolved is carried, the declarations start empty.
let toShape (resolve: ResolveModel) : ShapeModel =
    { Harvest = resolve.Harvest
      ExportTypes = resolve.ExportTypes
      Types = resolve.Types
      NotFollowed = resolve.NotFollowed
      DeclNames = Map.empty
      DeclOrders = Map.empty
      DeclParams = Map.empty
      ExportMembers = []
      TypeVars = Map.empty
      KeyVars = Map.empty
      Decls = [] }

/// The generated module's name: the config override, or the entry package's name under the
/// O7 naming contract (`@scope/pkg-name` -> `Scope.PkgName`).
let moduleName (ctx: Context) =
    match ctx.Config.ModuleName with
    | Some name -> name
    | None -> Naming.packageModule ctx.PackageName

/// Shape -> Render: declarations plus every finding of every earlier tier.
let toRender (ctx: Context) (shape: ShapeModel) (findings: Finding list) : RenderModel =
    { ModuleName = moduleName ctx
      PackageName = ctx.PackageName
      PackageDir = ctx.PackageDir
      Decls = shape.Decls
      Findings = findings
      Files = [] }

/// Runs the whole pipeline against a package directory and returns the rendered model without
/// touching the output directory - what tests diff against goldens.
let generate (config: GeneratorConfig) (packageDir: string) : Async<RenderModel> =
    async {
        let! mailbox, ctx = Bootstrap.start config packageDir
        use _ = mailbox :> IDisposable

        let! harvest, harvestFindings = runTier ctx Harvest.passes HarvestModel.Empty
        let! resolve, resolveFindings = runTier ctx Resolve.passes (toResolve harvest)
        let! shape, shapeFindings = runTier ctx Shape.passes (toShape resolve)

        let render = toRender ctx shape (harvestFindings @ resolveFindings @ shapeFindings)
        let! rendered, renderFindings = runTier ctx Render.passes render

        // Render passes are pure printers; a finding here means one widened silently earlier.
        return { rendered with Findings = rendered.Findings @ renderFindings }
    }

let private utf8NoBom = Text.UTF8Encoding false

/// Runs the pipeline and writes the rendered files into `outDir`, creating it if needed.
let run (config: GeneratorConfig) (packageDir: string) (outDir: string) : Async<RunReport> =
    async {
        let! rendered = generate config packageDir
        Directory.CreateDirectory outDir |> ignore

        for name, content in rendered.Files do
            File.WriteAllText(Path.Combine(outDir, name), content, utf8NoBom)

        return
            { ModuleName = rendered.ModuleName
              OutputFiles = rendered.Files |> List.map fst
              Findings = rendered.Findings
              Counts = Render.counts (Render.symbolTiers rendered) }
    }
