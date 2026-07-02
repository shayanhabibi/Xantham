module Xantham.Generator.Generator.Emission

// ─────────────────────────────────────────────────────────────────────────────
// PARTITIONED EMISSION (Phase 1, docs/PLAN.md): the recipe's [[entry]] order is
// the publish order; each crawlable entry becomes one compilation unit under
// `namespace rec Fidelity.CloudEdge`. The split happens at the RootModule.Modules
// seam: a package's top-level module (derived from its npm name by the same
// rules as the path plane) goes to its unit; root-level types/members, erased-
// union definitions, synthetic homes (SharedLiterals/LiteralUnions), and any
// unmapped modules pool into the FIRST unit (interim hosting until type-granular
// placement lands — forward-reference debt is MEASURED per unit by the gates,
// then burned down by the zod opaque-handle policy and placement refinement).
// Cross-unit references resolve through the shared namespace + project
// references; per-unit recursion is the tolerated interim (the module-rec
// lesson: recursion shrinks per-unit, never whole-program again).
// ─────────────────────────────────────────────────────────────────────────────

open System.Collections.Generic
open Fabulous.AST
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types

/// One compilation unit of the partitioned output.
type EmissionUnit = {
    Lib: string
    Package: string
    /// The top-level rendered module name this package owns (path-plane derivation).
    TopModule: string
    /// Libs this unit's project references (all earlier units, conservative DAG).
    References: string list
    /// Absolute path of a hand-shaped overlay that REPLACES this unit's rendered
    /// slice (the recipe's `overlay` key — e.g. the zod opaque-handle module).
    Overlay: string option
}

/// The npm-name -> top-level-module-name derivation, mirroring the path plane
/// (TypeRefRender.Paths): scoped packages merge to one segment; '-'/'/' are
/// PascalCase word boundaries ("@cloudflare/workers-types" -> "CloudflareWorkersTypes").
let packageTopModule (package: string) : string =
    package.TrimStart('@').Split([| '/'; '-'; '.' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun part ->
        if part.Length = 0 then part
        else string (System.Char.ToUpperInvariant part[0]) + part.Substring 1)
    |> String.concat ""

/// The publish-ordered unit plan from the recipe: one unit per lib-declaring
/// entry, in [[entry]] order; each unit references every earlier unit.
/// Policy-holder entries (crawl = false, e.g. zod) ARE units: their content is
/// reached transitively and renders under their top module regardless of seeding.
/// Overlay paths are recipe-relative and resolve against recipeDir.
let planUnits (recipeDir: string) (recipe: Recipe) : EmissionUnit list =
    recipe.Entries
    |> List.choose (fun entry -> entry.Lib |> Option.map (fun lib -> entry, lib))
    |> List.fold (fun (planned: EmissionUnit list) (entry, lib) ->
        {
            Lib = lib
            Package = entry.Package
            TopModule = packageTopModule entry.Package
            References = planned |> List.map _.Lib |> List.rev
            Overlay = entry.Overlay |> Option.map (fun o -> System.IO.Path.Combine(recipeDir, o))
        } :: planned) []
    |> List.rev

/// The result of splitting the rendered tree across the plan.
type SplitResult = {
    Units: (EmissionUnit * RootModule) list
    /// Top-level module names that matched no unit and pooled into the first
    /// unit (synthetic homes, erased-dep modules, chunk leaks) — a SIGNAL, not
    /// an error: the pool shrinks as placement and policies land.
    Pooled: string list
}

/// Split a collected RootModule into per-unit slices at the top-module seam.
/// Total: every module lands in exactly one unit; root types/members and
/// unmatched modules pool into the FIRST unit.
let splitRoot (plan: EmissionUnit list) (root: RootModule) : SplitResult =
    match plan with
    | [] -> failwith "emission plan is empty: the recipe declared no crawlable entries"
    | first :: _ ->
    let byTopModule =
        plan |> List.map (fun u -> u.TopModule, u) |> dict
    let slices =
        plan
        |> List.map (fun unit ->
            unit,
            {
                Types = Dictionary()
                Members = Dictionary()
                Modules = Dictionary()
            })
    let sliceOf lib = slices |> List.find (fun (u, _) -> u.Lib = lib) |> snd
    let firstSlice = sliceOf first.Lib
    let pooled = ResizeArray<string>()
    for KeyValue(name, module') in root.Modules do
        match byTopModule.TryGetValue name with
        | true, unit -> (sliceOf unit.Lib).Modules.Add(name, module')
        | _ ->
            pooled.Add name
            firstSlice.Modules.Add(name, module')
    for KeyValue(name, typ) in root.Types do firstSlice.Types.Add(name, typ)
    for KeyValue(name, m) in root.Members do firstSlice.Members.Add(name, m)
    {
        Units = slices
        Pooled = pooled |> List.ofSeq
    }

/// Render one unit file. MIRROR of Render.Collection.renderRoot with a recursive
/// namespace wrapper instead of an anonymous module — if a render arm is added
/// there, add it here (single wrapper difference keeps both readable; unifying
/// behind an inline builder abstraction traded worse than the mirror).
let renderUnitFile (ctx: GeneratorContext) (root: RootModule) =
    Ast.Oak() {
        (Ast.Namespace "Fidelity.CloudEdge") {
            // Self-contained units: the opens the conformance harness used to
            // prepend to the monolith are part of every unit file itself.
            Ast.Open "System"
            Ast.Open "Fable.Core"
            Ast.Open "Fable.Core.JS"
            Ast.Open "Fable.Core.JsInterop"
            for KeyValue(_, render) in root.Types do
                match render with
                | Anchored.TypeRender.TypeDefn typeLikeRender ->
                    TypeLikeRender.renderInterface ctx typeLikeRender
                | Anchored.TypeRender.TypeAlias typeAliasRender ->
                    match typeAliasRender with
                    | TypeAliasRender.Alias typeAliasRenderRef ->
                        TypeAliasRender.renderTypeAlias ctx typeAliasRenderRef
                    | TypeAliasRender.TypeDefn typeLikeRender ->
                        TypeLikeRender.renderInterface ctx typeLikeRender
                    | TypeAliasRender.StringUnion literalUnionRender ->
                        LiteralUnionRender.renderUnion ctx literalUnionRender
                    | TypeAliasRender.EnumUnion literalUnionRender ->
                        LiteralUnionRender.renderEnum ctx literalUnionRender
                    | TypeAliasRender.Function functionLikeRender ->
                        FunctionLikeRender.renderBinding ctx functionLikeRender
                | Anchored.TypeRender.StringUnion literalUnionRender ->
                    LiteralUnionRender.renderUnion ctx literalUnionRender
                | Anchored.TypeRender.EnumUnion literalUnionRender ->
                    LiteralUnionRender.renderEnum ctx literalUnionRender
                | _ -> ()
            for module' in root.Modules.Values do
                // Bodyless `type X =` is rejected by the compiler (FS0547) — mirror
                // of the renderRoot/renderModule guard.
                if module'.Members.Count > 0 then
                    renderModuleInterface ctx module'
            for module' in root.Modules.Values do
                renderModule ctx module'
        }
        |> _.toRecursive()
    }

/// Erased-union arities beyond Fable.Core's U2..U9 as their OWN first-unit file.
/// The arity set accumulates GLOBALLY while unit bodies render, so this must be
/// produced AFTER every unit's pass-1 render (the monolith forced all renders
/// before its definitions loop for the same reason); a separate compiled-first
/// file keeps the ordering structural instead of textual.
let renderErasedUnionsFile () =
    Ast.Oak() {
        (Ast.Namespace "Fidelity.CloudEdge") {
            Ast.Open "Fable.Core"
            for i in erasedUnion.UnionLengths |> Seq.sort do
                SpecialRender.renderErasedUnion i
        }
    }

/// Emission-only formatter config: a very wide line keeps long generic
/// annotations on ONE line, never entering Fantomas 7.0.1's buggy multi-line
/// generic wrapping (closing `>` outdented one column -> downstream FS0010
/// parse cascade — the pinned toolchain floor, docs/toolchain-fantomas-
/// fabulous-ast.md). Config-level avoidance, not a formatter patch; the legacy
/// monolith keeps the default width so its gates stay byte-stable.
let private wideFormat =
    { Fantomas.Core.FormatConfig.Default with MaxLineLength = 100000 }

/// The generated fsproj for one unit: net10.0 library, Fable.Core for the
/// interop attributes, project references to every earlier unit. The FIRST unit
/// (no references) also compiles the Xantham support library — an [<AutoOpen>]
/// under Fable.Core.JsInterop that activates in later units through the
/// assembly reference when that namespace is open.
let unitFsproj (supportLibrary: string option) (unit: EmissionUnit) : string =
    // Compile ORDER is load-bearing (F# single-pass): support library, then the
    // erased-union definitions, then the unit body. Only the first unit carries
    // the two preludes; later units see both through the assembly reference.
    let compiles =
        [
            if unit.References.IsEmpty then
                match supportLibrary with
                | Some lib -> $"        <Compile Include=\"{lib}\"><Link>XanthamFableCoreLibrary.fs</Link></Compile>"
                | None -> ()
                "        <Compile Include=\"ErasedUnions.fs\" />"
            $"        <Compile Include=\"{unit.Lib}.fs\" />"
        ]
        |> String.concat "\n"
    let references =
        unit.References
        |> List.map (fun lib -> $"        <ProjectReference Include=\"../{lib}/{lib}.fsproj\" />")
        |> String.concat "\n"
    $"""<Project Sdk="Microsoft.NET.Sdk">
    <PropertyGroup>
        <TargetFramework>net10.0</TargetFramework>
        <!-- F#7 relaxed indentation absorbs the FS0058 half of the Fantomas 7.0.1 layout
             floor; the wide-format emission avoids the FS0010 wrapping half entirely. -->
        <LangVersion>7.0</LangVersion>
        <GenerateDocumentationFile>false</GenerateDocumentationFile>
        <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    </PropertyGroup>
    <ItemGroup>
{compiles}
    </ItemGroup>
    <ItemGroup>
        <PackageReference Include="Fable.Core" Version="4.5.0" />
{references}
    </ItemGroup>
</Project>
"""

/// Emit every unit of the plan into outDir/<Lib>/{<Lib>.fs, <Lib>.fsproj}.
/// Returns (unit, file, generated-line-count) for the report.
let emitUnits (ctx: GeneratorContext) (supportLibrary: string option) (plan: EmissionUnit list) (root: RootModule) (outDir: string) =
    let split = splitRoot plan root
    if not split.Pooled.IsEmpty then
        eprintfn $"""pooled into {plan.Head.Lib}: {split.Pooled |> String.concat ", "}"""
    // Pass 1 — render EVERY unit body first: erased-union arities accumulate
    // globally while bodies render, so the definitions file must come after.
    let sources =
        split.Units
        |> List.map (fun (unit, slice) ->
            let source =
                match unit.Overlay with
                | Some overlayPath ->
                    // The overlay REPLACES the unit's OWN rendered module (the policy's
                    // enforcement point: the machinery surface is not emitted; the
                    // hand-shaped fragment is appended at namespace level). Pooled and
                    // other slice modules still render — retained units inherit from
                    // pooled types resident in this file (measured 2026-07-03).
                    let dropped =
                        match slice.Modules.TryGetValue unit.TopModule with
                        | true, machinery ->
                            slice.Modules.Remove unit.TopModule |> ignore
                            machinery.Types.Count + machinery.Modules.Count
                        | _ -> 0
                    eprintfn $"overlay: {unit.Lib} <- {overlayPath} (module {unit.TopModule} not emitted: {dropped} rendered node(s) replaced by the hand-shaped fragment)"
                    let rendered =
                        renderUnitFile ctx slice
                        |> Gen.mkOak
                        |> Gen.runWith wideFormat
                    rendered + "\n" + System.IO.File.ReadAllText overlayPath
                | None ->
                    renderUnitFile ctx slice
                    |> Gen.mkOak
                    |> Gen.runWith wideFormat
            unit, source)
    // Pass 2 — the complete arity set exists now.
    let erasedUnions = renderErasedUnionsFile () |> Gen.mkOak |> Gen.runWith wideFormat
    sources
    |> List.mapi (fun i (unit, source) ->
        let dir = System.IO.Path.Combine(outDir, unit.Lib)
        System.IO.Directory.CreateDirectory dir |> ignore
        if i = 0 then
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, "ErasedUnions.fs"), erasedUnions)
        let file = System.IO.Path.Combine(dir, unit.Lib + ".fs")
        System.IO.File.WriteAllText(file, source)
        System.IO.File.WriteAllText(System.IO.Path.Combine(dir, unit.Lib + ".fsproj"), unitFsproj supportLibrary unit)
        unit, file, (source |> Seq.filter ((=) '\n') |> Seq.length))
