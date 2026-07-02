namespace Xantham

// ─────────────────────────────────────────────────────────────────────────────
// The recipe MODEL, shared by both sides of the pipeline:
//   encoder (Fable):    src/Xantham.Fable/Recipe.fs   — smol-toml boundary
//   generator (.NET):   Recipe.Load.fs                — Tomlyn boundary
// The model is pure data; each side parses its TOML at its own interop boundary
// into these types and stays typed downstream. The generator consumes the recipe
// for area assignment and partitioned emission (docs/GOALS.md Done(L0): "the
// manifest is consumed by generation, not advisory").
// ─────────────────────────────────────────────────────────────────────────────

/// How an [[entry]] contributes files to the crawl.
type RecipeEntryKind =
    /// Resolved through the package's exports map (the ecosystem's own declaration).
    | Module
    /// A file added directly as a compilation root (ambient packages).
    | AmbientRoot

/// Declared handling for surface that is not bound one-to-one.
type DependencyPolicy =
    /// Bind only the closed set of types appearing in bound public signatures.
    | BindTouchpoints
    /// Degrade references to obj and record an advisory-ledger entry.
    | EraseWithAdvisory
    /// One opaque handle type plus a hand-curated builder set (the zod shape).
    | OpaqueHandle
    /// Out of the declared scope; entries requiring it are deferred.
    | OutOfScope
    /// Runtime-only dependency with zero type-surface exposure; nothing to bind.
    | NoSurface

module DependencyPolicy =
    /// Total parse of the recipe's policy strings; unknown values are explicit errors.
    let parse (text: string) : Result<DependencyPolicy, string> =
        match text with
        | "bind-touchpoints" -> Ok BindTouchpoints
        | "erase-with-advisory" -> Ok EraseWithAdvisory
        | "opaque-handle" -> Ok OpaqueHandle
        | "out-of-scope" -> Ok OutOfScope
        | "no-surface" -> Ok NoSurface
        | other -> Error $"unknown policy '{other}' (expected bind-touchpoints | erase-with-advisory | opaque-handle | out-of-scope | no-surface)"

/// One [[entry]] of a recipe: a package bound as (or reserved for) a library.
type RecipeEntry = {
    Package: string
    Kind: RecipeEntryKind
    /// AmbientRoot only: path of the root file inside the package directory.
    Root: string option
    /// Module only: subpath specifiers ("." | "./mcp" | ...). ["."] when omitted.
    Entries: string list
    /// False marks a policy-holder entry reached transitively, never crawled.
    Crawl: bool
    /// Target F# library name (Fidelity.CloudEdge.*), when declared.
    Lib: string option
    /// Emission policy carried on the entry itself (e.g. zod opaque-handle).
    Policy: DependencyPolicy option
    /// Publish-order edges: libs this entry's lib depends on.
    DependsOn: string list
    /// Recipe-relative path of a hand-shaped overlay file that REPLACES this
    /// entry's rendered unit at emission (the Farscape Overlay discipline:
    /// developer-owned refinement, declared in the recipe, stable across regens).
    Overlay: string option
}

/// A [dependencies."pkg"] rule for a reached package not bound as a library.
type DependencyRule = {
    Package: string
    Policy: DependencyPolicy
}

type Recipe = {
    Entries: RecipeEntry list
    Dependencies: DependencyRule list
} with
    /// The declared role of a package, if any: an entry's lib or a dependency policy.
    member this.RoleOf(package: string) =
        this.Entries
        |> List.tryFind (fun e -> e.Package = package)
        |> function
            | Some entry -> Some(Choice1Of2 entry)
            | None ->
                this.Dependencies
                |> List.tryFind (fun d -> d.Package = package)
                |> Option.map Choice2Of2
