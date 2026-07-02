module Xantham.Generator.Generator.OpaqueHandleSubstitution

// ─────────────────────────────────────────────────────────────────────────────
// OPAQUE-HANDLE path substitution (recipe policy `opaque-handle`, e.g. zod).
// EMISSION-MODE ONLY: registered on Customisation.Interceptors.Paths.TypePaths
// when a recipe is loaded, so the legacy monolith (and its gates) is untouched.
//
// Seam rationale (measured 2026-07-03): Paths.TypePaths fires at EVERY TypePath
// construction — including TypeAliasRemap targets, which the ResolvedTypePrelude
// seam (LibEsSubstitution's) never sees. One root-segment predicate covers the
// package's whole subtree (Zod, Zod.V3.*, Zod.V4.* — versions nest under the one
// top module per the vN twin fix). Surviving generic applications compile via
// the overlay's phantom arity aliases (ZodType<'A,'B> = ZodType), so no argument
// machinery is touched.
// ─────────────────────────────────────────────────────────────────────────────

open Xantham
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath

/// (topModule, handleTypeName) for every recipe entry under the opaque-handle
/// policy WITH an overlay to land on. Handle name convention: <TopModule>Type
/// ("Zod" -> "ZodType"), matching the hand-shaped overlay fragment.
let handlesOf (recipe: Recipe) : (string * string) list =
    recipe.Entries
    |> List.filter (fun e -> e.Policy = Some OpaqueHandle && e.Overlay.IsSome)
    |> List.map (fun e ->
        let top = Emission.packageTopModule e.Package
        top, top + "Type")

/// The top-level module names owned by erase-with-advisory dependency rules:
/// the package-name derivation plus any recipe-declared `modules` overrides
/// (namespace-derived module names the derivation cannot see). References
/// rooted at these rewrite to the `Erased.<Top>` advisory aliases; the modules
/// themselves are dropped at emission with ledger entries.
let erasedTopsOf (recipe: Recipe) : string list =
    recipe.Dependencies
    |> List.filter (fun d -> d.Policy = EraseWithAdvisory)
    |> List.collect (fun d -> Emission.packageTopModule d.Package :: d.Modules)
    |> List.distinct

let private rootSegment (modulePath: ModulePath) =
    ModulePath.flatten modulePath
    |> List.tryHead
    |> Option.map Name.Case.valueOrModified

/// Rewrite any TypePath rooted under an opaque-handle package's top module to
/// the handle's path, and any path rooted under an erased top module to its
/// `Erased.<Top>` advisory alias. Idempotent: handle and Erased paths rewrite
/// to themselves.
let rewriteTypePath (handles: (string * string) list) (erasedTops: string list) (typePath: TypePath) : TypePath =
    match rootSegment typePath.Parent with
    | None -> typePath
    | Some root ->
        match handles |> List.tryFind (fun (top, _) -> top = root) with
        | Some(top, handle) -> TypePath.create handle (ModulePath.init top)
        | None ->
            if erasedTops |> List.contains root
            then TypePath.create root (ModulePath.init "Erased")
            else typePath
