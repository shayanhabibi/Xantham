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

let private rootSegment (modulePath: ModulePath) =
    ModulePath.flatten modulePath
    |> List.tryHead
    |> Option.map Name.Case.valueOrModified

/// Rewrite any TypePath rooted under an opaque-handle package's top module to
/// the handle's path. Idempotent: the handle's own path rewrites to itself.
let rewriteTypePath (handles: (string * string) list) (typePath: TypePath) : TypePath =
    match handles, rootSegment typePath.Parent with
    | [], _ | _, None -> typePath
    | handles, Some root ->
        match handles |> List.tryFind (fun (top, _) -> top = root) with
        | Some(top, handle) -> TypePath.create handle (ModulePath.init top)
        | None -> typePath
