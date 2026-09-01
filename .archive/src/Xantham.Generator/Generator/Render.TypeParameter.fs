[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module TypeParameter =
    /// Drop typar constraints whose underlying resolved type can't be
    /// represented as an F# typar bound. F# requires the constraint
    /// target to be an unsealed reference type (interface or unsealed
    /// class). Constraints to primitives, unions, tuples, literals,
    /// etc. trigger FS0663 ("constrained to always be 'X'") and FS0698
    /// ("the type used for the constraint is sealed"). The typar stays
    /// in the declaration so call sites' type-argument positions
    /// remain correct; only the constraint is dropped.
    ///
    /// Walks the resolved-type graph through transparent wrappers
    /// (`Optional`, `ReadOnly`, `TypeReference` with `ResolvedType`
    /// set) so alias chains are caught even when the prerendered
    /// `TypeRefRender` is a `ConcretePath` to the alias name.
    ///
    /// Categories dropped:
    /// - `Primitive _` — F# primitives are sealed (number, string, bool, etc.)
    /// - `Union _` — TS unions erase to U2/U3/etc.; not real F# types
    /// - `Intersection _` — TS intersections erase; not real F# types
    /// - `Tuple _` — F# tuples are sealed
    /// - `Literal _` — can't constrain typar to a specific literal value
    /// - `Array _` — F# arrays are sealed
    /// - `Index _` (keyof X) — no F# equivalent
    /// - `IndexedAccess _` (T[K]) — no F# equivalent
    /// - `TypeQuery _` (typeof X) — no F# equivalent
    /// - `TemplateLiteral _` — no F# equivalent
    /// - `Conditional _`, `Predicate _`, `Substitution _` — TS-only constructs
    /// - `EnumCase _` — can't constrain to a single enum case
    ///
    /// Categories KEPT (real F# constraint targets):
    /// - `Interface _`, `Class _`, `Enum _`, `TypeParameter _`, `GlobalThis`
    /// - `TypeLiteral _` — kept for now; some Zod V3/V4 generics rely on
    ///   inline-object bounds for member-shape disambiguation. Adding
    ///   it caused the DeepPartialInternal-style hangs documented in
    ///   `multi-valued-typestore.md`; safer to leave it out of this filter.
    ///
    /// History: a much earlier broader filter (any `Intrinsic` target at
    /// the render level) regressed +31 by breaking V3 ZodType emission;
    /// that approach matched on rendered TypeRefAtoms (`proptypekey<X>`,
    /// `obj`, etc.) which leaked load-bearing constraints. This
    /// resolved-type-level filter is more precise.
    let rec private isVacuousResolved (rt: ResolvedType) =
        match rt with
        // Categories F# can't express as a typar bound.
        | ResolvedType.Primitive _
        | ResolvedType.Union _
        | ResolvedType.Intersection _
        | ResolvedType.Tuple _
        | ResolvedType.Literal _
        | ResolvedType.Array _
        | ResolvedType.Index _
        | ResolvedType.IndexedAccess _
        | ResolvedType.TypeQuery _
        | ResolvedType.TemplateLiteral _
        | ResolvedType.Conditional _
        | ResolvedType.Predicate _
        | ResolvedType.Substitution _
        | ResolvedType.EnumCase _ -> true
        // Transparent wrappers — walk through to the underlying type.
        | ResolvedType.Optional opt ->
            match opt.Type.Value with
            | ResolvedType.TypeReference tr ->
                match tr.ResolvedType with
                | Some resolved -> isVacuousResolved resolved.Value
                | None -> false
            | inner -> isVacuousResolved inner
        | ResolvedType.ReadOnly inner -> isVacuousResolved inner
        | ResolvedType.TypeReference tr ->
            match tr.ResolvedType with
            | Some resolved -> isVacuousResolved resolved.Value
            | None -> false
        // Keep: Interface, Class, Enum, TypeParameter, GlobalThis, TypeLiteral.
        | _ -> false
    let isVacuousLazyConstraint (lazyTy: LazyResolvedType) =
        isVacuousResolved lazyTy.Value

    let render (ctx: GeneratorContext) scopeStore (typar: TypeParameter)  =
        let metadata = {
            Path = Path.create TransientTypePath.Anchored
            Original = Path.create TransientTypePath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
        let scopeStore =
            typar.Name
            |> RenderScopeStore.appendNameToPathContext scopeStore
        {
            Prelude.TypeParameterRender.Name = typar.Name
            Metadata = metadata
            Constraint =
                typar.Constraint
                |> Option.filter (isVacuousLazyConstraint >> not)
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Documentation = typar.Documentation
        }
    