[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module TypeParameter =
    /// Drop typar constraints whose underlying resolved type is the
    /// `Any` or `Unknown` primitive — TS source like
    /// `class Foo<T extends Bar>` where `Bar` collapsed to `any` during
    /// encoding (lib.es substitution, declaration-merge augmentation,
    /// or the encoder's normalization step). These constraints render
    /// as `'T :> obj` (or `'T :> obj option`), which F# rejects:
    /// FS0698 ("the type used for the constraint is sealed") and
    /// FS0663 ("constrained to always be 'X'"). The typar stays in
    /// the declaration so call sites' type arguments remain
    /// positionally correct; only the constraint is dropped.
    ///
    /// Walks the resolved-type graph through transparent wrappers
    /// (`Optional`, `ReadOnly`, `TypeReference` with `ResolvedType`
    /// set) so an alias chain like `AgentContext → DurableObjectState
    /// → Primitive Any` is caught even though the prerendered
    /// `TypeRefRender` is a `ConcretePath` to the alias name.
    ///
    /// Why not also drop other primitive-bottomed constraints
    /// (`string` / `int` / etc.)? An earlier broader filter (any
    /// `Intrinsic` target) regressed +31 (FS0033/FS0661 cascades,
    /// including breaking V3 ZodType emission). Some Intrinsic-typed
    /// constraints are load-bearing downstream. See
    /// `docs/plans/post-pr2-progress.md` "Attempted follow-up: drop
    /// Intrinsic-target constraints (reverted)" for the empirical
    /// data. The `Any`/`Unknown` filter is the conservative subset.
    let rec private isAnyLikeResolved (rt: ResolvedType) =
        match rt with
        | ResolvedType.Primitive TypeKindPrimitive.Any
        | ResolvedType.Primitive TypeKindPrimitive.Unknown -> true
        | ResolvedType.Optional opt -> isAnyLikeResolved opt.Type.Value
        | ResolvedType.ReadOnly inner -> isAnyLikeResolved inner
        | ResolvedType.TypeReference tr ->
            match tr.ResolvedType with
            | Some resolved -> isAnyLikeResolved resolved.Value
            | None -> false
        | _ -> false
    let isVacuousLazyConstraint (lazyTy: LazyResolvedType) =
        isAnyLikeResolved lazyTy.Value

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
    