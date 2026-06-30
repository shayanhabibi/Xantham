[<AutoOpen>]
module Xantham.Generator.Generator.Render_TypeParameter

open System.ComponentModel
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module TypeParameter =
    /// An F# `'T :> C` subtype constraint requires `C` to be a NON-SEALED type (an interface or a
    /// non-sealed class). TypeScript freely writes `T extends string` / `extends number` / `extends
    /// SomeEnum` / `extends "a"|"b"`, but in F# `string`/`float`/`int`/`bool`/`bigint` are sealed and
    /// enums/string-enums/literal-unions are sealed too, so emitting `'T :> string` is invalid F#
    /// (FS0698 "the type used for the constraint is sealed" + FS0663 "constrained to always be
    /// 'string'"). For erased Fable bindings the constraint is non-load-bearing, so DROP a
    /// sealed-type constraint and leave the parameter unconstrained. KEEP genuine interface/class
    /// constraints (`extends Event`, `extends Stubable`, branded RPC types) — those are valid F#.
    let private isValidSubtypeConstraint (constraintType: ResolvedType) : bool =
        match constraintType with
        | ResolvedType.Interface _
        | ResolvedType.Class _ -> true
        // A TypeReference resolves to (or names) the constrained type; treat a reference whose
        // resolved body is an Interface/Class as a valid constraint, anything else as sealed.
        | ResolvedType.TypeReference { ResolvedType = Some (Resolve (ResolvedType.Interface _ | ResolvedType.Class _)) }
        | ResolvedType.TypeReference { Type = Resolve (ResolvedType.Interface _ | ResolvedType.Class _); ResolvedType = None } -> true
        // Primitives (string/float/int/bool/bigint), enums, string-enums, literal-unions, and
        // anonymous/structural shapes are sealed or non-coercible — not valid `:>` constraints.
        | _ -> false

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
                // Drop sealed-type constraints (`extends string`/number/enum/literal-union): `'T :>
                // string` is invalid F#. Keep interface/class constraints (`extends Event`, ...).
                |> Option.filter (fun c -> isValidSubtypeConstraint c.Value)
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Default =
                typar.Default
                |> Option.map (ctx.PreludeGetTypeRef ctx scopeStore)
                |> Option.toValueOption
            Documentation = typar.Documentation
        }
    