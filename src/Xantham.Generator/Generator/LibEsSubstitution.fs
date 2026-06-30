module Xantham.Generator.Generator.LibEsSubstitution

// Faithful TS-stdlib (lib.es) -> F#/Fable name mappings. These names have no F#
// definition in the emitted surface, so a by-name reference would dangle (FS0039).
// `substitute name` returns the Fable equivalent's prefix (any type arguments are applied
// by the caller, so `PromiseLike<T>` -> `Promise<'T>` is preserved), or None to leave the
// reference untouched. `obj` is used ONLY for genuinely-dynamic types (`Function`); every
// other entry is a real type.
//
// NB: lib.dom types are no longer in scope (the encoder restricts `lib` to esnext — see
// docs/fixing-methodology.md), so prior lib.dom-only entries (HTMLCollectionOf, Attr) were
// removed as dead. Keep this map to lib.es types that genuinely still appear.
//
// This is the single source of truth consumed by the ResolvedTypePrelude interceptor in
// Render.fs and exercised directly by the generator unit tests.
let substitute (name: string) : string option =
    match name with
    | "Array" -> Some "ResizeArray"                  // JS array -> mutable F# ResizeArray (prefix-swap keeps the element arg)
    | "Error" -> Some "exn"
    | "PromiseLike" -> Some "Promise"                // Fable.Core.JS.Promise (open Fable.Core.JS)
    | "IterableIterator" | "Iterator"
    | "ArrayIterator" | "AsyncIterableIterator"
    | "Iterable" -> Some "seq"                       // Fable maps seq<'T> to a JS iterable
    | "ReadonlyArray" -> Some "System.Collections.Generic.IReadOnlyList"
    | "Function" | "CallableFunction" | "NewableFunction" -> Some "obj"  // dynamic, no Fable equivalent
    | _ -> None

// ── ResolvedTypePrelude interceptor ─────────────────────────────────────────────
// The single source of truth for the lib.es name/arg substitution, consumed by the
// generator's `Customisation.Interceptors.ResolvedTypePrelude` (Render.fs) and exercised
// directly by the generator unit tests (so the bare-generic-collection arg recovery below
// is verified in isolation, not only via a full cf regen).
open System
open Fabulous.AST
open Xantham
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Decoder
open Xantham.Decoder.Case
open Xantham.Decoder.ArenaInterner

// `Array`/`Error`/... are stdlib types the TS checker attributes to a `typescript` source
// (not flagged IsLibEs). The substitution map is the safety gate: workers-types' own
// `typescript`-sourced types (Response, Request, ...) are NOT in the map, so they fall
// through untouched.
let private isStdlibSourced (source: QualifiedNamePart option) =
    match source with
    | Some (QualifiedNamePart.Normal s | QualifiedNamePart.Abnormal(s, _)) ->
        s.Contains("typescript", StringComparison.OrdinalIgnoreCase)
    | None -> false

// Targets that ARE generic in F#/Fable, so a bare reference to the corresponding generic
// lib.es interface must carry its element argument(s). The typed arrays / `obj` / `exn`
// targets are NOT here (they are non-generic in Fable, or genuinely dynamic).
let private genericCollectionTargets = set [ "ResizeArray"; "seq"; "System.Collections.Generic.IReadOnlyList" ]

// A bare reference to a GENERIC lib.es collection (e.g. `Array`'s own self-returning methods
// `with`/`toSpliced`/... record their return as a bare `Interface:Array` — the implicit
// self-application `Array<'T>` is lost upstream in the IR, so NO surrounding TypeReference
// carries the element arg). Substituting such a bare reference to a name-only `ResizeArray`
// drops the type argument and emits `ResizeArray` (FS0033 "expects 1 type argument(s) but is
// given 0"). Recover the args from the substituted INTERFACE's OWN declared type parameters:
// `Array<'T>` -> `ResizeArray<'T>`. The TypeReference args-carrying arm (RenderScope.Prelude.fs,
// `applyArgsToCollectionHead`) detects this generic-collection head and replaces the
// placeholder type-params with the real element args, so a `TypeReference{Array,[elem]}` still
// renders `ResizeArray<elem>` (no double-wrap).
let private substituteLibEsGeneric (typeParameters: Lazy<TypeParameter> list) (libEsName: Name<Case.pascal>) (renderScope: RenderScope) : RenderScope =
    match substitute (Name.Case.valueOrSource libEsName) with
    | Some target ->
        let headWidget = Ast.LongIdent target
        let ref =
            if genericCollectionTargets.Contains target && not (List.isEmpty typeParameters) then
                // Build `target<'P1, 'P2, ...>` from the interface's own type params.
                let head =
                    RenderScopeStore.TypeRefAtom.Unsafe.createWidget headWidget
                    |> RenderScopeStore.TypeRef.Unsafe.createAtom
                    |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
                let argRefs =
                    typeParameters
                    |> List.map (fun tp ->
                        tp.Value.Name
                        |> Name.Case.valueOrModified
                        |> Ast.LongIdent
                        |> RenderScopeStore.TypeRefAtom.Unsafe.createWidget
                        |> RenderScopeStore.TypeRef.Unsafe.createAtom
                        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false)
                RenderScopeStore.TypeRefMolecule.Unsafe.createPrefix head argRefs
                |> RenderScopeStore.TypeRef.Unsafe.createMolecule
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind renderScope.TypeRef.Nullable
            else
                RenderScopeStore.TypeRefAtom.Unsafe.createWidget headWidget
                |> RenderScopeStore.TypeRef.Unsafe.createAtom
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind renderScope.TypeRef.Nullable
        { renderScope with TypeRef = ref; Render = Render.RefOnly ref }
    | None ->
        { renderScope with Render = Render.RefOnly renderScope.TypeRef }

let private substituteLibEs (libEsName: Name<Case.pascal>) renderScope =
    substituteLibEsGeneric [] libEsName renderScope

/// The lib.es ResolvedTypePrelude substitution: maps stdlib type references to their
/// faithful F#/Fable equivalents, recovering element args for bare generic-collection refs.
let prelude : ResolvedType -> RenderScope -> RenderScope =
    function
    | ResolvedType.Interface { IsLibEs = true; Name = name; TypeParameters = tps } -> substituteLibEsGeneric tps name
    | ResolvedType.Class { IsLibEs = true; Name = name } -> substituteLibEs name
    | ResolvedType.Enum { IsLibEs = true } -> fun renderScope ->
        { renderScope with Render = Render.RefOnly renderScope.TypeRef }
    // Stdlib types attributed to a `typescript` source but not IsLibEs-flagged
    // (Array, the typed arrays, ...) — substitute only when the name is in the map.
    | ResolvedType.Interface { Source = src; Name = name; TypeParameters = tps } when isStdlibSourced src && (substitute (Name.Case.valueOrSource name)).IsSome ->
        substituteLibEsGeneric tps name
    | ResolvedType.Class { Source = src; Name = name } when isStdlibSourced src && (substitute (Name.Case.valueOrSource name)).IsSome ->
        substituteLibEs name
    | _ -> id
