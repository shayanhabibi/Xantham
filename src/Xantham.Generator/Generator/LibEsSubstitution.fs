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
    | "StringIterator" | "RegExpStringIterator"      // lib.es iterator types Fable doesn't expose
    | "Generator" | "AsyncGenerator"
    | "Iterable" -> Some "seq"                       // Fable maps seq<'T> to a JS iterable
    | "ReadonlyArray" | "ConcatArray" -> Some "System.Collections.Generic.IReadOnlyList"
    | "Function" | "CallableFunction" | "NewableFunction" -> Some "obj"  // dynamic, no Fable equivalent
    // lib.es/lib.dom globals the encoder attributes to `typescript` but Fable does not define — these
    // have no faithful generic F#/Fable type, so erase to `obj` (always valid; the ref is opaque
    // JS interop). Left undefined they render as a bare, dangling `RegExp`/`ReadonlyMap`/... name.
    | "RegExp" | "RegExpMatchArray" | "RegExpExecArray"
    | "ReadonlyMap" | "WeakMap" | "WeakSet"
    | "PropertyKey" | "TemplateStringsArray"
    | "ArrayBufferView" | "ArrayLike" -> Some "obj"
    // Fable.Core 5.0.0-beta.4 declares BigInt64Array but has NO BigUint64Array at all
    // (only the DataView get/setBigUint64 methods) — erase the unrepresentable type.
    | "BigUint64Array" -> Some "obj"
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
        // INTRINSIC head, not a Widget: widgets are OPAQUE to the anchor-stage scrubs
        // (the pinned placement constraint) — an obj-target head baked as a widget hid
        // `obj<args>` applications from collapseOpaquePrefixes (FS0033 "obj ... given 1").
        // An Intrinsic renders identically (Ast.LongIdent of the raw text).
        let headAtom =
            RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic target
            |> RenderScopeStore.TypeRef.Unsafe.createAtom
            |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
        let ref =
            if genericCollectionTargets.Contains target && not (List.isEmpty typeParameters) then
                // Build `target<'P1, 'P2, ...>` from the interface's own type params.
                let head = headAtom
                let argRefs =
                    typeParameters
                    |> List.map (fun tp ->
                        // INTRINSIC, not Widget: a widget is OPAQUE to the anchor-stage
                        // orphan-typar scrub (the pinned placement constraint), so a
                        // placeholder typar baked as a widget LEAKS into scopes that do
                        // not declare it (`ResizeArray<'T>` inside a non-generic home —
                        // FS0039 "The type parameter 'T is not defined"). An Intrinsic
                        // renders identically and stays scrubbable.
                        tp.Value.Name
                        |> Name.Case.valueOrModified
                        |> RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic
                        |> RenderScopeStore.TypeRef.Unsafe.createAtom
                        |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false)
                RenderScopeStore.TypeRefMolecule.Unsafe.createPrefix head argRefs
                |> RenderScopeStore.TypeRef.Unsafe.createMolecule
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind renderScope.TypeRef.Nullable
            else
                RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic target
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
