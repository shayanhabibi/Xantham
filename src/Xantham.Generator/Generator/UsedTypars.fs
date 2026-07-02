[<AutoOpen>]
module Xantham.Generator.Generator.UsedTypars

open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator

/// THE single emitted-typar oracle for type-alias declarations.
///
/// A generic alias whose checker-collapsed body no longer RENDERS any of its declared type
/// parameters (`type AgentNamespace<'Agentic> = option<obj>`) is a PHANTOM-TYPAR alias — F#
/// rejects an abbreviation with unused typars. The decl-side prune (Render.TypeAlias) and the
/// ref-side arity oracle (prerenderTypeAliases -> TypeAliasArity -> the cdc0108/general-args
/// alignment) MUST agree, or the surface emits `Name<obj,..>` against `type Name = ...` (the
/// 511->707 reverted-regression class). Both sides therefore call THIS function — same
/// computation, same instance — so they cannot diverge. There is deliberately NO path-keyed
/// cache table: TypePath is not injective (lib.dom/workers twins, the Typescript-parent prune)
/// and a keyed table reintroduces cross-alias clobber.
///
/// The walk MIRRORS the prerender arms' rendering semantics — a typar counts as USED only where
/// the render would actually emit it:
///   - `ResolvedType.TypeParameter`         -> used (the bare-typar render arm emits it).
///   - Union/Intersection/Tuple/Array/...   -> recurse (all components render).
///   - `TypeReference` args                 -> used only where an APPLY arm would render them
///     (the cdc0108 arm: remap-keyed alias body or generic Interface/Class head; the general
///     args arm) and NOT where a DROP arm discards them (inner already-instantiated reference;
///     resolved-body guard failure). Cross-alias truncation is over-approximated at DECLARED
///     arity (never another alias's emitted arity — that would be a cross-alias fixpoint).
///   - `TypeLiteral`/`Interface`/`Class` MEMBERS -> NOT used: a typar living only inside a
///     hoisted structural body is SCRUBBED to `obj` at its emitted home today (the a5a2652
///     anchor-stage orphan guard), so the alias decl never renders it. The stage-D lambda-lift
///     flips this per-literal when a home actually abstracts over its free typars.
///
/// FAIL-SAFE DIRECTIONS (why mirror drift cannot hard-break): counting an unused typar as used
/// (over-keep) leaves the status-quo unused-typar diagnostic; missing a used one (over-prune)
/// lets the body's typar occurrence hit the anchor-stage orphan scrub and erase to `obj`. Both
/// are soft; the arity-agreement gate measures outcomes mechanically.
///
/// ORDERING CONTRACT: the remap-keyed test consults `ctx.TypeAliasRemap`, so this must run only
/// after prerenderTypeAliases PHASE 1 has registered EVERY alias remap (the two-phase split) —
/// mid-registration consultation makes prune decisions enumeration-order-dependent.
///
/// Cycle-safe (ReferenceEquality visited set; the interner never forces outgoing lazies at
/// construction, so per-node forcing terminates) and dangling-key-guarded: a lazy whose TypeKey
/// is missing from the type map (the lib.dom-twin class) throws on force — degrade that branch
/// to "no typars used" with a diagnostic instead of crashing the generation.
let usedTyparNames (ctx: GeneratorContext) (body: ResolvedType) : Set<string> =
    let visited = System.Collections.Generic.HashSet<ResolvedType>()
    let used = System.Collections.Generic.HashSet<string>()
    let guarded (label: string) (force: unit -> unit) =
        try force () with ex ->
            eprintfn "Warning: usedTypars walk degraded at %s (dangling reference?): %s" label ex.Message
    let rec walk (rt: ResolvedType) =
        if visited.Add rt then
            match rt with
            | ResolvedType.TypeParameter tp ->
                used.Add(Name.Case.valueOrModified tp.Name) |> ignore
            | ResolvedType.Union u -> for t in u.Types do guarded "union member" (fun () -> walk t.Value)
            | ResolvedType.Intersection i -> for t in i.Types do guarded "intersection member" (fun () -> walk t.Value)
            | ResolvedType.Tuple t -> for e in t.Types do guarded "tuple element" (fun () -> walk e.Type.Value)
            | ResolvedType.Array e -> walk e
            | ResolvedType.ReadOnly e -> walk e
            | ResolvedType.Optional o -> walk (ResolvedType.TypeReference o)
            | ResolvedType.IndexedAccess ia ->
                guarded "index-access object" (fun () -> walk ia.Object.Value)
                guarded "index-access index" (fun () -> walk ia.Index.Value)
            | ResolvedType.Index ix -> guarded "keyof" (fun () -> walk ix.Type.Value)
            | ResolvedType.Conditional c ->
                for lbl, l in [ "check", c.Check; "extends", c.Extends; "true", c.True; "false", c.False ] do
                    guarded ("conditional " + lbl) (fun () -> walk l.Value)
            | ResolvedType.Substitution s ->
                guarded "substitution base" (fun () -> walk s.Base.Value)
            | ResolvedType.TypeQuery q -> guarded "typeof" (fun () -> walk q.Type.Value)
            | ResolvedType.TypeReference tr ->
                let argsRender =
                    // Mirror of the reference arms in prerender:
                    //  - inner already-instantiated `TypeReference<..>` -> outer args DROPPED
                    //  - resolved body remap-keyed or generic Interface/Class -> args APPLIED (cdc0108)
                    //  - anything else with args reaching the general args arm -> args APPLIED
                    //  - resolved-body guard failure (non-generic named head, not remapped) -> DROPPED
                    match tr.ResolvedType with
                    | Some inner ->
                        let mutable applied = false
                        guarded "reference resolved-body" (fun () ->
                            applied <-
                                match inner.Value with
                                | ResolvedType.TypeReference { TypeArguments = _ :: _ } -> false
                                | ResolvedType.Interface i -> i.TypeParameters.Length > 0
                                | ResolvedType.Class c -> c.TypeParameters.Length > 0
                                | bodyValue -> ctx.TypeAliasRemap.ContainsKey bodyValue)
                        applied
                    | None ->
                        let mutable applied = true
                        guarded "reference head" (fun () ->
                            applied <-
                                match tr.Type.Value with
                                | ResolvedType.TypeReference { TypeArguments = _ :: _ } -> false
                                | _ -> true)
                        applied
                match tr.ResolvedType with
                | Some inner -> guarded "reference body" (fun () -> walk inner.Value)
                | None -> guarded "reference head" (fun () -> walk tr.Type.Value)
                if argsRender then
                    for a in tr.TypeArguments do guarded "reference arg" (fun () -> walk a.Value)
            // Structural literal body — the DECL-oracle rule, tier-precise:
            //   - SHARED-HOMED literal (SharedLiteralHomes-assigned): the stage-4 lift makes the
            //     home ABSTRACT over its free typars and the single cached reference APPLIES them
            //     uniformly at EVERY referrer — including this alias's body — so the member typars
            //     DO render here: they COUNT (JSONSchema<'Value,'SchemaType> keeps its typars and
            //     its body emits `Home<'Value,'SchemaType>`).
            //   - Any other literal: member typars are scrubbed at (or graft-lifted onto) the
            //     hoisted home, whose caseRef args orphan-scrub to obj when the alias is pruned —
            //     the alias decl itself never renders them: they do NOT count. Broadening this to
            //     "count when not SharedLiterals-assigned" was tried and REVERTED (+192: kept
            //     decls whose literals never graft-lift). Precise graft-position awareness for the
            //     single-owner case is a future refinement.
            | ResolvedType.TypeLiteral lit when ctx.SharedLiteralHomes.ContainsKey rt ->
                let walkLazy label (l: LazyResolvedType) = guarded label (fun () -> walk l.Value)
                let walkParams (ps: Parameter list) =
                    for p in ps do walkLazy "member param" p.Type
                for m in lit.Members do
                    match m with
                    | Member.Property p -> walkLazy "property" p.Type
                    | Member.GetAccessor g -> walkLazy "get-accessor" g.Type
                    | Member.SetAccessor s -> walkLazy "set-accessor" s.ArgumentType
                    | Member.IndexSignature ix -> walkLazy "index-sig" ix.Type; walkParams ix.Parameters
                    | Member.Method ms -> for mm in ms do walkLazy "method" mm.Type; walkParams mm.Parameters
                    | Member.CallSignature cs -> for c in cs do walkLazy "call-sig" c.Type; walkParams c.Parameters
                    | Member.ConstructSignature cs -> for c in cs do walkLazy "construct-sig" c.Type; walkParams c.Parameters
            | ResolvedType.TypeLiteral _
            | ResolvedType.Interface _
            | ResolvedType.Class _
            // Non-typar leaves.
            | ResolvedType.Primitive _
            | ResolvedType.Literal _
            | ResolvedType.TemplateLiteral _
            | ResolvedType.Enum _
            | ResolvedType.EnumCase _
            | ResolvedType.Predicate _
            | ResolvedType.GlobalThis -> ()
    walk body
    Set.ofSeq used

/// Free TypeParameter NODES referenced by a hoisted LITERAL's members, in first-encounter order,
/// deduped by name — the HOME-side walk for the lambda-lift: unlike the decl oracle above (which
/// excludes literal members where no lift renders them), a LIFTED home does render its members,
/// so the home's declared typars come from exactly this set. Nodes (not just names) so the home
/// decl can carry real TypeParameterRenders. Same reference-arm mirror, same guards.
let literalMemberTyparNodes (ctx: GeneratorContext) (rt: ResolvedType) : TypeParameter list =
    match rt with
    | ResolvedType.TypeLiteral lit ->
        let seen = System.Collections.Generic.HashSet<string>()
        let ordered = ResizeArray<TypeParameter>()
        let collectFrom (l: LazyResolvedType) =
            try
                // usedTyparNames gives the name-set per the arm mirror; re-walk shallowly to keep
                // node identity: collect every TypeParameter node whose name the mirror reports.
                let names = usedTyparNames ctx l.Value
                if not (Set.isEmpty names) then
                    let visited = System.Collections.Generic.HashSet<ResolvedType>()
                    let rec grab (x: ResolvedType) =
                        if visited.Add x then
                            match x with
                            | ResolvedType.TypeParameter tp ->
                                let n = Name.Case.valueOrModified tp.Name
                                if Set.contains n names && seen.Add n then ordered.Add tp
                            | ResolvedType.Union u -> for t in u.Types do (try grab t.Value with _ -> ())
                            | ResolvedType.Intersection i -> for t in i.Types do (try grab t.Value with _ -> ())
                            | ResolvedType.Tuple t -> for e in t.Types do (try grab e.Type.Value with _ -> ())
                            | ResolvedType.Array e | ResolvedType.ReadOnly e -> grab e
                            | ResolvedType.Optional o -> grab (ResolvedType.TypeReference o)
                            | ResolvedType.IndexedAccess ia ->
                                (try grab ia.Object.Value with _ -> ()); (try grab ia.Index.Value with _ -> ())
                            | ResolvedType.Index ix -> (try grab ix.Type.Value with _ -> ())
                            | ResolvedType.Conditional c ->
                                for b in [ c.Check; c.Extends; c.True; c.False ] do (try grab b.Value with _ -> ())
                            | ResolvedType.Substitution s -> (try grab s.Base.Value with _ -> ())
                            | ResolvedType.TypeQuery q -> (try grab q.Type.Value with _ -> ())
                            | ResolvedType.TypeReference tr ->
                                (try (match tr.ResolvedType with Some r -> grab r.Value | None -> grab tr.Type.Value) with _ -> ())
                                for a in tr.TypeArguments do (try grab a.Value with _ -> ())
                            | _ -> ()
                    grab l.Value
            with ex ->
                eprintfn "Warning: literalMemberTyparNodes degraded (dangling reference?): %s" ex.Message
        for m in lit.Members do
            match m with
            | Member.Property p -> collectFrom p.Type
            | Member.GetAccessor g -> collectFrom g.Type
            | Member.SetAccessor s -> collectFrom s.ArgumentType
            | Member.IndexSignature ix -> collectFrom ix.Type; for p in ix.Parameters do collectFrom p.Type
            | Member.Method ms -> for mm in ms do collectFrom mm.Type; for p in mm.Parameters do collectFrom p.Type
            | Member.CallSignature cs -> for c in cs do collectFrom c.Type; for p in c.Parameters do collectFrom p.Type
            | Member.ConstructSignature cs -> for c in cs do collectFrom c.Type; for p in c.Parameters do collectFrom p.Type
        List.ofSeq ordered
    | _ -> []

/// Name-set convenience over `literalMemberTyparNodes` (the caseLiteralRef filter).
let literalMemberTyparNames (ctx: GeneratorContext) (rt: ResolvedType) : Set<string> =
    literalMemberTyparNodes ctx rt
    |> List.map (fun tp -> Name.Case.valueOrModified tp.Name)
    |> Set.ofList

/// True when the alias's emitted declaration keeps its declared typars: at least one declared
/// typar is rendered by the body. All-or-nothing (fully-phantom prune only); partial (subset)
/// pruning needs slot-wise arg alignment in the reference arms and is deliberately out of scope.
let aliasKeepsTypars (ctx: GeneratorContext) (typ: TypeAlias) : bool =
    match typ.TypeParameters with
    | [] -> true
    | declared ->
        let declaredNames =
            declared |> List.map (fun tp -> Name.Case.valueOrModified tp.Value.Name) |> Set.ofList
        usedTyparNames ctx typ.Type.Value
        |> Set.intersect declaredNames
        |> Set.isEmpty
        |> not
