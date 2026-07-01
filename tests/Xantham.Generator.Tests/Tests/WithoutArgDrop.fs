module Xantham.Generator.Tests.Tests.WithoutArgDrop

open Expecto
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Mocking.ArenaInterner.ResolvedType

// ── Characterization: TypeReference arg-drop when ResolvedType is populated ──────
//
// ROOT CAUSE (generator):
//   src/Xantham.Generator/Generator/RenderScope.Prelude.fs, the prerender match on
//   `ResolvedType.TypeReference`. The FIRST arm is
//       | ResolvedType.TypeReference { ResolvedType = Some innerResolvedType }
//       | ResolvedType.TypeReference { Type = innerResolvedType; TypeArguments = [] } ->
//           innerResolvedType |> prerender ctx scope ...
//   When a reference carries BOTH a populated `ResolvedType` (the encoder's resolved
//   instantiation of the body) AND non-empty `TypeArguments`, this arm matches on the
//   `ResolvedType = Some` alternative and renders the RESOLVED BODY alone — silently
//   discarding `TypeArguments`. The args-carrying arm
//       | ResolvedType.TypeReference { TypeArguments = (_ :: _); Type = innerResolvedType } ->
//   (which builds the `Prefix (head, args)` molecule, e.g. `Foo<A, B>`) is never reached
//   for these references because the earlier arm wins.
//
// SURFACE SYMPTOM (Cloudflare workers-types):
//   The mapped/utility TypeAlias `Without<T, U>` (and sibling `Rpc.UnstubifyAll<A>`) is
//   referenced as `TsTypeReference { Type = Without; TypeArguments = [A; B]; ResolvedType = Some <body> }`.
//   The reference renders as bare `Without` (no args) while the definition is emitted as a
//   2-generic `type Without<'U, 'T>` — yielding FS0033 "expects 2 type argument(s) but is
//   given 0" (224 occurrences; UnstubifyAll 22 occurrences). The IR DOES carry the args
//   (verified in cf-staged.json: refs to key 4048 hold TypeArguments [4063;4047] AND
//   ResolvedType Some 4067), and the decoder's buildFromTypeReference preserves them
//   (Arena.Interner.fs ~773-778) — so the drop is purely the generator arm above.
//
// This isolates the defect WITHOUT the alias/remap machinery: a plain reference to a
// generic Interface that carries both TypeArguments and a populated ResolvedType must
// render its arguments. Pending until the prerender arm is reordered/guarded so that a
// reference with non-empty TypeArguments builds the Prefix application regardless of
// whether ResolvedType is populated.

// Render against a FRESH context PER CALL. The shared module-level context in TypeRefRender is
// a single mutable PreludeRenders cache; these two cases share the same generic interface
// instance (`genericFoo`) and arg primitives, and Expecto runs the cases in PARALLEL — so a
// shared context lets the two renders race on the `genericFoo` key (the InFlight/`obj` recursion
// guard can fire for one while the other registers it), caching `Foo` as `obj` and surfacing as
// `option<obj<...>>`. A per-call context isolates each render (matching the real generator, which
// prerenders this key once on a single thread).
let private testRender (expectedTypeText: string) (ref: ResolvedType) =
    TestHelper.prerender GeneratorContext.Empty ref
    |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef expectedTypeText

// A generic interface `Foo<'T, 'U>` (2 declared type parameters) at module path Pkg.
let private genericFoo =
    Interface.create "Foo"
    |> Interface.withPath [ "Pkg" ]
    |> Interface.withTypeParameters [
        TypeParameter.create "T"
        TypeParameter.create "U"
    ]
    |> Interface.wrap

// A reference `Foo<string, int>` that ALSO carries a populated ResolvedType — exactly the
// shape the encoder emits for references to a mapped-type alias like `Without<T, U>`.
let private referenceWithResolvedAndArgs =
    genericFoo
    |> TypeReference.create
    |> TypeReference.withTypeArguments [
        primitive TypeKindPrimitive.String
        primitive TypeKindPrimitive.Integer
    ]
    // The resolved instantiation the encoder attaches (here, the same generic body).
    |> TypeReference.withResolvedType genericFoo
    |> TypeReference.wrap

let argDropTests =
    testList "TypeReference arg-drop (Without / UnstubifyAll class)" [
        // CONTROL: same reference WITHOUT a populated ResolvedType renders its args correctly
        // (proves the args-carrying Prefix arm works — the defect is solely arm ordering).
        testCase "control: generic reference WITHOUT ResolvedType renders its arguments" <| fun _ ->
            genericFoo
            |> TypeReference.create
            |> TypeReference.withTypeArguments [
                primitive TypeKindPrimitive.String
                primitive TypeKindPrimitive.Integer
            ]
            |> TypeReference.wrap
            |> testRender "Pkg.Foo<string, int>"
            ||> Flip.Expect.equal "generic ref without ResolvedType should carry its args"

        // PENDING — the defect: a reference that carries BOTH TypeArguments and a populated
        // ResolvedType must still render `Pkg.Foo<string, int>`. Today the
        // `ResolvedType = Some` arm in RenderScope.Prelude.fs matches first and drops the
        // args, rendering bare `Pkg.Foo` -> FS0033 at the surface (the Without/UnstubifyAll bug).
        //
        // CORRECTED ROOT (two fixes ATTEMPTED + REVERTED 2026-06-29; the obstacle is a CYCLE):
        //   Attempt A — guard the `ResolvedType = Some` arm with `TypeArguments = []` so args-
        //     carrying refs fall through to the existing Prefix arm (which re-prerenders the
        //     `Type` as the head): STACK-OVERFLOWED real generation, and the head rendered `obj`
        //     not the named alias (it re-resolved `Type` instead of using the resolved body).
        //   Attempt B — a dedicated arm using the RESOLVED BODY as the (named, cycle-safe) head
        //     and eagerly prerendering the args into a Prefix: STILL STACK-OVERFLOWED. The
        //     overflow stack shows `RenderScope.Prelude.postfixArguments` → the TYPE ARGUMENTS
        //     themselves transitively reference back to this same node (self-referential mapped-
        //     type instantiation), and eagerly prerendering them re-enters the cycle before this
        //     scope is registered. The `ResolvedType = Some` short-circuit exists PRECISELY
        //     because it renders the body alone and NEVER touches the cyclic args.
        // So the real fix cannot eagerly prerender the args here. It needs the scope REGISTERED
        // before the args are prerendered (so the recursion hits an existing entry), or the args
        // rendered LAZILY — a change to the prerender/scope-registration protocol, not a local
        // arm. ACCEPTANCE TEST for any fix MUST be a real cf regen WITHOUT stack overflow, not
        // this isolated render (which is acyclic and would pass a wrong fix). See memory
        // xantham-mapped-type-argdrop.
        // FIXED 2026-06-30. The `ResolvedType = Some innerResolvedType; TypeArguments = (_::_)`
        // arm in RenderScope.Prelude.fs now builds the `Prefix (head, args)` application when the
        // resolved body is a generic Interface/Class (CASE 1 here — `Foo<'T,'U>`) or a generic
        // type-alias body registered in `ctx.TypeAliasRemap` (CASE 2 — the Without/UnstubifyAll
        // surface). The head is the resolved body's NAMED identity (cycle-safe); the scope is
        // REGISTERED before the args are prerendered, so a self-referential arg cycle-breaks via
        // the cache-hit arm rather than overflowing. An already-instantiated `TypeReference<..>`
        // body or an inline (non-remapped) structural body still falls through and renders alone
        // (no double-wrap). Acceptance was a real cf regen WITHOUT stack overflow (FS0033
        // 704 -> 534; the "given 0" arg-drop subclass 436 -> 236).
        testCase "generic reference WITH populated ResolvedType must NOT drop its arguments" <| fun _ ->
            referenceWithResolvedAndArgs
            |> testRender "Pkg.Foo<string, int>"
            ||> Flip.Expect.equal "ResolvedType-populated generic ref must still carry its type arguments"
    ]

// ── Alias-arity padding of a remapped mapped-type-alias body (the Without given-0 root) ──────
//
// ROOT (re-localised at HEAD 480828a, instrumented): the 88 "given 0" Without FS0033 cases do NOT
// reach the cdc0108 arm as `TypeReference{ResolvedType=Some; TypeArguments=(_::_)}`. The encoder
// resolves a `Without<A,B>` reference in a member/union/heritage position to the alias's bare
// structural BODY (a `TypeLiteral`) with the application's `TypeArguments` DROPPED ENTIRELY — every
// such site reaches the `remap` helper as the bare body, no surrounding TypeReference, no args
// recoverable. `remap` substitutes the body with the bare alias NAME (`ctx.TypeAliasRemap[body]`),
// but the alias is emitted as `type Without<'U,'T>`, so the bare name is FS0033 "expects 2 but given
// 0". Since the real args are unrecoverable, the fix pads the remapped name to the alias's DECLARED
// arity with `obj` placeholders (`Without<obj,obj>`) — recorded per remap key in `TypeAliasArity`
// during `prerenderTypeAliases`, applied by `padAliasNameToArity`. `obj` is a trivial intrinsic atom
// (cycle-safe to build, never re-enters prerender), preserving the remap cycle-break.
//
// A constrained alias (`type EventListener<'T when 'T :> Event> = ...`) is deliberately NOT padded:
// `EventListener<obj>` is FS0001 ("obj not compatible with Event"), so its arity is left unrecorded
// and the bare name passes through as the pre-existing FS0033 it was at HEAD (no NEW error class).

// The `Without` body: a TypeLiteral reduced to a string index signature. The exact shape is
// immaterial here — what matters is that it is a NOMINAL/structural body registered in the remap.
let private withoutBody : ResolvedType =
    TypeLiteral.empty |> TypeLiteral.wrap

// The alias NAME ref (`Without`), built exactly as prerenderTypeAliases does: a ConcretePath atom
// for the alias's path. Reuse the prerender of an Interface named "Without" to obtain a clean
// ConcretePath name ref (the remap target's identity).
let private aliasNameRef (ctx: GeneratorContext) =
    Interface.create "Without"
    |> Interface.withPath [ "Pkg" ]
    |> Interface.wrap
    |> TestHelper.prerender ctx

[<Tests>]
let aliasArityPadTests =
    testList "Remapped mapped-type-alias body pads to declared arity" [
        // THE FIX: a reference whose resolved body is a remapped mapped-type-alias body, carrying
        // NO args (they were dropped upstream), renders the alias name PADDED to its declared arity
        // (`Without<obj, obj>`), NOT the bare `Without` (which would be FS0033 "given 0").
        testCase "remapped body with recorded arity 2 renders Without<obj, obj>" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let nameRef = aliasNameRef ctx
            // Simulate prerenderTypeAliases for a generic UNCONSTRAINED alias `Without<'U,'T>`:
            // body -> name in TypeAliasRemap, and arity 2 in TypeAliasArity.
            GeneratorContext.Prelude.addTypeAliasRemap ctx withoutBody nameRef
            GeneratorContext.Prelude.addTypeAliasArity ctx withoutBody 2
            // An empty TypeLiteral body prerenders nullable, so the reference is `option<...>` —
            // the load-bearing assertion is the `Without<obj, obj>` payload (padded to arity 2),
            // never a bare `Without`.
            TestHelper.prerender ctx withoutBody
            |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef "option<Pkg.Without<obj, obj>>"
            ||> Flip.Expect.equal "remapped alias body must pad to declared arity (Without<obj, obj>)"

        // CONTROL: the SAME remapped body WITHOUT a recorded arity (the path taken for a
        // CONSTRAINED alias, whose arity is deliberately not recorded to avoid `Foo<obj>` FS0001)
        // renders the bare alias name — unaffected by the padding. Proves padding is gated on the
        // arity entry, not applied to every remapped body.
        testCase "control: remapped body WITHOUT recorded arity renders the bare name" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let nameRef = aliasNameRef ctx
            GeneratorContext.Prelude.addTypeAliasRemap ctx withoutBody nameRef
            // No addTypeAliasArity — mirrors a constrained alias (arity left unrecorded).
            TestHelper.prerender ctx withoutBody
            |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef "option<Pkg.Without>"
            ||> Flip.Expect.equal "remapped body without recorded arity must render the bare name"

        // IDENTITY alias (`type Identity<T> = T`): its resolved body is a bare TypeParameter.
        // `prerenderTypeAliases` deliberately does NOT record arity for such a body (the `isIdentityAlias`
        // guard on `recordArity`). Were the arity recorded, `padAliasNameToArity` would pad the remapped
        // name to `Identity<obj>`, which then collides with the real application args and emits the
        // unparseable double-generic `Identity<obj><realArgs>` (the zod `Identity`/`Flatten` breakage).
        // With arity UNRECORDED the bare name is emitted and real args apply cleanly. This pins the
        // no-pad OUTCOME the guard produces (parallel to the constrained-alias control above).
        testCase "identity-alias body (bare TypeParameter) renders bare — no obj padding" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let identityBody = TypeParameter.create "T" |> TypeParameter.wrap
            let nameRef =
                Interface.create "Identity" |> Interface.withPath [ "Pkg" ] |> Interface.wrap
                |> TestHelper.prerender ctx
            GeneratorContext.Prelude.addTypeAliasRemap ctx identityBody nameRef
            // No addTypeAliasArity — exactly what the isIdentityAlias guard enforces for a
            // bare-TypeParameter body. The remapped identity body must render the bare name.
            TestHelper.prerender ctx identityBody
            |> Xantham.Generator.Tests.Tests.TypeRefRender.testTypeRef "Pkg.Identity"
            ||> Flip.Expect.equal "identity-alias body must render the bare name (never Identity<obj>)"
    ]
