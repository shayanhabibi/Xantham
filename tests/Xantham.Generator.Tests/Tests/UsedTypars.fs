module Xantham.Generator.Tests.Tests.UsedTypars

(*
Coverage plane for the EMITTED-TYPAR ORACLE (Generator/UsedTypars.fs) — the single function that
decides whether a generic alias keeps its declared typars (decl side, Render.TypeAlias) AND what
reference arity is recorded for it (ref side, prerenderTypeAliases phase 2). Decl and refs call
the SAME oracle so they cannot diverge — the 511->707 reverted-regression class.

The walk MIRRORS prerender's reference arms: typars count as used only where the render would
emit them. These tests pin each mirror rule against the arm it mirrors. FAIL-SAFE: over-keep =
status-quo unused-typar diagnostic; over-prune = anchor-stage orphan-scrub obj erasure — both
soft, which is why the mirror may approximate but must never diverge decl-from-refs.
*)

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner

let private ctx () = GeneratorContext.Empty

let private tp name = ResolvedType.TypeParameter.create name
let private typarRT name = ResolvedType.TypeParameter.wrap (tp name)
let private lazyOf (rt: ResolvedType) : LazyResolvedType = LazyContainer.CreateFromValue rt
let private typeRef (inner: ResolvedType) (args: ResolvedType list) (resolved: ResolvedType option) =
    ResolvedType.TypeReference {
        Type = lazyOf inner
        TypeArguments = args |> List.map lazyOf
        ResolvedType = resolved |> Option.map lazyOf
    }
let private aliasWith (typars: string list) (body: ResolvedType) =
    ResolvedType.TypeAlias.create body "Subject"
    |> ResolvedType.TypeAlias.withTypeParameters (typars |> List.map tp)

[<Tests>]
let usedTyparsTests =
    testList "UsedTypars oracle" [

        // The bare-typar arm renders the typar — an identity alias always keeps (and is
        // therefore never pruned; prerenderTypeAliases routes it to the obj-remap unrecorded).
        testCase "identity alias (body = 'T) keeps its typar" <| fun _ ->
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] (typarRT "T"))
            |> Flip.Expect.isTrue "body IS the typar — rendered, kept"

        testCase "fully-phantom alias (literal-union body) prunes" <| fun _ ->
            let body = ResolvedType.Union.create [ ResolvedType.Literal.wrap (ResolvedType.Literal.createString "a") ]
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isFalse "no typar anywhere in the body"

        testCase "typar inside a union member is used (union members all render)" <| fun _ ->
            let body = ResolvedType.Union.create [ typarRT "T"; ResolvedType.primitive TypeKindPrimitive.String ]
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isTrue "union members render — typar kept"

        // A typar that appears ONLY as another typar's name is not enough — only the ALIAS's own
        // declared names count (name-set intersection).
        testCase "foreign typar in the body does not keep the alias's own typars" <| fun _ ->
            let body = ResolvedType.Union.create [ typarRT "U" ]
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isFalse "'U is not 'T — the alias's own typars are unused"

        // MIRROR of the L688/L694 drop arms: a reference whose resolved body is NOT remap-keyed
        // and NOT a generic Interface/Class drops its args — a typar living only in those args is
        // never rendered (the AgentNamespace<'Agentic> = option<obj> shape).
        testCase "typar only in DROPPED reference args prunes (AgentNamespace shape)" <| fun _ ->
            let anyBody = ResolvedType.primitive TypeKindPrimitive.Any
            let body = typeRef anyBody [ typarRT "Agentic" ] (Some anyBody)
            aliasKeepsTypars (ctx ()) (aliasWith [ "Agentic" ] body)
            |> Flip.Expect.isFalse "resolved body is a bare primitive: args are dropped, typar unused"

        // MIRROR of the cdc0108 APPLY arm (remap-keyed body): with the alias body registered in
        // TypeAliasRemap (phase-1 complete table), args ARE applied — the typar is used.
        testCase "typar in APPLIED args of a remap-keyed reference keeps" <| fun _ ->
            let c = ctx ()
            let referencedAliasBody =
                ResolvedType.Union.create [ ResolvedType.primitive TypeKindPrimitive.String ]
            // simulate phase 1: the referenced alias's body is remap-registered
            let dummyRef =
                RenderScopeStore.TypeRefAtom.Unsafe.createIntrinsic "obj"
                |> RenderScopeStore.TypeRef.Unsafe.createAtom
                |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
            GeneratorContext.Prelude.addTypeAliasRemap c referencedAliasBody dummyRef
            let body = typeRef referencedAliasBody [ typarRT "T" ] (Some referencedAliasBody)
            aliasKeepsTypars c (aliasWith [ "T" ] body)
            |> Flip.Expect.isTrue "remap-keyed head applies its args (cdc0108 arm) — typar used"

        // MIRROR of the cdc0108 APPLY arm (generic Interface head).
        testCase "typar in args of a generic-interface reference keeps" <| fun _ ->
            let iface =
                ResolvedType.Interface.create "Container"
                |> ResolvedType.Interface.withTypeParameters [ tp "E" ]
                |> ResolvedType.Interface.wrap
            let body = typeRef iface [ typarRT "T" ] (Some iface)
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isTrue "generic named head applies its args — typar used"

        // MIRROR of the inner-already-instantiated DROP (the L563/L694 double-wrap guard).
        testCase "typar in OUTER args of an already-instantiated reference prunes" <| fun _ ->
            let inner =
                typeRef (ResolvedType.primitive TypeKindPrimitive.String)
                        [ ResolvedType.primitive TypeKindPrimitive.Number ] None
            let body = typeRef inner [ typarRT "T" ] (Some inner)
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isFalse "outer args of an instantiated inner reference are dropped"

        // STAGE-2 RULE (flips per-literal at the stage-D lambda-lift): a typar living only inside
        // a hoisted structural body's members is SCRUBBED to obj at its emitted home — the alias
        // decl never renders it, so it must NOT count as used (decl typars would be unused, and
        // the recorded arity would disagree with the pruned decl).
        testCase "typar only inside TypeLiteral members prunes (pre-lift rule)" <| fun _ ->
            let lit =
                ResolvedType.TypeLiteral.empty
                |> ResolvedType.TypeLiteral.withMembers
                    [ ResolvedType.Property.create "value" (typarRT "T") |> ResolvedType.Property.wrap ]
                |> ResolvedType.TypeLiteral.wrap
            let body = ResolvedType.Union.create [ lit; ResolvedType.primitive TypeKindPrimitive.Boolean ]
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] body)
            |> Flip.Expect.isFalse "literal-member typars are scrubbed at the hoisted home today (JSONSchema class)"

        // Cycle safety: a self-referential body terminates via the ReferenceEquality visited set.
        testCase "cyclic body terminates" <| fun _ ->
            let mutable holder = Unchecked.defaultof<ResolvedType>
            let cyc =
                ResolvedType.Union {
                    Union.Types = [ { Data = LazyContainer<_,_>.DummyTypeKey; Result = lazy holder } ]
                }
            holder <- cyc
            aliasKeepsTypars (ctx ()) (aliasWith [ "T" ] cyc)
            |> Flip.Expect.isFalse "terminates; no typar found"
    ]

// =====================================================================================
// STAGE-4 shared-home lift plane: the decl-oracle's SHARED-homed arm (members count
// because the lift renders them as uniform home args at every referrer), the home-side
// node walk, and the canonical-home prerender arm's Prefix-wrapped uniform reference.
// =====================================================================================

let private sharedLit memberTyparName =
    ResolvedType.TypeLiteral.empty
    |> ResolvedType.TypeLiteral.withMembers
        [ ResolvedType.Property.create "value" (typarRT memberTyparName) |> ResolvedType.Property.wrap ]
    |> ResolvedType.TypeLiteral.wrap

let private homePath () =
    ModulePath.createFromList [ "SharedLiterals" ] |> TypePath.create "LiftedHome"

[<Tests>]
let sharedHomeLiftTests =
    testList "stage-4 shared-home lift" [

        // The PRECISE decl-oracle condition: a SharedLiterals-assigned literal's member typars
        // COUNT (the lift renders them as home args at every referrer, including the alias body).
        // Contrast with the unshared case above ("typar only inside TypeLiteral members prunes").
        testCase "shared-homed literal member typars COUNT for the decl oracle" <| fun _ ->
            let c = ctx ()
            let lit = sharedLit "T"
            GeneratorContext.SharedLiterals.addHome c lit (homePath ())
            let body = ResolvedType.Union.create [ lit; ResolvedType.primitive TypeKindPrimitive.Boolean ]
            aliasKeepsTypars c (aliasWith [ "T" ] body)
            |> Flip.Expect.isTrue "the lifted home applies 'T uniformly — the alias body renders it (JSONSchema class)"

        // The home-side walk: nodes in first-encounter member order, deduped by name.
        testCase "literalMemberTyparNodes: ordered, deduped" <| fun _ ->
            let lit =
                ResolvedType.TypeLiteral.empty
                |> ResolvedType.TypeLiteral.withMembers
                    [ ResolvedType.Property.create "b" (typarRT "U") |> ResolvedType.Property.wrap
                      ResolvedType.Property.create "a" (typarRT "T") |> ResolvedType.Property.wrap
                      ResolvedType.Property.create "c" (typarRT "U") |> ResolvedType.Property.wrap ]
                |> ResolvedType.TypeLiteral.wrap
            literalMemberTyparNodes (ctx ()) lit
            |> List.map (fun tp -> Name.Case.valueOrModified tp.Name)
            |> Flip.Expect.equal "member order, first occurrence wins" [ "'U"; "'T" ]

        // The canonical-home arm: a shared free-typar literal prerenders as a UNIFORM Prefix
        // application over the home path, and the home's lifted typars are registered for the
        // anchor-stage TypeParameters override.
        testCase "canonical-home prerender lifts: Prefix(home, typar) + HoistedHomeTypars" <| fun _ ->
            let c = ctx ()
            let lit = sharedLit "T"
            GeneratorContext.SharedLiterals.addHome c lit (homePath ())
            let rendered = TestHelper.prerender c lit
            (match rendered.Kind with
             | Prelude.TypeRefKind.Molecule (Prelude.TypeRefMolecule.Prefix (_, [ arg ])) ->
                 match arg.Kind with
                 | Prelude.TypeRefKind.Atom (Prelude.TypeRefAtom.Intrinsic "'T") -> true
                 | _ -> false
             | _ -> false)
            |> Flip.Expect.isTrue "the cached shared-home ref applies the lifted typar uniformly"
            GeneratorContext.Prelude.tryGetHoistedHomeTypars c lit
            |> ValueOption.isSome
            |> Flip.Expect.isTrue "the home's typars are registered for the anchor-stage decl override"
    ]
