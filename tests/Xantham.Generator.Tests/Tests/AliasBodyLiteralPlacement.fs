module Xantham.Generator.Tests.Tests.AliasBodyLiteralPlacement

// Isolated characterization of the SELF-CYCLIC ALIAS-ABBREVIATION bug (FS0953 on the
// cloudflare surface, 218 occurrences).
//
// THE BUG (reproduced end-to-end below):
//   A type alias whose BODY is a UNION (or ARRAY) of *distinct anonymous object
//   literals* renders as an illegal self-cyclic abbreviation:
//       type EmailAttachment = U2<EmailAttachment, EmailAttachment>
//       type AiImageClassificationOutput = ResizeArray<AiImageClassificationOutput>
//   even though the IR is NOT cyclic — the union/array members are two distinct
//   TypeLiteral nodes (distinct anonymous shapes), NOT references back to the alias.
//
// MECHANISM (the nameless-Anchored collapse, same family as NestedUnderTypePlacement):
//   The alias body's anonymous union/array member TypeLiterals are prerendered in
//   prerender's TypeLiteral `_,_` arm (RenderScope.Prelude.fs) with a NAMELESS
//   `TransientTypePath.Anchored` root. Unlike a property whose name extends the
//   PathContext, a union/array member is anonymous, so the scope's PathContext stays
//   `Anchored`. When the alias is anchored under its export path, anchoring an
//   `Anchored` (empty trace) transient against the alias's Type anchor COLLAPSES onto
//   exactly the alias's own path/name. Both members collapse to the alias name, so the
//   union molecule renders `U2<EmailAttachment, EmailAttachment>` — a cyclic abbreviation.
//
//   There are TWO coupled defects:
//   (1) prerender's Union "Others" / Array branches walk anonymous members with a scope
//       whose PathContext is NOT extended by any member name, so each member grafts the
//       same nameless `Anchored` root.
//   (2) `createTransientPath` (Types/RenderScope.Prelude.fs) records the GRAFTED path in
//       the TypeStore (which drives the def's placement) but returns a ref atom built from
//       the UN-grafted `path` argument — an emission/reference asymmetry. So even after (1)
//       names the members, the molecule's REFERENCE atoms stay nameless.
//
// CORRECT behaviour (asserted): each anonymous member literal must get its OWN nested
// identity, so the alias body references distinct nested names (not the alias name) and
// the abbreviation is NOT self-cyclic.
//
// STATUS — FIXED (active testCase). The fix lives in `Render.TypeAlias.caseLiteralRef`
// (positional `Case{i}`/`Element` identity grafted into the alias scope's TypeStore + the
// member ref re-derived from that grafted path) and, for template-literal members, in
// `Render.Transient.TemplateLiteral.render` (nameless `Anchored` path + `Name = ValueNone` so
// the def is the path-derived abbreviation `type Case{i} = string` rather than the old
// hard-named `TemplateLiteral`). On the real cloudflare surface this cut FS0953 from 218 -> 8
// (object-literal + template-literal-union families both gone) with zero new own-`Case{i}`
// dangles. The deeper over-minting/duplicate-emission root (a few aliases emitted in two
// scopes) is still tracked upstream — see the project memory notes (union-overminting,
// hoist-dedup-rootcause) — but it no longer blocks these alias-body literal cases.

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

// Build an inline object literal with >=3 plain string properties (so it hoists),
// each with a DISTINCT property name so the two literals are structurally distinct.
let private objLit (names: string list) =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        for n in names do Property.create n (primitive TypeKindPrimitive.String) |> Property.wrap
    ]
    |> TypeLiteral.wrap

// Render a TypeAlias export end-to-end, mirroring the real generator pipeline:
//   prerenderTypeAliases (register the alias-body remap) ->
//   registerAnchorFromExport (prerender + anchor the alias) ->
//   collectModules -> renderRoot.
let private renderTypeAliasSurface (alias: TypeAlias) : string =
    let ctx = GeneratorContext.Empty
    let export = ResolvedExport.TypeAlias alias
    // Mirror prerenderTypeAliases (RenderScope.Prelude.fs): register the alias-body
    // remap so the alias self-protects (the body is rendered as the alias name where it
    // is referenced, while the alias's own definition keeps the real body render).
    if not (RenderScope_Prelude.ArenaInterner.isShareableAliasBody alias.Type.Value) then
        let path = Path.Interceptors.pipeTypeAlias ctx alias
        let aliasRef =
            RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
            |> RenderScopeStore.TypeRef.Unsafe.createAtom
            |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
        GeneratorContext.Prelude.addTypeAliasRemap ctx alias.Type.Value aliasRef
    registerAnchorFromExport ctx export
    let root = RootModule.collectModules ctx
    Ast.Oak() {
        Ast.AnonymousModule() {
            renderRoot ctx root
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// type EmailAttachment = { disposition: ...; contentId: ... } | { disposition: ...; cid: ... }
// The two union members are DISTINCT anonymous object literals.
let private unionAlias =
    let memberA = objLit [ "disposition"; "contentId"; "filename" ]
    let memberB = objLit [ "disposition"; "cid"; "name" ]
    Union.create [ memberA; memberB ]
    |> fun body -> TypeAlias.create body "EmailAttachment"

// type AiImageClassificationOutput = Array<{ label; score; index }>
let private arrayAlias =
    let element = objLit [ "label"; "score"; "index" ]
    TypeAlias.create (Array.create element) "AiImageClassificationOutput"

// type WorkflowRetentionDuration = number | `${number} second` | `${number} minute` | ...
// The body is a union mixing a PRIMITIVE (number -> float) with several distinct TEMPLATE LITERAL
// members. Each template literal prerenders to a nameless `TransientTypePath.Anchored` root that —
// like an anonymous object literal — collapses onto the owner alias name when anchored, so the
// alias renders the illegal self-cyclic `U<n><X, X, ...>` (FS0953). Each template-literal member
// must instead receive a positional `Case{i}` identity (erased to `string`), and the `float`
// primitive must be preserved as a bare union member (NOT dropped — the union arity must stay U3).
let private templateLiteralUnionAlias =
    let secondTpl = TemplateLiteral.create [ ""; " second" ] [ primitive TypeKindPrimitive.Number ] |> TemplateLiteral.wrap
    let minuteTpl = TemplateLiteral.create [ ""; " minute" ] [ primitive TypeKindPrimitive.Number ] |> TemplateLiteral.wrap
    Union.create [ primitive TypeKindPrimitive.Number; secondTpl; minuteTpl ]
    |> fun body -> TypeAlias.create body "WorkflowRetentionDuration"

let aliasBodyLiteralTests = testList "alias body of anonymous literals" [
    // The union alias of two distinct anonymous object literals must render the non-cyclic
    // `U2<EmailAttachment.Case0, EmailAttachment.Case1>` with two distinct nested defs, NOT the
    // illegal self-cyclic `U2<EmailAttachment, EmailAttachment>` (FS0953). Fixed in
    // Render.TypeAlias by giving each anonymous-literal union member a positional `Case{i}`
    // identity rooted under the alias's companion module.
    testCase "union of distinct anonymous literals does not render as self-cyclic U2<X, X>" <| fun _ ->
        let out = renderTypeAliasSurface unionAlias
        let abbrevLine =
            out.Split('\n')
            |> Array.map _.Trim()
            |> Array.tryFind (_.StartsWith("type EmailAttachment ="))
        match abbrevLine with
        | Some line ->
            // The cyclic shape is `type EmailAttachment = U2<EmailAttachment, EmailAttachment>`.
            Expect.isFalse
                (line.Contains("U2<EmailAttachment, EmailAttachment>")
                 || line.Contains("U2<EmailAttachment,EmailAttachment>"))
                (sprintf "alias must not be a self-cyclic abbreviation; got: %s" line)
        | None ->
            // No abbreviation line: the alias is emitted as a TypeDefn (also acceptable).
            ()

    // The array alias of an anonymous object literal must render `ResizeArray<X.Element>` with a
    // nested `X.Element` def, NOT the self-cyclic `ResizeArray<X>`. Fixed in Render.TypeAlias by
    // giving the array element an `Element` identity rooted under the alias's companion module.
    testCase "array of an anonymous literal does not render as self-cyclic ResizeArray<X>" <| fun _ ->
        let out = renderTypeAliasSurface arrayAlias
        let abbrevLine =
            out.Split('\n')
            |> Array.map _.Trim()
            |> Array.tryFind (_.StartsWith("type AiImageClassificationOutput ="))
        match abbrevLine with
        | Some line ->
            Expect.isFalse
                (line.Contains("ResizeArray<AiImageClassificationOutput>"))
                (sprintf "alias must not be a self-cyclic abbreviation; got: %s" line)
        | None -> ()

    // PROOF of the positive shape: the union alias references the two POSITIONAL case names and
    // emits a DISTINCT nested def for each (the members are structurally distinct, so they must NOT
    // be deduped). This is the emission/reference symmetry the fix establishes.
    testCase "union of distinct anonymous literals emits distinct Case0/Case1 nested defs" <| fun _ ->
        let out = renderTypeAliasSurface unionAlias
        Expect.isTrue
            (out.Contains("EmailAttachment.Case0") && out.Contains("EmailAttachment.Case1"))
            (sprintf "alias must reference EmailAttachment.Case0 and .Case1; got:\n%s" out)
        // Each distinct member literal must be emitted as its own nested type def.
        Expect.isTrue (out.Contains("type Case0")) (sprintf "missing nested 'type Case0' def:\n%s" out)
        Expect.isTrue (out.Contains("type Case1")) (sprintf "missing nested 'type Case1' def:\n%s" out)
        // The two members are distinct (memberA has `contentId`/`filename`; memberB has `cid`/`name`),
        // so both distinguishing property sets must survive — i.e. the members were NOT deduped.
        Expect.isTrue (out.Contains("contentId") && out.Contains("cid"))
            (sprintf "distinct member properties must both be present (no dedup); got:\n%s" out)

    // A union body mixing a PRIMITIVE with TEMPLATE-LITERAL members must NOT render the self-cyclic
    // `U3<WorkflowRetentionDuration, WorkflowRetentionDuration, ...>`. Each template literal gets a
    // positional `Case{i}` identity (erased to `string`); the primitive stays a bare member.
    testCase "union of number + template literals does not render as self-cyclic U<n><X, X, ...>" <| fun _ ->
        let out = renderTypeAliasSurface templateLiteralUnionAlias
        let abbrevLine =
            out.Split('\n')
            |> Array.map _.Trim()
            |> Array.tryFind (_.StartsWith("U3<"))
            |> Option.orElse (
                out.Split('\n') |> Array.map _.Trim()
                |> Array.tryFind (_.StartsWith("type WorkflowRetentionDuration =")))
        match abbrevLine with
        | Some _ ->
            // The cyclic shape repeats the alias name as a bare (non-dotted) type argument.
            Expect.isFalse
                (out.Contains("WorkflowRetentionDuration,\n") && out.Contains("U3<\n"))
                "template-literal union alias must not be a self-cyclic abbreviation"
            // No bare self-reference as a union arg.
            Expect.isFalse
                (out.Replace(" ", "").Replace("\n", "").Contains("U3<WorkflowRetentionDuration,WorkflowRetentionDuration"))
                (sprintf "alias must not be self-cyclic; got:\n%s" out)
        | None -> ()

    // PROOF of the positive shape: each template-literal member receives a distinct positional
    // `Case{i}` identity erased to `string`, the references are `.Case{i}`-qualified (not the bare
    // alias name), and the `number` primitive member is preserved (arity not reduced).
    testCase "union of number + template literals emits distinct Case{i} = string defs and keeps the primitive" <| fun _ ->
        let out = renderTypeAliasSurface templateLiteralUnionAlias
        // Two template-literal members -> two positional case identities, each erased to string.
        Expect.isTrue
            (out.Contains("WorkflowRetentionDuration.Case0") && out.Contains("WorkflowRetentionDuration.Case1"))
            (sprintf "alias must reference WorkflowRetentionDuration.Case0 and .Case1; got:\n%s" out)
        Expect.isTrue
            (out.Contains("type Case0 = string") && out.Contains("type Case1 = string"))
            (sprintf "each template-literal member must emit a nested 'type Case{i} = string' def; got:\n%s" out)
        // The `number` primitive member must survive (float), so the union is NOT reduced to only
        // the template-literal cases.
        Expect.isTrue (out.Contains("float"))
            (sprintf "the number primitive union member must be preserved as float; got:\n%s" out)
]

[<Tests>]
let tests = testList "AliasBodyLiteralPlacement" [
    aliasBodyLiteralTests
]
