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
// STATUS — PENDING (ptestCase). A confined generator fix (positional `CaseN`/`Element`
// PathContext on the Union/Array member prerender + re-deriving the member ref from the
// grafted TypeStore path) makes BOTH cases below pass in isolation and cuts FS0953 on the
// real cloudflare surface from 218 -> 20. BUT it net-regresses the real surface
// (total 1772 -> 1894; FS0039 554 -> 862) because alias bodies are frequently reached
// FIRST through a DEEP property context (e.g. some interface has `attachments:
// Array<EmailAttachment>`, so the union body is prerendered/cached at
// `...Attachments.Element` before the alias's own top-level prerender). The member literals
// then get DEEP names (`EmailAttachment.Attachments.Element.Case0`) that the placement
// machinery (anchorPreludeExportScope) cannot emit at that depth, so the references dangle
// (FS0039). The clean fix lives upstream (prerender-cache identity / union over-minting,
// encoder-side), not in a generator path patch — see the project memory notes
// (union-overminting, hoist-dedup-rootcause). Marked pending until that root is addressed.

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

let aliasBodyLiteralTests = testList "alias body of anonymous literals" [
    // PENDING — fails today: the union alias collapses to `U2<X, X>` (FS0953). Asserts the
    // CORRECT (non-cyclic) emission; goes green once the upstream root (see header) is fixed.
    ptestCase "union of distinct anonymous literals does not render as self-cyclic U2<X, X>" <| fun _ ->
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

    // PENDING — fails today: the array alias collapses to `ResizeArray<X>` referencing itself.
    ptestCase "array of an anonymous literal does not render as self-cyclic ResizeArray<X>" <| fun _ ->
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
]

[<Tests>]
let tests = testList "AliasBodyLiteralPlacement" [
    aliasBodyLiteralTests
]
