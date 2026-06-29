module Xantham.Generator.Tests.Tests.NestedUnderTypePlacement

// Isolated characterization of the NESTED-HOIST PLACEMENT bug (the dominant FS0039
// class on the cloudflare surface, ~900 errors).
//
// THE BUG (reproduced end-to-end below, see `multiLevelNestedHoist`):
//   A property/return type that is an inline object literal HOISTS to a per-owner
//   nested type. When such a hoisted literal is ITSELF nested under ANOTHER hoisted
//   literal (i.e. two levels deep), the INNER hoisted type's DEFINITION is dropped:
//   its reference is emitted (correctly path-qualified, e.g. `GetSchedules.TimeRange`)
//   but no `type TimeRange` is ever placed under `module GetSchedules`, so every such
//   reference dangles (FS0039 "type 'TimeRange' is not defined in '...GetSchedules'").
//
// MECHANISM (the emission-vs-reference asymmetry):
//   * The REFERENCE atom for the inner literal is built (in Render.Member /
//     createTransientPath, RenderScope.Prelude.fs:216) against the OUTER literal's
//     scope whose PathContext was extended with the property name -> the stored
//     transient carries the leaf `TimeRange` (`Moored(Anchored, "TimeRange")`).
//   * The DEFINITION's placement, however, is driven by
//     `anchorPreludeAnchorScope` (RenderScope.Anchored.fs:497-504). Its Transient
//     branch anchors the inner scope's OWN `Root`, which is the NAMELESS
//     `TransientTypePath.Anchored` (set in prerender's TypeLiteral `_,_` arm,
//     RenderScope.Prelude.fs:467). Anchoring an `Anchored` (empty trace) transient
//     against the parent's Type anchor COLLAPSES to exactly the parent's path
//     (`Mod.Agent.GetSchedules`) — the `TimeRange` leaf carried only by the
//     parent-stored transient is discarded. So the inner def is placed at the SAME
//     TypePath as its parent (verified: both `anchors` entries flatten to
//     `["Mod";"Agent";"GetSchedules"]`), then truncated to module `Mod.Agent` and
//     merged into / shadowed by the parent type. No `module GetSchedules` / `type
//     TimeRange` is emitted; the reference dangles.
//
// ROOT (NamePath.fs): `TypePath = { Parent: ModulePath; Name }` — a type's parent is
//   always a MODULE. A type nested under a TYPE is representable only as a companion
//   module (the single-level case works — see `singleLevelNestedHoist`). The defect
//   is NOT "no representable target"; it is that the inner scope's nameless
//   `Anchored` root is anchored instead of the parent-stored named transient, so the
//   leaf is lost and the def collapses onto its parent.
//
// The single-level case (a method/property whose type is one object literal) is
// CORRECT and emitted; that passing test pins the contrast. The two-level case is
// marked pending (ptestCase) until the placement is fixed.

open System.Collections.Generic
open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Mocking.ArenaInterner.ResolvedType

// Build an inline object literal with >=3 plain string properties (so it hoists).
let private objLit (names: string list) =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        for n in names do Property.create n (primitive TypeKindPrimitive.String) |> Property.wrap
    ]
    |> TypeLiteral.wrap

// Render an interface export end-to-end: register -> collectModules -> renderRoot.
// This is exactly the pipeline `Render.fs` main runs (processExports omitted: we
// register the single interface directly).
let private renderInterfaceSurface (iface: Interface) : string =
    let ctx = GeneratorContext.Empty
    registerAnchorFromExport ctx (ResolvedExport.Interface iface)
    let root = RootModule.collectModules ctx
    Ast.Oak() {
        Ast.AnonymousModule() {
            renderRoot ctx root
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// ---------------------------------------------------------------------------
// SINGLE-LEVEL nested hoist — CORRECT today (companion module emitted).
// interface Owner { content: { x; y; z } }  ->
//   module Mod =
//     type Owner = abstract content: Owner.Content ...
//     module Owner = type Content = ...
// ---------------------------------------------------------------------------
let singleLevelTests = testList "single-level nested hoist (correct)" [
    testCase "property object-literal emits BOTH the ref and a companion-module def" <| fun _ ->
        let iface =
            { (Interface.create "Owner" |> Interface.withPath [ "Mod" ]) with
                Members = [ Property.create "content" (objLit [ "x"; "y"; "z" ]) |> Property.wrap ] }
        let out = renderInterfaceSurface iface
        // Reference is emitted, path-qualified against the owning type.
        Expect.stringContains out "abstract content: Owner.Content" "ref Owner.Content emitted"
        // AND the companion module + nested type definition is emitted.
        Expect.stringContains out "module Owner =" "companion module Owner emitted"
        Expect.stringContains out "type Content =" "nested type Content definition emitted"
]

// ---------------------------------------------------------------------------
// MULTI-LEVEL (two-deep) nested hoist — the BUG. Pending until fixed.
// interface Agent { getSchedules(criteria): { timeRange: { fromMs; toMs; scale }; id; description } }
// `Criteria` hoists under the method -> Mod.Agent.GetSchedules (CORRECT, emitted).
// `TimeRange` hoists under Criteria -> SHOULD be Mod.Agent.GetSchedules.TimeRange,
// emitted as `module GetSchedules = type TimeRange = ...`. TODAY: dropped/merged.
// ---------------------------------------------------------------------------
let private agentWithDeepNestedLiteral =
    let timeRange = objLit [ "fromMs"; "toMs"; "scale" ]
    let criteria =
        TypeLiteral.empty
        |> TypeLiteral.withMembers [
            Property.create "timeRange" timeRange |> Property.wrap
            Property.create "id" (primitive TypeKindPrimitive.String) |> Property.wrap
            Property.create "description" (primitive TypeKindPrimitive.String) |> Property.wrap
        ]
        |> TypeLiteral.wrap
    let getSchedules : Method = {
        Name = Name.Camel.create "getSchedules"
        Parameters = [ primitive TypeKindPrimitive.String |> Parameter.create "criteria" ]
        Type = LazyContainer.CreateFromValue criteria
        Documentation = []
        IsOptional = false
        IsStatic = false
    }
    { (Interface.create "Agent" |> Interface.withPath [ "Mod" ]) with
        Members = [ Member.Method [ getSchedules ] ] }

let multiLevelTests = testList "multi-level nested hoist placement" [
    // PENDING — fails today: the inner `TimeRange` definition is dropped.
    //
    // Confirmed failure mode (instrumented): the Agent export scope's `anchors` dict
    // gets TWO entries — the Criteria literal AND the TimeRange literal — but BOTH
    // flatten to the SAME TypePath ["Mod";"Agent";"GetSchedules"]. The inner literal
    // is anchored from its OWN nameless `TransientTypePath.Anchored` root (not the
    // parent-stored `Moored(Anchored,"TimeRange")`), so the `TimeRange` leaf is lost
    // and the def collapses onto its parent. Result render:
    //
    //   module Agent =
    //     type GetSchedules =
    //         abstract timeRange: GetSchedules.TimeRange with get, set   // ref dangles
    //         ... (TimeRange's own members fromMs/toMs/scale wrongly MERGED in here) ...
    //   // NO `module GetSchedules` / `type TimeRange` anywhere.
    //
    // CORRECT behaviour (asserted): emit a companion `module GetSchedules` holding
    // `type TimeRange`, so the `GetSchedules.TimeRange` reference resolves.
    testCase "inner (two-deep) object-literal emits its nested-type definition, not just the ref" <| fun _ ->
        let out = renderInterfaceSurface agentWithDeepNestedLiteral
        // The outer hoist works today: GetSchedules is emitted under module Agent.
        Expect.stringContains out "type GetSchedules =" "outer hoisted type GetSchedules emitted"
        // The inner reference is emitted, path-qualified against GetSchedules.
        Expect.stringContains out "GetSchedules.TimeRange" "inner ref GetSchedules.TimeRange emitted"
        // THE BUG: the inner DEFINITION must also be emitted as a companion module of
        // GetSchedules. Today there is no `module GetSchedules` and no `type TimeRange`.
        Expect.stringContains out "module GetSchedules =" "companion module GetSchedules emitted for the inner hoist"
        Expect.stringContains out "type TimeRange =" "inner nested type TimeRange definition emitted"
]

[<Tests>]
let tests = testList "NestedUnderTypePlacement" [
    singleLevelTests
    multiLevelTests
]
