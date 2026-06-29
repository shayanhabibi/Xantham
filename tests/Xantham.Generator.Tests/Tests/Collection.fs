module Xantham.Generator.Tests.Tests.Collection

// Isolated test suite for the GENERATOR COLLECTION pass.
//
// Pass under test: `RootModuleCollector.collectModules` (Render.Collection.fs).
// CONTRACT: given a GeneratorContext whose `AnchorRenders` hold anchored
// RenderScopes (each carrying a `Root: Choice<TypePath, MemberPath>` and a lazy
// `Anchored.TypeRender`), assemble the flat set of anchored renders into a module
// tree `RootModuleCollector { Types: ResizeArray<TypeRender>; Modules: Dictionary<string, ModuleCollector> }`.
//
// The PLACEMENT rule (Render.Collection.fs:145-149): for each render with an
// anchored TypePath, FLATTEN the path to a `string list`, then `List.truncate
// (length - 1)` — i.e. DROP THE LAST SEGMENT (the type's own name) to obtain the
// MODULE PATH the type lives under. So a type flattening to [A; B; C] is placed
// in module-path [A; B] (nested module A.B). Then `collect` walks that module
// path from the dummy root, creating intermediate ModuleCollector nodes on demand
// and finally `Types.Add`-ing the render at the leaf module.
//
// We drive the pass two ways:
//   1. END-TO-END through `registerAnchorFromExport` (RenderScope.Anchored.fs):
//      build a `ResolvedExport.Interface` from the Mocking helpers with a chosen
//      `withPath` module prefix, register it (populates ctx.AnchorRenders with a
//      real anchored RenderScope), then call collectModules and assert the tree.
//      Note: `Mocking.Interface.create "Bar"` with no withPath yields the
//      single-segment qualified name [Name "Bar"], which `createModulePath` maps
//      to the synthetic `Global` module — so a "bare" interface flattens to
//      [Global; Bar] and lands UNDER module `Global`, NOT at the anonymous root.
//      `withPath ["Foo"]` yields [Foo; Bar] -> module Foo.
//   2. By HAND-BUILDING RenderScopes with arbitrary `Root` TypePaths (reusing a
//      real anchored TypeRender harvested from a registered interface as the lazy
//      payload) and `addResolvedType`-ing them in — this lets us reach paths the
//      export pipeline can't synthesise (e.g. a true root-level type whose module
//      path is empty), and to assert prefix-sharing / intermediate-module
//      construction precisely.

open System.Collections.Generic
open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Mocking.ArenaInterner.ResolvedType

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

// Register an interface export and return the fresh context it populated.
let private ctxWithInterfaces (interfaces: Interface list) =
    let ctx = GeneratorContext.Empty
    interfaces
    |> List.iter (fun iface ->
        registerAnchorFromExport ctx (ResolvedExport.Interface iface))
    ctx

// Harvest ONE real anchored TypeRender + TypeRef from a registered interface so
// we can reuse it as the lazy payload of a hand-built RenderScope. The collection
// pass never inspects the payload's content (it only `Types.Add`s it), so any
// well-formed anchored render is a faithful stand-in.
let private sampleAnchoredRender () =
    let ctx = GeneratorContext.Empty
    let iface = Interface.create "Sample"
    registerAnchorFromExport ctx (ResolvedExport.Interface iface)
    ctx.AnchorRenders
    |> Seq.pick (function
        | KeyValue(_, Choice2Of2 scope) -> Some scope
        | _ -> None)
    |> fun scope -> scope.Render

// Build a hand RenderScope anchored at a concrete TypePath, reusing a shared
// anchored render payload. `addResolvedType` keys by ResolvedType, so each scope
// needs a distinct ResolvedType key or it overwrites — we key by the interface
// the path was built from.
let private handScope (keyType: ResolvedType) (path: TypePath) (render: Anchored.Render) : Anchored.RenderScope =
    {
        Type = keyType
        Root = Choice1Of2 path
        TypeRef = Anchored.TypeRefRender.create false path
        Render = render
        Anchors = Dictionary<ResolvedType, TypePath * Anchored.Render>()
    }

let private addScope (ctx: GeneratorContext) (scope: Anchored.RenderScope) =
    GeneratorContext.Anchored.addResolvedType ctx scope.Type (Choice2Of2 scope)

// Flatten a registered scope's anchored TypePath to plain strings (the exact
// input the placement rule consumes).
let private rootPathStrings (scope: Anchored.RenderScope) =
    match scope.Root with
    | Choice1Of2 typePath ->
        TypePath.flatten typePath |> List.map Name.Case.valueOrModified
    | Choice2Of2 _ -> []

// Recursively flatten a built tree into (modulePath, typeCount) pairs for easy
// assertion. modulePath is the chain of ModuleCollector.Name keys.
let rec private describe (prefix: string list) (modules: Dictionary<string, ModuleCollector>) =
    [
        for KeyValue(name, m) in modules do
            let here = prefix @ [ name ]
            yield here, m.Types.Count
            yield! describe here m.Modules
    ]

// ---------------------------------------------------------------------------
// PLACEMENT RULE: path truncation drops the type's own name.
// ---------------------------------------------------------------------------
let placementTests = testList "placement rule (truncate last segment)" [
    testCase "bare interface (no module prefix) anchors under the synthetic Global module" <| fun _ ->
        // FullyQualifiedName [Name "Bar"] -> module path [] -> createModulePath -> Global.
        let ctx = ctxWithInterfaces [ Interface.create "Bar" ]
        let scope =
            ctx.AnchorRenders
            |> Seq.pick (function KeyValue(_, Choice2Of2 s) -> Some s | _ -> None)
        rootPathStrings scope
        |> Flip.Expect.equal "bare interface flattens to [Global; Bar]" [ "Global"; "Bar" ]

    testCase "nested interface flattens to [Module; Type]" <| fun _ ->
        let ctx = ctxWithInterfaces [ Interface.create "Bar" |> Interface.withPath [ "Foo" ] ]
        let scope =
            ctx.AnchorRenders
            |> Seq.pick (function KeyValue(_, Choice2Of2 s) -> Some s | _ -> None)
        rootPathStrings scope
        |> Flip.Expect.equal "Foo.Bar flattens to [Foo; Bar]" [ "Foo"; "Bar" ]

    testCase "type lands in the module obtained by dropping its last path segment" <| fun _ ->
        // Foo.Bar -> module path [Foo]; the type Bar lives under module Foo.
        let ctx = ctxWithInterfaces [ Interface.create "Bar" |> Interface.withPath [ "Foo" ] ]
        let root = RootModuleCollector.collectModules ctx
        // No types at the anonymous root.
        Flip.Expect.equal "no root-level types" 0 root.Types.Count
        // Exactly one module `Foo` holding exactly one type.
        Flip.Expect.equal "tree shape" [ [ "Foo" ], 1 ] (describe [] root.Modules)
]

// ---------------------------------------------------------------------------
// DEEP NESTING: intermediate modules are built on the way down.
// ---------------------------------------------------------------------------
let deepNestingTests = testList "intermediate module construction" [
    testCase "deeply nested type creates every intermediate module node" <| fun _ ->
        // A.B.C.Leaf -> module path [A; B; C]; collect must create A, A.B, A.B.C.
        let ctx = ctxWithInterfaces [ Interface.create "Leaf" |> Interface.withPath [ "A"; "B"; "C" ] ]
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "no root types" 0 root.Types.Count
        let described = describe [] root.Modules |> List.sortBy fst
        Flip.Expect.equal
            "every intermediate module exists; only the leaf module holds the type"
            [ [ "A" ], 0; [ "A"; "B" ], 0; [ "A"; "B"; "C" ], 1 ]
            described

    testCase "the leaf module node actually contains the rendered type" <| fun _ ->
        let ctx = ctxWithInterfaces [ Interface.create "Leaf" |> Interface.withPath [ "A"; "B" ] ]
        let root = RootModuleCollector.collectModules ctx
        let a = root.Modules["A"]
        let b = a.Modules["B"]
        Flip.Expect.equal "A is empty of types" 0 a.Types.Count
        Flip.Expect.equal "B holds exactly the leaf" 1 b.Types.Count
]

// ---------------------------------------------------------------------------
// PREFIX SHARING: two types under the same module prefix share module nodes.
// ---------------------------------------------------------------------------
let prefixSharingTests = testList "shared-prefix module reuse" [
    testCase "two types in the same module share a single module node" <| fun _ ->
        let ctx =
            ctxWithInterfaces [
                Interface.create "Alpha" |> Interface.withPath [ "Shared" ]
                Interface.create "Beta" |> Interface.withPath [ "Shared" ]
            ]
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "exactly one Shared module" 1 root.Modules.Count
        Flip.Expect.equal "both types collected into the one Shared module" 2 root.Modules["Shared"].Types.Count

    testCase "types sharing only an outer prefix branch at the divergence point" <| fun _ ->
        // Shared.Left.A and Shared.Right.B share `Shared`, then fork.
        let ctx =
            ctxWithInterfaces [
                Interface.create "A" |> Interface.withPath [ "Shared"; "Left" ]
                Interface.create "B" |> Interface.withPath [ "Shared"; "Right" ]
            ]
        let root = RootModuleCollector.collectModules ctx
        let described = describe [] root.Modules |> List.sortBy fst
        Flip.Expect.equal
            "Shared is shared; Left and Right are siblings each holding one type"
            [ [ "Shared" ], 0
              [ "Shared"; "Left" ], 1
              [ "Shared"; "Right" ], 1 ]
            described

    testCase "disjoint module prefixes produce sibling top-level modules" <| fun _ ->
        let ctx =
            ctxWithInterfaces [
                Interface.create "A" |> Interface.withPath [ "First" ]
                Interface.create "B" |> Interface.withPath [ "Second" ]
            ]
        let root = RootModuleCollector.collectModules ctx
        let described = describe [] root.Modules |> List.sortBy fst
        Flip.Expect.equal
            "two disjoint top-level modules each with one type"
            [ [ "First" ], 1; [ "Second" ], 1 ]
            described
]

// ---------------------------------------------------------------------------
// ROOT PLACEMENT: a type whose flattened path is length 1 truncates to the
// empty module path and lands directly in the anonymous root's Types.
// Reached only via a hand-built RenderScope (the export pipeline always injects
// at least a Global/source module segment).
// ---------------------------------------------------------------------------
let rootPlacementTests = testList "root-level placement (empty module path)" [
    testCase "single-segment path places the type at the anonymous root" <| fun _ ->
        let render = sampleAnchoredRender ()
        let ctx = GeneratorContext.Empty
        // ModulePath.init "" is the empty-named root module; flatten skips empty
        // segments, so this TypePath flattens to just [Top] (length 1) and
        // truncates to [] -> root.
        let rootType =
            ModulePath.init ""
            |> TypePath.create "Top"
        addScope ctx (handScope (primitive TypeKindPrimitive.Any) rootType render)
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "type placed at anonymous root" 1 root.Types.Count
        Flip.Expect.equal "no modules created" 0 root.Modules.Count

    testCase "a root type and a nested type coexist" <| fun _ ->
        let render = sampleAnchoredRender ()
        let ctx = GeneratorContext.Empty
        let rootType = ModulePath.init "" |> TypePath.create "Top"
        let nestedType = ModulePath.init "Mod" |> TypePath.create "Inner"
        addScope ctx (handScope (primitive TypeKindPrimitive.Any) rootType render)
        addScope ctx (handScope (primitive TypeKindPrimitive.String) nestedType render)
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "one root-level type" 1 root.Types.Count
        Flip.Expect.equal "one nested module" [ [ "Mod" ], 1 ] (describe [] root.Modules)
]

// ---------------------------------------------------------------------------
// EMPTY / FILTERING: collectModules only consumes Choice2Of2 (RenderScope)
// entries; a Choice1Of2 (bare TypeRefRender ref-only) anchor is ignored, and an
// empty context yields an empty tree.
// ---------------------------------------------------------------------------
let filteringTests = testList "input filtering" [
    testCase "empty context yields an empty tree" <| fun _ ->
        let ctx = GeneratorContext.Empty
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "no types" 0 root.Types.Count
        Flip.Expect.equal "no modules" 0 root.Modules.Count

    testCase "a ref-only (Choice1Of2) anchor contributes nothing to the tree" <| fun _ ->
        let ctx = GeneratorContext.Empty
        // Register a bare TypeRefRender (the shape used when an export is ignored).
        let path = ModulePath.init "Foo" |> TypePath.create "Bar"
        let ref = Anchored.TypeRefRender.create false path
        GeneratorContext.Anchored.addResolvedType ctx (primitive TypeKindPrimitive.Any) (Choice1Of2 ref)
        let root = RootModuleCollector.collectModules ctx
        Flip.Expect.equal "ref-only anchor ignored: no types" 0 root.Types.Count
        Flip.Expect.equal "ref-only anchor ignored: no modules" 0 root.Modules.Count
]

[<Tests>]
let tests = testList "Collection" [
    placementTests
    deepNestingTests
    prefixSharingTests
    rootPlacementTests
    filteringTests
]
