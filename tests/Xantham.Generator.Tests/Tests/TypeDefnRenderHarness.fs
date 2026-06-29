module Xantham.Generator.Tests.Tests.TypeDefnRenderHarness

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

// Harness for rendering a full type DEFINITION (interface/class members, enum/string-literal
// union cases) to its F# source string — the dual of `TypeRefRender.testRender`, which only
// renders a type REFERENCE. This exercises the otherwise-uncovered emission passes
// (Render.Member, Render.TypeShapes, Render.Enum, Render.Transient).
//
// Pipeline (mirrors what `renderRoot`/`renderModule` do in Render.Collection.fs):
//   ResolvedType.Interface/Enum/Union
//     -> TestHelper.prerender ctx   (populates PreludeRenders with a RenderScope whose
//                                     `Render` is a lazy Concrete.TypeRender)
//     -> GeneratorContext.Prelude.tryGet ctx   (recover that RenderScope)
//     -> Render.Concrete.anchor ctx   (Concrete.Render -> Anchored.Render = ref * lazy TypeRender)
//     -> snd >> _.Value   (the Anchored.TypeRender)
//     -> renderInterface | renderUnion | renderEnum   (-> WidgetBuilder<TypeDefn>)
//     -> Ast.Oak() { Ast.AnonymousModule() { ... } } |> Gen.mkOak |> Gen.run

/// Anchor a prerendered render scope into an `Anchored.TypeRender`.
///
/// A nominal definition (interface/class/named enum) registers a `Render.Concrete` tuple,
/// anchored directly. An anonymous string-literal / numeric-literal union is HOISTED by the
/// prelude to a canonical `LiteralUnions.<name>` type carrying a `Render.Transient` tuple whose
/// own absolute path is its anchor (mirrors `anchorPreludeAnchorScope`'s Anchored+Transient
/// branch in RenderScope.Anchored.fs).
let private anchorToTypeRender (ctx: GeneratorContext) (resolvedType: ResolvedType) : Anchored.TypeRender =
    TestHelper.prerender ctx resolvedType |> ignore
    match GeneratorContext.Prelude.tryGet ctx resolvedType with
    | ValueSome { Render = Render.Concrete renderTuple } ->
        Render.Concrete.anchor ctx renderTuple
        |> snd
        |> _.Value
    | ValueSome { Root = ValueSome (TypeLikePath.Anchored path); Render = Render.Transient renderTuple } ->
        Render.Transient.anchor ctx (AnchorPath.create path) renderTuple
        |> snd
        |> _.Value
    | ValueSome other ->
        failwithf "Expected a Concrete or hoisted-Transient render scope for a full TypeDefn but got: %A" other.Render
    | ValueNone ->
        failwith "Expected a prerendered render scope to be registered in the context"

let private renderTypeDefnWidget (ctx: GeneratorContext) (typeRender: Anchored.TypeRender) =
    Ast.Oak() {
        Ast.AnonymousModule() {
            match typeRender with
            | Anchored.TypeRender.TypeDefn typeLikeRender ->
                TypeLikeRender.renderInterface ctx typeLikeRender
            | Anchored.TypeRender.StringUnion literalUnionRender ->
                LiteralUnionRender.renderUnion ctx literalUnionRender
            | Anchored.TypeRender.EnumUnion literalUnionRender ->
                LiteralUnionRender.renderEnum ctx literalUnionRender
            | other ->
                failwithf "renderTypeDefn does not support this TypeRender shape: %A" other
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

/// Render a full type DEFINITION (interface/class with members, or an enum / string-literal
/// union) to its emitted F# source string.
let renderTypeDefn (resolvedType: ResolvedType) : string =
    // A FRESH context per call: GeneratorContext's prelude render cache is keyed by value-equal
    // ResolvedType, so sharing one ctx across tests lets a primitive rendered (and collapsed)
    // in one test poison every later test that mentions a value-equal type — making primitive
    // assertions order-dependent. Per-call isolation makes each render deterministic.
    let ctx = GeneratorContext.Empty
    resolvedType
    |> anchorToTypeRender ctx
    |> renderTypeDefnWidget ctx

let private interfaceWithProperties =
    let iface =
        { (Mocking.ArenaInterner.ResolvedType.Interface.create "Point") with
            Members = [
                Property.create "x" (primitive TypeKindPrimitive.Number) |> Property.wrap
                Property.create "y" (primitive TypeKindPrimitive.Number) |> Property.wrap
                Property.create "label" (primitive TypeKindPrimitive.String)
                |> Property.readOnly
                |> Property.wrap
            ] }
    ResolvedType.Interface iface

let private interfaceWithMethod =
    let greet : Method = {
        Name = Name.Camel.create "greet"
        Parameters = [ primitive TypeKindPrimitive.String |> Parameter.create "name" ]
        Type = LazyContainer.CreateFromValue (primitive TypeKindPrimitive.String)
        Documentation = []
        IsOptional = false
        IsStatic = false
    }
    let iface =
        { (Mocking.ArenaInterner.ResolvedType.Interface.create "Greeter") with
            Members = [ Member.Method [ greet ] ] }
    ResolvedType.Interface iface

let private stringLiteralUnion =
    [
        Literal.wrap (Literal.create "primary-only")
        Literal.wrap (Literal.create "all-replicas")
    ]
    |> Union.create

[<Tests>]
let tests =
    testList "TypeDefnRenderHarness" [
        // (a) An interface with several properties emits one `abstract` member per property.
        // Accessor maps directly: ReadWrite -> `with get, set`, ReadOnly -> `with get`.
        // Member ORDER is the prelude's emission order (reversed vs. source) — assert what is
        // actually produced, not what is intuitive.
        testCase "interface with properties renders abstract members" <| fun _ ->
            renderTypeDefn interfaceWithProperties
            |> Flip.Expect.equal "interface property TypeDefn"
                (String.concat "\n" [
                    "type Point ="
                    "    abstract label: string with get"
                    "    abstract y: float with get, set"
                    "    abstract x: float with get, set"
                ])

        // (b) A method member emits a single curried-signature abstract member with named
        // parameters: `abstract greet: name: string -> string`.
        testCase "interface with method renders abstract method" <| fun _ ->
            renderTypeDefn interfaceWithMethod
            |> Flip.Expect.equal "interface method TypeDefn"
                (String.concat "\n" [
                    "type Greeter ="
                    "    abstract greet: name: string -> string"
                ])

        // (c) An anonymous string-literal union is hoisted to a canonical `LiteralUnions.<name>`
        // DU; the harness renders that hoisted TypeDefn. Each case carries a `[<CompiledName>]`
        // mapping the Pascal case name back to its raw JS string literal. The hoisted name is
        // derived from the (sorted, joined) literal values.
        testCase "string-literal union renders DU cases with CompiledName" <| fun _ ->
            renderTypeDefn stringLiteralUnion
            |> Flip.Expect.equal "string-literal union TypeDefn"
                (String.concat "\n" [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type AllReplicasPrimaryOnly ="
                    "    | [<CompiledName(\"primary-only\")>] PrimaryOnly"
                    "    | [<CompiledName(\"all-replicas\")>] AllReplicas"
                ])
    ]
