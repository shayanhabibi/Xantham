module Xantham.Generator.Tests.Tests.EnumRender

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Mocking.ArenaInterner.ResolvedType

// Exhaustive coverage of the enum / literal-union BODY emission passes reachable through the
// `renderTypeDefn` harness:
//   * Render.Enum.fs        — a NAMED `ResolvedType.Enum` (Enum.render -> renderEnum / renderStringUnion)
//   * Render.Transient.fs   — an anonymous literal `ResolvedType.Union` hoisted to a canonical
//                             `LiteralUnions.<name>` (Union.renderLiterals -> renderEnumLiterals /
//                             renderUnionLiterals), plus the single-literal erase-to-primitive path.
// Final case emission is in TypeRender.Render.fs (LiteralCaseRender.renderEnumCase / renderUnionCase,
// LiteralUnionRender.renderEnum / renderUnion). Every expected string below was probed first via
// renderTypeDefn, then asserted verbatim.
let private render = Xantham.Generator.Tests.Tests.TypeDefnRenderHarness.renderTypeDefn

let private lines = String.concat "\n"

// ---- named enums (Render.Enum.fs) ------------------------------------------------------------

/// A named enum whose cases are ALL int-valued. `Enum.render` (Render.Enum.fs:87) routes to
/// `renderEnum`, producing a CLR-style `Ast.Enum` (`type X = | Case = n`). NOTE: a named int enum
/// is NOT decorated with `[<RequireQualifiedAccess; StringEnum>]` (that decoration is only added
/// to the StringUnion/DU path in TypeRender.Render.fs:186) — it is a plain F# enum.
let private intEnum =
    let e = Mocking.ArenaInterner.ResolvedType.Enum.create "Color"
    { e with
        Members =
            [ EnumCase.create "Red" (TsLiteral.Int 0) e |> Lazy.CreateFromValue
              EnumCase.create "Green" (TsLiteral.Int 1) e |> Lazy.CreateFromValue
              EnumCase.create "Blue" (TsLiteral.Int 2) e |> Lazy.CreateFromValue ] }
    |> ResolvedType.Enum

/// A named enum with NON-int (string) case values. `Enum.render` falls through to
/// `renderStringUnion` (Render.Enum.fs:88-90 guard: `forall IsInt`), so it renders as a Fable
/// StringEnum DU with a `[<CompiledName>]` per case, NOT a CLR enum.
let private stringValuedEnum =
    let e = Mocking.ArenaInterner.ResolvedType.Enum.create "Suit"
    { e with
        Members =
            [ EnumCase.create "Hearts" (TsLiteral.String "hearts") e |> Lazy.CreateFromValue
              EnumCase.create "Spades" (TsLiteral.String "spades") e |> Lazy.CreateFromValue ] }
    |> ResolvedType.Enum

/// A named enum whose case NAMES are reserved-word-ish ("type", "class"). The mocked EnumCase
/// builder Pascal-cases the name, so they sanitize to `Type` / `Class`.
let private sanitizeEnum =
    let e = Mocking.ArenaInterner.ResolvedType.Enum.create "Weird"
    { e with
        Members =
            [ EnumCase.create "type" (TsLiteral.Int 0) e |> Lazy.CreateFromValue
              EnumCase.create "class" (TsLiteral.Int 1) e |> Lazy.CreateFromValue ] }
    |> ResolvedType.Enum

// ---- anonymous literal unions (Render.Transient.fs) ------------------------------------------

/// Two string literals. Hoisted to `LiteralUnions.<name>` where the name is the SORTED+joined
/// PascalCase of the values (AllReplicas + PrimaryOnly -> `AllReplicasPrimaryOnly`). Each case
/// carries a `[<CompiledName>]` mapping back to the raw JS literal.
let private stringLitUnion2 =
    [ Literal.wrap (Literal.create "primary-only")
      Literal.wrap (Literal.create "all-replicas") ]
    |> Union.create

/// Three string literals incl. a hyphenated one. "scale-down" -> case `ScaleDown` with
/// `[<CompiledName("scale-down")>]`. Case emission order follows SOURCE order (scale-down, cover,
/// contain), while the hoisted TYPE name is sorted (Contain, Cover, ScaleDown).
let private stringLitUnion3 =
    [ Literal.wrap (Literal.create "scale-down")
      Literal.wrap (Literal.create "cover")
      Literal.wrap (Literal.create "contain") ]
    |> Union.create

/// All-int literal union -> `renderEnumLiterals` (Render.Transient.fs:151-162 guard). Hoisted to
/// a CLR `Ast.Enum`; numeric case names are backtick-escaped (`` ``1`` = 1 ``).
let private intLitUnion =
    [ Literal.wrap (Literal.create 1)
      Literal.wrap (Literal.create 2)
      Literal.wrap (Literal.create 3) ]
    |> Union.create

/// A literal union MIXING a string and an int. Because not every member is int, it routes to the
/// StringUnion DU path. The string case gets `[<CompiledName>]`, the int case gets
/// `[<CompiledValue(n)>]` (TypeRender.Render.fs:154-156) with a backtick-escaped name.
let private mixedLitUnion =
    [ Literal.wrap (Literal.create "alpha")
      Literal.wrap (Literal.create 7) ]
    |> Union.create

/// A string literal whose value already equals its PascalCase name ("Foo") gets NO CompiledName
/// (TypeRender.Render.fs:151 `value <> name` guard); the other ("bar") does.
let private litValueEqualsName =
    [ Literal.wrap (Literal.create "Foo")
      Literal.wrap (Literal.create "bar") ]
    |> Union.create

/// A SINGLE-value literal union. Per a prior fix, a single-member literal union erases to its
/// underlying primitive (here `string`) rather than being hoisted to a 1-case DU — so the prelude
/// registers a `Render.RefOnly` atom, not a renderable TypeDefn. Characterized by the harness
/// rejecting it (it is not a Concrete/Transient TypeDefn scope).
let private singleLitUnion =
    [ Literal.wrap (Literal.create "solo") ]
    |> Union.create

[<Tests>]
let tests =
    testList "EnumRender" [
        // (a) named int enum -> plain CLR `Ast.Enum`, no StringEnum decoration.
        testCase "named int enum renders CLR enum with values" <| fun _ ->
            render intEnum
            |> Flip.Expect.equal "int enum TypeDefn"
                (lines [
                    "type Color ="
                    "    | Red = 0"
                    "    | Green = 1"
                    "    | Blue = 2"
                ])

        // (b) named enum with string case VALUES -> StringEnum DU (renderStringUnion fallthrough).
        testCase "named string-valued enum renders StringEnum DU" <| fun _ ->
            render stringValuedEnum
            |> Flip.Expect.equal "string-valued enum TypeDefn"
                (lines [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type Suit ="
                    "    | [<CompiledName(\"hearts\")>] Hearts"
                    "    | [<CompiledName(\"spades\")>] Spades"
                ])

        // (c) enum case-name sanitization (Pascal-casing) for "type"/"class".
        testCase "enum case names are sanitized" <| fun _ ->
            render sanitizeEnum
            |> Flip.Expect.equal "sanitized enum TypeDefn"
                (lines [
                    "type Weird ="
                    "    | Type = 0"
                    "    | Class = 1"
                ])

        // (d) 2-member string-literal union -> hoisted StringEnum DU with CompiledName per case.
        testCase "two-member string-literal union renders StringEnum DU" <| fun _ ->
            render stringLitUnion2
            |> Flip.Expect.equal "string-literal union (2) TypeDefn"
                (lines [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type AllReplicasPrimaryOnly ="
                    "    | [<CompiledName(\"primary-only\")>] PrimaryOnly"
                    "    | [<CompiledName(\"all-replicas\")>] AllReplicas"
                ])

        // (e) 3+ member string-literal union; hyphenated "scale-down" -> ScaleDown via CompiledName.
        testCase "three-member string-literal union maps special chars via CompiledName" <| fun _ ->
            render stringLitUnion3
            |> Flip.Expect.equal "string-literal union (3) TypeDefn"
                (lines [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type ContainCoverScaleDown ="
                    "    | [<CompiledName(\"scale-down\")>] ScaleDown"
                    "    | [<CompiledName(\"cover\")>] Cover"
                    "    | [<CompiledName(\"contain\")>] Contain"
                ])

        // (f) all-int literal union -> hoisted CLR enum (renderEnumLiterals), backtick-escaped names.
        testCase "int-literal union renders CLR enum" <| fun _ ->
            render intLitUnion
            |> Flip.Expect.equal "int-literal union TypeDefn"
                (lines [
                    "type I1I2I3 ="
                    "    | ``1`` = 1"
                    "    | ``2`` = 2"
                    "    | ``3`` = 3"
                ])

        // (g) mixed string+int literal union -> StringEnum DU; int case uses CompiledValue.
        testCase "mixed literal union renders StringEnum DU with CompiledValue for int" <| fun _ ->
            render mixedLitUnion
            |> Flip.Expect.equal "mixed literal union TypeDefn"
                (lines [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type AlphaI7 ="
                    "    | [<CompiledName(\"alpha\")>] Alpha"
                    "    | [<CompiledValue(7)>] ``7``"
                ])

        // (h) CompiledName is omitted when the literal value already equals its PascalCase name.
        testCase "string-literal union omits CompiledName when value equals case name" <| fun _ ->
            render litValueEqualsName
            |> Flip.Expect.equal "value-equals-name union TypeDefn"
                (lines [
                    "[<RequireQualifiedAccess; StringEnum(CaseRules.None)>]"
                    "type FooBar ="
                    "    | Foo"
                    "    | [<CompiledName(\"bar\")>] Bar"
                ])

        // (i) single-value literal union erases to its primitive — not a renderable TypeDefn.
        // The harness rejects the resulting RefOnly atom; assert the characteristic failure.
        testCase "single-value literal union erases to primitive (no TypeDefn)" <| fun _ ->
            Expect.throwsC
                (fun () -> render singleLitUnion |> ignore)
                (fun ex ->
                    Expect.stringContains ex.Message "RefOnly"
                        "single literal should erase to a RefOnly primitive atom, not a hoisted DU")
    ]
