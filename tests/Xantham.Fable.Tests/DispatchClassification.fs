module DispatchClassification

// ---------------------------------------------------------------------------
// PER-PASS ISOLATION COVERAGE — ENCODER DISPATCH CLASSIFICATION
//
// The encoder dispatch passes (src/Xantham.Fable/Reading/Dispatch/*.fs) are
// side-effecting: TypeFlagPrimary.dispatch / TypeFlagLiteral.dispatch /
// TypeFlagObject.dispatch / LiteralTokenNode.dispatch all take a live
// `TypeScriptReader` (a ts-morph program + checker) and a `XanthamTag`, and
// mutate reactive signals. Those CANNOT be unit-tested without a live program.
//
// But the *classification* decision — "which DU case does a TS type/node map
// to, given its flags/kind?" — is the pure, high-value core that decides the
// type-mapping at the SOURCE. It lives in the `.Create` static members in
// src/Xantham.Fable/Types/XanTagKind.fs and is driven entirely by the lookup
// tables in `Internal` (typeFlagPrimaryKindSet / typeFlagLiteralKindSet /
// typeFlagObjectKindSet / literalTokenNodeKindSet / *KindSetMap). Each `Create`
// only reads `.flags` / `.objectFlags` / `.kind` off the supplied object, so a
// minimal mock object carrying just those fields exercises the exact branch
// selection the live encoder uses.
//
// This is Fable -> JS, so we build mocks with `createObj` and `unbox`. We assert
// the CORRECT classification (literal-before-primitive priority, UniqueESSymbol-
// before-ESSymbol, EnumLiteral-before-StringLiteral when both bits are set,
// objectFlags Reference-before-Anonymous, kind-exact maps, etc.).
// ---------------------------------------------------------------------------

open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Fable.Mocha

// A ts-morph Type is classified purely off its `.flags`. Build a mock carrying
// just that bit (or a combination of bits) and run the real classifier.
let private typeWithFlags (flags: Ts.TypeFlags) : Ts.Type =
    createObj [ "flags" ==> flags ] |> unbox

// A ts-morph ObjectType is classified off its `.objectFlags` (the `.flags`
// must carry Object too for TypeFlagPrimary.Create to route here).
let private objectTypeWithFlags (objectFlags: Ts.ObjectFlags) : Ts.ObjectType =
    createObj [
        "flags" ==> Ts.TypeFlags.Object
        "objectFlags" ==> objectFlags
    ] |> unbox

// A ts-morph Node is classified off its `.kind`.
let private nodeWithKind (kind: Ts.SyntaxKind) : Ts.Node =
    createObj [ "kind" ==> kind ] |> unbox

// ---------------------------------------------------------------------------
// TypeFlagPrimary.Create — TS TypeFlags -> primary IR category.
//   This is THE dispatch decision in TypeFlagPrimary.dispatch (the match on the
//   constructed `tag`). Cover every primary case + the priority-ordering edges.
// ---------------------------------------------------------------------------
let private typeFlagPrimaryTests =
    testList "TypeFlagPrimary.Create" [

        // --- the pure primitive flags -------------------------------------
        testCase "Any -> Any" <| fun _ ->
            "TypeFlags.Any classifies as TypeFlagPrimary.Any"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Any)).IsAny)

        testCase "Unknown -> Unknown" <| fun _ ->
            "TypeFlags.Unknown classifies as TypeFlagPrimary.Unknown"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Unknown)).IsUnknown)

        testCase "String -> String" <| fun _ ->
            "TypeFlags.String classifies as TypeFlagPrimary.String"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.String)).IsString)

        testCase "Number -> Number" <| fun _ ->
            "TypeFlags.Number classifies as TypeFlagPrimary.Number"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Number)).IsNumber)

        testCase "Boolean -> Boolean" <| fun _ ->
            "TypeFlags.Boolean classifies as TypeFlagPrimary.Boolean"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Boolean)).IsBoolean)

        testCase "BigInt -> BigInt" <| fun _ ->
            "TypeFlags.BigInt classifies as TypeFlagPrimary.BigInt"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.BigInt)).IsBigInt)

        testCase "ESSymbol -> ESSymbol" <| fun _ ->
            "TypeFlags.ESSymbol classifies as TypeFlagPrimary.ESSymbol"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.ESSymbol)).IsESSymbol)

        testCase "Void -> Void" <| fun _ ->
            "TypeFlags.Void classifies as TypeFlagPrimary.Void"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Void)).IsVoid)

        testCase "Undefined -> Undefined" <| fun _ ->
            "TypeFlags.Undefined classifies as TypeFlagPrimary.Undefined"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Undefined)).IsUndefined)

        testCase "Null -> Null" <| fun _ ->
            "TypeFlags.Null classifies as TypeFlagPrimary.Null"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Null)).IsNull)

        testCase "Never -> Never" <| fun _ ->
            "TypeFlags.Never classifies as TypeFlagPrimary.Never"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Never)).IsNever)

        testCase "NonPrimitive -> NonPrimitive" <| fun _ ->
            "TypeFlags.NonPrimitive classifies as TypeFlagPrimary.NonPrimitive"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.NonPrimitive)).IsNonPrimitive)

        testCase "Composite Unit flag routes by first-matching table entry (UniqueESSymbol bit)" <| fun _ ->
            // TypeFlags.Unit (109472) is a synthetic composite that never reaches
            // Create from a real single-flag type; this documents the table's
            // priority contract: Create does `Array.find (fst >> flags.HasFlag)`,
            // and HasFlag is a SUBSET test. Unit (109472) = Enum(32) | Boolean(16) |
            // Void(16384) | Undefined(32768) | Null(65536) | UniqueESSymbol(8192).
            // The first table entry whose whole flag is a subset is UniqueESSymbol
            // (8192), which precedes Enum/Boolean/Void/etc. in array order. So a
            // composite resolves to the highest-priority constituent, NOT to the
            // explicit trailing `Unit -> NonPrimitive` entry. (Documents priority,
            // not a bug — real types never carry this composite.)
            let result = TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Unit)
            "Unit's first table-matching constituent bit (UniqueESSymbol) wins"
            |> Expect.isTrue result.IsUniqueESSymbol

        // --- structured / instantiable ------------------------------------
        testCase "TypeParameter -> TypeParameter" <| fun _ ->
            "TypeFlags.TypeParameter classifies as TypeFlagPrimary.TypeParameter"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.TypeParameter)).IsTypeParameter)

        testCase "Union -> Union" <| fun _ ->
            "TypeFlags.Union classifies as TypeFlagPrimary.Union"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Union)).IsUnion)

        testCase "Intersection -> Intersection" <| fun _ ->
            "TypeFlags.Intersection classifies as TypeFlagPrimary.Intersection"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Intersection)).IsIntersection)

        testCase "Index -> Index" <| fun _ ->
            "TypeFlags.Index classifies as TypeFlagPrimary.Index"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Index)).IsIndex)

        testCase "IndexedAccess -> IndexedAccess" <| fun _ ->
            "TypeFlags.IndexedAccess classifies as TypeFlagPrimary.IndexedAccess"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.IndexedAccess)).IsIndexedAccess)

        testCase "Conditional -> Conditional" <| fun _ ->
            "TypeFlags.Conditional classifies as TypeFlagPrimary.Conditional"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Conditional)).IsConditional)

        testCase "Substitution -> Substitution" <| fun _ ->
            "TypeFlags.Substitution classifies as TypeFlagPrimary.Substitution"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.Substitution)).IsSubstitution)

        testCase "TemplateLiteral -> TemplateLiteral" <| fun _ ->
            "TypeFlags.TemplateLiteral classifies as TypeFlagPrimary.TemplateLiteral"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.TemplateLiteral)).IsTemplateLiteral)

        testCase "StringMapping -> StringMapping" <| fun _ ->
            "TypeFlags.StringMapping classifies as TypeFlagPrimary.StringMapping"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.StringMapping)).IsStringMapping)

        // --- nested DU routing: Literal & Object --------------------------
        testCase "StringLiteral -> Literal" <| fun _ ->
            "TypeFlags.StringLiteral routes into the Literal nested DU"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.StringLiteral)).IsLiteral)

        testCase "NumberLiteral -> Literal" <| fun _ ->
            "TypeFlags.NumberLiteral routes into the Literal nested DU"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.NumberLiteral)).IsLiteral)

        testCase "BooleanLiteral -> Literal" <| fun _ ->
            "TypeFlags.BooleanLiteral routes into the Literal nested DU"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.BooleanLiteral)).IsLiteral)

        testCase "BigIntLiteral -> Literal" <| fun _ ->
            "TypeFlags.BigIntLiteral routes into the Literal nested DU"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.BigIntLiteral)).IsLiteral)

        testCase "Object -> Object (Anonymous)" <| fun _ ->
            // flags carry Object; objectFlags carry Anonymous.
            let mock : Ts.Type =
                createObj [
                    "flags" ==> Ts.TypeFlags.Object
                    "objectFlags" ==> Ts.ObjectFlags.Anonymous
                ] |> unbox
            "TypeFlags.Object routes into the Object nested DU"
            |> Expect.isTrue ((TypeFlagPrimary.Create mock).IsObject)

        // --- PRIORITY EDGES: composite flags must pick the SPECIFIC case ---
        testCase "UniqueESSymbol beats ESSymbol (8192|4096 -> UniqueESSymbol literal)" <| fun _ ->
            // The primary table lists UniqueESSymbol before ESSymbol, so a type
            // carrying both bits must classify as UniqueESSymbol, NOT ESSymbol.
            let bits = Ts.TypeFlags.UniqueESSymbol ||| Ts.TypeFlags.ESSymbol
            "A unique-symbol type (both bits set) classifies as UniqueESSymbol"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags bits)).IsUniqueESSymbol)

        testCase "EnumLiteral string member beats StringLiteral (1024|128 -> Literal)" <| fun _ ->
            // An enum string member carries EnumLiteral ||| StringLiteral. The table
            // lists the EnumLiteral handler first, so it must route to Literal via the
            // EnumLiteral path, not the bare StringLiteral path. Both still land on
            // Literal at the PRIMARY level; the literal-level test below proves the
            // EnumLiteral subcase is selected.
            let bits = Ts.TypeFlags.EnumLiteral ||| Ts.TypeFlags.StringLiteral
            "Enum string member (both bits) routes to Literal"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags bits)).IsLiteral)

        testCase "BooleanLiteral beats Boolean (literal table precedes primitive Boolean)" <| fun _ ->
            // A single boolean-literal constituent carries BooleanLiteral (512). The
            // table places BooleanLiteral before the primitive Boolean (16), so it
            // must route to Literal, not the Boolean primitive.
            "A boolean literal classifies as Literal, not the Boolean primitive"
            |> Expect.isTrue ((TypeFlagPrimary.Create (typeWithFlags Ts.TypeFlags.BooleanLiteral)).IsLiteral)
    ]

// ---------------------------------------------------------------------------
// TypeFlagLiteral.Create — the literal sub-classifier driving
//   TypeFlagLiteral.dispatch (string/int/float/bool/bigint/enum/symbol).
//   Priority order in the table: UniqueESSymbol, EnumLiteral, Boolean, BigInt,
//   Number, String.
// ---------------------------------------------------------------------------
let private typeFlagLiteralTests =
    testList "TypeFlagLiteral.Create" [

        testCase "StringLiteral -> String" <| fun _ ->
            "StringLiteral classifies as TypeFlagLiteral.String"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.StringLiteral)).IsString)

        testCase "NumberLiteral -> Number" <| fun _ ->
            "NumberLiteral classifies as TypeFlagLiteral.Number"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.NumberLiteral)).IsNumber)

        testCase "BooleanLiteral -> Boolean" <| fun _ ->
            "BooleanLiteral classifies as TypeFlagLiteral.Boolean"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.BooleanLiteral)).IsBoolean)

        testCase "BigIntLiteral -> BigInt" <| fun _ ->
            "BigIntLiteral classifies as TypeFlagLiteral.BigInt"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.BigIntLiteral)).IsBigInt)

        testCase "EnumLiteral -> EnumLiteral" <| fun _ ->
            "EnumLiteral classifies as TypeFlagLiteral.EnumLiteral"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.EnumLiteral)).IsEnumLiteral)

        testCase "UniqueESSymbol -> UniqueESSymbol" <| fun _ ->
            "UniqueESSymbol classifies as TypeFlagLiteral.UniqueESSymbol"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.UniqueESSymbol)).IsUniqueESSymbol)

        // --- PRIORITY EDGES inside the literal table ----------------------
        testCase "EnumLiteral|StringLiteral -> EnumLiteral (enum member, not String)" <| fun _ ->
            // The literal table lists EnumLiteral before String. An enum string member
            // (EnumLiteral ||| StringLiteral) must classify as the EnumLiteral subcase
            // so the dispatcher routes to the enum-member declaration (its
            // SEnumCaseBuilder), NOT as a bare string literal.
            let bits = Ts.TypeFlags.EnumLiteral ||| Ts.TypeFlags.StringLiteral
            "Enum string member classifies as EnumLiteral, not String"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags bits)).IsEnumLiteral)

        testCase "EnumLiteral|NumberLiteral -> EnumLiteral (numeric enum member)" <| fun _ ->
            // Numeric enum members carry EnumLiteral ||| NumberLiteral. EnumLiteral
            // precedes Number in the table.
            let bits = Ts.TypeFlags.EnumLiteral ||| Ts.TypeFlags.NumberLiteral
            "Numeric enum member classifies as EnumLiteral, not Number"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags bits)).IsEnumLiteral)

        testCase "UniqueESSymbol checked first in literal table" <| fun _ ->
            // UniqueESSymbol is the first entry — verify it isn't shadowed by anything.
            "UniqueESSymbol is selected as the first literal entry"
            |> Expect.isTrue ((TypeFlagLiteral.Create (typeWithFlags Ts.TypeFlags.UniqueESSymbol)).IsUniqueESSymbol)
    ]

// ---------------------------------------------------------------------------
// TypeFlagObject.Create — objectFlags -> object IR category, driving
//   TypeFlagObject.dispatch (class / interface / tuple / reference / anonymous
//   / mapped / instantiated / evolving-array). Priority order in the table:
//   Class, Interface, Tuple, Reference, Anonymous, Mapped, Instantiated,
//   EvolvingArray.
// ---------------------------------------------------------------------------
let private typeFlagObjectTests =
    testList "TypeFlagObject.Create" [

        testCase "Class -> Class" <| fun _ ->
            "ObjectFlags.Class classifies as TypeFlagObject.Class"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Class)).IsClass)

        testCase "Interface -> Interface" <| fun _ ->
            "ObjectFlags.Interface classifies as TypeFlagObject.Interface"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Interface)).IsInterface)

        testCase "Tuple -> Tuple" <| fun _ ->
            "ObjectFlags.Tuple classifies as TypeFlagObject.Tuple"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Tuple)).IsTuple)

        testCase "Reference -> Reference" <| fun _ ->
            "ObjectFlags.Reference classifies as TypeFlagObject.Reference"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Reference)).IsReference)

        testCase "Anonymous -> Anonymous" <| fun _ ->
            "ObjectFlags.Anonymous classifies as TypeFlagObject.Anonymous"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Anonymous)).IsAnonymous)

        testCase "Mapped -> Mapped" <| fun _ ->
            "ObjectFlags.Mapped classifies as TypeFlagObject.Mapped"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Mapped)).IsMapped)

        testCase "Instantiated -> Instantiated" <| fun _ ->
            "ObjectFlags.Instantiated classifies as TypeFlagObject.Instantiated"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.Instantiated)).IsInstantiated)

        testCase "EvolvingArray -> EvolvingArray" <| fun _ ->
            "ObjectFlags.EvolvingArray classifies as TypeFlagObject.EvolvingArray"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags Ts.ObjectFlags.EvolvingArray)).IsEvolvingArray)

        // --- PRIORITY EDGES inside the object table -----------------------
        testCase "Reference|Tuple -> Tuple (tuple is checked before reference)" <| fun _ ->
            // A tuple type instance carries Tuple ||| Reference (a tuple IS a type
            // reference to the tuple's synthetic structure). The table lists Tuple
            // before Reference, so it must classify as Tuple, not a bare Reference.
            let bits = Ts.ObjectFlags.Reference ||| Ts.ObjectFlags.Tuple
            "Tuple-reference classifies as Tuple, not Reference"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags bits)).IsTuple)

        testCase "Reference|Interface -> Interface (interface checked before reference)" <| fun _ ->
            // A generic interface instantiation carries Interface (or Class) ||| Reference.
            // The table lists Interface before Reference -> must be Interface.
            let bits = Ts.ObjectFlags.Reference ||| Ts.ObjectFlags.Interface
            "Generic interface instantiation classifies as Interface, not Reference"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags bits)).IsInterface)

        testCase "Class|Interface -> Class (ClassOrInterface, class wins)" <| fun _ ->
            // A class type carries the ClassOrInterface composite (Class ||| Interface).
            // Class precedes Interface in the table.
            let bits = Ts.ObjectFlags.Class ||| Ts.ObjectFlags.Interface
            "ClassOrInterface bits classify as Class, not Interface"
            |> Expect.isTrue ((TypeFlagObject.Create (objectTypeWithFlags bits)).IsClass)
    ]

// ---------------------------------------------------------------------------
// LiteralTokenNodes.Create — SyntaxKind -> literal-token DU, driving the
//   syntactic literal-token dispatch (LiteralTokenNode.dispatch). This is the
//   .d.ts AST-node side (string/numeric/bigint/true/false/null/prefix-unary/
//   no-substitution-template). Exact-kind map, no flag priority.
// ---------------------------------------------------------------------------
let private literalTokenNodeTests =
    testList "LiteralTokenNodes.Create" [

        testCase "StringLiteral kind -> StringLiteral" <| fun _ ->
            "SyntaxKind.StringLiteral classifies as LiteralTokenNodes.StringLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.StringLiteral)).IsStringLiteral)

        testCase "NumericLiteral kind -> NumericLiteral" <| fun _ ->
            "SyntaxKind.NumericLiteral classifies as LiteralTokenNodes.NumericLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.NumericLiteral)).IsNumericLiteral)

        testCase "BigIntLiteral kind -> BigIntLiteral" <| fun _ ->
            "SyntaxKind.BigIntLiteral classifies as LiteralTokenNodes.BigIntLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.BigIntLiteral)).IsBigIntLiteral)

        testCase "TrueKeyword kind -> TrueLiteral" <| fun _ ->
            "SyntaxKind.TrueKeyword classifies as LiteralTokenNodes.TrueLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.TrueKeyword)).IsTrueLiteral)

        testCase "FalseKeyword kind -> FalseLiteral" <| fun _ ->
            "SyntaxKind.FalseKeyword classifies as LiteralTokenNodes.FalseLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.FalseKeyword)).IsFalseLiteral)

        testCase "NullKeyword kind -> NullLiteral" <| fun _ ->
            "SyntaxKind.NullKeyword classifies as LiteralTokenNodes.NullLiteral"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.NullKeyword)).IsNullLiteral)

        testCase "PrefixUnaryExpression kind -> PrefixUnaryExpression" <| fun _ ->
            "SyntaxKind.PrefixUnaryExpression classifies as LiteralTokenNodes.PrefixUnaryExpression"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.PrefixUnaryExpression)).IsPrefixUnaryExpression)

        testCase "NoSubstitutionTemplateLiteral kind -> NoSubstitutionTemplateLiteral" <| fun _ ->
            "SyntaxKind.NoSubstitutionTemplateLiteral classifies as the matching case"
            |> Expect.isTrue ((LiteralTokenNodes.Create (nodeWithKind Ts.SyntaxKind.NoSubstitutionTemplateLiteral)).IsNoSubstitutionTemplateLiteral)

        testCase "IsLiteralTokenNodeKind true for a literal kind" <| fun _ ->
            "StringLiteral is recognised as a literal-token kind"
            |> Expect.isTrue (LiteralTokenNodes.IsLiteralTokenNodeKind (nodeWithKind Ts.SyntaxKind.StringLiteral))

        testCase "IsLiteralTokenNodeKind false for a non-literal kind" <| fun _ ->
            "InterfaceDeclaration is NOT a literal-token kind"
            |> Expect.isFalse (LiteralTokenNodes.IsLiteralTokenNodeKind (nodeWithKind Ts.SyntaxKind.InterfaceDeclaration))
    ]

// ---------------------------------------------------------------------------
// Node-category dispatch (XanTagKind.Create on Ts.Node) — the top-level fan-out
//   that decides whether a .d.ts node is a TypeDeclaration / MemberDeclaration /
//   TypeNode / ModulesAndExports / Ignore. This is the first dispatch a node
//   hits in the encoder. Cover the discriminators + a representative of each
//   sub-category and the priority of the if/elif chain.
// ---------------------------------------------------------------------------
let private nodeCategoryTests =
    testList "Node category dispatch (TypeDeclaration/TypeNode/Member/...)" [

        // TypeDeclaration sub-category (kind map)
        testCase "InterfaceDeclaration -> TypeDeclaration.Interface" <| fun _ ->
            let td = TypeDeclaration.Create (nodeWithKind Ts.SyntaxKind.InterfaceDeclaration)
            "InterfaceDeclaration kind maps to TypeDeclaration.Interface"
            |> Expect.isTrue td.IsInterface

        testCase "TypeAliasDeclaration -> TypeDeclaration.TypeAlias" <| fun _ ->
            let td = TypeDeclaration.Create (nodeWithKind Ts.SyntaxKind.TypeAliasDeclaration)
            "TypeAliasDeclaration kind maps to TypeDeclaration.TypeAlias"
            |> Expect.isTrue td.IsTypeAlias

        testCase "ClassDeclaration -> TypeDeclaration.Class" <| fun _ ->
            let td = TypeDeclaration.Create (nodeWithKind Ts.SyntaxKind.ClassDeclaration)
            "ClassDeclaration kind maps to TypeDeclaration.Class"
            |> Expect.isTrue td.IsClass

        testCase "EnumDeclaration -> TypeDeclaration.Enum" <| fun _ ->
            let td = TypeDeclaration.Create (nodeWithKind Ts.SyntaxKind.EnumDeclaration)
            "EnumDeclaration kind maps to TypeDeclaration.Enum"
            |> Expect.isTrue td.IsEnum

        testCase "FunctionDeclaration -> TypeDeclaration.FunctionDeclaration" <| fun _ ->
            let td = TypeDeclaration.Create (nodeWithKind Ts.SyntaxKind.FunctionDeclaration)
            "FunctionDeclaration kind maps to TypeDeclaration.FunctionDeclaration"
            |> Expect.isTrue td.IsFunctionDeclaration

        // MemberDeclaration sub-category (kind map)
        testCase "PropertySignature -> MemberDeclaration.PropertySignature" <| fun _ ->
            let md = MemberDeclaration.Create (nodeWithKind Ts.SyntaxKind.PropertySignature)
            "PropertySignature kind maps to MemberDeclaration.PropertySignature"
            |> Expect.isTrue md.IsPropertySignature

        testCase "MethodSignature -> MemberDeclaration.MethodSignature" <| fun _ ->
            let md = MemberDeclaration.Create (nodeWithKind Ts.SyntaxKind.MethodSignature)
            "MethodSignature kind maps to MemberDeclaration.MethodSignature"
            |> Expect.isTrue md.IsMethodSignature

        testCase "IndexSignature -> MemberDeclaration.IndexSignature" <| fun _ ->
            let md = MemberDeclaration.Create (nodeWithKind Ts.SyntaxKind.IndexSignature)
            "IndexSignature kind maps to MemberDeclaration.IndexSignature"
            |> Expect.isTrue md.IsIndexSignature

        testCase "Parameter -> MemberDeclaration.Parameter" <| fun _ ->
            let md = MemberDeclaration.Create (nodeWithKind Ts.SyntaxKind.Parameter)
            "Parameter kind maps to MemberDeclaration.Parameter"
            |> Expect.isTrue md.IsParameter

        // TypeNode sub-category (kind map) — syntactic, distinct from semantic flags
        testCase "TypeReference -> TypeNode.TypeReference" <| fun _ ->
            let tn = TypeNode.Create (nodeWithKind Ts.SyntaxKind.TypeReference)
            "TypeReference kind maps to TypeNode.TypeReference"
            |> Expect.isTrue tn.IsTypeReference

        testCase "ArrayType -> TypeNode.ArrayType" <| fun _ ->
            let tn = TypeNode.Create (nodeWithKind Ts.SyntaxKind.ArrayType)
            "ArrayType kind maps to TypeNode.ArrayType"
            |> Expect.isTrue tn.IsArrayType

        testCase "UnionType -> TypeNode.UnionType" <| fun _ ->
            let tn = TypeNode.Create (nodeWithKind Ts.SyntaxKind.UnionType)
            "UnionType kind maps to TypeNode.UnionType"
            |> Expect.isTrue tn.IsUnionType

        testCase "StringKeyword -> TypeNode.StringKeyword" <| fun _ ->
            let tn = TypeNode.Create (nodeWithKind Ts.SyntaxKind.StringKeyword)
            "StringKeyword kind maps to TypeNode.StringKeyword"
            |> Expect.isTrue tn.IsStringKeyword

        // The XanTagKind.Create(node) top-level fan-out priority (TypeDeclaration
        // is checked before TypeNode; TypeParameter kind is in BOTH maps, and the
        // chain checks TypeDeclaration first -> classifies as TypeDeclaration).
        testCase "TypeParameter node -> TypeDeclaration (TypeDeclaration checked before TypeNode)" <| fun _ ->
            // SyntaxKind.TypeParameter is registered in BOTH typeDeclarationKindSetMap
            // and typeNodeKindSetMap. XanTagKind.Create's if/elif tries
            // IsTypeDeclarationKind first, so it must classify as a TypeDeclaration.
            let tag = XanTagKind.Create (nodeWithKind Ts.SyntaxKind.TypeParameter)
            "A bare TypeParameter node fans out to the TypeDeclaration branch"
            |> Expect.isTrue tag.IsTypeDeclaration

        // ModulesAndExports branch
        testCase "ExportDeclaration node -> ModulesAndExports branch" <| fun _ ->
            let tag = XanTagKind.Create (nodeWithKind Ts.SyntaxKind.ExportDeclaration)
            "An ExportDeclaration fans out to the ModulesAndExports branch"
            |> Expect.isTrue tag.IsModulesAndExports

        testCase "ImportDeclaration node -> ModulesAndExports branch" <| fun _ ->
            let tag = XanTagKind.Create (nodeWithKind Ts.SyntaxKind.ImportDeclaration)
            "An ImportDeclaration fans out to the ModulesAndExports branch"
            |> Expect.isTrue tag.IsModulesAndExports

        // Ignore branch: a modifier keyword is intentionally ignored
        testCase "ExportKeyword node -> Ignore branch (modifier)" <| fun _ ->
            let tag = XanTagKind.Create (nodeWithKind Ts.SyntaxKind.ExportKeyword)
            "A bare modifier keyword fans out to the Ignore branch"
            |> Expect.isTrue tag.IsIgnore
    ]

let dispatchClassificationTests =
    testList "Encoder dispatch classification (pure)" [
        typeFlagPrimaryTests
        typeFlagLiteralTests
        typeFlagObjectTests
        literalTokenNodeTests
        nodeCategoryTests
    ]
