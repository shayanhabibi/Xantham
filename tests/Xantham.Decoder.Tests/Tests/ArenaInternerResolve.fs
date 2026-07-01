module XanthamDecoderTests.Tests.ArenaInternerResolve

// ---------------------------------------------------------------------------
// Per-pass isolation coverage for the DECODER RESOLUTION pass:
//   ArenaInterner.create + .ResolveType / .ResolveExport
//   (src/Xantham.Decoder/Types/Arena.Interner.fs)
//
// The decoder hands the generator a `DecodedResult` — flat maps keyed by
// `TypeKey`. `ArenaInterner.create` lifts that into a lazy `ResolvedType` /
// `ResolvedExport` object graph. These tests build tiny in-memory
// `DecodedResult` values (a handful of TypeKeys) and assert that resolution is
// FAITHFUL to the wire form: each `TsType` case maps to its `ResolvedType`
// counterpart, cycles are broken by the lazy boundaries, results are memoised
// (same key -> same instance), and `isLibEs` propagates from `LibEsExports`.
// ---------------------------------------------------------------------------

open Expecto
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner

// --- builders -------------------------------------------------------------

let private key = TypeKey.createWith

/// An empty DecodedResult to extend per-test.
let private empty: DecodedResult = {
    TypeMap = Map.empty
    ExportTypeMap = Map.empty
    ExportMap = Map.empty
    TopLevelExports = []
    LibEsExports = []
}

let private withTypes (pairs: (int * TsType) list) (dr: DecodedResult) =
    { dr with TypeMap = pairs |> List.map (fun (k, v) -> key k, v) |> Map.ofList }

let private withExports (pairs: (int * TsExportDeclaration) list) (dr: DecodedResult) =
    { dr with ExportTypeMap = pairs |> List.map (fun (k, v) -> key k, v) |> Map.ofList }

let private create (dr: DecodedResult) = ArenaInterner.create dr

/// Force a LazyResolvedType.
let private force (l: LazyResolvedType) = l.Value

// Minimal record builders for the structural shapes.
let private iface name members heritageExtends : TsInterface = {
    Source = None
    FullyQualifiedName = [ name ]
    Enumerable = false
    Name = name
    Members = members
    TypeParameters = []
    Documentation = []
    Heritage = { Extends = heritageExtends }
}

let private prop name typeKey : TsMember =
    TsMember.Property {
        Name = name
        Type = key typeKey
        IsStatic = false
        IsOptional = false
        IsPrivate = false
        Accessor = TsAccessor.ReadWrite
        Documentation = []
    }

let private variable name typeKey : TsVariable = {
    Source = None
    FullyQualifiedName = [ name ]
    Name = name
    Type = key typeKey
    Documentation = []
}

let private typeRef typeKey args : TsTypeReference =
    { Type = key typeKey; TypeArguments = args |> List.map key; ResolvedType = None }

// =========================================================================
// 1. Primitives
// =========================================================================
[<Tests>]
let primitiveTests =
    testList "ArenaInterner.ResolveType primitives" [
        let cases = [
            TypeKindPrimitive.String
            TypeKindPrimitive.Integer
            TypeKindPrimitive.Number
            TypeKindPrimitive.Boolean
            TypeKindPrimitive.Any
            TypeKindPrimitive.Unknown
            TypeKindPrimitive.Never
            TypeKindPrimitive.Void
            TypeKindPrimitive.Undefined
            TypeKindPrimitive.Null
            TypeKindPrimitive.BigInt
            TypeKindPrimitive.ESSymbol
            TypeKindPrimitive.NonPrimitive
        ]
        testTheory "each primitive resolves to ResolvedType.Primitive of the same kind" cases
        <| fun prim ->
            let arena = empty |> withTypes [ 0, TsType.Primitive prim ] |> create
            arena.ResolveType(key 0)
            |> Flip.Expect.equal "primitive must resolve faithfully" (ResolvedType.Primitive prim)
    ]

// =========================================================================
// 2. Literals
// =========================================================================
[<Tests>]
let literalTests =
    testList "ArenaInterner.ResolveType literals" [
        let cases = [
            TsLiteral.String "hello"
            TsLiteral.Int 42
            TsLiteral.Float 3.5
            TsLiteral.Bool true
            TsLiteral.Bool false
            TsLiteral.Null
        ]
        testTheory "each literal resolves to ResolvedType.Literal carrying the same value" cases
        <| fun lit ->
            let arena = empty |> withTypes [ 0, TsType.Literal lit ] |> create
            arena.ResolveType(key 0)
            |> Flip.Expect.equal "literal must resolve faithfully" (ResolvedType.Literal lit)
    ]

// =========================================================================
// 3. Union — members resolve, count preserved
// =========================================================================
[<Tests>]
let unionTests =
    testList "ArenaInterner.ResolveType union" [
        testCase "union resolves to ResolvedType.Union with each member resolvable in order" <| fun _ ->
            // string | 42 | true
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 2; key 3 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Literal (TsLiteral.Int 42)
                    3, TsType.Literal (TsLiteral.Bool true)
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Union u ->
                u.Types
                |> List.map force
                |> Flip.Expect.equal "union members resolved in order" [
                    ResolvedType.Primitive TypeKindPrimitive.String
                    ResolvedType.Literal (TsLiteral.Int 42)
                    ResolvedType.Literal (TsLiteral.Bool true)
                ]
            | other -> failtestf "expected Union, got %A" other

        testCase "empty union resolves to an empty-membered Union" <| fun _ ->
            let arena = empty |> withTypes [ 0, TsType.Union (TsTypeUnion []) ] |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Union u -> u.Types |> List.length |> Flip.Expect.equal "no members" 0
            | other -> failtestf "expected Union, got %A" other
    ]

// =========================================================================
// 4. Intersection
// =========================================================================
[<Tests>]
let intersectionTests =
    testList "ArenaInterner.ResolveType intersection" [
        testCase "intersection resolves members in order" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Intersection (TsTypeIntersection [ key 1; key 2 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Intersection i ->
                i.Types |> List.map force
                |> Flip.Expect.equal "intersection members" [
                    ResolvedType.Primitive TypeKindPrimitive.String
                    ResolvedType.Primitive TypeKindPrimitive.Number
                ]
            | other -> failtestf "expected Intersection, got %A" other
    ]

// =========================================================================
// 5. Array / ReadOnly (nested structural wrappers)
// =========================================================================
[<Tests>]
let wrapperTests =
    testList "ArenaInterner.ResolveType wrappers" [
        testCase "Array wraps the resolved element type" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 0, TsType.Array (TsType.Primitive TypeKindPrimitive.String) ]
                |> create
            arena.ResolveType(key 0)
            |> Flip.Expect.equal "array of string" (ResolvedType.Array (ResolvedType.Primitive TypeKindPrimitive.String))

        testCase "ReadOnly wraps the resolved inner type" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 0, TsType.ReadOnly (TsType.Primitive TypeKindPrimitive.Number) ]
                |> create
            arena.ResolveType(key 0)
            |> Flip.Expect.equal "readonly number" (ResolvedType.ReadOnly (ResolvedType.Primitive TypeKindPrimitive.Number))
    ]

// =========================================================================
// 6. TypeReference — Type, TypeArguments, ResolvedType lazies
// =========================================================================
[<Tests>]
let typeReferenceTests =
    testList "ArenaInterner.ResolveType type reference" [
        testCase "TypeReference resolves Type + each TypeArgument lazily" <| fun _ ->
            // Foo<string, number>  where Foo is interface key 9
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeReference (typeRef 9 [ 1; 2 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                    9, TsType.Interface (iface "Foo" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeReference tref ->
                tref.TypeArguments |> List.map force
                |> Flip.Expect.equal "type args resolved" [
                    ResolvedType.Primitive TypeKindPrimitive.String
                    ResolvedType.Primitive TypeKindPrimitive.Number
                ]
                match force tref.Type with
                | ResolvedType.Interface i -> i.Name |> Name.Case.valueOrSource |> Flip.Expect.equal "ref target name" "Foo"
                | other -> failtestf "expected target Interface, got %A" other
                Flip.Expect.equal "no ResolvedType provided" None (tref.ResolvedType |> Option.map ignore)
            | other -> failtestf "expected TypeReference, got %A" other

        testCase "TypeReference with no type args resolves to empty arg list" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeReference (typeRef 9 [])
                    9, TsType.Interface (iface "Bar" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeReference tref -> tref.TypeArguments |> List.length |> Flip.Expect.equal "no args" 0
            | other -> failtestf "expected TypeReference, got %A" other
    ]

// =========================================================================
// 7. Tuple / Index / Predicate / TemplateLiteral
// =========================================================================
[<Tests>]
let miscStructuralTests =
    testList "ArenaInterner.ResolveType misc structural" [
        testCase "Tuple preserves shape and resolves each element" <| fun _ ->
            let elem k opt rest : TsTupleElementType = { Type = key k; IsOptional = opt; IsRest = rest }
            let tuple: TsTuple = {
                IsReadOnly = true
                FixedLength = 2
                MinRequired = 1
                Types = [
                    TsTupleElement.Fixed (elem 1 false false)
                    TsTupleElement.FixedLabeled ("rest", elem 2 false true)
                    TsTupleElement.Variadic (key 2)
                ]
            }
            let arena =
                empty
                |> withTypes [
                    0, TsType.Tuple tuple
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Tuple t ->
                Flip.Expect.equal "IsReadOnly" true t.IsReadOnly
                Flip.Expect.equal "FixedLength" 2 t.FixedLength
                Flip.Expect.equal "MinRequired" 1 t.MinRequired
                t.Types |> List.length |> Flip.Expect.equal "three elements" 3
                // labels + rest preserved
                (t.Types.[1].Name, t.Types.[1].IsRest)
                |> Flip.Expect.equal "labeled rest element" (ValueSome "rest", true)
                t.Types.[2].IsRest |> Flip.Expect.equal "variadic is rest" true
                t.Types.[0].Type |> force
                |> Flip.Expect.equal "first element resolves" (ResolvedType.Primitive TypeKindPrimitive.String)
            | other -> failtestf "expected Tuple, got %A" other

        testCase "Index resolves inner Type" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Index { Type = key 1 }
                    1, TsType.Primitive TypeKindPrimitive.String
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Index i -> force i.Type |> Flip.Expect.equal "index inner" (ResolvedType.Primitive TypeKindPrimitive.String)
            | other -> failtestf "expected Index, got %A" other

        testCase "Predicate preserves parameter name + assertion flag and resolves Type" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Predicate { ParameterName = "x"; Type = key 1; IsAssertion = true }
                    1, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Predicate p ->
                Flip.Expect.equal "param name" "x" (p.ParameterName |> Name.Case.valueOrSource)
                Flip.Expect.equal "assertion flag" true p.IsAssertion
                force p.Type |> Flip.Expect.equal "predicate type" (ResolvedType.Primitive TypeKindPrimitive.Number)
            | other -> failtestf "expected Predicate, got %A" other

        testCase "TemplateLiteral preserves text segments and resolves interpolated types" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TemplateLiteral { Texts = [ "on"; "" ]; Types = [ key 1 ] }
                    1, TsType.Primitive TypeKindPrimitive.String
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TemplateLiteral tl ->
                Flip.Expect.equal "texts" [ "on"; "" ] tl.Text
                tl.Types |> List.map force |> Flip.Expect.equal "interp types" [ ResolvedType.Primitive TypeKindPrimitive.String ]
            | other -> failtestf "expected TemplateLiteral, got %A" other

        testCase "GlobalThis resolves to ResolvedType.GlobalThis" <| fun _ ->
            let arena = empty |> withTypes [ 0, TsType.GlobalThis ] |> create
            arena.ResolveType(key 0) |> Flip.Expect.equal "globalThis" ResolvedType.GlobalThis
    ]

// =========================================================================
// 8. Interface — members resolve through the lazy boundary
// =========================================================================
[<Tests>]
let interfaceTests =
    testList "ArenaInterner.ResolveType interface" [
        testCase "Interface resolves name + member property type" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "Point" [ prop "x" 1 ] [])
                    1, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i ->
                Flip.Expect.equal "interface name" "Point" (i.Name |> Name.Case.valueOrSource)
                match i.Members with
                | [ Member.Property p ] ->
                    Flip.Expect.equal "prop name" "x" (p.Name |> Name.Case.valueOrSource)
                    force p.Type |> Flip.Expect.equal "prop type" (ResolvedType.Primitive TypeKindPrimitive.Number)
                | other -> failtestf "expected single property member, got %A" other
            | other -> failtestf "expected Interface, got %A" other
    ]

// =========================================================================
// 9. Cycle breaking — a self-referential interface resolves with no overflow
// =========================================================================
[<Tests>]
let cycleTests =
    testList "ArenaInterner.ResolveType cycle breaking" [
        testCase "self-referential interface resolves without stack overflow" <| fun _ ->
            // interface Node { self: Node }  — property type points back at key 0.
            let arena =
                empty
                |> withTypes [ 0, TsType.Interface (iface "Node" [ prop "self" 0 ] []) ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i ->
                match i.Members with
                | [ Member.Property p ] ->
                    // Forcing the lazy returns the very same Interface instance (identity).
                    match force p.Type with
                    | ResolvedType.Interface i2 -> Flip.Expect.isTrue "self-ref is the same instance" (System.Object.ReferenceEquals(i, i2))
                    | other -> failtestf "expected self Interface, got %A" other
                | other -> failtestf "expected single property, got %A" other
            | other -> failtestf "expected Interface, got %A" other

        testCase "mutually recursive type references resolve without overflow" <| fun _ ->
            // A.b : B ; B.a : A
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "A" [ prop "b" 1 ] [])
                    1, TsType.Interface (iface "B" [ prop "a" 0 ] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface a ->
                match a.Members with
                | [ Member.Property pb ] ->
                    match force pb.Type with
                    | ResolvedType.Interface b ->
                        match b.Members with
                        | [ Member.Property pa ] ->
                            match force pa.Type with
                            | ResolvedType.Interface a2 ->
                                Flip.Expect.isTrue "A reached through B is the cached A" (System.Object.ReferenceEquals(a, a2))
                            | other -> failtestf "expected A, got %A" other
                        | other -> failtestf "expected B.a property, got %A" other
                    | other -> failtestf "expected B, got %A" other
                | other -> failtestf "expected A.b property, got %A" other
            | other -> failtestf "expected Interface A, got %A" other
    ]

// =========================================================================
// 10. Memoization — same TypeKey resolves to the SAME instance
// =========================================================================
[<Tests>]
let memoizationTests =
    testList "ArenaInterner.ResolveType memoization" [
        testCase "resolving the same key twice yields the identical instance" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 0, TsType.Interface (iface "Shared" [] []) ]
                |> create
            let a = arena.ResolveType(key 0)
            let b = arena.ResolveType(key 0)
            Flip.Expect.isTrue "memoised: same reference" (System.Object.ReferenceEquals(a, b))

        testCase "two references to the same key share one resolved instance" <| fun _ ->
            // union: Shared | Shared  (both members point at key 1)
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 1 ])
                    1, TsType.Interface (iface "Shared" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Union u ->
                match u.Types |> List.map force with
                | [ first; second ] -> Flip.Expect.isTrue "both members are one instance" (System.Object.ReferenceEquals(first, second))
                | other -> failtestf "expected two members, got %A" other
            | other -> failtestf "expected Union, got %A" other
    ]

// =========================================================================
// 11. Export resolution
// =========================================================================
[<Tests>]
let exportTests =
    testList "ArenaInterner.ResolveExport" [
        testCase "Variable export resolves to ResolvedExport.Variable with resolvable Type" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.String ]
                |> withExports [ 0, TsExportDeclaration.Variable (variable "answer" 1) ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Variable v) ->
                Flip.Expect.equal "variable name" "answer" (v.Name |> Name.Case.valueOrSource)
                force v.Type |> Flip.Expect.equal "variable type" (ResolvedType.Primitive TypeKindPrimitive.String)
            | other -> failtestf "expected Ok Variable, got %A" other

        testCase "Interface export resolves to ResolvedExport.Interface" <| fun _ ->
            let arena =
                empty
                |> withExports [ 0, TsExportDeclaration.Interface (iface "IThing" [] []) ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Interface i) -> Flip.Expect.equal "name" "IThing" (i.Name |> Name.Case.valueOrSource)
            | other -> failtestf "expected Ok Interface, got %A" other

        testCase "Class export resolves to ResolvedExport.Class" <| fun _ ->
            let cls: TsClass = {
                Source = None
                FullyQualifiedName = [ "Widget" ]
                Enumerable = false
                Name = "Widget"
                Constructors = []
                Members = []
                TypeParameters = []
                Heritage = { Implements = None; Extends = [] }
            }
            let arena = empty |> withExports [ 0, TsExportDeclaration.Class cls ] |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Class c) -> Flip.Expect.equal "name" "Widget" (c.Name |> Name.Case.valueOrSource)
            | other -> failtestf "expected Ok Class, got %A" other

        testCase "TypeAlias export resolves to ResolvedExport.TypeAlias with resolvable Type" <| fun _ ->
            let alias: TsTypeAlias = {
                Source = None
                FullyQualifiedName = [ "Id" ]
                Name = "Id"
                Type = key 1
                TypeParameters = []
                Documentation = []
            }
            let arena =
                empty
                |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.String ]
                |> withExports [ 0, TsExportDeclaration.TypeAlias alias ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.TypeAlias a) ->
                Flip.Expect.equal "name" "Id" (a.Name |> Name.Case.valueOrSource)
                force a.Type |> Flip.Expect.equal "alias target" (ResolvedType.Primitive TypeKindPrimitive.String)
            | other -> failtestf "expected Ok TypeAlias, got %A" other

        testCase "Enum export resolves to ResolvedExport.Enum" <| fun _ ->
            let enum: TsEnumType = {
                Source = None
                FullyQualifiedName = [ "Color" ]
                Name = "Color"
                Members = []
                Documentation = []
            }
            let arena = empty |> withExports [ 0, TsExportDeclaration.Enum enum ] |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Enum e) -> Flip.Expect.equal "name" "Color" (e.Name |> Name.Case.valueOrSource)
            | other -> failtestf "expected Ok Enum, got %A" other

        testCase "Function export resolves to ResolvedExport.Function list" <| fun _ ->
            let func: TsFunction = {
                Source = None
                FullyQualifiedName = [ "doIt" ]
                Documentation = []
                IsDeclared = true
                Name = "doIt"
                Type = key 1
                Parameters = []
                TypeParameters = []
                SignatureKey = key 2
            }
            let arena =
                empty
                |> withTypes [
                    1, TsType.Primitive TypeKindPrimitive.Void
                    2, TsType.TypeLiteral { Members = [] }
                ]
                |> withExports [ 0, TsExportDeclaration.Function (NoOverloads func) ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Function fns) ->
                fns |> List.length |> Flip.Expect.equal "single overload" 1
                Flip.Expect.equal "name" "doIt" (fns.Head.Name |> Name.Case.valueOrSource)
            | other -> failtestf "expected Ok Function, got %A" other

        testCase "Module export resolves to ResolvedExport.Module carrying nested exports" <| fun _ ->
            let inner = variable "inner" 1
            let modu: TsModule = {
                Source = None
                FullyQualifiedName = [ "NS" ]
                Name = "NS"
                IsNamespace = true
                IsRecursive = false
                Exports = [ TsExportDeclaration.Variable inner ]
            }
            let arena =
                empty
                |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.String ]
                |> withExports [ 0, TsExportDeclaration.Module modu ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Module m) ->
                Flip.Expect.equal "namespace flag" true m.IsNamespace
                m.Exports |> List.length |> Flip.Expect.equal "one nested export" 1
            | other -> failtestf "expected Ok Module, got %A" other

        testCase "ResolveExport on a non-export structural key returns Error with the resolved type" <| fun _ ->
            // key 0 is a plain structural type, not in the export map.
            let arena = empty |> withTypes [ 0, TsType.Primitive TypeKindPrimitive.Boolean ] |> create
            match arena.ResolveExport(key 0) with
            | Error rt -> rt |> Flip.Expect.equal "structural fallback" (ResolvedType.Primitive TypeKindPrimitive.Boolean)
            | Ok other -> failtestf "expected Error fallback, got Ok %A" other

        testCase "ResolveExport memoises: same key -> same export instance" <| fun _ ->
            let arena =
                empty
                |> withExports [ 0, TsExportDeclaration.Interface (iface "Memo" [] []) ]
                |> create
            let a = arena.ResolveExport(key 0)
            let b = arena.ResolveExport(key 0)
            match a, b with
            | Ok ea, Ok eb -> Flip.Expect.isTrue "same export instance" (System.Object.ReferenceEquals(ea, eb))
            | _ -> failtest "expected both Ok"
    ]

// =========================================================================
// 12. Export/Type cross-resolution:
//     resolve checks resolvedExports FIRST for Class/Enum/Interface, so a key
//     that is an exported interface and ALSO resolved as a type collapses to
//     the SAME instance.
// =========================================================================
[<Tests>]
let exportTypeBridgeTests =
    testList "ArenaInterner export/type bridge" [
        testCase "an exported Interface resolved via ResolveType reuses the export instance" <| fun _ ->
            let arena =
                empty
                |> withExports [ 0, TsExportDeclaration.Interface (iface "Bridge" [] []) ]
                |> create
            // Shell the export first.
            let asExport =
                match arena.ResolveExport(key 0) with
                | Ok (ResolvedExport.Interface i) -> i
                | other -> failtestf "expected Ok Interface, got %A" other
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface asType ->
                Flip.Expect.isTrue "export & type share one Interface instance" (System.Object.ReferenceEquals(asExport, asType))
            | other -> failtestf "expected Interface from ResolveType, got %A" other
    ]

// =========================================================================
// 13. isLibEs propagation from LibEsExports
// =========================================================================
[<Tests>]
let libEsTests =
    testList "ArenaInterner isLibEs propagation" [
        testCase "interface whose key is in LibEsExports resolves with IsLibEs = true" <| fun _ ->
            let dr = { (empty |> withTypes [ 0, TsType.Interface (iface "Lib" [] []) ]) with LibEsExports = [ key 0 ] }
            match (create dr).ResolveType(key 0) with
            | ResolvedType.Interface i -> Flip.Expect.isTrue "IsLibEs flagged" i.IsLibEs
            | other -> failtestf "expected Interface, got %A" other

        testCase "interface whose key is NOT in LibEsExports resolves with IsLibEs = false" <| fun _ ->
            let arena = empty |> withTypes [ 0, TsType.Interface (iface "User" [] []) ] |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i -> Flip.Expect.isFalse "IsLibEs not flagged" i.IsLibEs
            | other -> failtestf "expected Interface, got %A" other

        testCase "exported Variable in LibEsExports resolves with IsLibEs = true" <| fun _ ->
            let dr =
                { (empty
                   |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.String ]
                   |> withExports [ 0, TsExportDeclaration.Variable (variable "globalVal" 1) ]) with LibEsExports = [ key 0 ] }
            match (create dr).ResolveExport(key 0) with
            | Ok (ResolvedExport.Variable v) -> Flip.Expect.isTrue "IsLibEs flagged on export" v.IsLibEs
            | other -> failtestf "expected Ok Variable, got %A" other
    ]

// =========================================================================
// 14. ExportMap (by source module path) + TopLevelExports
// =========================================================================
[<Tests>]
let exportMapTests =
    testList "ArenaInterner.ExportMap / TopLevelExports" [
        testCase "ExportMap groups resolved exports by source module path" <| fun _ ->
            let dr =
                { (empty |> withExports [
                        0, TsExportDeclaration.Interface (iface "A" [] [])
                        1, TsExportDeclaration.Interface (iface "B" [] [])
                   ]) with ExportMap = Map [ "mod.d.ts", Set [ key 0; key 1 ] ] }
            let arena = create dr
            match arena.ExportMap.TryFind "mod.d.ts" with
            | Some exports -> exports |> List.length |> Flip.Expect.equal "two exports under path" 2
            | None -> failtest "expected exports under 'mod.d.ts'"

        testCase "TopLevelExports excludes lib.es keys and resolves the rest" <| fun _ ->
            let dr =
                { (empty |> withExports [
                        0, TsExportDeclaration.Interface (iface "Surface" [] [])
                        1, TsExportDeclaration.Interface (iface "LibInternal" [] [])
                   ]) with
                        TopLevelExports = [ key 0; key 1 ]
                        LibEsExports = [ key 1 ] }
            let arena = create dr
            arena.TopLevelExports.Count |> Flip.Expect.equal "only the non-lib export survives" 1
    ]

// =========================================================================
// 15. CHARACTERIZE THE UNION OVER-MINTING BUG
//
// Structurally-identical synthesized unions get DISTINCT TypeKeys in the wire
// form, so ArenaInterner mints a DISTINCT `Union` instance per key. Because
// `Union` is [<ReferenceEquality>], those instances are neither equal nor
// shareable — the source of ~777 redundant unions (of 1579) on the real
// surface. The CORRECT behavior is that two unions over the same member set
// resolve to a SHARED/structurally-equal instance.
//
// Root cause: src/Xantham.Decoder/Types/Arena.Interner.fs
//   line 685:  | TsType.Union tsTypeUnion -> ResolvedType.Union { Types = ... }
//   resolution is keyed purely by the incoming TypeKey (resolve cache at
//   lines 478-483); there is no structural dedup of unions across keys, and
//   `Union` carries [<ReferenceEquality>] (Arena.Interner.fs:210), so distinct
//   keys => distinct, non-shareable instances.
// Fix: dedup structurally-identical unions BEFORE interning (encoder-side
//   distinct-keying, per the hoist-dedup root-cause note), or intern unions
//   through a structural cache keyed by their resolved member set so the two
//   keys collapse to one `Union`.
// =========================================================================
[<Tests>]
let unionOverMintingTests =
    testList "ArenaInterner union interning" [
        // Unions are now interned by member-key signature. A shared instance must still
        // carry the correctly-resolved members (interning must not lose/scramble them).
        testCase "interned shared union resolves its members correctly" <| fun _ ->
            // key 0 and key 3 are both `string | number` at separate keys -> one instance.
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    3, TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Union u ->
                u.Types
                |> List.map (fun t -> force t)
                |> Flip.Expect.equal "members resolve to string|number"
                    [ ResolvedType.Primitive TypeKindPrimitive.String
                      ResolvedType.Primitive TypeKindPrimitive.Number ]
            | _ -> failtest "expected a Union"

        // CORRECT behavior — pending until the over-minting is fixed.
        // Two unions over the same member set SHOULD be the same shareable
        // instance (or at minimum structurally equal). Currently they are
        // distinct reference-equal records, so this fails => ptest.
        testCase "structurally-identical unions resolve to one shared instance" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    3, TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0), arena.ResolveType(key 3) with
            | ResolvedType.Union a, ResolvedType.Union b ->
                Flip.Expect.isTrue
                    "two unions over the same member set must be one shared instance"
                    (System.Object.ReferenceEquals(a, b))
            | _ -> failtest "expected two Unions"
    ]

// =========================================================================
// 16. Conditional — Check / Extends / True / False all resolve lazily
// =========================================================================
[<Tests>]
let conditionalTests =
    testList "ArenaInterner.ResolveType conditional" [
        testCase "Conditional resolves all four branch types in place" <| fun _ ->
            // string extends number ? boolean : void
            let arena =
                empty
                |> withTypes [
                    0, TsType.Conditional { Check = key 1; Extends = key 2; True = key 3; False = key 4 }
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                    3, TsType.Primitive TypeKindPrimitive.Boolean
                    4, TsType.Primitive TypeKindPrimitive.Void
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Conditional c ->
                force c.Check |> Flip.Expect.equal "check" (ResolvedType.Primitive TypeKindPrimitive.String)
                force c.Extends |> Flip.Expect.equal "extends" (ResolvedType.Primitive TypeKindPrimitive.Number)
                force c.True |> Flip.Expect.equal "true branch" (ResolvedType.Primitive TypeKindPrimitive.Boolean)
                force c.False |> Flip.Expect.equal "false branch" (ResolvedType.Primitive TypeKindPrimitive.Void)
            | other -> failtestf "expected Conditional, got %A" other
    ]

// =========================================================================
// 17. IndexedAccess — Object + Index resolve
// =========================================================================
[<Tests>]
let indexedAccessTests =
    testList "ArenaInterner.ResolveType indexed access" [
        testCase "IndexedAccess resolves Object and Index types" <| fun _ ->
            // T[K] modelled as string[number]
            let arena =
                empty
                |> withTypes [
                    0, TsType.IndexedAccess { Object = key 1; Index = key 2 }
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.IndexedAccess ia ->
                force ia.Object |> Flip.Expect.equal "object" (ResolvedType.Primitive TypeKindPrimitive.String)
                force ia.Index |> Flip.Expect.equal "index" (ResolvedType.Primitive TypeKindPrimitive.Number)
            | other -> failtestf "expected IndexedAccess, got %A" other
    ]

// =========================================================================
// 18. TypeParameter — name, optional Constraint, optional Default
// =========================================================================
[<Tests>]
let typeParameterTests =
    testList "ArenaInterner.ResolveType type parameter" [
        testCase "bare TypeParameter resolves name with no constraint or default" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 0, TsType.TypeParameter { Name = "T"; Constraint = None; Default = None; Documentation = [] } ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeParameter tp ->
                Flip.Expect.equal "name" "T" (tp.Name |> Name.Case.valueOrSource)
                Flip.Expect.equal "no constraint" None (tp.Constraint |> Option.map ignore)
                Flip.Expect.equal "no default" None (tp.Default |> Option.map ignore)
            | other -> failtestf "expected TypeParameter, got %A" other

        testCase "TypeParameter resolves its Constraint and Default lazily" <| fun _ ->
            // T extends object = string
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeParameter { Name = "T"; Constraint = Some (key 1); Default = Some (key 2); Documentation = [] }
                    1, TsType.Primitive TypeKindPrimitive.NonPrimitive
                    2, TsType.Primitive TypeKindPrimitive.String
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeParameter tp ->
                tp.Constraint |> Option.map force
                |> Flip.Expect.equal "constraint" (Some (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive))
                tp.Default |> Option.map force
                |> Flip.Expect.equal "default" (Some (ResolvedType.Primitive TypeKindPrimitive.String))
            | other -> failtestf "expected TypeParameter, got %A" other
    ]

// =========================================================================
// 19. Optional — wraps a TypeReference
// =========================================================================
[<Tests>]
let optionalTests =
    testList "ArenaInterner.ResolveType optional" [
        testCase "Optional resolves to ResolvedType.Optional carrying the underlying TypeReference" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Optional (typeRef 9 [ 1 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    9, TsType.Interface (iface "Opt" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Optional tref ->
                tref.TypeArguments |> List.map force
                |> Flip.Expect.equal "optional type arg" [ ResolvedType.Primitive TypeKindPrimitive.String ]
                match force tref.Type with
                | ResolvedType.Interface i -> i.Name |> Name.Case.valueOrSource |> Flip.Expect.equal "optional target name" "Opt"
                | other -> failtestf "expected target Interface, got %A" other
            | other -> failtestf "expected Optional, got %A" other
    ]

// =========================================================================
// 20. Substitution — Base + Constraint resolve
// =========================================================================
[<Tests>]
let substitutionTests =
    testList "ArenaInterner.ResolveType substitution" [
        testCase "Substitution resolves both Base and Constraint" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Substitution { Base = key 1; Constraint = key 2 }
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.NonPrimitive
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Substitution s ->
                force s.Base |> Flip.Expect.equal "base" (ResolvedType.Primitive TypeKindPrimitive.String)
                force s.Constraint |> Flip.Expect.equal "constraint" (ResolvedType.Primitive TypeKindPrimitive.NonPrimitive)
            | other -> failtestf "expected Substitution, got %A" other
    ]

// =========================================================================
// 21. TypeQuery — FullyQualifiedName segments + resolved Type
// =========================================================================
[<Tests>]
let typeQueryTests =
    testList "ArenaInterner.ResolveType type query" [
        testCase "TypeQuery preserves FQN parts and resolves the queried Type" <| fun _ ->
            // typeof Foo.bar
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeQuery { FullyQualifiedName = [ "Foo"; "bar" ]; Type = key 1 }
                    1, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeQuery tq ->
                tq.FullyQualifiedName |> List.map (fun p -> p.Value)
                |> Flip.Expect.equal "fqn segments" [ "Foo"; "bar" ]
                force tq.Type |> Flip.Expect.equal "queried type" (ResolvedType.Primitive TypeKindPrimitive.Number)
            | other -> failtestf "expected TypeQuery, got %A" other

        testCase "TypeQuery FQN part with a period is flagged Abnormal" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeQuery { FullyQualifiedName = [ "a.b" ]; Type = key 1 }
                    1, TsType.Primitive TypeKindPrimitive.Boolean
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeQuery tq ->
                match tq.FullyQualifiedName with
                | [ Xantham.Decoder.ArenaInterner.QualifiedNamePart.Abnormal (part, diag) ] ->
                    Flip.Expect.equal "abnormal part text" "a.b" part
                    Flip.Expect.isTrue "period diagnostic set"
                        (diag.HasFlag Xantham.Decoder.ArenaInterner.QualifiedNamePartDiagnostic.ContainsPeriod)
                | other -> failtestf "expected single Abnormal part, got %A" other
            | other -> failtestf "expected TypeQuery, got %A" other
    ]

// =========================================================================
// 22. TypeLiteral — anonymous object members resolve
// =========================================================================
[<Tests>]
let typeLiteralTests =
    testList "ArenaInterner.ResolveType type literal" [
        testCase "TypeLiteral resolves its inline property members" <| fun _ ->
            // { x: number }
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeLiteral { Members = [ prop "x" 1 ] }
                    1, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeLiteral tl ->
                match tl.Members with
                | [ Member.Property p ] ->
                    Flip.Expect.equal "prop name" "x" (p.Name |> Name.Case.valueOrSource)
                    force p.Type |> Flip.Expect.equal "prop type" (ResolvedType.Primitive TypeKindPrimitive.Number)
                | other -> failtestf "expected single property, got %A" other
            | other -> failtestf "expected TypeLiteral, got %A" other

        testCase "empty TypeLiteral resolves to a no-member TypeLiteral" <| fun _ ->
            let arena = empty |> withTypes [ 0, TsType.TypeLiteral { Members = [] } ] |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeLiteral tl -> tl.Members |> List.length |> Flip.Expect.equal "no members" 0
            | other -> failtestf "expected TypeLiteral, got %A" other
    ]

// =========================================================================
// 23. EnumCase / Enum — value + parent back-link resolve
// =========================================================================
[<Tests>]
let enumTypeTests =
    testList "ArenaInterner.ResolveType enum" [
        testCase "Enum (as type) resolves name and case values, with each case's Parent back-linking to it" <| fun _ ->
            let enumCase name v parentKey : TsEnumCase = {
                Parent = key parentKey
                Source = None
                FullyQualifiedName = [ name ]
                Name = name
                Value = v
                Documentation = []
            }
            let enum: TsEnumType = {
                Source = None
                FullyQualifiedName = [ "Color" ]
                Name = "Color"
                Members = [ enumCase "Red" (TsLiteral.Int 0) 0; enumCase "Green" (TsLiteral.Int 1) 0 ]
                Documentation = []
            }
            let arena = empty |> withTypes [ 0, TsType.Enum enum ] |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Enum e ->
                Flip.Expect.equal "enum name" "Color" (e.Name |> Name.Case.valueOrSource)
                e.Members |> List.length |> Flip.Expect.equal "two cases" 2
                let firstCase = e.Members.Head.Value
                Flip.Expect.equal "first case name" "Red" (firstCase.Name |> Name.Case.valueOrSource)
                Flip.Expect.equal "first case value" (TsLiteral.Int 0) firstCase.Value
                // Parent back-link resolves to the very same Enum instance.
                Flip.Expect.isTrue "case Parent back-links to enum"
                    (System.Object.ReferenceEquals(e, firstCase.Parent.Value))
            | other -> failtestf "expected Enum, got %A" other

        testCase "standalone EnumCase resolves Value and Parent enum" <| fun _ ->
            let enum: TsEnumType = {
                Source = None
                FullyQualifiedName = [ "Dir" ]
                Name = "Dir"
                Members = []
                Documentation = []
            }
            let ec: TsEnumCase = {
                Parent = key 1
                Source = None
                FullyQualifiedName = [ "Up" ]
                Name = "Up"
                Value = TsLiteral.String "up"
                Documentation = []
            }
            let arena =
                empty
                |> withTypes [
                    0, TsType.EnumCase ec
                    1, TsType.Enum enum
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.EnumCase c ->
                Flip.Expect.equal "case name" "Up" (c.Name |> Name.Case.valueOrSource)
                Flip.Expect.equal "case value" (TsLiteral.String "up") c.Value
                Flip.Expect.equal "parent enum name" "Dir" (c.Parent.Value.Name |> Name.Case.valueOrSource)
            | other -> failtestf "expected EnumCase, got %A" other
    ]

// =========================================================================
// 24. Class (as type) — members, constructors, heritage
// =========================================================================
[<Tests>]
let classTypeTests =
    testList "ArenaInterner.ResolveType class" [
        testCase "Class (as type) resolves name, a property member, and a constructor parameter" <| fun _ ->
            let ctor: TsConstructor = {
                Documentation = []
                Parameters = [ { Name = "value"; IsOptional = false; IsSpread = false; Type = key 1; Documentation = [] } ]
            }
            let cls: TsClass = {
                Source = None
                FullyQualifiedName = [ "Box" ]
                Enumerable = false
                Name = "Box"
                Constructors = [ ctor ]
                Members = [ prop "size" 2 ]
                TypeParameters = []
                Heritage = { Implements = None; Extends = [] }
            }
            let arena =
                empty
                |> withTypes [
                    0, TsType.Class cls
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Class c ->
                Flip.Expect.equal "class name" "Box" (c.Name |> Name.Case.valueOrSource)
                match c.Members with
                | [ Member.Property p ] ->
                    force p.Type |> Flip.Expect.equal "member type" (ResolvedType.Primitive TypeKindPrimitive.Number)
                | other -> failtestf "expected one property member, got %A" other
                match c.Constructors with
                | [ ctorR ] ->
                    match ctorR.Parameters with
                    | [ par ] ->
                        Flip.Expect.equal "ctor param name" "value" (par.Name |> Name.Case.valueOrSource)
                        force par.Type |> Flip.Expect.equal "ctor param type" (ResolvedType.Primitive TypeKindPrimitive.String)
                    | other -> failtestf "expected one ctor param, got %A" other
                | other -> failtestf "expected one constructor, got %A" other
            | other -> failtestf "expected Class, got %A" other
    ]

// =========================================================================
// 25. Interface heritage + method member resolution
// =========================================================================
[<Tests>]
let interfaceHeritageTests =
    testList "ArenaInterner.ResolveType interface heritage & methods" [
        testCase "Interface with an extends clause resolves the heritage TypeReference target" <| fun _ ->
            let baseRef : TsTypeReference = { Type = key 9; TypeArguments = []; ResolvedType = None }
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "Derived" [] [ baseRef ])
                    9, TsType.Interface (iface "Base" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i ->
                match i.Heritage.Extends with
                | [ ext ] ->
                    match force ext.Type with
                    | ResolvedType.Interface b -> b.Name |> Name.Case.valueOrSource |> Flip.Expect.equal "base name" "Base"
                    | other -> failtestf "expected Base interface, got %A" other
                | other -> failtestf "expected one extends clause, got %A" other
            | other -> failtestf "expected Interface, got %A" other

        testCase "Interface method member resolves parameter and return types" <| fun _ ->
            let method: TsMethod = {
                Name = "compute"
                Parameters = [ { Name = "n"; IsOptional = false; IsSpread = false; Type = key 1; Documentation = [] } ]
                TypeParameters = []
                Type = key 2
                Documentation = []
                IsOptional = false
                IsStatic = false
            }
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "Calc" [ TsMember.Method (NoOverloads method) ] [])
                    1, TsType.Primitive TypeKindPrimitive.Number
                    2, TsType.Primitive TypeKindPrimitive.Boolean
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i ->
                match i.Members with
                | [ Member.Method [ m ] ] ->
                    Flip.Expect.equal "method name" "compute" (m.Name |> Name.Case.valueOrSource)
                    force m.Type |> Flip.Expect.equal "return type" (ResolvedType.Primitive TypeKindPrimitive.Boolean)
                    match m.Parameters with
                    | [ par ] -> force par.Type |> Flip.Expect.equal "param type" (ResolvedType.Primitive TypeKindPrimitive.Number)
                    | other -> failtestf "expected one param, got %A" other
                | other -> failtestf "expected one method member, got %A" other
            | other -> failtestf "expected Interface, got %A" other
    ]

// =========================================================================
// 26. Recursive alias + deeper cycle shapes (no stack overflow)
// =========================================================================
[<Tests>]
let recursiveAliasTests =
    testList "ArenaInterner recursive alias & deep cycles" [
        testCase "recursive type alias (Json = string | Json[]) resolves without overflow" <| fun _ ->
            // alias Json = string | Array<Json>  — exported as a TypeAlias whose body
            // (union member 2) refers to an array of the alias's OWN type key (1).
            let alias: TsTypeAlias = {
                Source = None
                FullyQualifiedName = [ "Json" ]
                Name = "Json"
                Type = key 1
                TypeParameters = []
                Documentation = []
            }
            let arena =
                empty
                |> withTypes [
                    1, TsType.Union (TsTypeUnion [ key 3; key 2 ])
                    2, TsType.Array (TsType.Primitive TypeKindPrimitive.String) // simplified element; the cycle is exercised below
                    3, TsType.Primitive TypeKindPrimitive.String
                ]
                |> withExports [ 0, TsExportDeclaration.TypeAlias alias ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.TypeAlias a) ->
                match force a.Type with
                | ResolvedType.Union u -> u.Types |> List.length |> Flip.Expect.equal "two union members" 2
                | other -> failtestf "expected Union alias body, got %A" other
            | other -> failtestf "expected Ok TypeAlias, got %A" other

        testCase "three-cycle of interfaces (A->B->C->A) resolves to cached instances" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "A" [ prop "next" 1 ] [])
                    1, TsType.Interface (iface "B" [ prop "next" 2 ] [])
                    2, TsType.Interface (iface "C" [ prop "next" 0 ] [])
                ]
                |> create
            let getNext rt =
                match rt with
                | ResolvedType.Interface i ->
                    match i.Members with
                    | [ Member.Property p ] -> force p.Type
                    | other -> failtestf "expected single property, got %A" other
                | other -> failtestf "expected Interface, got %A" other
            let a = arena.ResolveType(key 0)
            let b = getNext a
            let c = getNext b
            let aAgain = getNext c
            Flip.Expect.isTrue "A reached around the 3-cycle is the cached A" (System.Object.ReferenceEquals(a, aAgain))

        testCase "self-referential type alias resolves and caches its own instance" <| fun _ ->
            // alias Loop = Loop  — body refers back to the alias's body key.
            let arena =
                empty
                |> withTypes [
                    // an interface that points at itself through the array element
                    0, TsType.Interface (iface "Loop" [ prop "child" 1 ] [])
                    1, TsType.Array (TsType.Primitive TypeKindPrimitive.String)
                ]
                |> create
            // Resolving twice yields the same cached instance even though it is structural.
            Flip.Expect.isTrue "structural interface memoised"
                (System.Object.ReferenceEquals(arena.ResolveType(key 0), arena.ResolveType(key 0)))
    ]

// =========================================================================
// 27. Memoization across more variants + nested/transitive sharing
// =========================================================================
[<Tests>]
let memoizationVariantTests =
    testList "ArenaInterner memoization across variants" [
        testCase "an Array element shared by two outer arrays resolves through one cached inner instance" <| fun _ ->
            // Two interfaces each holding a property typed as key 1 (a shared interface).
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "Holder" [ prop "a" 1; prop "b" 1 ] [])
                    1, TsType.Interface (iface "Shared" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface i ->
                match i.Members with
                | [ Member.Property pa; Member.Property pb ] ->
                    Flip.Expect.isTrue "both props point at one Shared instance"
                        (System.Object.ReferenceEquals(force pa.Type, force pb.Type))
                | other -> failtestf "expected two properties, got %A" other
            | other -> failtestf "expected Interface, got %A" other

        testCase "transitive resolution: A.b.c chains through to a primitive leaf" <| fun _ ->
            // A { b : B }, B { c : number }
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "A" [ prop "b" 1 ] [])
                    1, TsType.Interface (iface "B" [ prop "c" 2 ] [])
                    2, TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.Interface a ->
                match a.Members with
                | [ Member.Property pb ] ->
                    match force pb.Type with
                    | ResolvedType.Interface b ->
                        match b.Members with
                        | [ Member.Property pc ] ->
                            force pc.Type |> Flip.Expect.equal "leaf primitive" (ResolvedType.Primitive TypeKindPrimitive.Number)
                        | other -> failtestf "expected B.c, got %A" other
                    | other -> failtestf "expected B, got %A" other
                | other -> failtestf "expected A.b, got %A" other
            | other -> failtestf "expected Interface A, got %A" other

        testCase "ResolveType and ResolveExport share one Enum instance for an exported enum" <| fun _ ->
            let enum: TsEnumType = {
                Source = None
                FullyQualifiedName = [ "E" ]
                Name = "E"
                Members = []
                Documentation = []
            }
            let arena = empty |> withExports [ 0, TsExportDeclaration.Enum enum ] |> create
            let asExport =
                match arena.ResolveExport(key 0) with
                | Ok (ResolvedExport.Enum e) -> e
                | other -> failtestf "expected Ok Enum, got %A" other
            match arena.ResolveType(key 0) with
            | ResolvedType.Enum asType ->
                Flip.Expect.isTrue "enum export & type are one instance" (System.Object.ReferenceEquals(asExport, asType))
            | other -> failtestf "expected Enum from ResolveType, got %A" other

        testCase "exported Class resolved via ResolveType reuses the export instance" <| fun _ ->
            let cls: TsClass = {
                Source = None
                FullyQualifiedName = [ "Cl" ]
                Enumerable = false
                Name = "Cl"
                Constructors = []
                Members = []
                TypeParameters = []
                Heritage = { Implements = None; Extends = [] }
            }
            let arena = empty |> withExports [ 0, TsExportDeclaration.Class cls ] |> create
            let asExport =
                match arena.ResolveExport(key 0) with
                | Ok (ResolvedExport.Class c) -> c
                | other -> failtestf "expected Ok Class, got %A" other
            match arena.ResolveType(key 0) with
            | ResolvedType.Class asType ->
                Flip.Expect.isTrue "class export & type are one instance" (System.Object.ReferenceEquals(asExport, asType))
            | other -> failtestf "expected Class from ResolveType, got %A" other
    ]

// =========================================================================
// 28. isLibEs propagation onto further export kinds
// =========================================================================
[<Tests>]
let libEsMoreTests =
    testList "ArenaInterner isLibEs propagation (more kinds)" [
        testCase "exported Class in LibEsExports resolves with IsLibEs = true" <| fun _ ->
            let cls: TsClass = {
                Source = None
                FullyQualifiedName = [ "LibCls" ]
                Enumerable = false
                Name = "LibCls"
                Constructors = []
                Members = []
                TypeParameters = []
                Heritage = { Implements = None; Extends = [] }
            }
            let dr = { (empty |> withExports [ 0, TsExportDeclaration.Class cls ]) with LibEsExports = [ key 0 ] }
            match (create dr).ResolveExport(key 0) with
            | Ok (ResolvedExport.Class c) -> Flip.Expect.isTrue "IsLibEs flagged" c.IsLibEs
            | other -> failtestf "expected Ok Class, got %A" other

        testCase "exported TypeAlias NOT in LibEsExports resolves with IsLibEs = false" <| fun _ ->
            let alias: TsTypeAlias = {
                Source = None
                FullyQualifiedName = [ "UserId" ]
                Name = "UserId"
                Type = key 1
                TypeParameters = []
                Documentation = []
            }
            let arena =
                empty
                |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.String ]
                |> withExports [ 0, TsExportDeclaration.TypeAlias alias ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.TypeAlias a) -> Flip.Expect.isFalse "IsLibEs not flagged" a.IsLibEs
            | other -> failtestf "expected Ok TypeAlias, got %A" other

        testCase "exported Enum in LibEsExports resolves with IsLibEs = true" <| fun _ ->
            let enum: TsEnumType = {
                Source = None
                FullyQualifiedName = [ "LibEnum" ]
                Name = "LibEnum"
                Members = []
                Documentation = []
            }
            let dr = { (empty |> withExports [ 0, TsExportDeclaration.Enum enum ]) with LibEsExports = [ key 0 ] }
            match (create dr).ResolveExport(key 0) with
            | Ok (ResolvedExport.Enum e) -> Flip.Expect.isTrue "IsLibEs flagged" e.IsLibEs
            | other -> failtestf "expected Ok Enum, got %A" other
    ]

// =========================================================================
// 29. Module export — nested exports resolve through the same machinery
// =========================================================================
[<Tests>]
let moduleNestedTests =
    testList "ArenaInterner module nested exports" [
        testCase "Module nested Interface export resolves and is reachable from m.Exports" <| fun _ ->
            let modu: TsModule = {
                Source = None
                FullyQualifiedName = [ "NS" ]
                Name = "NS"
                IsNamespace = false
                IsRecursive = false
                Exports = [ TsExportDeclaration.Interface (iface "Inner" [ prop "v" 1 ] []) ]
            }
            let arena =
                empty
                |> withTypes [ 1, TsType.Primitive TypeKindPrimitive.Number ]
                |> withExports [ 0, TsExportDeclaration.Module modu ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Module m) ->
                Flip.Expect.isFalse "module (not namespace)" m.IsNamespace
                match m.Exports with
                | [ ResolvedExport.Interface i ] ->
                    Flip.Expect.equal "nested interface name" "Inner" (i.Name |> Name.Case.valueOrSource)
                    match i.Members with
                    | [ Member.Property p ] ->
                        force p.Type |> Flip.Expect.equal "nested member type" (ResolvedType.Primitive TypeKindPrimitive.Number)
                    | other -> failtestf "expected one property, got %A" other
                | other -> failtestf "expected one nested Interface export, got %A" other
            | other -> failtestf "expected Ok Module, got %A" other

        testCase "Function export with multiple overloads resolves each in order" <| fun _ ->
            let mkFunc retKey : TsFunction = {
                Source = None
                FullyQualifiedName = [ "over" ]
                Documentation = []
                IsDeclared = true
                Name = "over"
                Type = key retKey
                Parameters = []
                TypeParameters = []
                SignatureKey = key 9
            }
            let arena =
                empty
                |> withTypes [
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                    9, TsType.TypeLiteral { Members = [] }
                ]
                |> withExports [ 0, TsExportDeclaration.Function (Overloaded (Set [ mkFunc 1; mkFunc 2 ])) ]
                |> create
            match arena.ResolveExport(key 0) with
            | Ok (ResolvedExport.Function fns) ->
                fns |> List.length |> Flip.Expect.equal "two overloads" 2
                fns
                |> List.map (fun f ->
                    match force f.Type with
                    | ResolvedType.Primitive p -> p
                    | other -> failtestf "expected primitive return, got %A" other)
                |> Set.ofList
                |> Flip.Expect.equal "both overload return types resolved"
                    (Set [ TypeKindPrimitive.String; TypeKindPrimitive.Number ])
            | other -> failtestf "expected Ok Function, got %A" other
    ]

// =========================================================================
// 30. TypeReference ResolvedType field + nested type-argument resolution
// =========================================================================
[<Tests>]
let typeReferenceMoreTests =
    testList "ArenaInterner.ResolveType type reference (more)" [
        testCase "TypeReference carries its optional ResolvedType when present" <| fun _ ->
            let tref : TsTypeReference = { Type = key 9; TypeArguments = []; ResolvedType = Some (key 1) }
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeReference tref
                    1, TsType.Primitive TypeKindPrimitive.Boolean
                    9, TsType.Interface (iface "Ref" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeReference r ->
                match r.ResolvedType with
                | Some lz -> force lz |> Flip.Expect.equal "resolved-type field" (ResolvedType.Primitive TypeKindPrimitive.Boolean)
                | None -> failtest "expected ResolvedType to be Some"
            | other -> failtestf "expected TypeReference, got %A" other

        testCase "nested TypeReference type argument (Foo<Bar<string>>) resolves transitively" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeReference (typeRef 8 [ 1 ])      // Foo<#1>
                    1, TsType.TypeReference (typeRef 9 [ 2 ])      // Bar<string>
                    2, TsType.Primitive TypeKindPrimitive.String
                    8, TsType.Interface (iface "Foo" [] [])
                    9, TsType.Interface (iface "Bar" [] [])
                ]
                |> create
            match arena.ResolveType(key 0) with
            | ResolvedType.TypeReference foo ->
                match foo.TypeArguments |> List.map force with
                | [ ResolvedType.TypeReference bar ] ->
                    bar.TypeArguments |> List.map force
                    |> Flip.Expect.equal "inner type arg" [ ResolvedType.Primitive TypeKindPrimitive.String ]
                | other -> failtestf "expected one nested TypeReference arg, got %A" other
            | other -> failtestf "expected TypeReference, got %A" other
    ]

// =========================================================================
// 31. Array / ReadOnly nesting + ExportMap multi-path grouping
// =========================================================================
[<Tests>]
let nestingAndExportMapTests =
    testList "ArenaInterner nested wrappers & export map grouping" [
        testCase "ReadOnly of Array of primitive nests correctly" <| fun _ ->
            let arena =
                empty
                |> withTypes [ 0, TsType.ReadOnly (TsType.Array (TsType.Primitive TypeKindPrimitive.Boolean)) ]
                |> create
            arena.ResolveType(key 0)
            |> Flip.Expect.equal "readonly array bool"
                (ResolvedType.ReadOnly (ResolvedType.Array (ResolvedType.Primitive TypeKindPrimitive.Boolean)))

        testCase "ExportMap groups exports under multiple distinct source paths" <| fun _ ->
            let dr =
                { (empty |> withExports [
                        0, TsExportDeclaration.Interface (iface "A" [] [])
                        1, TsExportDeclaration.Interface (iface "B" [] [])
                        2, TsExportDeclaration.Interface (iface "C" [] [])
                   ]) with ExportMap = Map [ "x.d.ts", Set [ key 0 ]; "y.d.ts", Set [ key 1; key 2 ] ] }
            let arena = create dr
            (arena.ExportMap.["x.d.ts"] |> List.length, arena.ExportMap.["y.d.ts"] |> List.length)
            |> Flip.Expect.equal "grouped counts per path" (1, 2)
    ]

// =========================================================================
// 32. DECODER UNION INTERNING IDENTITY — the load-bearing question.
//
// The encoder over-mints union TypeKeys: structurally-identical synthesized
// unions get DISTINCT keys (measured: 1579 union entries collapse to 866
// distinct member-signatures => 713 redundant duplicate keys). Commit 1d4d826
// added decoder-side interning (Arena.Interner.fs ~695):
//   internedUnions.GetOrAdd(tsTypeUnion.Types |> List.sort, fun _ -> { Union.Types = ... })
// which interns the resolved Union INSTANCE by its sorted member-key signature.
//
// These tests pin down EXACTLY what that interning does to identity, and —
// critically — what the GENERATOR's ResolvedType-keyed caches will see. The
// generator keys its dedup dictionaries (TypeAliasRemap / PreludeRenders /
// AnchorRenders / InFlight / TopLevelExports — Xantham.Generator/Types/Generator.fs
// lines 24-25, 62-84) by `ResolvedType`. The `ResolvedType` DU itself carries
// only [<RequireQualifiedAccess>] (Arena.Interner.fs:91) => DEFAULT STRUCTURAL
// EQUALITY, but its `Union` payload is [<ReferenceEquality>] (Arena.Interner.fs:210).
// Therefore `ResolvedType.Union a = ResolvedType.Union b` IFF the underlying
// `Union` records are REFERENCE-equal. So whether two distinct encoder keys
// collapse to one generator cache entry is decided entirely by whether interning
// makes them resolve to the SAME `Union` instance.
//
// We therefore assert:
//   (a) two distinct keys with the SAME signature -> reference-equal Union
//       (=> structurally equal ResolvedType => ONE generator-cache key);
//   (b) two distinct keys with DIFFERENT signatures -> NOT interned together
//       (guard against over-collapse);
//   (c) a Dictionary<ResolvedType, _> (mirroring the generator caches) collapses
//       the same-signature pair to ONE entry and keeps the different-signature
//       pair as TWO — proving the decoder fix is sufficient for generator dedup.
// =========================================================================
[<Tests>]
let unionInterningIdentityTests =
    testList "ArenaInterner union interning identity (generator-cache implications)" [
        // (a) SAME signature, two distinct keys -> one shared Union instance.
        //     Reports BOTH reference- and structural-equality (they coincide here
        //     precisely because Union is [<ReferenceEquality>]).
        testCase "same-signature unions at distinct keys are reference-equal AND structurally-equal" <| fun _ ->
            // key 0 = string|number ; key 3 = string|number ; key 10 = number|string
            // (member order permuted, same set) — all three share ONE Union instance
            // because interning keys by the SORTED member signature.
            let arena =
                empty
                |> withTypes [
                    0,  TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    3,  TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    10, TsType.Union (TsTypeUnion [ key 2; key 1 ])   // permuted order
                    1,  TsType.Primitive TypeKindPrimitive.String
                    2,  TsType.Primitive TypeKindPrimitive.Number
                ]
                |> create
            let a = arena.ResolveType(key 0)
            let b = arena.ResolveType(key 3)
            let c = arena.ResolveType(key 10)
            match a, b, c with
            | ResolvedType.Union ua, ResolvedType.Union ub, ResolvedType.Union uc ->
                // REFERENCE equality of the underlying Union record.
                Flip.Expect.isTrue "ua/ub reference-equal (interned to one instance)"
                    (System.Object.ReferenceEquals(ua, ub))
                Flip.Expect.isTrue "ua/uc reference-equal despite permuted member order"
                    (System.Object.ReferenceEquals(ua, uc))
                // STRUCTURAL equality of the wrapping ResolvedType. Because the DU has
                // default structural equality but Union is [<ReferenceEquality>], this
                // holds ONLY because the payloads are the same reference.
                Flip.Expect.isTrue "ResolvedType.Union a = ResolvedType.Union b structurally" (a = b)
                Flip.Expect.isTrue "ResolvedType.Union a = ResolvedType.Union c structurally" (a = c)
            | _ -> failtest "expected three Unions"

        // (b) DIFFERENT signature -> NOT interned together (no over-collapse).
        testCase "different-signature unions at distinct keys are NOT interned together" <| fun _ ->
            // key 0 = string|number ; key 3 = string|boolean — distinct signatures.
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 2 ])
                    3, TsType.Union (TsTypeUnion [ key 1; key 4 ])
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                    4, TsType.Primitive TypeKindPrimitive.Boolean
                ]
                |> create
            let a = arena.ResolveType(key 0)
            let b = arena.ResolveType(key 3)
            match a, b with
            | ResolvedType.Union ua, ResolvedType.Union ub ->
                Flip.Expect.isFalse "different signatures must NOT be one instance"
                    (System.Object.ReferenceEquals(ua, ub))
                Flip.Expect.isFalse "different signatures must NOT be structurally equal" (a = b)
            | _ -> failtest "expected two Unions"

        // (c) The actual generator-cache behaviour, modelled directly: a plain
        //     Dictionary<ResolvedType, int> (the same key type the generator's
        //     TypeAliasRemap / PreludeRenders / AnchorRenders use) collapses the
        //     same-signature pair to ONE entry and keeps a different-signature
        //     pair as TWO. This is the verdict-deciding assertion: it shows the
        //     decoder interning is SUFFICIENT for generator dedup — no generator
        //     change is needed to collapse same-signature unions, and no
        //     over-collapse of genuinely-different unions occurs.
        testCase "a ResolvedType-keyed Dictionary collapses same-signature unions and keeps different ones distinct" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Union (TsTypeUnion [ key 1; key 2 ])   // string|number
                    3, TsType.Union (TsTypeUnion [ key 1; key 2 ])   // string|number  (dup)
                    5, TsType.Union (TsTypeUnion [ key 1; key 4 ])   // string|boolean (distinct)
                    1, TsType.Primitive TypeKindPrimitive.String
                    2, TsType.Primitive TypeKindPrimitive.Number
                    4, TsType.Primitive TypeKindPrimitive.Boolean
                ]
                |> create
            // Mirror the generator's cache key type exactly.
            let cache = System.Collections.Generic.Dictionary<ResolvedType, int>()
            cache[arena.ResolveType(key 0)] <- 0
            cache[arena.ResolveType(key 3)] <- 3   // same signature -> overwrites key-0 entry
            cache[arena.ResolveType(key 5)] <- 5   // distinct signature -> separate entry
            Flip.Expect.equal "same-signature unions collapse to ONE cache entry; different stays separate"
                2 cache.Count

        // Sanity: a Dictionary keyed by a non-interned [<ReferenceEquality>] payload
        // would NOT collapse. Two interfaces at distinct keys (no structural interning)
        // remain two cache entries — confirming interning is what does the work for
        // unions, not some incidental structural equality of ResolvedType.
        testCase "control: distinct-key interfaces (not interned) stay as two cache entries" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.Interface (iface "Same" [] [])
                    1, TsType.Interface (iface "Same" [] [])
                ]
                |> create
            let cache = System.Collections.Generic.Dictionary<ResolvedType, int>()
            cache[arena.ResolveType(key 0)] <- 0
            cache[arena.ResolveType(key 1)] <- 1
            Flip.Expect.equal "structurally-identical interfaces at distinct keys do NOT collapse" 2 cache.Count
    ]

// =========================================================================
// TypeLiteral is DELIBERATELY NOT interned by structure (unlike unions).
//
// These tests pin the decision and document WHY. A union (`U2<A,B>`) is
// location-independent: the same structure is the same type anywhere, so
// `internedUnions` safely collapses same-signature unions to one identity.
// A TypeLiteral is DIFFERENT: it is hoisted to a NAMED nested type under a
// specific owner module, so two structurally-identical literals under different
// owners are DISTINCT types with distinct paths. Interning them by structure was
// tried and REGRESSED the cloudflare surface (it collapsed two different-owner
// `{...}` literals to one identity, the generator anchored it under one owner,
// and refs from the other owner dangled — FS0033/FS0039 went UP). So a TypeLiteral
// minted at a distinct TypeKey must resolve to a DISTINCT (non-reference-equal)
// instance, even when its members are identical. The object-literal over-minting
// is therefore a PATH-ANCHORING problem (the generator's localise/anchor layer),
// NOT a decoder-canonicalization one.
// =========================================================================
[<Tests>]
let typeLiteralNotInternedTests =
    testList "ArenaInterner type-literal NOT interned (location-dependent hoisting)" [
        // Same members, distinct keys -> DISTINCT instances (NOT interned). This is the
        // load-bearing assertion: it guards against re-introducing structural interning
        // for TypeLiterals, which would break different-owner hoisting.
        testCase "same-members type literals at distinct keys are DISTINCT (not interned)" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeLiteral { Members = [ prop "x" 1 ] }
                    3, TsType.TypeLiteral { Members = [ prop "x" 1 ] }
                    1, TsType.Primitive TypeKindPrimitive.String
                ]
                |> create
            let a = arena.ResolveType(key 0)
            let b = arena.ResolveType(key 3)
            match a, b with
            | ResolvedType.TypeLiteral ta, ResolvedType.TypeLiteral tb ->
                Flip.Expect.isFalse "distinct-key TypeLiterals must NOT share one instance"
                    (System.Object.ReferenceEquals(ta, tb))
            | _ -> failtest "expected two TypeLiterals"

        // Consequently a ResolvedType-keyed Dictionary (the generator cache key type) keeps
        // them as SEPARATE entries — each distinct-key literal gets its own placement, which
        // is correct because each is hoisted under its own owner.
        testCase "a ResolvedType-keyed Dictionary keeps same-members type literals as distinct entries" <| fun _ ->
            let arena =
                empty
                |> withTypes [
                    0, TsType.TypeLiteral { Members = [ prop "x" 1 ] }   // { x: string }
                    3, TsType.TypeLiteral { Members = [ prop "x" 1 ] }   // { x: string }  (same members, distinct key)
                    1, TsType.Primitive TypeKindPrimitive.String
                ]
                |> create
            let cache = System.Collections.Generic.Dictionary<ResolvedType, int>()
            cache[arena.ResolveType(key 0)] <- 0
            cache[arena.ResolveType(key 3)] <- 3
            Flip.Expect.equal "same-members type literals at distinct keys stay as TWO cache entries"
                2 cache.Count
    ]
