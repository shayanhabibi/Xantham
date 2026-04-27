module Main

open System.Collections.Generic
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Node.Api
open Fable.Mocha
open Fable.Core.Testing
open Xantham.Schema

// -----------------------------------------------------------------------
// Infrastructure
// -----------------------------------------------------------------------

let createTestReader (fileName: string) =
    let filePath = path.join(__SOURCE_DIRECTORY__, $"/TypeFiles/{fileName}.d.ts")
    let reader = TypeScriptReader.create filePath
    reader

let createMultiFileTestReader (fileNames: string array) =
    fileNames
    |> Array.map (fun f -> path.join(__SOURCE_DIRECTORY__, "TypeFiles", $"{f}.d.ts"))
    |> TypeScriptReader.createFor

let createSubdirTestReader (relPath: string) =
    let filePath = path.join(__SOURCE_DIRECTORY__, "TypeFiles", $"{relPath}.d.ts")
    TypeScriptReader.create filePath

let runReader (reader: TypeScriptReader) = read reader

// -----------------------------------------------------------------------
// Lookup helpers
// None-returning variants prefixed 'try'; bare variants throw on miss.
// Never assert TypeKey identity values — those are runtime-generated IDs.
// -----------------------------------------------------------------------

/// Find the first interface in the result whose Name matches, or None.
let tryFindInterface name (result: EncodedResult) =
    result.ExportedDeclarations
    |> Seq.tryPick (function
        | KeyValue(_, TsExportDeclaration.Interface iface) when iface.Name = name -> Some iface
        | _ -> None
        )
    |> Option.orElseWith (fun () ->
        result.ExportedDeclarations
        |> Seq.tryPick (function
            | KeyValue(_, TsExportDeclaration.Interface iface) when iface.Name = name -> Some iface
            | _ -> None
            )
        )

/// Find the first interface in the result whose Name matches; throws on miss.
let findInterface name result =
    tryFindInterface name result
    |> Option.defaultWith (fun () -> failwithf "Interface '%s' not found in result" name)

/// Find the first enum whose Name matches; throws on miss.
let findEnum name (result: EncodedResult) =
    result.Types |> Seq.pick (function
        | KeyValue(_, TsType.Enum e) when e.Name = name -> Some e
        | _ -> None)

/// Find the first FunctionDeclaration whose head-variant Name matches; throws on miss.
/// Works for both NoOverloads (single) and Overloaded (merged) constructs.
let findFunction name (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (function
        | KeyValue(k, TsExportDeclaration.Function fd) when k > TypeKey 0 && fd.ValueOrHead.Name = name -> Some fd
        | _ -> None)

/// Return the TsProperty record for a named property in a member list; throws on miss.
let findProperty name (members: TsMember list) =
    members |> List.pick (function
        | TsMember.Property p when p.Name = name -> Some p
        | _ -> None)

/// Return the method overloadable construct for a named method; throws on miss.
let findMethod name (members: TsMember list) =
    members |> List.pick (function
        | TsMember.Method m when m.ValueOrHead.Name = name -> Some m
        | _ -> None)

let findType (typeKey: TypeKey) (result: EncodedResult) =
    result.Types
    |> Seq.pick (fun (KeyValue(key, node)) -> if key = typeKey then Some node else None)
let findExportByKey (typeKey: TypeKey) (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (fun (KeyValue(key, node)) -> if key = typeKey then Some node else None)

/// Find the first type alias whose Name matches; throws on miss.
let findAlias name (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (function
        | KeyValue(_, TsExportDeclaration.TypeAlias a) when a.Name = name -> Some a
        | _ -> None)

/// Find the first class whose Name matches; throws on miss.
let findClass name (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (function
        | KeyValue(_, TsExportDeclaration.Class c) when c.Name = name -> Some c
        | _ -> None)

/// Find the first variable whose Name matches; throws on miss.
let findVariable name (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (function
        | KeyValue(_, TsExportDeclaration.Variable v) when v.Name = name -> Some v
        | _ -> None)

/// Find the first module/namespace whose Name matches; throws on miss.
let findModule name (result: EncodedResult) =
    result.ExportedDeclarations |> Seq.pick (function
        | KeyValue(_, TsExportDeclaration.Module m) when m.Name = name -> Some m
        | _ -> None)

// -----------------------------------------------------------------------
// Fixture: aliases.d.ts
// export type GInt = number
//
// -----------------------------------------------------------------------
let aliasTests =
    testList "aliases.d.ts" [
        let result = createTestReader "aliases" |> runReader
        testCase "GInt is a TypeAlias" <| fun _ ->
            let alias = result |> findAlias "GInt"
            "Alias has underlying type of number"
            |> Expect.equal alias.Type TypeKindPrimitive.Number.TypeKey
    ]

// -----------------------------------------------------------------------
// Fixture: basic.d.ts
//   export type BaseObject    = { name: string }
//   export interface BaseInterface { name: string; age: number }
// -----------------------------------------------------------------------
let basicTests =
    testList "basic.d.ts" [
        let result = createTestReader "basic" |> runReader
        testCase "result contains at least one interface with members" <| fun _ ->
            "Should have an interface with at least one member"
            |> Expect.exists result.ExportedDeclarations (fun kv ->
                match kv.Value with
                | TsExportDeclaration.Interface { Members = _ :: _ } -> true
                | _ -> false)

        testCase "result contains exactly one interface and one alias" <| fun _ ->
            let count =
                result.ExportedDeclarations
                |> Seq.filter (fun kv -> kv.Value.IsInterface || kv.Value.IsTypeAlias)
                |> Seq.length
            "" |> Expect.equal count 2

        testCase "BaseInterface has properties named 'name' and 'age'" <| fun _ ->
            let iface = result |> findInterface "BaseInterface"
            let names =
                iface.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names ["name"; "age"]
    ]

// -----------------------------------------------------------------------
// Fixture: merge.d.ts
//   export interface MergeProps { name: string }  (first declaration)
//   export interface MergeProps { age: number  }  (second declaration, same TypeKey)
//   export type Dummy = {}
//   export type D = { name: MergeProps['age'] }
// -----------------------------------------------------------------------
let mergeTests =
    testList "merge.d.ts" [
        let result = createTestReader "merge" |> runReader
        testCase "duplicate MergeProps declarations merged into exactly one interface node" <| fun _ ->
            // Two source declarations of MergeProps share a TypeKey; the pipeline must
            // produce exactly one node for that key — not two separate entries.
            let count =
                result.Types
                |> Seq.filter (function
                    | KeyValue(_, TsType.Interface iface) -> iface.Name = "MergeProps"
                    | _ -> false)
                |> Seq.length
            "MergeProps should appear exactly once in result" |> Expect.equal count 1

        let iface = result |> findInterface "MergeProps"
        testCase "merged MergeProps has 2 members (one from each declaration)" <| fun _ ->
            // 'name' came from the first declaration, 'age' from the second
            "" |> Expect.hasLength iface.Members 2

        testCase "merged MergeProps has members named 'name' and 'age'" <| fun _ ->
            let names =
                iface.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names ["name"; "age"]

        testCase "two distinct interfaces present after merge (MergeProps + D)" <| fun _ ->
            let count =
                result.ExportedDeclarations
                |> Seq.filter (fun typ -> typ.Value.IsInterface || typ.Value.IsTypeAlias)
                |> Seq.length
            // MergeProps (merged) and D (type literal interface) = 2
            "" |> Expect.equal count 2
    ]

// -----------------------------------------------------------------------
// Fixture: extends.d.ts
//   export interface BaseInterface    { parentProp: string }
//   export interface ExtendedInterface extends BaseInterface { childProp: number }
//
// Fixture: multiple-extends.d.ts
//   export interface BaseInterface    { parentProp: string }
//   export interface ExtendedInterface  extends BaseInterface { childProp: number }
//   export interface ExtendedInterface2 extends BaseInterface { childProp2: number }
// -----------------------------------------------------------------------
let heritageTests =
    testList "extends.d.ts" [
        let result = createTestReader "extends" |> runReader
        testCase "heritage TypeKey of extended interface is present in result" <| fun _ ->
            result.ExportedDeclarations
            |> Seq.pick (function
                | KeyValue(_, TsExportDeclaration.Interface { Heritage = { Extends = [ extend ] } }) ->
                    Some extend.Type
                | _ -> None)
            |> fun typeKey ->
                "Heritage TypeKey should resolve to a node in result"
                |> Expect.exists result.Types (_.Key >> (=) typeKey)

        let iface = result |> findInterface "ExtendedInterface"
        testCase "ExtendedInterface has exactly 1 heritage entry" <| fun _ ->
            "" |> Expect.hasLength iface.Heritage.Extends 1

        testCase "ExtendedInterface has its own member 'childProp'" <| fun _ ->
            "ExtendedInterface should declare childProp"
            |> Expect.exists iface.Members (function
                | TsMember.Property p -> p.Name = "childProp"
                | _ -> false)
    ]

let multipleExtendsTests =
    testList "multiple-extends.d.ts" [
        let result = createTestReader "multiple-extends" |> runReader
        testCase "all heritage TypeKeys resolve to nodes in result" <| fun _ ->
            let extendedKeys =
                result.ExportedDeclarations
                |> Seq.choose (function
                    | KeyValue(_, TsExportDeclaration.Interface { Heritage = { Extends = [ extend ] } }) ->
                        Some extend.Type
                    | _ -> None)
                |> Seq.toArray
            let resultKeys = result.Types.Keys |> Seq.toArray
            "All heritage keys should be in result" |> Expect.containsAll resultKeys extendedKeys

        testCase "both extending interfaces are present in result" <| fun _ ->
            let extendingNames =
                result.ExportedDeclarations
                |> Seq.choose (function
                    | KeyValue(_, TsExportDeclaration.Interface iface) when not (List.isEmpty iface.Heritage.Extends) ->
                        Some iface.Name
                    | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll extendingNames ["ExtendedInterface"; "ExtendedInterface2"]
    ]

// -----------------------------------------------------------------------
// Fixture: members.d.ts
//   WithProperties:  name: string; age?: number; readonly active: boolean
//   WithMethods:     greet(message: string): void; add(a: number, b: number): number
//   WithSignatures:  (x: number): boolean; new(value: string): object; [key: string]: unknown
// -----------------------------------------------------------------------
let memberTests =
    testList "members.d.ts" [

        let result = createTestReader "members" |> runReader

        // --- WithProperties: member count and kinds ---
        testCase "WithProperties has exactly 3 members" <| fun _ ->
            let iface = result |> findInterface "WithProperties"
            "" |> Expect.hasLength iface.Members 3

        testCase "WithProperties all members are TsMember.Property" <| fun _ ->
            let iface = result |> findInterface "WithProperties"
            "All members should be Property"
            |> Expect.all iface.Members (function TsMember.Property _ -> true | _ -> false)

        testCase "WithProperties has properties named 'name', 'age', 'active'" <| fun _ ->
            let iface = result |> findInterface "WithProperties"
            let names =
                iface.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names ["name"; "age"; "active"]

        // --- WithProperties: IsOptional flag ---

        testCase "'name' is not optional (required property)" <| fun _ ->
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "name"
            "name.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'age' is optional (declared with ?)" <| fun _ ->
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "age"
            "age.IsOptional should be true" |> Expect.isTrue p.IsOptional

        // --- WithProperties: Accessor flag ---

        testCase "'name' has ReadWrite accessor (mutable)" <| fun _ ->
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "name"
            "name.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'active' has ReadOnly accessor (declared readonly)" <| fun _ ->
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "active"
            "active.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly

        // --- WithMethods ---

        testCase "WithMethods has exactly 2 members" <| fun _ ->
            let iface = result |> findInterface "WithMethods"
            "" |> Expect.hasLength iface.Members 2

        testCase "WithMethods all members are TsMember.Method" <| fun _ ->
            let iface = result |> findInterface "WithMethods"
            "All members should be Method"
            |> Expect.all iface.Members (function TsMember.Method _ -> true | _ -> false)

        testCase "WithMethods has methods named 'greet' and 'add'" <| fun _ ->
            let iface = result |> findInterface "WithMethods"
            let names =
                iface.Members |> List.choose (function TsMember.Method m -> Some m.ValueOrHead.Name | _ -> None)
            "" |> Expect.containsAll names ["greet"; "add"]

        testCase "'greet' method has 1 parameter" <| fun _ ->
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "greet"
            "" |> Expect.hasLength m.ValueOrHead.Parameters 1

        testCase "'greet' parameter is named 'message'" <| fun _ ->
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "greet"
            let p = m.ValueOrHead.Parameters.[0]
            "greet param should be named message" |> Expect.equal p.Name "message"

        testCase "'add' method has 2 parameters" <| fun _ ->
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "add"
            "" |> Expect.hasLength m.ValueOrHead.Parameters 2

        testCase "'add' parameter names are 'a' and 'b'" <| fun _ ->
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "add"
            let names = m.ValueOrHead.Parameters |> List.map _.Name
            "" |> Expect.containsAll names ["a"; "b"]

        // --- WithSignatures ---

        testCase "WithSignatures has a TsMember.CallSignature" <| fun _ ->
            let iface = result |> findInterface "WithSignatures"
            "Should have a CallSignature"
            |> Expect.exists iface.Members (function TsMember.CallSignature _ -> true | _ -> false)

        testCase "WithSignatures has a TsMember.ConstructSignature" <| fun _ ->
            let iface = result |> findInterface "WithSignatures"
            "Should have a ConstructSignature"
            |> Expect.exists iface.Members (function TsMember.ConstructSignature _ -> true | _ -> false)

        testCase "WithSignatures has a TsMember.IndexSignature" <| fun _ ->
            let iface = result |> findInterface "WithSignatures"
            "Should have an IndexSignature"
            |> Expect.exists iface.Members (function TsMember.IndexSignature _ -> true | _ -> false)
    ]

// -----------------------------------------------------------------------
// Fixture: overloads.d.ts
//   export function process(x: string): string;  ← overload 1
//   export function process(x: number): number;  ← overload 2
//   export function identity(x: string): string;
// -----------------------------------------------------------------------
let overloadTests =
    testList "overloads.d.ts" [
        let result = createTestReader "overloads" |> runReader
        testCase "both 'process' and 'identity' are present in result" <| fun _ ->
            let names =
                result.ExportedDeclarations
                |> Seq.choose (function
                    | KeyValue(_, TsExportDeclaration.Function fd) -> Some fd.ValueOrHead.Name
                    | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll names ["process"; "identity"]

        testCase "'process' is emitted as TsOverloadableConstruct.Overloaded" <| fun _ ->
            // Two distinct declarations → pipeline must merge into Overloaded
            let fd = result |> findFunction "process"
            match fd with
            | Overloaded _ -> Expect.pass()
            | NoOverloads _ -> failwith "'process' should be Overloaded, not NoOverloads"

        testCase "'process' Overloaded set contains 2 variants" <| fun _ ->
            let fd = result |> findFunction "process"
            "process should have 2 overload variants" |> Expect.equal fd.Values.Length 2

        testCase "'identity' is emitted as TsOverloadableConstruct.NoOverloads" <| fun _ ->
            // Single declaration → no merging needed, must remain NoOverloads
            let fd = result |> findFunction "identity"
            match fd with
            | NoOverloads _ -> Expect.pass()
            | Overloaded _ -> failwith "'identity' should be NoOverloads, not Overloaded"
    ]

// -----------------------------------------------------------------------
// Fixture: optional-readonly.d.ts
//   export interface Modifiers {
//     required: string
//     optional?: number
//     readonly readonlyProp: boolean
//     readonly optReadonly?: string
//   }
// -----------------------------------------------------------------------
let modifierTests =
    testList "optional-readonly.d.ts" [
        let result = createTestReader "optional-readonly" |> runReader
        testCase "Modifiers interface has exactly 4 properties" <| fun _ ->
            let iface = result |> findInterface "Modifiers"
            "" |> Expect.hasLength iface.Members 4

        // IsOptional flag

        testCase "'required' IsOptional = false" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "required"
            "required.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'optional' IsOptional = true" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optional"
            "optional.IsOptional should be true" |> Expect.isTrue p.IsOptional

        testCase "'readonlyProp' IsOptional = false" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "readonlyProp"
            "readonlyProp.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'optReadonly' IsOptional = true" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optReadonly"
            "optReadonly.IsOptional should be true" |> Expect.isTrue p.IsOptional

        // Accessor flag

        testCase "'required' Accessor = ReadWrite (mutable)" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "required"
            "required.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'optional' Accessor = ReadWrite (mutable)" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optional"
            "optional.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'readonlyProp' Accessor = ReadOnly" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "readonlyProp"
            "readonlyProp.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly

        testCase "'optReadonly' Accessor = ReadOnly" <| fun _ ->
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optReadonly"
            "optReadonly.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly
    ]

// -----------------------------------------------------------------------
// Fixture: enum.d.ts
//   export enum Direction { Up = 0, Down = 1, Left = 2, Right = 3 }
// -----------------------------------------------------------------------
let enumTests =
    testList "enum.d.ts" [
        let result = createTestReader "enum" |> runReader
        testCase "'Direction' enum is present in result" <| fun _ ->
            "Direction should be in result"
            |> Expect.exists result.Types (function
                | KeyValue(_, TsType.Enum e) when e.Name = "Direction" -> true
                | _ -> false)

        testCase "'Direction' has exactly 4 members" <| fun _ ->
            let e = result |> findEnum "Direction"
            "" |> Expect.hasLength e.Members 4

        testCase "'Direction' member names are Up, Down, Left, Right" <| fun _ ->
            let e = result |> findEnum "Direction"
            "" |> Expect.containsAll (e.Members |> List.map _.Name) ["Up"; "Down"; "Left"; "Right"]

        testCase "'Up' has TsLiteral.Int 0" <| fun _ ->
            let e = result |> findEnum "Direction"
            let up = e.Members |> List.find (fun c -> c.Name = "Up")
            "Up.Value should be Int 0" |> Expect.equal up.Value (TsLiteral.Int 0)

        testCase "'Down' has TsLiteral.Int 1" <| fun _ ->
            let e = result |> findEnum "Direction"
            let down = e.Members |> List.find (fun c -> c.Name = "Down")
            "Down.Value should be Int 1" |> Expect.equal down.Value (TsLiteral.Int 1)

        testCase "'Right' has TsLiteral.Int 3" <| fun _ ->
            let e = result |> findEnum "Direction"
            let right = e.Members |> List.find (fun c -> c.Name = "Right")
            "Right.Value should be Int 3" |> Expect.equal right.Value (TsLiteral.Int 3)
    ]

// -----------------------------------------------------------------------
// Fixture: generics.d.ts
//   export interface Box<T>    { value: T }
//   export interface Pair<A,B> { first: A; second: B }
// -----------------------------------------------------------------------
let genericsTests =
    testList "generics.d.ts" [
        let result = createTestReader "generics" |> runReader
        testCase "'Box' has exactly 1 type parameter" <| fun _ ->
            let iface = result |> findInterface "Box"
            "" |> Expect.hasLength iface.TypeParameters 1

        testCase "'Box' type parameter is named 'T'" <| fun _ ->
            let iface = result |> findInterface "Box"
            let (_, tp) = iface.TypeParameters.[0]
            "Box type param should be T" |> Expect.equal tp.Name "T"

        testCase "'Box' has a member named 'value'" <| fun _ ->
            let iface = result |> findInterface "Box"
            "Box should have property 'value'"
            |> Expect.exists iface.Members (function TsMember.Property p -> p.Name = "value" | _ -> false)

        testCase "'Pair' has exactly 2 type parameters" <| fun _ ->
            let iface = result |> findInterface "Pair"
            "" |> Expect.hasLength iface.TypeParameters 2

        testCase "'Pair' type parameter names are 'A' and 'B'" <| fun _ ->
            let iface = result |> findInterface "Pair"
            let names = iface.TypeParameters |> List.map (fun (_, tp) -> tp.Name)
            "" |> Expect.containsAll names ["A"; "B"]

        testCase "'Pair' has members named 'first' and 'second'" <| fun _ ->
            let iface = result |> findInterface "Pair"
            let names =
                iface.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names ["first"; "second"]
    ]

// -----------------------------------------------------------------------
// Fixture: function.d.ts
//   export function greet(name: string, greeting?: string): string
//   export function collect(...items: number[]): number[]
//   export function ping(): void
// -----------------------------------------------------------------------
let functionTests =
    testList "function.d.ts" [
        let fd = createTestReader "function" |> runReader
        testCase "'greet' has exactly 2 parameters" <| fun _ ->
            let fd = fd |> findFunction "greet"
            "" |> Expect.hasLength fd.ValueOrHead.Parameters 2

        testCase "'greet' first parameter 'name' is not optional" <| fun _ ->
            let fd = fd |> findFunction "greet"
            let p = fd.ValueOrHead.Parameters |> List.find (fun p -> p.Name = "name")
            "name param should not be optional" |> Expect.isFalse p.IsOptional

        testCase "'greet' first parameter 'name' is not a spread" <| fun _ ->
            let fd = fd |> findFunction "greet"
            let p = fd.ValueOrHead.Parameters |> List.find (fun p -> p.Name = "name")
            "name param should not be spread" |> Expect.isFalse p.IsSpread

        testCase "'greet' second parameter 'greeting' is optional" <| fun _ ->
            let fd = fd |> findFunction "greet"
            let p = fd.ValueOrHead.Parameters |> List.find (fun p -> p.Name = "greeting")
            "greeting param should be optional" |> Expect.isTrue p.IsOptional

        testCase "'collect' has exactly 1 parameter" <| fun _ ->
            let fd = fd |> findFunction "collect"
            "" |> Expect.hasLength fd.ValueOrHead.Parameters 1

        testCase "'collect' parameter 'items' is a spread (rest) param" <| fun _ ->
            let fd = fd |> findFunction "collect"
            let p = fd.ValueOrHead.Parameters.[0]
            "items should be spread" |> Expect.isTrue p.IsSpread

        testCase "'collect' spread parameter 'items' is not optional" <| fun _ ->
            // Rest parameters cannot be optional in TypeScript
            let fd = fd |> findFunction "collect"
            let p = fd.ValueOrHead.Parameters.[0]
            "items should not be optional" |> Expect.isFalse p.IsOptional

        testCase "'ping' has 0 parameters" <| fun _ ->
            let fd = fd |> findFunction "ping"
            "" |> Expect.hasLength fd.ValueOrHead.Parameters 0
    ]

let indexAccessTests =
    testList "index-access.d.ts" [
        let result = createTestReader "index-access" |> runReader
        testList "Known access is resolved to underlying types" [
            let accessedInterface = findInterface "TestedInterface" result
            testCase "stringAccess resolves to string" <| fun _ ->
                let accessedProperty = findProperty "stringAccess" accessedInterface.Members
                "Type should be string"
                |> Expect.equal accessedProperty.Type TypeKindPrimitive.String.TypeKey
            testCase "numberAccess resolves to number" <| fun _ ->
                let accessedProperty = findProperty "numberAccess" accessedInterface.Members
                "Type should be number"
                |> Expect.equal accessedProperty.Type TypeKindPrimitive.Number.TypeKey
            testCase "booleanAccess resolves to boolean" <| fun _ ->
                let accessedProperty = findProperty "booleanAccess" accessedInterface.Members
                let typ = findType accessedProperty.Type result
                $"Type should be boolean instead of: {findType accessedProperty.Type result}"
                |> Expect.isTrue (match typ with TsType.Primitive TypeKindPrimitive.Boolean -> true | _ -> false)
        ]
        let accessedInterface = findInterface "GenericTest" result
        let property = findProperty "accessedProperty" accessedInterface.Members
        let typ = result |> findType property.Type
        testList "Unresolved access preserves type access semantics" [
            testCase "Underlying type preserves semantics" <| fun _ ->
                "Type should be IndexAccess"
                |> Expect.isTrue (match typ with TsType.IndexedAccess _ -> true | _ -> false)
            let indexAccessType =
                match typ with
                | TsType.IndexedAccess indexAccess -> indexAccess
                | _ -> failwith "Type is not IndexAccessType"
            testCase "Index is type parameter" <| fun _ ->
                let typ = findType indexAccessType.Index result
                "Type should be a type parameter"
                |> Expect.isTrue (
                    match typ with
                    | TsType.TypeReference ({ ResolvedType = Some typKey } | { Type = typKey }) ->
                        findType typKey result
                        |> _.IsTypeParameter
                    | TsType.TypeParameter _ -> true
                    | _ -> false
                    )
            testCase "Object accessed is 'AccessedInterface'" <| fun _ ->
                let nestedType =
                    findType indexAccessType.Object result
                "Type should be an interface by the name of AccessedInterface"
                |> Expect.isTrue (
                    match nestedType with
                    | TsType.TypeReference { ResolvedType = Some typKey } ->
                        match findType typKey result with
                        | TsType.Interface { Name = "AccessedInterface" } -> true
                        | _ -> false
                    | TsType.Interface { Name = "AccessedInterface" } -> true
                    | _ -> false
                    )
        ]

    ]

// -----------------------------------------------------------------------
// Fixture: union.d.ts
//   export type StringOrNumber = string | number
//   export type ThreeWay       = string | number | null
// -----------------------------------------------------------------------

let unionTests =
    testList "union.d.ts" [
        let result = createTestReader "union" |> runReader

        testCase "'StringOrNumber' alias is present in result" <| fun _ ->
            "StringOrNumber should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.TypeAlias a) when a.Name = "StringOrNumber" -> true
                | _ -> false)

        let strOrNum = result |> findAlias "StringOrNumber"

        testCase "'StringOrNumber' Type resolves to TsType.Union" <| fun _ ->
            let node = result |> findType strOrNum.Type
            "StringOrNumber.Type should be Union" |> Expect.isTrue (match node with TsType.Union _ -> true | _ -> false)

        testCase "'StringOrNumber' union has exactly 2 members" <| fun _ ->
            match result |> findType strOrNum.Type with
            | TsType.Union u -> "" |> Expect.hasLength u.Types 2
            | _ -> failwith "Expected Union"

        testCase "'StringOrNumber' union members include string and number TypeKeys" <| fun _ ->
            match result |> findType strOrNum.Type with
            | TsType.Union u ->
                "" |> Expect.containsAll u.Types [ TypeKindPrimitive.String.TypeKey; TypeKindPrimitive.Number.TypeKey ]
            | _ -> failwith "Expected Union"

        testCase "'ThreeWay' union has exactly 3 members" <| fun _ ->
            let threeWay = result |> findAlias "ThreeWay"
            match result |> findType threeWay.Type with
            | TsType.Union u -> "" |> Expect.hasLength u.Types 3
            | _ -> failwith "Expected Union"
    ]

// -----------------------------------------------------------------------
// Fixture: intersection.d.ts
//   export interface Named { name: string; }
//   export interface Aged  { age: number; }
//   export type Person = Named & Aged
// -----------------------------------------------------------------------

let intersectionTests =
    testList "intersection.d.ts" [
        let result = createTestReader "intersection" |> runReader

        testCase "'Named' and 'Aged' interfaces are both in result" <| fun _ ->
            let names =
                result.ExportedDeclarations
                |> Seq.choose (function
                    | KeyValue(_, TsExportDeclaration.Interface iface) -> Some iface.Name
                    | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll names [ "Named"; "Aged" ]

        let person = result |> findAlias "Person"

        testCase "'Person' alias Type resolves to TsType.Intersection" <| fun _ ->
            let node = result |> findType person.Type
            "Person.Type should be Intersection" |> Expect.isTrue (match node with TsType.Intersection _ -> true | _ -> false)

        testCase "'Person' intersection has exactly 2 entries" <| fun _ ->
            match result |> findType person.Type with
            | TsType.Intersection i -> "" |> Expect.hasLength i.Types 2
            | _ -> failwith "Expected Intersection"
    ]

// -----------------------------------------------------------------------
// Fixture: tuple.d.ts
//   export type Coord        = [number, number]
//   export type Triple       = [string, number, boolean]
//   export type WithOptional = [string, number?]
//   export type WithRest     = [string, ...number[]]
// -----------------------------------------------------------------------

let tupleTests =
    testList "tuple.d.ts" [
        let result = createTestReader "tuple" |> runReader

        testCase "'Coord' alias Type resolves to TsType.Tuple" <| fun _ ->
            let alias = result |> findAlias "Coord"
            let node = result |> findType alias.Type
            "Coord.Type should be Tuple" |> Expect.isTrue (match node with TsType.Tuple _ -> true | _ -> false)

        testCase "'Coord' has FixedLength 2 and MinRequired 2" <| fun _ ->
            let alias = result |> findAlias "Coord"
            match result |> findType alias.Type with
            | TsType.Tuple t ->
                "" |> Expect.equal t.FixedLength 2
                "" |> Expect.equal t.MinRequired 2
            | _ -> failwith "Expected Tuple"

        testCase "'Triple' has FixedLength 3 and MinRequired 3" <| fun _ ->
            let alias = result |> findAlias "Triple"
            match result |> findType alias.Type with
            | TsType.Tuple t ->
                "" |> Expect.equal t.FixedLength 3
                "" |> Expect.equal t.MinRequired 3
            | _ -> failwith "Expected Tuple"

        testCase "'WithOptional' has FixedLength 2 and MinRequired 1" <| fun _ ->
            let alias = result |> findAlias "WithOptional"
            match result |> findType alias.Type with
            | TsType.Tuple t ->
                "" |> Expect.equal t.FixedLength 2
                "" |> Expect.equal t.MinRequired 1
            | _ -> failwith "Expected Tuple"

        testCase "'WithRest' has FixedLength 1 and MinRequired 1" <| fun _ ->
            let alias = result |> findAlias "WithRest"
            match result |> findType alias.Type with
            | TsType.Tuple t ->
                "" |> Expect.equal t.FixedLength 1
                "" |> Expect.equal t.MinRequired 1
            | _ -> failwith "Expected Tuple"

        testCase "'WithRest' has 2 Types entries and last entry IsRest" <| fun _ ->
            let alias = result |> findAlias "WithRest"
            match result |> findType alias.Type with
            | TsType.Tuple t ->
                "" |> Expect.hasLength t.Types 2
                "" |> Expect.isTrue (List.last t.Types).IsRest
            | _ -> failwith "Expected Tuple"
    ]

// -----------------------------------------------------------------------
// Fixture: class.d.ts
//   export class Animal {
//       constructor(name: string, age: number);
//       speak(): string;
//       readonly name: string;
//   }
// -----------------------------------------------------------------------

let classTests =
    testList "class.d.ts" [
        let result = createTestReader "class" |> runReader

        testCase "'Animal' class is present in result" <| fun _ ->
            "Animal should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Class c) when c.Name = "Animal" -> true
                | _ -> false)

        let cls = result |> findClass "Animal"

        testCase "'Animal' has exactly 1 constructor" <| fun _ ->
            "" |> Expect.hasLength cls.Constructors 1

        testCase "'Animal' constructor has 2 parameters" <| fun _ ->
            "" |> Expect.hasLength cls.Constructors.[0].Parameters 2

        testCase "'Animal' constructor parameter names are 'name' and 'age'" <| fun _ ->
            let names = cls.Constructors.[0].Parameters |> List.map _.Name
            "" |> Expect.containsAll names [ "name"; "age" ]

        testCase "'Animal' has a 'speak' method member" <| fun _ ->
            "Animal should have 'speak' method"
            |> Expect.exists cls.Members (function
                | TsMember.Method m when m.ValueOrHead.Name = "speak" -> true
                | _ -> false)

        testCase "'Animal' has a 'name' property member" <| fun _ ->
            "Animal should have 'name' property"
            |> Expect.exists cls.Members (function
                | TsMember.Property p when p.Name = "name" -> true
                | _ -> false)

        testCase "'Animal.name' property is ReadOnly" <| fun _ ->
            let prop = findProperty "name" cls.Members
            "" |> Expect.equal prop.Accessor TsAccessor.ReadOnly
    ]

// -----------------------------------------------------------------------
// Fixture: variable.d.ts
//   export declare const VERSION: string;
//   export declare let   count:   number;
//   export declare const PI:      number;
// -----------------------------------------------------------------------

let variableTests =
    testList "variable.d.ts" [
        let result = createTestReader "variable" |> runReader

        testCase "'VERSION' variable is present in result" <| fun _ ->
            "VERSION should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Variable v) when v.Name = "VERSION" -> true
                | _ -> false)

        testCase "'VERSION' Type resolves to string primitive" <| fun _ ->
            let v = result |> findVariable "VERSION"
            "" |> Expect.equal v.Type TypeKindPrimitive.String.TypeKey

        testCase "'count' variable is present in result" <| fun _ ->
            "count should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Variable v) when v.Name = "count" -> true
                | _ -> false)

        testCase "'count' Type resolves to number primitive" <| fun _ ->
            let v = result |> findVariable "count"
            "" |> Expect.equal v.Type TypeKindPrimitive.Number.TypeKey

        testCase "'PI' Type resolves to number primitive" <| fun _ ->
            let v = result |> findVariable "PI"
            "" |> Expect.equal v.Type TypeKindPrimitive.Number.TypeKey
    ]

let utilityTests =
    testList "utility.d.ts" [
        let result = createTestReader "utility-types" |> runReader
        let utilityInterface = findInterface "IUtilityInterface" result
        testCase "'UtilityInterface' is present and correctly resolved" <| fun _ ->
            let stringProperty = findProperty "stringProperty" utilityInterface.Members
            let numberProperty = findProperty "numberProperty" utilityInterface.Members
            let booleanProperty = findProperty "booleanProperty" utilityInterface.Members
            "All properties are required"
            |> Expect.isFalse (stringProperty.IsOptional || numberProperty.IsOptional || booleanProperty.IsOptional)
        testCase "All utility definitions are present" <| fun _ ->
            let utilityTypes =
                result.ExportedDeclarations
                |> Seq.choose (_.Value >> function
                    | TsExportDeclaration.TypeAlias a -> Some a
                    | _ -> None
                    )
            (Seq.length utilityTypes >= 5, "At least 5 Aliases are present")
            ||> Expect.isTrue
        testCase "Partial utility definition is correct" <| fun _ ->
            let partialUtility = result |> findAlias "PartialInterface"
            let partialUtilityType =
                result
                |> findType partialUtility.Type
                |> function
                    | TsType.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsType.TypeLiteral { Members = members } ->
                        members
                    | _ -> failwith "Expected TypeLiteral"
            "Partial is a resolved type reference to a type literal with 3 members"
            |> Expect.hasLength partialUtilityType 3
            "Partial is a resolved type reference to a type literal with 3 optional members"
            |> Expect.all partialUtilityType (function
                | TsMember.Property { IsOptional = true } -> true
                | _ -> false
                )
        testCase "Omit utility definition is correct" <| fun _ ->
            let omitUtility = result |> findAlias "OmitInterface"
            let omitUtilityMembers =
                result
                |> findType omitUtility.Type
                |> function
                    | TsType.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsType.TypeLiteral { Members = members } ->
                        members
                    | _ -> failwith "Expected TypeLiteral"
            "Omit is resolved to type reference -> type literal -> 2 members"
            |> Expect.hasLength omitUtilityMembers 2
            "Omit does not contain 'omitted' member stringProperty"
            |> Expect.isFalse (omitUtilityMembers |> List.exists (function TsMember.Property { Name = "stringProperty" } -> true | _ -> false))
        testCase "Pick utility definition is correct" <| fun _ ->
            let pickUtility = result |> findAlias "PickInterface"
            let pickUtilityMembers =
                result
                |> findType pickUtility.Type
                |> function
                    | TsType.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsType.TypeLiteral { Members = members } ->
                        members
                    | _ -> failwith "Expected TypeLiteral"
            "Pick is resolved to type reference -> type literal -> 1 member"
            |> Expect.hasLength pickUtilityMembers 1
            "Pick is resolved to string property"
            |> Expect.isTrue (
                match pickUtilityMembers with
                | TsMember.Property { Name = "stringProperty"; IsOptional = false } :: _ -> true
                | _ -> false
                )
        testCase "Nested utility types flatten at first type reference" <| fun _ ->
            let nestedUtility = result |> findAlias "DerivedInterface"
            let nestedUtilityMembers =
                result
                |> findType nestedUtility.Type
                |> function
                    | TsType.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsType.TypeLiteral { Members = members } ->
                        members
                    | _ -> failwith "Expected TypeLiteral"
            "Utility type is resolved to type literal with 2 members"
            |> Expect.hasLength nestedUtilityMembers 2
            "Utility type is resolved to optional members"
            |> Expect.all nestedUtilityMembers (function
                | TsMember.Property { IsOptional = true } -> true
                | _ -> false
                )
        testList "Generic utility types preserves semantic information" [
            let genericUtility = result |> findAlias "GenericUtility"
            let genericUtilityType =
                result
                |> findType genericUtility.Type
                |> function
                    | TsType.TypeReference typeRef -> typeRef
                    | _ -> failwith "Expected TypeReference"
            testCase "Utility type preserves semantic information of type" <| fun _ ->
                let typ = findExportByKey genericUtilityType.Type result
                "Expect to keep semantic info of: GenericUtility<T extends keyof IUtilityInterface> = Pick<PartialInterface, T>" |> Expect.isTrue (
                    match typ with
                    | TsExportDeclaration.TypeAlias { Name = "Pick" } -> true
                    | _ -> false
                    )
            testCase "Utility type arguments preserve semantic information" <| fun _ ->
                "" |> Expect.hasLength genericUtilityType.TypeArguments 2
                #nowarn 25
                let [ taOne; taTwo ] = genericUtilityType.TypeArguments
                #warnon 25
                "First type argument should be a type alias"
                |> Expect.isTrue (
                    result
                    |> findExportByKey taOne
                    |> _.IsTypeAlias
                    )
                "Second type argument should be a type parameter"
                |> Expect.isTrue (
                    result
                    |> findType taTwo
                    |> _.IsTypeParameter
                    )
        ]
    ]

let typeOperatorTests =
    testList "type-operators.d.ts" [
        let result = createTestReader "type-operators" |> runReader
        testCase "'InterfaceKey' is present" <| fun _ ->
            try
            findAlias "InterfaceKey" result
            |> Ok
            with _ -> Error ()
            |> Expect.isOk
            <| "Expected to find 'InterfaceKey' in result"
        testCase "'InterfaceKey' resolves to union of string literals" <| fun _ ->
            let alias = findAlias "InterfaceKey" result
            let aliasType = result |> findType alias.Type
            "Expected 'InterfaceKey' to be a Alias -> Union"
            |> Expect.isTrue (match aliasType with TsType.Union _ -> true | _ -> false)
            let types =
                match aliasType with
                | TsType.Union (TsTypeUnion types) -> types
                | _ -> failwith "Expected Union"
            "Expected union to contain 3 members"
            |> Expect.hasLength types 3
            "Expected union to contain literals only"
            |> Expect.all types (findType >> (fun fn -> fn result) >> (fun n -> match n with TsType.Literal _ -> true | _ -> false))
            "Expected union to contain strings 'stringProperty'; 'numberProperty'; 'booleanProperty'"
            |> Expect.containsAll (
                List.map (fun typ ->
                    match findType typ result with
                    | TsType.Literal (TsLiteral.String s) -> s
                    | _ -> failwith "Expected Literal"
                    ) types
                ) [ "stringProperty"; "numberProperty"; "booleanProperty" ]
    ]

// -----------------------------------------------------------------------
// Fixture: enum-nonsequential.d.ts
//   export enum HttpStatus   { Ok = 200, NotFound = 404, ServerError = 500 }
//   export enum Temperature  { AbsoluteZero = -273, Freezing = 0, Boiling = 100 }
// -----------------------------------------------------------------------

let enumNonSequentialTests =
    testList "enum-nonsequential.d.ts" [
        let result = createTestReader "enum-nonsequential" |> runReader

        testCase "'HttpStatus' enum is present in result" <| fun _ ->
            "HttpStatus should be in result"
            |> Expect.exists result.Types (function
                | KeyValue(_, TsType.Enum e) when e.Name = "HttpStatus" -> true
                | _ -> false)

        testCase "'HttpStatus' has exactly 3 members" <| fun _ ->
            "" |> Expect.hasLength (result |> findEnum "HttpStatus").Members 3

        testCase "'HttpStatus.Ok' has value 200" <| fun _ ->
            let m = result |> findEnum "HttpStatus" |> _.Members |> List.find (fun c -> c.Name = "Ok")
            "" |> Expect.equal m.Value (TsLiteral.Int 200)

        testCase "'HttpStatus.NotFound' has value 404" <| fun _ ->
            let m = result |> findEnum "HttpStatus" |> _.Members |> List.find (fun c -> c.Name = "NotFound")
            "" |> Expect.equal m.Value (TsLiteral.Int 404)

        testCase "'HttpStatus.ServerError' has value 500" <| fun _ ->
            let m = result |> findEnum "HttpStatus" |> _.Members |> List.find (fun c -> c.Name = "ServerError")
            "" |> Expect.equal m.Value (TsLiteral.Int 500)

        testCase "'Temperature.AbsoluteZero' has negative value -273" <| fun _ ->
            let m = result |> findEnum "Temperature" |> _.Members |> List.find (fun c -> c.Name = "AbsoluteZero")
            "" |> Expect.equal m.Value (TsLiteral.Int -273)

        testCase "'Temperature.Freezing' = 0 and 'Boiling' = 100" <| fun _ ->
            let e = result |> findEnum "Temperature"
            let fz = e.Members |> List.find (fun c -> c.Name = "Freezing")
            let bo = e.Members |> List.find (fun c -> c.Name = "Boiling")
            "" |> Expect.equal fz.Value (TsLiteral.Int 0)
            "" |> Expect.equal bo.Value (TsLiteral.Int 100)
    ]

// -----------------------------------------------------------------------
// Fixture: enum-string.d.ts
//   export enum Color { Red = "RED", Green = "GREEN", Blue = "BLUE" }
// -----------------------------------------------------------------------

let enumStringTests =
    testList "enum-string.d.ts" [
        let result = createTestReader "enum-string" |> runReader

        testCase "'Color' enum is present in result" <| fun _ ->
            "Color should be in result"
            |> Expect.exists result.Types (function
                | KeyValue(_, TsType.Enum e) when e.Name = "Color" -> true
                | _ -> false)

        testCase "'Color' has exactly 3 members" <| fun _ ->
            "" |> Expect.hasLength (result |> findEnum "Color").Members 3

        testCase "'Color' all members have TsLiteral.String values" <| fun _ ->
            "All Color members should have String values"
            |> Expect.all (result |> findEnum "Color").Members (fun c ->
                match c.Value with TsLiteral.String _ -> true | _ -> false)

        testCase "'Color.Red' has value \"RED\"" <| fun _ ->
            let m = result |> findEnum "Color" |> _.Members |> List.find (fun c -> c.Name = "Red")
            "" |> Expect.equal m.Value (TsLiteral.String "RED")

        testCase "'Color.Green' has value \"GREEN\"" <| fun _ ->
            let m = result |> findEnum "Color" |> _.Members |> List.find (fun c -> c.Name = "Green")
            "" |> Expect.equal m.Value (TsLiteral.String "GREEN")

        testCase "'Color.Blue' has value \"BLUE\"" <| fun _ ->
            let m = result |> findEnum "Color" |> _.Members |> List.find (fun c -> c.Name = "Blue")
            "" |> Expect.equal m.Value (TsLiteral.String "BLUE")
    ]

// -----------------------------------------------------------------------
// Fixture: accessors.d.ts
//   export interface WithAccessors {
//     get readable(): string;  set writable(value: string)
//     get readWrite(): number; set readWrite(value: number)
//   }
// -----------------------------------------------------------------------

let accessorTests =
    testList "accessors.d.ts" [
        let result = createTestReader "accessors" |> runReader
        let iface = result |> findInterface "WithAccessors"

        testCase "'WithAccessors' has a GetAccessor named 'readable'" <| fun _ ->
            "Should have GetAccessor 'readable'"
            |> Expect.exists iface.Members (function
                | TsMember.GetAccessor g when g.Name = "readable" -> true
                | _ -> false)

        testCase "'WithAccessors' has a SetAccessor named 'writable'" <| fun _ ->
            "Should have SetAccessor 'writable'"
            |> Expect.exists iface.Members (function
                | TsMember.SetAccessor s when s.Name = "writable" -> true
                | _ -> false)

        testCase "'readable' GetAccessor Type resolves to string primitive" <| fun _ ->
            let g =
                iface.Members
                |> List.pick (function TsMember.GetAccessor g when g.Name = "readable" -> Some g | _ -> None)
            "" |> Expect.equal g.Type TypeKindPrimitive.String.TypeKey

        testCase "'readWrite' has both a GetAccessor and a SetAccessor entry" <| fun _ ->
            let gets = iface.Members |> List.filter (function TsMember.GetAccessor g when g.Name = "readWrite" -> true | _ -> false)
            let sets = iface.Members |> List.filter (function TsMember.SetAccessor s when s.Name = "readWrite" -> true | _ -> false)
            "" |> Expect.hasLength gets 1
            "" |> Expect.hasLength sets 1
    ]

// -----------------------------------------------------------------------
// Fixture: namespace.d.ts
//   declare namespace Geometry { interface Point { x; y }; interface Circle { center; radius } }
// -----------------------------------------------------------------------

let namespaceTests =
    testList "namespace.d.ts" [
        let result = createTestReader "namespace" |> runReader

        testCase "'Geometry' module is present in result" <| fun _ ->
            "Geometry should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Module m) when m.Name = "Geometry" -> true
                | _ -> false)

        let m = result |> findModule "Geometry"

        testCase "'Geometry' IsNamespace = true" <| fun _ ->
            "" |> Expect.isTrue m.IsNamespace

        testCase "'Geometry' Types list has exactly 2 entries" <| fun _ ->
            "" |> Expect.hasLength m.Exports 2

        testCase "all types in 'Geometry' resolve to interfaces" <| fun _ ->
            "All Geometry.Types should resolve to Interface nodes"
            |> Expect.all m.Exports (function TsExportDeclaration.Interface _ -> true | _ -> false)

        testCase "'Point' and 'Circle' interfaces are in the result" <| fun _ ->
            let names =
                result.ExportedDeclarations
                |> Seq.choose (function KeyValue(_, TsExportDeclaration.Interface i) -> Some i.Name | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll names [ "Point"; "Circle" ]

        testCase "'Point' has members named 'x' and 'y'" <| fun _ ->
            let names =
                (result |> findInterface "Point").Members
                |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names [ "x"; "y" ]
    ]

// -----------------------------------------------------------------------
// Fixture: conditional.d.ts
//   export type IsString<T> = T extends string ? true : false
// -----------------------------------------------------------------------

let conditionalTests =
    testList "conditional.d.ts" [
        let result = createTestReader "conditional" |> runReader
        let alias = result |> findAlias "IsString"

        testCase "'IsString' alias Type resolves to TsType.Conditional" <| fun _ ->
            "IsString.Type should be Conditional"
            |> Expect.isTrue (match result |> findType alias.Type with TsType.Conditional _ -> true | _ -> false)

        testCase "'IsString' Conditional.Extends resolves to string primitive" <| fun _ ->
            match result |> findType alias.Type with
            | TsType.Conditional c -> "" |> Expect.equal c.Extends TypeKindPrimitive.String.TypeKey
            | _ -> failwith "Expected Conditional"

        testCase "'IsString' Conditional.True resolves to TsLiteral.Bool true" <| fun _ ->
            match result |> findType alias.Type with
            | TsType.Conditional c ->
                match result |> findType c.True with
                | TsType.Literal (TsLiteral.Bool b) -> "" |> Expect.isTrue b
                | _ -> failwith "Expected Bool literal"
            | _ -> failwith "Expected Conditional"

        testCase "'IsString' Conditional.False resolves to TsLiteral.Bool false" <| fun _ ->
            match result |> findType alias.Type with
            | TsType.Conditional c ->
                match result |> findType c.False with
                | TsType.Literal (TsLiteral.Bool b) -> "" |> Expect.isFalse b
                | _ -> failwith "Expected Bool literal"
            | _ -> failwith "Expected Conditional"
    ]

// -----------------------------------------------------------------------
// Fixture: template-literal.d.ts
//   export type EventName    = `on${string}`           → texts=["on",""]    types=[string]
//   export type KeyValuePair = `${string}:${string}`   → texts=["",":",""]  types=[string,string]
// -----------------------------------------------------------------------

let templateLiteralTests =
    testList "template-literal.d.ts" [
        let result = createTestReader "template-literal" |> runReader
        let eventAlias = result |> findAlias "EventName"

        testCase "'EventName' alias Type resolves to TsType.TemplateLiteral" <| fun _ ->
            "EventName.Type should be TemplateLiteral"
            |> Expect.isTrue (match result |> findType eventAlias.Type with TsType.TemplateLiteral _ -> true | _ -> false)

        testCase "'EventName' has 2 Texts entries" <| fun _ ->
            match result |> findType eventAlias.Type with
            | TsType.TemplateLiteral t -> "" |> Expect.hasLength t.Texts 2
            | _ -> failwith "Expected TemplateLiteral"

        testCase "'EventName' first text segment is \"on\"" <| fun _ ->
            match result |> findType eventAlias.Type with
            | TsType.TemplateLiteral t -> "" |> Expect.equal (List.head t.Texts) "on"
            | _ -> failwith "Expected TemplateLiteral"

        testCase "'EventName' has 1 interpolated Type entry" <| fun _ ->
            match result |> findType eventAlias.Type with
            | TsType.TemplateLiteral t -> "" |> Expect.hasLength t.Types 1
            | _ -> failwith "Expected TemplateLiteral"

        testCase "'EventName' interpolated type is string primitive" <| fun _ ->
            match result |> findType eventAlias.Type with
            | TsType.TemplateLiteral t -> "" |> Expect.equal t.Types.[0] TypeKindPrimitive.String.TypeKey
            | _ -> failwith "Expected TemplateLiteral"

        testCase "'KeyValuePair' has 2 interpolated Type entries" <| fun _ ->
            let kv = result |> findAlias "KeyValuePair"
            match result |> findType kv.Type with
            | TsType.TemplateLiteral t -> "" |> Expect.hasLength t.Types 2
            | _ -> failwith "Expected TemplateLiteral"

        testCase "'KeyValuePair' separator text segment is \":\"" <| fun _ ->
            let kv = result |> findAlias "KeyValuePair"
            match result |> findType kv.Type with
            | TsType.TemplateLiteral t ->
                // texts = [""; ":"; ""] — index 1 is the separator between the two interpolations
                "" |> Expect.equal t.Texts.[1] ":"
            | _ -> failwith "Expected TemplateLiteral"
    ]

// -----------------------------------------------------------------------
// Fixture: ref-enum.d.ts
//   export declare enum RefEnum { Number5 = 5, Number9 = 9, Number2 = 2 }
//   export type RefEnumUnion = RefEnum.Number5 | RefEnum.Number9
//
// Tests that enum members used as types in a union resolve to TsType.EnumCase
// entries, and that the enum itself is emitted with all 3 cases in order.
// -----------------------------------------------------------------------

let refEnumTests =
    testList "ref-enum.d.ts" [
        let result = createTestReader "ref-enum" |> runReader

        testCase "'RefEnum' is present in result" <| fun _ ->
            "'RefEnum' should appear in result"
            |> Expect.exists result.Types (function
                | KeyValue(_, TsType.Enum e) when e.Name = "RefEnum" -> true
                | _ -> false)

        let enum = result |> findEnum "RefEnum"

        testCase "'RefEnum' has exactly 3 members" <| fun _ ->
            "" |> Expect.hasLength enum.Members 3

        testCase "'RefEnum' member names are Number5, Number9, Number2" <| fun _ ->
            let names = enum.Members |> List.map _.Name
            "" |> Expect.containsAll names [ "Number5"; "Number9"; "Number2" ]

        testCase "'RefEnum.Number5' has value Int 5" <| fun _ ->
            let m = enum.Members |> List.find (fun c -> c.Name = "Number5")
            "" |> Expect.equal m.Value (TsLiteral.Int 5)

        testCase "'RefEnum.Number9' has value Int 9" <| fun _ ->
            let m = enum.Members |> List.find (fun c -> c.Name = "Number9")
            "" |> Expect.equal m.Value (TsLiteral.Int 9)

        testCase "'RefEnum.Number2' has value Int 2" <| fun _ ->
            let m = enum.Members |> List.find (fun c -> c.Name = "Number2")
            "" |> Expect.equal m.Value (TsLiteral.Int 2)

        testCase "'RefEnumUnion' alias is present" <| fun _ ->
            "'RefEnumUnion' should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.TypeAlias a) when a.Name = "RefEnumUnion" -> true
                | _ -> false)

        let union = result |> findAlias "RefEnumUnion"

        testCase "'RefEnumUnion' type resolves to TsType.Union" <| fun _ ->
            "RefEnumUnion.Type should be Union"
            |> Expect.isTrue (match result |> findType union.Type with TsType.Union _ -> true | _ -> false)

        testCase "'RefEnumUnion' union has exactly 2 members" <| fun _ ->
            match result |> findType union.Type with
            | TsType.Union u -> "" |> Expect.hasLength u.Types 2
            | _ -> failwith "Expected Union"

        testCase "'RefEnumUnion' both union members resolve to TsType.EnumCase" <| fun _ ->
            match result |> findType union.Type with
            | TsType.Union u ->
                "Both union members should be EnumCase nodes"
                |> Expect.all u.Types (fun key ->
                    match result |> findType key with
                    | TsType.EnumCase _ -> true
                    | _ -> false)
            | _ -> failwith "Expected Union"

        testCase "'RefEnumUnion' contains enum cases Number5 and Number9" <| fun _ ->
            match result |> findType union.Type with
            | TsType.Union u ->
                let caseNames =
                    u.Types
                    |> List.choose (fun key ->
                        match result |> findType key with
                        | TsType.EnumCase c -> Some c.Name
                        | _ -> None)
                "" |> Expect.containsAll caseNames [ "Number5"; "Number9" ]
            | _ -> failwith "Expected Union"

        testCase "'RefEnumUnion' case values are Int 5 and Int 9" <| fun _ ->
            match result |> findType union.Type with
            | TsType.Union u ->
                let values =
                    u.Types
                    |> List.choose (fun key ->
                        match result |> findType key with
                        | TsType.EnumCase c -> Some c.Value
                        | _ -> None)
                "" |> Expect.containsAll values [ TsLiteral.Int 5; TsLiteral.Int 9 ]
            | _ -> failwith "Expected Union"
    ]

// -----------------------------------------------------------------------
// Fixture: function-ref.d.ts
//   export declare function getPerson(name: string): object;
//   export interface Api { person: typeof getPerson }
//
// Tests that `typeof X` (TypeQuery) on a function reference resolves to a
// TypeLiteral containing a CallSignature matching the function's signature.
// -----------------------------------------------------------------------

let functionRefTests =
    testList "function-ref.d.ts" [
        let result = createTestReader "function-ref" |> runReader

        testCase "'getPerson' function is present" <| fun _ ->
            "'getPerson' should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Function f) when f.ValueOrHead.Name = "getPerson" -> true
                | _ -> false)

        testCase "'getPerson' has 1 parameter" <| fun _ ->
            let fn = result |> findFunction "getPerson"
            "" |> Expect.hasLength fn.ValueOrHead.Parameters 1

        testCase "'getPerson' parameter is named 'name'" <| fun _ ->
            let fn = result |> findFunction "getPerson"
            "" |> Expect.equal fn.ValueOrHead.Parameters.[0].Name "name"

        testCase "'getPerson' parameter 'name' is type string" <| fun _ ->
            let fn = result |> findFunction "getPerson"
            "" |> Expect.equal fn.ValueOrHead.Parameters.[0].Type TypeKindPrimitive.String.TypeKey

        testCase "'Api' interface is present" <| fun _ ->
            "'Api' should be in result"
            |> Expect.exists result.ExportedDeclarations (function
                | KeyValue(_, TsExportDeclaration.Interface i) when i.Name = "Api" -> true
                | _ -> false)

        let api = result |> findInterface "Api"

        testCase "'Api' has a 'person' property member" <| fun _ ->
            "Api should have member 'person'"
            |> Expect.exists api.Members (function
                | TsMember.Property p when p.Name = "person" -> true
                | _ -> false)

        testCase "'Api.person' type resolves to TypeLiteral (typeof getPerson)" <| fun _ ->
            let prop = api.Members |> findProperty "person"
            let typ = result |> findType prop.Type
            "typeof getPerson should be a TypeLiteral (anonymous function type)"
            |> Expect.isTrue (match typ with TsType.TypeLiteral _ -> true | _ -> false)

        testCase "'Api.person' TypeLiteral has exactly 1 CallSignature" <| fun _ ->
            let prop = api.Members |> findProperty "person"
            match result |> findType prop.Type with
            | TsType.TypeLiteral { Members = members } ->
                let callSigs = members |> List.filter (function TsMember.CallSignature _ -> true | _ -> false)
                "" |> Expect.hasLength callSigs 1
            | _ -> failwith "Expected TypeLiteral"

        testCase "'Api.person' call signature has 1 parameter named 'name'" <| fun _ ->
            let prop = api.Members |> findProperty "person"
            match result |> findType prop.Type with
            | TsType.TypeLiteral { Members = members } ->
                match members |> List.pick (function TsMember.CallSignature c -> Some c | _ -> None) with
                | TsOverloadableConstruct.NoOverloads cs ->
                    "" |> Expect.hasLength cs.Parameters 1
                    "" |> Expect.equal cs.Parameters.[0].Name "name"
                | _ -> failwith "Expected NoOverloads call signature"
            | _ -> failwith "Expected TypeLiteral"

        testCase "'Api.person' call signature return type is NonPrimitive (object)" <| fun _ ->
            let prop = api.Members |> findProperty "person"
            match result |> findType prop.Type with
            | TsType.TypeLiteral { Members = members } ->
                match members |> List.pick (function TsMember.CallSignature c -> Some c | _ -> None) with
                | TsOverloadableConstruct.NoOverloads cs ->
                    let returnType = result |> findType cs.Type
                    "Return type of getPerson is 'object' → NonPrimitive"
                    |> Expect.equal returnType (TsType.Primitive TypeKindPrimitive.NonPrimitive)
                | _ -> failwith "Expected NoOverloads"
            | _ -> failwith "Expected TypeLiteral"
    ]

let multiFileTests =
    testList "multi-file/vectors.d.ts" [
        let result = createTestReader "multi-file/vectors" |> runReader
        testCase "vectors.d.ts types are present" <| fun _ ->
            let vector2d = result |> tryFindInterface "Vector2D"
            let vector3d = result |> tryFindInterface "Vector3D"
            "Vector2D should be in result"
            |> Expect.isSome vector2d
            "Vector3D should be in result"
            |> Expect.isSome vector3d
        testCase "shapes.d.ts imported types are present" <| fun _ ->
            let point2d = result |> tryFindInterface "Point2D"
            "Point2D should be in result"
            |> Expect.isSome point2d
        testCase "non-imported shapes.d.ts types are absent" <| fun _ ->
            "Scalar should be unreachable"
            |> Expect.throws (fun () ->
                result
                |> findAlias "Scalar"
                |> ignore
                )
        testCase "Vector2D extends Point2D" <| fun _ ->
            let heritage =
                result
                |> findInterface "Vector2D"
                |> _.Heritage.Extends
            "Only one type should be in heritage"
            |> Expect.hasLength heritage 1
            let resolvedTypeKey =
                heritage
                |> List.head
                |> function
                    | { ResolvedType = Some typeKey }
                    | { Type = typeKey } -> typeKey
            match
                result
                |> findType resolvedTypeKey
            with
            | TsType.Interface { Name = name } ->
                "Interface should be Point2D"
                |> Expect.equal name "Point2D"
            | _ -> failwith "Expected Interface"
        testCase "Vector3D extends Vector2D" <| fun _ ->
            let heritage =
                result
                |> findInterface "Vector3D"
                |> _.Heritage.Extends
            "Only one type should be in heritage"
            |> Expect.hasLength heritage 1
            let resolvedTypeKey =
                heritage
                |> List.head
                |> function
                    | { ResolvedType = Some typeKey }
                    | { Type = typeKey } -> typeKey
            match
                result
                |> findType resolvedTypeKey
            with
            | TsType.Interface { Name = name } ->
                "Interface should be Vector2D"
                |> Expect.equal name "Vector2D"
            | _ -> failwith "Expected Interface"
    ]

// -----------------------------------------------------------------------
// Source / import name resolution
// -----------------------------------------------------------------------

// Fixture: packages/ui-kit/index.d.ts  (has package.json with name "ui-kit")
//   Button   : interface
//   ButtonSize : type alias
//
// Expected: Source = Some "ui-kit" for all exported types
let packageSourceTests =
    testList "source: package.json name" [
        let result = createSubdirTestReader "packages/ui-kit/index" |> runReader
        testCase "Button interface Source = 'ui-kit'" <| fun _ ->
            let iface = result |> findInterface "Button"
            "Source should be Some 'ui-kit'"
            |> Expect.equal iface.Source (Some "ui-kit")
        testCase "ButtonSize alias Source = 'ui-kit'" <| fun _ ->
            let alias = result |> findAlias "ButtonSize"
            "Source should be Some 'ui-kit'"
            |> Expect.equal alias.Source (Some "ui-kit")
        testCase "createButton function Source = 'ui-kit'" <| fun _ ->
            let fn = result |> findFunction "createButton"
            "Source should be Some 'ui-kit'"
            |> Expect.equal fn.ValueOrHead.Source (Some "ui-kit")
        testCase "DEFAULT_SIZE variable Source = 'ui-kit'" <| fun _ ->
            let v = result |> findVariable "DEFAULT_SIZE"
            "Source should be Some 'ui-kit'"
            |> Expect.equal v.Source (Some "ui-kit")
        testCase "ButtonVariant enum Source = 'ui-kit'" <| fun _ ->
            let e = result |> findEnum "ButtonVariant"
            "Source should be Some 'ui-kit'"
            |> Expect.equal e.Source (Some "ui-kit")
        testCase "ButtonGroup class Source = 'ui-kit'" <| fun _ ->
            let c = result |> findClass "ButtonGroup"
            "Source should be Some 'ui-kit'"
            |> Expect.equal c.Source (Some "ui-kit")
    ]

// Fixture: multi-file/vectors.d.ts imports { Point2D } from "./shapes"
//
// shapes.d.ts is resolved via the import specifier "./shapes", so types
// originating from shapes.d.ts should have Source = Some "./shapes".
// vectors.d.ts is the entry file — its Source comes from the fallback
// (no import points *to* it), so it should still be Some _.
let importSourceTests =
    testList "source: import specifier" [
        let result = createTestReader "multi-file/vectors" |> runReader
        testCase "Point2D Source = './shapes' (imported module specifier)" <| fun _ ->
            let iface = result |> findInterface "Point2D"
            "Source should be Some './shapes'"
            |> Expect.equal iface.Source (Some "./shapes")
        testCase "Vector2D Source is Some (entry file fallback)" <| fun _ ->
            let iface = result |> findInterface "Vector2D"
            "Source should be Some _"
            |> Expect.isSome iface.Source
        testCase "Vector3D Source matches Vector2D Source (same file)" <| fun _ ->
            let v2 = result |> findInterface "Vector2D"
            let v3 = result |> findInterface "Vector3D"
            "Types from the same file should share Source"
            |> Expect.equal v3.Source v2.Source
    ]

// Fixture: basic.d.ts — single file, no package.json with a name in its tree
//
// The fallback path resolver produces a Source from the directory path.
// We just verify it's non-None (the exact value depends on the filesystem).
let fallbackSourceTests =
    testList "source: fallback path" [
        let result = createTestReader "basic" |> runReader
        testCase "BaseInterface Source is Some" <| fun _ ->
            let iface = result |> findInterface "BaseInterface"
            "Source should be Some _"
            |> Expect.isSome iface.Source
        testCase "BaseObject alias Source matches BaseInterface Source (same file)" <| fun _ ->
            let iface = result |> findInterface "BaseInterface"
            let alias = result |> findAlias "BaseObject"
            "Types in the same file should share Source"
            |> Expect.equal alias.Source iface.Source
    ]

// Fixture: namespace.d.ts — namespace Geometry { Point, Circle }
//
// TsModule.Source should be set; nested interfaces share the module's Source.
let namespaceSourceTests =
    testList "source: namespace" [
        let result = createTestReader "namespace" |> runReader
        testCase "Geometry module Source is Some" <| fun _ ->
            let m = result |> findModule "Geometry"
            "Source should be Some _"
            |> Expect.isSome m.Source
        testCase "nested interfaces share the namespace Source" <| fun _ ->
            let m = result |> findModule "Geometry"
            let point = result |> findInterface "Point"
            "Nested interface should have the same Source as its namespace"
            |> Expect.equal point.Source m.Source
    ]

let remappedSourceTests =
    testList "source: remapped barrel file sources" [
        let result = createSubdirTestReader "packages/solid-js/types/index" |> runReader
        // todo - why don't these type files pick up the package.json source?
        ptestCase "Top level interface source = 'solid-js'" <| fun _ ->
            let iface = result |> findInterface "InterfaceProp"
            "Source should be some"
            |> Expect.isSome iface.Source
            "Interface should have source 'solid-js'"
            |> Expect.equal iface.Source (Some "solid-js")
        // todo - why don't these type files pick up the package.json source?
        ptestCase "Exports JSX namespace with source 'solid-js'" <| fun _ ->
            let m = result |> findModule "JSX"
            "Source should be some"
            |> Expect.isSome m.Source
            "JSX namespace defined in another file should have source 'solid-js'"
            |> Expect.equal m.Source (Some "solid-js")
    ]

// Fixture: import-package.d.ts — imports Button from ./packages/ui-kit/index
//
// The import specifier is "./packages/ui-kit/index", so types from
// ui-kit/index.d.ts get Source = Some "./packages/ui-kit/index" (not the
// package.json name — import specifier takes priority for resolved files).
// AppButton (declared locally) gets a different Source via fallback.
let importPackageSourceTests =
    testList "source: cross-file package import" [
        let result = createTestReader "import-package" |> runReader
        testCase "AppButton Source is Some (local file)" <| fun _ ->
            let iface = result |> findInterface "AppButton"
            "Source should be Some _"
            |> Expect.isSome iface.Source
        testCase "Button Source = './packages/ui-kit/index' (import specifier)" <| fun _ ->
            let iface = result |> findInterface "Button"
            "Source should be Some './packages/ui-kit/index'"
            |> Expect.equal iface.Source (Some "./packages/ui-kit/index")
        testCase "AppButton and Button have different Sources (different files)" <| fun _ ->
            let app = result |> findInterface "AppButton"
            let btn = result |> findInterface "Button"
            "Sources from different files should differ"
            |> Expect.notEqual app.Source btn.Source
        testCase "ButtonLabel alias has same Source as AppButton (same file)" <| fun _ ->
            let app = result |> findInterface "AppButton"
            let label = result |> findAlias "ButtonLabel"
            "Types from same file should share Source"
            |> Expect.equal label.Source app.Source
    ]

// Fixture: multi-file/vectors.d.ts + shapes.d.ts — both as explicit entry points
//
// Import-map priority holds even when both files are entry points:
// shapes.d.ts types still get Source = "./shapes".
let multiFileSourceTests =
    testList "source: multi-file entry" [
        let result = createMultiFileTestReader [| "multi-file/vectors"; "multi-file/shapes" |] |> runReader
        testCase "Point2D Source = './shapes' (imported file)" <| fun _ ->
            let iface = result |> findInterface "Point2D"
            "Source should be Some './shapes'"
            |> Expect.equal iface.Source (Some "./shapes")
        testCase "Vector3D Source matches Vector2D Source (same entry file)" <| fun _ ->
            let v2 = result |> findInterface "Vector2D"
            let v3 = result |> findInterface "Vector3D"
            "Types from the same entry file should share Source"
            |> Expect.equal v3.Source v2.Source
        testCase "Vector2D Source differs from Point2D Source" <| fun _ ->
            let v2 = result |> findInterface "Vector2D"
            let p2d = result |> findInterface "Point2D"
            "Entry file Source should differ from imported file Source"
            |> Expect.notEqual v2.Source p2d.Source
    ]

// -----------------------------------------------------------------------
// Source: cross-package (two sibling packages)
// -----------------------------------------------------------------------

// packages/data-layer/ imports from ../validators/index
// data-layer entry types → fallback → "@app/data-layer" (own package.json)
// validators types → import specifier → "../validators/index"
let crossPackageSourceTests =
    testList "source: cross-package siblings" [
        let result = createSubdirTestReader "packages/data-layer/index" |> runReader
        testCase "DataModel Source = '@app/data-layer' (entry file package.json)" <| fun _ ->
            let iface = result |> findInterface "DataModel"
            "Source should be '@app/data-layer'"
            |> Expect.equal iface.Source (Some "@app/data-layer")
        testCase "ModelId alias Source = '@app/data-layer'" <| fun _ ->
            let alias = result |> findAlias "ModelId"
            "Source should be '@app/data-layer'"
            |> Expect.equal alias.Source (Some "@app/data-layer")
        testCase "Validator Source = '../validators/index' (import specifier)" <| fun _ ->
            let iface = result |> findInterface "Validator"
            "Source should be '../validators/index'"
            |> Expect.equal iface.Source (Some "../validators/index")
        testCase "DataModel and Validator have different Sources" <| fun _ ->
            let dm = result |> findInterface "DataModel"
            let v = result |> findInterface "Validator"
            "Types from different packages should have different Sources"
            |> Expect.notEqual dm.Source v.Source
    ]

// Fixture: packages/validators/index.d.ts — loaded directly as entry
//
// Fallback walks up to its own package.json → "@app/validators".
let validatorsDirectSourceTests =
    testList "source: validators package direct" [
        let result = createSubdirTestReader "packages/validators/index" |> runReader
        testCase "Validator Source = '@app/validators' (own package.json)" <| fun _ ->
            let iface = result |> findInterface "Validator"
            "Source should be '@app/validators'"
            |> Expect.equal iface.Source (Some "@app/validators")
        testCase "ValidationResult Source = '@app/validators'" <| fun _ ->
            let alias = result |> findAlias "ValidationResult"
            "Source should be '@app/validators'"
            |> Expect.equal alias.Source (Some "@app/validators")
    ]

// -----------------------------------------------------------------------
// Source: nested sub-package (package.json at two levels)
// -----------------------------------------------------------------------

// framework/index.d.ts imports from ./plugins/index.
// The plugins/ subfolder has its OWN package.json ("@app/framework-plugins").
// Entry types → fallback → "@app/framework" (outer package.json).
// Plugin types → import specifier → "./plugins/index".
let nestedSubPackageSourceTests =
    testList "source: nested sub-package via import" [
        let result = createSubdirTestReader "packages/framework/index" |> runReader
        testCase "Framework Source = '@app/framework' (entry package.json)" <| fun _ ->
            let iface = result |> findInterface "Framework"
            "Source should be '@app/framework'"
            |> Expect.equal iface.Source (Some "@app/framework")
        testCase "FrameworkConfig Source = '@app/framework'" <| fun _ ->
            let iface = result |> findInterface "FrameworkConfig"
            "Source should match Framework (same file)"
            |> Expect.equal iface.Source (Some "@app/framework")
        testCase "Plugin Source = './plugins/index' (import specifier)" <| fun _ ->
            let iface = result |> findInterface "Plugin"
            "Source should be './plugins/index'"
            |> Expect.equal iface.Source (Some "./plugins/index")
        testCase "Framework and Plugin have different Sources" <| fun _ ->
            let fw = result |> findInterface "Framework"
            let pl = result |> findInterface "Plugin"
            "Parent and sub-package types should have different Sources"
            |> Expect.notEqual fw.Source pl.Source
    ]

// Fixture: packages/framework/plugins/index.d.ts — loaded directly as entry.
// Fallback finds the sub-package's OWN package.json → "@app/framework-plugins",
// NOT the parent's "@app/framework".
let subPackageDirectSourceTests =
    testList "source: sub-package loaded directly" [
        let result = createSubdirTestReader "packages/framework/plugins/index" |> runReader
        testCase "Plugin Source = '@app/framework-plugins' (own package.json)" <| fun _ ->
            let iface = result |> findInterface "Plugin"
            "Source should be '@app/framework-plugins'"
            |> Expect.equal iface.Source (Some "@app/framework-plugins")
        testCase "PluginFactory Source = '@app/framework-plugins'" <| fun _ ->
            let alias = result |> findAlias "PluginFactory"
            "Source should be '@app/framework-plugins'"
            |> Expect.equal alias.Source (Some "@app/framework-plugins")
    ]

// -----------------------------------------------------------------------
// Source: deep transitive imports (3-level chain)
// -----------------------------------------------------------------------

// packages/app/index.d.ts → views/dashboard.d.ts → components/widget.d.ts
// AppConfig  → "my-app" (entry, fallback to package.json)
// Dashboard  → "./views/dashboard" (1st-level import specifier)
// Widget     → "../components/widget" (2nd-level, from dashboard's import)
let deepTransitiveSourceTests =
    testList "source: deep transitive imports" [
        let result = createSubdirTestReader "packages/app/index" |> runReader
        testCase "AppConfig Source = 'my-app' (entry package.json)" <| fun _ ->
            let iface = result |> findInterface "AppConfig"
            "Source should be 'my-app'"
            |> Expect.equal iface.Source (Some "my-app")
        testCase "Dashboard Source = './views/dashboard' (1st-level import)" <| fun _ ->
            let iface = result |> findInterface "Dashboard"
            "Source should be './views/dashboard'"
            |> Expect.equal iface.Source (Some "./views/dashboard")
        testCase "Widget Source = '../components/widget' (2nd-level transitive)" <| fun _ ->
            let iface = result |> findInterface "Widget"
            "Source should be '../components/widget'"
            |> Expect.equal iface.Source (Some "../components/widget")
        testCase "all three files have distinct Sources" <| fun _ ->
            let app = result |> findInterface "AppConfig"
            let dash = result |> findInterface "Dashboard"
            let widget = result |> findInterface "Widget"
            let sources = set [ app.Source; dash.Source; widget.Source ]
            "Three files should produce three distinct Sources"
            |> Expect.hasLength sources 3
    ]

// Fixture: packages/app/views/dashboard.d.ts — loaded as entry directly.
// Own fallback walks up to packages/app/package.json → "my-app".
// Its own import of ../components/widget is still resolved via specifier.
let middleOfChainDirectSourceTests =
    testList "source: middle of chain loaded directly" [
        let result = createSubdirTestReader "packages/app/views/dashboard" |> runReader
        testCase "Dashboard Source = 'my-app' (walks up to app package.json)" <| fun _ ->
            let iface = result |> findInterface "Dashboard"
            "Source should be 'my-app'"
            |> Expect.equal iface.Source (Some "my-app")
        testCase "Widget Source = '../components/widget' (import specifier)" <| fun _ ->
            let iface = result |> findInterface "Widget"
            "Source should be '../components/widget'"
            |> Expect.equal iface.Source (Some "../components/widget")
    ]

// Fixture: packages/app/components/widget.d.ts — loaded as entry directly.
// No imports; fallback walks up to packages/app/package.json → "my-app".
let leafDirectSourceTests =
    testList "source: leaf file loaded directly" [
        let result = createSubdirTestReader "packages/app/components/widget" |> runReader
        testCase "Widget Source = 'my-app' (walks up to app package.json)" <| fun _ ->
            let iface = result |> findInterface "Widget"
            "Source should be 'my-app'"
            |> Expect.equal iface.Source (Some "my-app")
        testCase "WidgetSize Source = 'my-app'" <| fun _ ->
            let alias = result |> findAlias "WidgetSize"
            "Source should match Widget (same file)"
            |> Expect.equal alias.Source (Some "my-app")
    ]

// -----------------------------------------------------------------------
// Suite
// -----------------------------------------------------------------------

let tests =
    testList "Xantham.Fable Tests" [
        basicTests
        mergeTests
        heritageTests
        multipleExtendsTests
        memberTests
        overloadTests
        modifierTests
        enumTests
        enumNonSequentialTests
        enumStringTests
        genericsTests
        functionTests
        indexAccessTests
        unionTests
        intersectionTests
        tupleTests
        classTests
        variableTests
        utilityTests
        typeOperatorTests
        accessorTests
        namespaceTests
        conditionalTests
        templateLiteralTests
        refEnumTests
        functionRefTests
        multiFileTests
        packageSourceTests
        importSourceTests
        fallbackSourceTests
        namespaceSourceTests
        importPackageSourceTests
        multiFileSourceTests
        remappedSourceTests
        crossPackageSourceTests
        validatorsDirectSourceTests
        nestedSubPackageSourceTests
        subPackageDirectSourceTests
        deepTransitiveSourceTests
        middleOfChainDirectSourceTests
        leafDirectSourceTests
        aliasTests
    ]

Mocha.runTests tests |> ignore
