module Main

open System.Collections.Generic
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Node.Api
open Fable.Mocha
open Fable.Core.Testing

// -----------------------------------------------------------------------
// Infrastructure
// -----------------------------------------------------------------------

let createTestReader (fileName: string) =
    let filePath = path.join(__SOURCE_DIRECTORY__, $"/TypeFiles/{fileName}.d.ts")
    let reader = TypeScriptReader.create filePath
    reader

let runReader (reader: TypeScriptReader) =
    let _exports =
        Internal.initialise reader
        |> Internal.getAndPrepareExports
    Internal.runReader reader
    |> Internal.assembleResults
    |> Internal.exciseDuplicateKeys
    |> fun split ->
        let splitMap = Dictionary<TypeKey, TsAstNode * TsAstNode array>()
        let orderedDuplicates = split.DuplicateGroups |> Internal.sortResultGroups
        let mergedDuplicates =
            orderedDuplicates
            |> Internal.mergeOverloads
            |> Internal.mergeMembersIntoWinner
        mergedDuplicates
        |> Internal.filterConflictDuplicatesOnly
        |> Seq.iter (fun group ->
            splitMap.Add(group.Key, (group.Winner.Node, group.Losers |> Array.map _.Node)))
        Internal.resolveDuplicates (Seq.map Internal.prune mergedDuplicates)
        |> Seq.append split.NonDuplicates
        |> Seq.sortBy _.Key
        |> Seq.map (fun ir -> KeyValuePair(ir.Key, ir.Node))
        |> Dictionary

// -----------------------------------------------------------------------
// Lookup helpers
// None-returning variants prefixed 'try'; bare variants throw on miss.
// Never assert TypeKey identity values — those are runtime-generated IDs.
// -----------------------------------------------------------------------

/// Find the first interface in the result whose Name matches, or None.
let tryFindInterface name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.tryPick (function
        | KeyValue(_, TsAstNode.Interface iface) when iface.Name = name -> Some iface
        | _ -> None)

/// Find the first interface in the result whose Name matches; throws on miss.
let findInterface name result =
    tryFindInterface name result
    |> Option.defaultWith (fun () -> failwithf "Interface '%s' not found in result" name)

/// Find the first enum whose Name matches; throws on miss.
let findEnum name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.pick (function
        | KeyValue(_, TsAstNode.Enum e) when e.Name = name -> Some e
        | _ -> None)

/// Find the first FunctionDeclaration whose head-variant Name matches; throws on miss.
/// Works for both NoOverloads (single) and Overloaded (merged) constructs.
let findFunction name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.pick (function
        | KeyValue(_, TsAstNode.FunctionDeclaration fd) when fd.ValueOrHead.Name = name -> Some fd
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

let findType (typeKey: TypeKey) (result: Dictionary<TypeKey, TsAstNode>) =
    result
    |> Seq.pick (fun (KeyValue(key, node)) -> if key = typeKey then Some node else None)

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
            |> Expect.exists result (fun kv ->
                match kv.Value with
                | TsAstNode.Interface { Members = _ :: _ } -> true
                | _ -> false)

        testCase "result contains exactly one interface and one alias" <| fun _ ->
            let count =
                result
                |> Seq.filter (fun kv -> kv.Value.IsInterface || kv.Value.IsAlias)
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
                result
                |> Seq.filter (function
                    | KeyValue(_, TsAstNode.Interface iface) -> iface.Name = "MergeProps"
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
            let count = result |> Seq.filter _.Value.IsInterface |> Seq.length
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
            result
            |> Seq.pick (function
                | KeyValue(_, TsAstNode.Interface { Heritage = { Extends = [ extend ] } }) ->
                    Some extend.Type
                | _ -> None)
            |> fun typeKey ->
                "Heritage TypeKey should resolve to a node in result"
                |> Expect.exists result (_.Key >> (=) typeKey)

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
                result
                |> Seq.choose (function
                    | KeyValue(_, TsAstNode.Interface { Heritage = { Extends = [ extend ] } }) ->
                        Some extend.Type
                    | _ -> None)
                |> Seq.toArray
            let resultKeys = result.Keys |> Seq.toArray
            "All heritage keys should be in result" |> Expect.containsAll resultKeys extendedKeys

        testCase "both extending interfaces are present in result" <| fun _ ->
            let extendingNames =
                result
                |> Seq.choose (function
                    | KeyValue(_, TsAstNode.Interface iface) when not (List.isEmpty iface.Heritage.Extends) ->
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

        // --- WithProperties: member count and kinds ---

        testCase "WithProperties has exactly 3 members" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithProperties"
            "" |> Expect.hasLength iface.Members 3

        testCase "WithProperties all members are TsMember.Property" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithProperties"
            "All members should be Property"
            |> Expect.all iface.Members (function TsMember.Property _ -> true | _ -> false)

        testCase "WithProperties has properties named 'name', 'age', 'active'" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithProperties"
            let names =
                iface.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            "" |> Expect.containsAll names ["name"; "age"; "active"]

        // --- WithProperties: IsOptional flag ---

        testCase "'name' is not optional (required property)" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "name"
            "name.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'age' is optional (declared with ?)" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "age"
            "age.IsOptional should be true" |> Expect.isTrue p.IsOptional

        // --- WithProperties: Accessor flag ---

        testCase "'name' has ReadWrite accessor (mutable)" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "name"
            "name.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'active' has ReadOnly accessor (declared readonly)" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let p = findInterface "WithProperties" result |> _.Members |> findProperty "active"
            "active.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly

        // --- WithMethods ---

        testCase "WithMethods has exactly 2 members" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithMethods"
            "" |> Expect.hasLength iface.Members 2

        testCase "WithMethods all members are TsMember.Method" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithMethods"
            "All members should be Method"
            |> Expect.all iface.Members (function TsMember.Method _ -> true | _ -> false)

        testCase "WithMethods has methods named 'greet' and 'add'" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithMethods"
            let names =
                iface.Members |> List.choose (function TsMember.Method m -> Some m.ValueOrHead.Name | _ -> None)
            "" |> Expect.containsAll names ["greet"; "add"]

        testCase "'greet' method has 1 parameter" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "greet"
            "" |> Expect.hasLength m.ValueOrHead.Parameters 1

        testCase "'greet' parameter is named 'message'" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "greet"
            let p = m.ValueOrHead.Parameters.[0]
            "greet param should be named message" |> Expect.equal p.Name "message"

        testCase "'add' method has 2 parameters" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "add"
            "" |> Expect.hasLength m.ValueOrHead.Parameters 2

        testCase "'add' parameter names are 'a' and 'b'" <| fun _ ->
            let result = createTestReader "members" |> runReader
            let m = findInterface "WithMethods" result |> _.Members |> findMethod "add"
            let names = m.ValueOrHead.Parameters |> List.map _.Name
            "" |> Expect.containsAll names ["a"; "b"]

        // --- WithSignatures ---

        testCase "WithSignatures has a TsMember.CallSignature" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithSignatures"
            "Should have a CallSignature"
            |> Expect.exists iface.Members (function TsMember.CallSignature _ -> true | _ -> false)

        testCase "WithSignatures has a TsMember.ConstructSignature" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithSignatures"
            "Should have a ConstructSignature"
            |> Expect.exists iface.Members (function TsMember.ConstructSignature _ -> true | _ -> false)

        testCase "WithSignatures has a TsMember.IndexSignature" <| fun _ ->
            let iface = createTestReader "members" |> runReader |> findInterface "WithSignatures"
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
        testCase "both 'process' and 'identity' are present in result" <| fun _ ->
            let result = createTestReader "overloads" |> runReader
            let names =
                result
                |> Seq.choose (function
                    | KeyValue(_, TsAstNode.FunctionDeclaration fd) -> Some fd.ValueOrHead.Name
                    | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll names ["process"; "identity"]

        testCase "'process' is emitted as TsOverloadableConstruct.Overloaded" <| fun _ ->
            // Two distinct declarations → pipeline must merge into Overloaded
            let fd = createTestReader "overloads" |> runReader |> findFunction "process"
            match fd with
            | Overloaded _ -> Expect.pass()
            | NoOverloads _ -> failwith "'process' should be Overloaded, not NoOverloads"

        testCase "'process' Overloaded set contains 2 variants" <| fun _ ->
            let fd = createTestReader "overloads" |> runReader |> findFunction "process"
            "process should have 2 overload variants" |> Expect.equal fd.Values.Length 2

        testCase "'identity' is emitted as TsOverloadableConstruct.NoOverloads" <| fun _ ->
            // Single declaration → no merging needed, must remain NoOverloads
            let fd = createTestReader "overloads" |> runReader |> findFunction "identity"
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
        testCase "Modifiers interface has exactly 4 properties" <| fun _ ->
            let iface = createTestReader "optional-readonly" |> runReader |> findInterface "Modifiers"
            "" |> Expect.hasLength iface.Members 4

        // IsOptional flag

        testCase "'required' IsOptional = false" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "required"
            "required.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'optional' IsOptional = true" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optional"
            "optional.IsOptional should be true" |> Expect.isTrue p.IsOptional

        testCase "'readonlyProp' IsOptional = false" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "readonlyProp"
            "readonlyProp.IsOptional should be false" |> Expect.isFalse p.IsOptional

        testCase "'optReadonly' IsOptional = true" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optReadonly"
            "optReadonly.IsOptional should be true" |> Expect.isTrue p.IsOptional

        // Accessor flag

        testCase "'required' Accessor = ReadWrite (mutable)" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "required"
            "required.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'optional' Accessor = ReadWrite (mutable)" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optional"
            "optional.Accessor should be ReadWrite" |> Expect.equal p.Accessor TsAccessor.ReadWrite

        testCase "'readonlyProp' Accessor = ReadOnly" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "readonlyProp"
            "readonlyProp.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly

        testCase "'optReadonly' Accessor = ReadOnly" <| fun _ ->
            let result = createTestReader "optional-readonly" |> runReader
            let p = findInterface "Modifiers" result |> _.Members |> findProperty "optReadonly"
            "optReadonly.Accessor should be ReadOnly" |> Expect.equal p.Accessor TsAccessor.ReadOnly
    ]

// -----------------------------------------------------------------------
// Fixture: enum.d.ts
//   export enum Direction { Up = 0, Down = 1, Left = 2, Right = 3 }
// -----------------------------------------------------------------------
let enumTests =
    testList "enum.d.ts" [
        testCase "'Direction' enum is present in result" <| fun _ ->
            let result = createTestReader "enum" |> runReader
            "Direction should be in result"
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Enum e) -> e.Name = "Direction"
                | _ -> false)

        testCase "'Direction' has exactly 4 members" <| fun _ ->
            let e = createTestReader "enum" |> runReader |> findEnum "Direction"
            "" |> Expect.hasLength e.Members 4

        testCase "'Direction' member names are Up, Down, Left, Right" <| fun _ ->
            let e = createTestReader "enum" |> runReader |> findEnum "Direction"
            "" |> Expect.containsAll (e.Members |> List.map _.Name) ["Up"; "Down"; "Left"; "Right"]

        testCase "'Up' has TsLiteral.Int 0" <| fun _ ->
            let e = createTestReader "enum" |> runReader |> findEnum "Direction"
            let up = e.Members |> List.find (fun c -> c.Name = "Up")
            "Up.Value should be Int 0" |> Expect.equal up.Value (TsLiteral.Int 0)

        testCase "'Down' has TsLiteral.Int 1" <| fun _ ->
            let e = createTestReader "enum" |> runReader |> findEnum "Direction"
            let down = e.Members |> List.find (fun c -> c.Name = "Down")
            "Down.Value should be Int 1" |> Expect.equal down.Value (TsLiteral.Int 1)

        testCase "'Right' has TsLiteral.Int 3" <| fun _ ->
            let e = createTestReader "enum" |> runReader |> findEnum "Direction"
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
        testCase "'Box' has exactly 1 type parameter" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Box"
            "" |> Expect.hasLength iface.TypeParameters 1

        testCase "'Box' type parameter is named 'T'" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Box"
            let (_, tp) = iface.TypeParameters.[0]
            "Box type param should be T" |> Expect.equal tp.Name "T"

        testCase "'Box' has a member named 'value'" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Box"
            "Box should have property 'value'"
            |> Expect.exists iface.Members (function TsMember.Property p -> p.Name = "value" | _ -> false)

        testCase "'Pair' has exactly 2 type parameters" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Pair"
            "" |> Expect.hasLength iface.TypeParameters 2

        testCase "'Pair' type parameter names are 'A' and 'B'" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Pair"
            let names = iface.TypeParameters |> List.map (fun (_, tp) -> tp.Name)
            "" |> Expect.containsAll names ["A"; "B"]

        testCase "'Pair' has members named 'first' and 'second'" <| fun _ ->
            let iface = createTestReader "generics" |> runReader |> findInterface "Pair"
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
                |> Expect.isTrue (match typ with TsAstNode.Primitive TypeKindPrimitive.Boolean -> true | _ -> false)
        ]
        let accessedInterface = findInterface "GenericTest" result
        let property = findProperty "accessedProperty" accessedInterface.Members
        let typ = result |> findType property.Type
        testList "Unresolved access preserves type access semantics" [
            testCase "Underlying type preserves semantics" <| fun _ ->
                "Type should be IndexAccess"
                |> Expect.isTrue typ.IsIndexAccessType
            let indexAccessType =
                match typ with
                | TsAstNode.IndexAccessType indexAccess -> indexAccess
                | _ -> failwith "Type is not IndexAccessType"
            testCase "Index is type parameter" <| fun _ ->
                let typ = findType indexAccessType.Index result
                "Type should be a type parameter"
                |> Expect.isTrue typ.IsTypeParameter
            testCase "Object accessed is 'AccessedInterface'" <| fun _ ->
                let nestedType =
                    findType indexAccessType.Object result 
                "Type should be an interface by the name of AccessedInterface"
                |> Expect.isTrue (
                    match nestedType with
                    | TsAstNode.Interface { Name = "AccessedInterface" } -> true
                    | _ -> false
                    )
        ]
            
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
        genericsTests
        functionTests
        indexAccessTests
    ]

Mocha.runTests tests |> ignore
