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
        | KeyValue(key, TsAstNode.Interface iface) when key > TypeKey 0 && iface.Name = name -> Some iface
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
        | KeyValue(k, TsAstNode.FunctionDeclaration fd) when k > TypeKey 0 && fd.ValueOrHead.Name = name -> Some fd
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

/// Find the first type alias whose Name matches; throws on miss.
let findAlias name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.pick (function
        | KeyValue(_, TsAstNode.Alias a) when a.Name = name -> Some a
        | _ -> None)

/// Find the first class whose Name matches; throws on miss.
let findClass name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.pick (function
        | KeyValue(_, TsAstNode.Class c) when c.Name = name -> Some c
        | _ -> None)

/// Find the first variable whose Name matches; throws on miss.
let findVariable name (result: Dictionary<TypeKey, TsAstNode>) =
    result |> Seq.pick (function
        | KeyValue(_, TsAstNode.Variable v) when v.Name = name -> Some v
        | _ -> None)

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
                result
                |> Seq.choose (function
                    | KeyValue(_, TsAstNode.FunctionDeclaration fd) -> Some fd.ValueOrHead.Name
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
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Enum e) -> e.Name = "Direction"
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
// Fixture: union.d.ts
//   export type StringOrNumber = string | number
//   export type ThreeWay       = string | number | null
// -----------------------------------------------------------------------

let unionTests =
    testList "union.d.ts" [
        let result = createTestReader "union" |> runReader

        testCase "'StringOrNumber' alias is present in result" <| fun _ ->
            "StringOrNumber should be in result"
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Alias a) when a.Name = "StringOrNumber" -> true
                | _ -> false)

        let strOrNum = result |> findAlias "StringOrNumber"

        testCase "'StringOrNumber' Type resolves to TsAstNode.Union" <| fun _ ->
            let node = result |> findType strOrNum.Type
            "StringOrNumber.Type should be Union" |> Expect.isTrue node.IsUnion

        testCase "'StringOrNumber' union has exactly 2 members" <| fun _ ->
            match result |> findType strOrNum.Type with
            | TsAstNode.Union u -> "" |> Expect.hasLength u.Types 2
            | _ -> failwith "Expected Union"

        testCase "'StringOrNumber' union members include string and number TypeKeys" <| fun _ ->
            match result |> findType strOrNum.Type with
            | TsAstNode.Union u ->
                "" |> Expect.containsAll u.Types [ TypeKindPrimitive.String.TypeKey; TypeKindPrimitive.Number.TypeKey ]
            | _ -> failwith "Expected Union"

        testCase "'ThreeWay' union has exactly 3 members" <| fun _ ->
            let threeWay = result |> findAlias "ThreeWay"
            match result |> findType threeWay.Type with
            | TsAstNode.Union u -> "" |> Expect.hasLength u.Types 3
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
                result
                |> Seq.choose (function
                    | KeyValue(_, TsAstNode.Interface iface) -> Some iface.Name
                    | _ -> None)
                |> Seq.toArray
            "" |> Expect.containsAll names [ "Named"; "Aged" ]

        let person = result |> findAlias "Person"

        testCase "'Person' alias Type resolves to TsAstNode.Intersection" <| fun _ ->
            let node = result |> findType person.Type
            "Person.Type should be Intersection" |> Expect.isTrue node.IsIntersection

        testCase "'Person' intersection has exactly 2 entries" <| fun _ ->
            match result |> findType person.Type with
            | TsAstNode.Intersection i -> "" |> Expect.hasLength i.Types 2
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

        testCase "'Coord' alias Type resolves to TsAstNode.Tuple" <| fun _ ->
            let alias = result |> findAlias "Coord"
            let node = result |> findType alias.Type
            "Coord.Type should be Tuple" |> Expect.isTrue node.IsTuple

        testCase "'Coord' has FixedLength 2 and MinRequired 2" <| fun _ ->
            let alias = result |> findAlias "Coord"
            match result |> findType alias.Type with
            | TsAstNode.Tuple t ->
                "" |> Expect.equal t.FixedLength 2
                "" |> Expect.equal t.MinRequired 2
            | _ -> failwith "Expected Tuple"

        testCase "'Triple' has FixedLength 3 and MinRequired 3" <| fun _ ->
            let alias = result |> findAlias "Triple"
            match result |> findType alias.Type with
            | TsAstNode.Tuple t ->
                "" |> Expect.equal t.FixedLength 3
                "" |> Expect.equal t.MinRequired 3
            | _ -> failwith "Expected Tuple"

        testCase "'WithOptional' has FixedLength 2 and MinRequired 1" <| fun _ ->
            let alias = result |> findAlias "WithOptional"
            match result |> findType alias.Type with
            | TsAstNode.Tuple t ->
                "" |> Expect.equal t.FixedLength 2
                "" |> Expect.equal t.MinRequired 1
            | _ -> failwith "Expected Tuple"

        testCase "'WithRest' has FixedLength 1 and MinRequired 1" <| fun _ ->
            let alias = result |> findAlias "WithRest"
            match result |> findType alias.Type with
            | TsAstNode.Tuple t ->
                "" |> Expect.equal t.FixedLength 1
                "" |> Expect.equal t.MinRequired 1
            | _ -> failwith "Expected Tuple"

        testCase "'WithRest' has 2 Types entries and last entry IsRest" <| fun _ ->
            let alias = result |> findAlias "WithRest"
            match result |> findType alias.Type with
            | TsAstNode.Tuple t ->
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
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Class c) when c.Name = "Animal" -> true
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
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Variable v) when v.Name = "VERSION" -> true
                | _ -> false)

        testCase "'VERSION' Type resolves to string primitive" <| fun _ ->
            let v = result |> findVariable "VERSION"
            "" |> Expect.equal v.Type TypeKindPrimitive.String.TypeKey

        testCase "'count' variable is present in result" <| fun _ ->
            "count should be in result"
            |> Expect.exists result (function
                | KeyValue(_, TsAstNode.Variable v) when v.Name = "count" -> true
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
                result
                |> Seq.choose (_.Value  >> function
                    | TsAstNode.Alias a -> Some a
                    | _ -> None
                    )
            "5 Aliases are present"
            |> Expect.hasLength utilityTypes 5
        testCase "Partial utility definition is correct" <| fun _ ->
            let partialUtility = result |> findAlias "PartialInterface"
            let partialUtilityType =
                result
                |> findType partialUtility.Type
                |> function
                    | TsAstNode.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsAstNode.TypeLiteral { Members = members } ->
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
                    | TsAstNode.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsAstNode.TypeLiteral { Members = members } ->
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
                    | TsAstNode.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsAstNode.TypeLiteral { Members = members } ->
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
                    | TsAstNode.TypeReference { ResolvedType = Some typeKey } ->
                        findType typeKey result
                    | _ -> failwith "Expected TypeReference"
                |> function
                    | TsAstNode.TypeLiteral { Members = members } ->
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
                    | TsAstNode.TypeReference typeRef -> typeRef
                    | _ -> failwith "Expected TypeReference"
            // Type checker actually still resolves the type down to a type literal
            // with an index signature on any object.
            ptestCase "Utility type is unresolved type reference" <| fun _ ->
                "" |> Expect.isTrue genericUtilityType.ResolvedType.IsNone
            // Semantic/syntactic information is lost.
            // We again receive a type literal with an index signature on any object.
            // likely because we use type checker immediately on type references?
            testCase "Utility type preserves semantic information" <| fun _ ->
                let typ = findType genericUtilityType.Type result
                "Expect to keep semantic info of: GenericUtility<T extends keyof IUtilityInterface> = Pick<PartialInterface, T>" |> Expect.equal (typ.ToString()) ""
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
            |> Expect.isTrue aliasType.IsUnion
            let types =
                match aliasType with
                | TsAstNode.Union (TsTypeUnion types) -> types
                | _ -> failwith "Expected Union"
            "Expected union to contain 3 members"
            |> Expect.hasLength types 3
            "Expected union to contain literals only"
            |> Expect.all types (findType >> (fun fn -> fn result) >> _.IsLiteral)
            "Expected union to contain strings 'stringProperty'; 'numberProperty'; 'booleanProperty'"
            |> Expect.containsAll (
                List.map (fun typ ->
                    match findType typ result with
                    | TsAstNode.Literal (TsLiteral.String s) -> s
                    | _ -> failwith "Expected Literal"
                    ) types
                ) [ "stringProperty"; "numberProperty"; "booleanProperty" ]
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
        unionTests
        intersectionTests
        tupleTests
        classTests
        variableTests
        utilityTests
        typeOperatorTests
    ]

Mocha.runTests tests |> ignore
