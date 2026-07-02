module EncoderMergeDedup

// ---------------------------------------------------------------------------
// PER-PASS ISOLATION COVERAGE — ENCODER IDENTITY / MERGE / DEDUP PIPELINE
//
// TARGET: the pure functions in src/Xantham.Fable/Read.fs (`Xantham.Fable.Main`
// nested `module Internal`) that form the encoder's IDENTITY layer — where
// duplicate declarations are deduped and a WINNER is selected. This is the root
// of the over-minting and lib.dom / workers "twin" selection behaviour and had
// ZERO direct coverage (only indirect e2e via real .d.ts fixtures).
//
// Everything here runs on CONSTRUCTED inputs — plain F# records/maps — so no
// live ts-morph program is needed. The few ts-morph `Ts.Symbol` values that
// `IdentityKey.Symbol` / `IdentityKey.AliasSymbol` carry are never dereferenced
// by the functions under test (only their IdentityKey case matters for the
// priority decision), so a minimal `createObj |> unbox` mock suffices.
//
// Functions covered:
//   Internal.identityPriority                       (winner-selection priority)
//   Internal.finaliseAssembly                       (group / distinct / sort)
//   Internal.trimTypeReferenceArrayTupleDuplicates  (TypeRef-vs-Array/Tuple dedup)
//   Internal.mergeTypes                             (Interface/TypeLiteral merge)
//   Internal.mergeExports                           (Interface merge + overload combine)
//   Internal.selectAndMergeWinnersInDuplicates      (promote List.head winner)
//
// Fable.Mocha Expect idiom (matches Program.fs / DispatchClassification.fs):
//   Expect.equal actual expected "message"      (message is the LAST arg)
//   Expect.isTrue value "message"
// ---------------------------------------------------------------------------

open Fable.Core.JsInterop
open TypeScript
open Xantham
open Xantham.Schema
open Xantham.Fable.Types.Tracer
open Xantham.Fable.Main
open Fable.Mocha

// ---------------------------------------------------------------------------
// Builders for the (constructed) inputs.
// ---------------------------------------------------------------------------

let private tk (i: int) : TypeKey = TypeKey.createWith i

// A ts-morph Symbol is never dereferenced by the functions under test (only the
// IdentityKey *case* drives priority), so a minimal mock carrying a name is fine.
let private mockSymbol (name: string) : Ts.Symbol =
    createObj [ "name" ==> name ] |> unbox

// IRResult is the assembly-stage record (Internal.IRResult).
let private irResult (identity: IdentityKey) (key: TypeKey) (node: 'T) : Internal.IRResult<'T> =
    { Identity = identity; Key = key; Node = node }

let private emptyInterface (name: string) (members: TsMember list) : TsInterface =
    {
        Source = None
        FullyQualifiedName = [ name ]
        Enumerable = false
        Name = name
        Members = members
        TypeParameters = []
        Documentation = []
        Heritage = { Extends = [] }
    }

let private prop (name: string) : TsMember =
    TsMember.Property {
        Name = name
        Type = tk -1
        IsStatic = false
        IsOptional = false
        IsPrivate = false
        Accessor = TsAccessor.ReadWrite
        Documentation = []
    }

let private emptyFunction (name: string) (paramType: int) : TsFunction =
    {
        Source = None
        FullyQualifiedName = [ name ]
        Documentation = []
        IsDeclared = true
        Name = name
        Type = tk -1
        Parameters = [
            { Name = "x"; IsOptional = false; IsSpread = false; Type = tk paramType; Documentation = [] }
        ]
        TypeParameters = []
        SignatureKey = tk 0
    }

let private typeRef (key: int) : TsType =
    TsType.TypeReference { Type = tk key; TypeArguments = []; ResolvedType = None }

let private arrayOf (key: int) : TsType =
    TsType.Array (typeRef key)

let private tupleOf (keys: int list) : TsType =
    TsType.Tuple {
        IsReadOnly = false
        FixedLength = List.length keys
        MinRequired = List.length keys
        Types = keys |> List.map (fun k -> TsTupleElement.Fixed { Type = tk k; IsOptional = false; IsRest = false })
    }

let private dup (identity: TsIdentityKey) (value: 'T) : DuplicateEncoding<'T> =
    { Identity = identity; Value = value }

let private emptyResult : EncodedResult =
    {
        ExportedDeclarations = Map.empty
        Types = Map.empty
        DuplicateExports = Map.empty
        DuplicateTypes = Map.empty
        TopLevelExports = []
        LibEsExports = []
        EntryExports = Map.empty
    }

// ---------------------------------------------------------------------------
// identityPriority — DeclarationPosition 0 < Symbol 1 < AliasSymbol 2 < Id 3.
// ---------------------------------------------------------------------------
let private identityPriorityTests =
    testList "identityPriority" [

        testCase "DeclarationPosition -> 0" <| fun _ ->
            Expect.equal
                (Internal.identityPriority (IdentityKey.DeclarationPosition("f", 0., 1.)))
                0
                "DeclarationPosition is highest priority"

        testCase "Symbol -> 1" <| fun _ ->
            Expect.equal
                (Internal.identityPriority (IdentityKey.Symbol (mockSymbol "S")))
                1
                "Symbol priority"

        testCase "AliasSymbol -> 2" <| fun _ ->
            Expect.equal
                (Internal.identityPriority (IdentityKey.AliasSymbol (mockSymbol "A")))
                2
                "AliasSymbol priority"

        testCase "Id -> 3" <| fun _ ->
            Expect.equal
                (Internal.identityPriority (IdentityKey.Id (tk 7)))
                3
                "Id is lowest priority"

        testCase "lower number wins the strict ordering" <| fun _ ->
            let decl = Internal.identityPriority (IdentityKey.DeclarationPosition("f", 0., 1.))
            let sym  = Internal.identityPriority (IdentityKey.Symbol (mockSymbol "S"))
            let ali  = Internal.identityPriority (IdentityKey.AliasSymbol (mockSymbol "A"))
            let id'  = Internal.identityPriority (IdentityKey.Id (tk 1))
            Expect.isTrue (decl < sym && sym < ali && ali < id') "DeclarationPosition < Symbol < AliasSymbol < Id"
    ]

// ---------------------------------------------------------------------------
// finaliseAssembly — group by Key, distinctBy Node, partition dupes vs non-dupes,
// sort each duplicate group by identityPriority so Results[0] is the WINNER.
// ---------------------------------------------------------------------------
let private finaliseAssemblyTests =
    testList "finaliseAssembly" [

        testCase "distinct keys stay non-duplicates" <| fun _ ->
            let results =
                [|
                    irResult (IdentityKey.Symbol (mockSymbol "A")) (tk 1) (typeRef 1)
                    irResult (IdentityKey.Symbol (mockSymbol "B")) (tk 2) (typeRef 2)
                |]
            let assembled = Internal.finaliseAssembly results
            Expect.equal assembled.DuplicateGroups.Length 0 "no duplicate groups"
            Expect.equal assembled.NonDuplicates.Length 2 "two non-duplicates"

        testCase "distinctBy Node collapses identical nodes for one key" <| fun _ ->
            // Same Key AND same Node from two identities -> distinctBy Node collapses
            // to a single entry, so it lands as a NON-duplicate (length 1 after distinct).
            let node = typeRef 1
            let results =
                [|
                    irResult (IdentityKey.Symbol (mockSymbol "A")) (tk 1) node
                    irResult (IdentityKey.DeclarationPosition("f", 0., 1.)) (tk 1) node
                |]
            let assembled = Internal.finaliseAssembly results
            Expect.equal assembled.DuplicateGroups.Length 0 "collapsed to non-duplicate"
            Expect.equal assembled.NonDuplicates.Length 1 "one non-duplicate after distinct"

        testCase "identical key + distinct nodes form a duplicate group" <| fun _ ->
            let results =
                [|
                    irResult (IdentityKey.Symbol (mockSymbol "A")) (tk 1) (typeRef 1)
                    irResult (IdentityKey.Symbol (mockSymbol "B")) (tk 1) (typeRef 2)
                |]
            let assembled = Internal.finaliseAssembly results
            Expect.equal assembled.DuplicateGroups.Length 1 "one duplicate group"
            Expect.equal assembled.NonDuplicates.Length 0 "no non-duplicates"
            Expect.equal assembled.DuplicateGroups.[0].Results.Length 2 "two results in the group"

        testCase "duplicate group sorted so highest-priority identity is Results[0] (winner)" <| fun _ ->
            // Feed in worst-priority-first to prove the SORT, not insertion order, wins.
            let results =
                [|
                    irResult (IdentityKey.Id (tk 9)) (tk 1) (typeRef 1)                          // priority 3
                    irResult (IdentityKey.AliasSymbol (mockSymbol "A")) (tk 1) (typeRef 2)        // priority 2
                    irResult (IdentityKey.Symbol (mockSymbol "S")) (tk 1) (typeRef 3)             // priority 1
                    irResult (IdentityKey.DeclarationPosition("f", 0., 1.)) (tk 1) (typeRef 4)    // priority 0  (winner)
                |]
            let assembled = Internal.finaliseAssembly results
            Expect.equal assembled.DuplicateGroups.Length 1 "one duplicate group"
            let winner = assembled.DuplicateGroups.[0].Results.[0]
            Expect.isTrue winner.Identity.IsDeclarationPosition "DeclarationPosition is the winner (head)"
            // and the rest are in ascending priority order
            let priorities = assembled.DuplicateGroups.[0].Results |> Array.map (fun r -> Internal.identityPriority r.Identity)
            Expect.equal priorities [| 0; 1; 2; 3 |] "sorted ascending by priority"
    ]

// ---------------------------------------------------------------------------
// trimTypeReferenceArrayTupleDuplicates — collapse a {TypeRef, Array|Tuple}
// duplicate PAIR down to just the Array/Tuple (moved into Types); leave other
// duplicate shapes untouched.
// ---------------------------------------------------------------------------
let private trimTests =
    testList "trimTypeReferenceArrayTupleDuplicates" [

        testCase "TypeRef + Array pair collapses to the Array in Types" <| fun _ ->
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 1, [ dup (Type (tk 1)) (typeRef 5); dup (Type (tk 1)) (arrayOf 5) ] ] }
            let trimmed = Internal.trimTypeReferenceArrayTupleDuplicates result
            Expect.isFalse (trimmed.DuplicateTypes.ContainsKey (tk 1)) "pair removed from DuplicateTypes"
            Expect.isTrue (trimmed.Types.[tk 1]).IsArray "collapsed to the Array side"

        testCase "Array + TypeRef pair (reversed order) also collapses to the Array" <| fun _ ->
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 2, [ dup (Type (tk 2)) (arrayOf 5); dup (Type (tk 2)) (typeRef 5) ] ] }
            let trimmed = Internal.trimTypeReferenceArrayTupleDuplicates result
            Expect.isFalse (trimmed.DuplicateTypes.ContainsKey (tk 2)) "pair removed"
            Expect.isTrue (trimmed.Types.[tk 2]).IsArray "collapsed to the Array side"

        testCase "TypeRef + Tuple pair collapses to the Tuple" <| fun _ ->
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 3, [ dup (Type (tk 3)) (typeRef 5); dup (Type (tk 3)) (tupleOf [ 5; 6 ]) ] ] }
            let trimmed = Internal.trimTypeReferenceArrayTupleDuplicates result
            Expect.isFalse (trimmed.DuplicateTypes.ContainsKey (tk 3)) "pair removed"
            Expect.isTrue (trimmed.Types.[tk 3]).IsTuple "collapsed to the Tuple side"

        testCase "non-matching duplicate pair is left untouched" <| fun _ ->
            // Two TypeRefs (no Array/Tuple component) must NOT be collapsed.
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 4, [ dup (Type (tk 4)) (typeRef 5); dup (Type (tk 4)) (typeRef 6) ] ] }
            let trimmed = Internal.trimTypeReferenceArrayTupleDuplicates result
            Expect.isTrue (trimmed.DuplicateTypes.ContainsKey (tk 4)) "kept as a duplicate"
            Expect.isFalse (trimmed.Types.ContainsKey (tk 4)) "not moved into Types"
    ]

// ---------------------------------------------------------------------------
// mergeTypes — merge Interface (and TypeLiteral) members across declaration-
// merged duplicate decls; the winner (head) absorbs the others' members.
// ---------------------------------------------------------------------------
let private mergeTypesTests =
    testList "mergeTypes" [

        testCase "two same-key interfaces merge into one with the union of members" <| fun _ ->
            let ifaceA = TsType.Interface (emptyInterface "Win" [ prop "a" ])
            let ifaceB = TsType.Interface (emptyInterface "Other" [ prop "b" ])
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 1, [ dup (Symbol "Win") ifaceA; dup (Symbol "Other") ifaceB ] ] }
            let merged = Internal.mergeTypes result
            // single merged winner with length 1 -> folded into Types, removed from DuplicateTypes.
            Expect.isFalse (merged.DuplicateTypes.ContainsKey (tk 1)) "merged group leaves DuplicateTypes"
            match merged.Types.[tk 1] with
            | TsType.Interface iface ->
                Expect.equal iface.Members.Length 2 "winner gained the other's member"
                Expect.equal iface.Name "Win" "winner identity (head) preserved"
            | other -> failwithf "expected merged Interface, got %A" other

        testCase "two same-key type literals merge their members" <| fun _ ->
            let litA = TsType.TypeLiteral { Members = [ prop "a" ] }
            let litB = TsType.TypeLiteral { Members = [ prop "b" ] }
            let result =
                { emptyResult with
                    DuplicateTypes = Map [ tk 2, [ dup (Type (tk 2)) litA; dup (Type (tk 2)) litB ] ] }
            let merged = Internal.mergeTypes result
            match merged.Types.[tk 2] with
            | TsType.TypeLiteral lit -> Expect.equal lit.Members.Length 2 "type literal members merged"
            | other -> failwithf "expected merged TypeLiteral, got %A" other

        testCase "non-mergeable duplicate (TypeRefs) passes through unchanged" <| fun _ ->
            // winner is a TypeReference -> not mergeable -> the group is returned as-is
            // and (length 2) stays in DuplicateTypes.
            let result =
                { emptyResult with
                    DuplicateTypes =
                        Map [ tk 3, [ dup (Type (tk 3)) (typeRef 5); dup (Type (tk 3)) (typeRef 6) ] ] }
            let merged = Internal.mergeTypes result
            Expect.isTrue (merged.DuplicateTypes.ContainsKey (tk 3)) "non-mergeable group stays a duplicate"
            Expect.equal (merged.DuplicateTypes.[tk 3]).Length 2 "both kept"
            Expect.isFalse (merged.Types.ContainsKey (tk 3)) "not promoted to Types"
    ]

// ---------------------------------------------------------------------------
// mergeExports — merge Interface members across decls AND combine Function
// overloads via TsOverloadableConstruct.Combine.
// ---------------------------------------------------------------------------
let private mergeExportsTests =
    testList "mergeExports" [

        testCase "two same-key interface exports merge member sets" <| fun _ ->
            let ifaceA = TsExportDeclaration.Interface (emptyInterface "Win" [ prop "a" ])
            let ifaceB = TsExportDeclaration.Interface (emptyInterface "Other" [ prop "b" ])
            let result =
                { emptyResult with
                    DuplicateExports = Map [ tk 1, [ dup (Symbol "Win") ifaceA; dup (Symbol "Other") ifaceB ] ] }
            let merged = Internal.mergeExports result
            Expect.isFalse (merged.DuplicateExports.ContainsKey (tk 1)) "merged group leaves DuplicateExports"
            match merged.ExportedDeclarations.[tk 1] with
            | TsExportDeclaration.Interface iface ->
                Expect.equal iface.Members.Length 2 "winner gained the other's member"
            | other -> failwithf "expected merged Interface export, got %A" other

        testCase "two function overloads combine into an Overloaded construct" <| fun _ ->
            let f1 = TsExportDeclaration.Function (TsOverloadableConstruct.NoOverloads (emptyFunction "f" -1))
            let f2 = TsExportDeclaration.Function (TsOverloadableConstruct.NoOverloads (emptyFunction "f" -3))
            let result =
                { emptyResult with
                    DuplicateExports = Map [ tk 2, [ dup (Symbol "f") f1; dup (Symbol "f") f2 ] ] }
            let merged = Internal.mergeExports result
            match merged.ExportedDeclarations.[tk 2] with
            | TsExportDeclaration.Function construct ->
                Expect.equal construct.Values.Length 2 "two distinct overloads combined"
            | other -> failwithf "expected combined Function export, got %A" other

        testCase "identical function overloads collapse to a single NoOverloads" <| fun _ ->
            // TsOverloadableConstruct.Combine of two equal NoOverloads stays NoOverloads.
            let f = emptyFunction "f" -1
            let f1 = TsExportDeclaration.Function (TsOverloadableConstruct.NoOverloads f)
            let f2 = TsExportDeclaration.Function (TsOverloadableConstruct.NoOverloads f)
            let result =
                { emptyResult with
                    DuplicateExports = Map [ tk 4, [ dup (Symbol "f") f1; dup (Symbol "f") f2 ] ] }
            let merged = Internal.mergeExports result
            match merged.ExportedDeclarations.[tk 4] with
            | TsExportDeclaration.Function construct ->
                Expect.equal construct.Values.Length 1 "identical overloads stay single"
            | other -> failwithf "expected single Function export, got %A" other

        testCase "non-mergeable export duplicate (Variable) passes through unchanged" <| fun _ ->
            let v name : TsExportDeclaration =
                TsExportDeclaration.Variable { Source = None; FullyQualifiedName = [ name ]; Name = name; Type = tk -1; Documentation = [] }
            let result =
                { emptyResult with
                    DuplicateExports = Map [ tk 3, [ dup (Symbol "v1") (v "v1"); dup (Symbol "v2") (v "v2") ] ] }
            let merged = Internal.mergeExports result
            Expect.isTrue (merged.DuplicateExports.ContainsKey (tk 3)) "non-mergeable export stays a duplicate"
            Expect.equal (merged.DuplicateExports.[tk 3]).Length 2 "both kept"
    ]

// ---------------------------------------------------------------------------
// selectAndMergeWinnersInDuplicates — promote the List.head winner from each
// remaining duplicate group into Types / ExportedDeclarations (does not remove
// the group; just publishes the winner if the key isn't already present).
// ---------------------------------------------------------------------------
let private selectWinnersTests =
    testList "selectAndMergeWinnersInDuplicates" [

        testCase "promotes head winner of a type duplicate group into Types" <| fun _ ->
            let winner = typeRef 5
            let loser = typeRef 6
            let result =
                { emptyResult with
                    DuplicateTypes = Map [ tk 1, [ dup (Type (tk 1)) winner; dup (Type (tk 1)) loser ] ] }
            let promoted = Internal.selectAndMergeWinnersInDuplicates result
            Expect.equal promoted.Types.[tk 1] winner "head value promoted into Types"

        testCase "promotes head winner of an export duplicate group into ExportedDeclarations" <| fun _ ->
            let winner = TsExportDeclaration.Interface (emptyInterface "Win" [])
            let loser = TsExportDeclaration.Interface (emptyInterface "Lose" [])
            let result =
                { emptyResult with
                    DuplicateExports = Map [ tk 2, [ dup (Symbol "Win") winner; dup (Symbol "Lose") loser ] ] }
            let promoted = Internal.selectAndMergeWinnersInDuplicates result
            Expect.equal promoted.ExportedDeclarations.[tk 2] winner "head export promoted"

        testCase "does not overwrite an existing non-duplicate entry for the same key" <| fun _ ->
            let existing = typeRef 99
            let dupWinner = typeRef 5
            let result =
                { emptyResult with
                    Types = Map [ tk 1, existing ]
                    DuplicateTypes = Map [ tk 1, [ dup (Type (tk 1)) dupWinner ] ] }
            let promoted = Internal.selectAndMergeWinnersInDuplicates result
            Expect.equal promoted.Types.[tk 1] existing "existing entry preserved (Some value branch)"
    ]

// ---------------------------------------------------------------------------
// internStructuralUnions — STRUCTURAL UNION INTERNING (the encoder identity root
// fix). Two structurally-identical synthesized unions (same member-key set) get
// minted DISTINCT keys by TypeKey.create (allocation, not structure). This pass
// derives identity from STRUCTURE: it keeps ONE canonical entry per sorted-member
// signature, drops the redundant entries, and rewrites EVERY reference to a dropped
// key onto the canonical key. UNIONS ONLY (location-independent); TypeLiterals are
// intentionally NOT interned (location-dependent — would collapse distinct owners).
// ---------------------------------------------------------------------------
let private union (members: int list) : TsType =
    members |> List.map tk |> TsTypeUnion |> TsType.Union

let private internTests =
    testList "internStructuralUnions" [

        testCase "two structurally-identical unions collapse to ONE canonical key" <| fun _ ->
            // union(string,number) minted at two keys (-20, -21) -> one survives.
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -1; -3 ] ] }
            let interned = Internal.internStructuralUnions result
            let unionKeys =
                interned.Types
                |> Map.toList
                |> List.choose (function k, TsType.Union _ -> Some k | _ -> None)
            Expect.equal unionKeys.Length 1 "one union entry remains after interning"
            // canonical = minimum key = -21 (more negative); -20 is the redundant drop.
            Expect.equal unionKeys.[0] (tk -21) "canonical is the minimum (most-negative) key"
            Expect.isFalse (interned.Types.ContainsKey (tk -20)) "redundant union key dropped"

        testCase "member ORDER does not matter (signature is sorted)" <| fun _ ->
            // [string;number] and [number;string] are the SAME union.
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -3; -1 ] ] }
            let interned = Internal.internStructuralUnions result
            let unionKeys =
                interned.Types |> Map.toList |> List.choose (function k, TsType.Union _ -> Some k | _ -> None)
            Expect.equal unionKeys.Length 1 "order-permuted unions share one identity"

        testCase "STRUCTURALLY DISTINCT unions keep DIFFERENT keys (no false collision)" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -1; -4 ] ] }
            let interned = Internal.internStructuralUnions result
            let unionKeys =
                interned.Types |> Map.toList |> List.choose (function k, TsType.Union _ -> Some k | _ -> None)
            Expect.equal unionKeys.Length 2 "distinct unions are NOT merged"
            Expect.isTrue (interned.Types.ContainsKey (tk -20)) "first distinct union kept"
            Expect.isTrue (interned.Types.ContainsKey (tk -21)) "second distinct union kept"

        testCase "references to a dropped union key are REWRITTEN to the canonical key" <| fun _ ->
            // A property whose Type points at the redundant union (-20) must, after
            // interning, point at the canonical (-21) so nothing dangles.
            let referer =
                TsType.Interface
                    (emptyInterface "Holder"
                        [ TsMember.Property { Name = "x"; Type = tk -20; IsStatic = false; IsOptional = false; IsPrivate = false; Accessor = TsAccessor.ReadWrite; Documentation = [] } ])
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -1; -3 ]; tk 1, referer ] }
            let interned = Internal.internStructuralUnions result
            match interned.Types.[tk 1] with
            | TsType.Interface iface ->
                match iface.Members.[0] with
                | TsMember.Property p -> Expect.equal p.Type (tk -21) "reference redirected to canonical key"
                | other -> failwithf "expected Property, got %A" other
            | other -> failwithf "expected Interface, got %A" other

        testCase "TopLevelExports / LibEsExports key lists are rewritten too" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -1; -3 ] ]
                    TopLevelExports = [ tk -20 ]
                    LibEsExports = [ tk -20 ] }
            let interned = Internal.internStructuralUnions result
            Expect.equal interned.TopLevelExports [ tk -21 ] "TopLevelExports remapped to canonical"
            Expect.equal interned.LibEsExports [ tk -21 ] "LibEsExports remapped to canonical"

        testCase "FIXPOINT: remapping members can create a NEW collision that is then collapsed" <| fun _ ->
            // -20 and -21 are identical (string|number) -> -21 canonical, -20 dropped.
            // -30 = union(-20, -4) and -31 = union(-21, -4): DISTINCT before remap (members
            // -20 vs -21), but once -20 -> -21 they become identical -> must collapse too.
            let result =
                { emptyResult with
                    Types =
                        Map [
                            tk -20, union [ -1; -3 ]
                            tk -21, union [ -1; -3 ]
                            tk -30, union [ -20; -4 ]
                            tk -31, union [ -21; -4 ]
                        ] }
            let interned = Internal.internStructuralUnions result
            let unionKeys =
                interned.Types |> Map.toList |> List.choose (function k, TsType.Union _ -> Some k | _ -> None)
            // expect 2 survivors: the {string,number} canonical and the {that,bool} canonical.
            Expect.equal unionKeys.Length 2 "fixpoint collapses the collision induced by member remap"

        testCase "TypeLiterals are NOT interned (only unions)" <| fun _ ->
            // Two structurally-identical type literals must BOTH survive (location-dependent).
            let lit = TsType.TypeLiteral { Members = [ prop "a" ] }
            let result = { emptyResult with Types = Map [ tk -20, lit; tk -21, lit ] }
            let interned = Internal.internStructuralUnions result
            Expect.isTrue (interned.Types.ContainsKey (tk -20)) "first type literal kept"
            Expect.isTrue (interned.Types.ContainsKey (tk -21)) "second type literal kept (NOT interned)"

        testCase "idempotent — interning an already-interned result is a no-op" <| fun _ ->
            let result =
                { emptyResult with
                    Types = Map [ tk -20, union [ -1; -3 ]; tk -21, union [ -1; -3 ] ] }
            let once = Internal.internStructuralUnions result
            let twice = Internal.internStructuralUnions once
            Expect.equal (twice.Types |> Map.toList) (once.Types |> Map.toList) "second pass changes nothing"
    ]

// ---------------------------------------------------------------------------
// Aggregated suite — registered in Program.fs and Xantham.Fable.Tests.fsproj.
// ---------------------------------------------------------------------------
let encoderMergeDedupTests =
    testList "EncoderMergeDedup" [
        identityPriorityTests
        finaliseAssemblyTests
        trimTests
        mergeTypesTests
        mergeExportsTests
        selectWinnersTests
        internTests
    ]
