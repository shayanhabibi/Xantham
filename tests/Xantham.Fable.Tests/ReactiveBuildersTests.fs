module ReactiveBuildersTests

// ---------------------------------------------------------------------------
// PER-COMPONENT ISOLATION COVERAGE — REACTIVE "S-BUILDER" LAYER
//
// TARGET: src/Xantham.Fable/Types/ReactiveBuilders.fs — the encoder's reactive
// builder layer. Each S-builder is a record whose fields are Signal-typed slots
// (TypeSignal = Signal<TypeKey>, Signal<ModuleName>, slot arrays of
// Signal<_ voption>); each exposes a `.Build() : Ts*` member that RESOLVES the
// signal slots and constructs the corresponding immutable Ts* value.
//
// This layer has ZERO ts-morph dependency — it is purely Signal-based, so every
// case here runs on CONSTRUCTED signals (Signal.source / TypeSignal.ofKey /
// pending + Set / FulfillWith). No live ts-morph program is needed.
//
// The three load-bearing contracts under test:
//   1. all-slots-filled  -> fully-populated Ts* value (fields carry resolved
//      signal values; Source: Signal<ModuleName> -> Some moduleString).
//   2. resolve() drops ValueNone slots: an array of Signal<_ voption> where
//      some are ValueNone and some ValueSome -> Build includes ONLY the
//      ValueSome ones, IN ORDER (how optional members/params get assembled).
//   3. LATE BINDING: fill a slot via pending()/Set AFTER constructing the
//      builder but BEFORE .Build() — Build sees the late value. This is the
//      whole point of the signal design (dispatcher fills slots post-creation).
//
// Fable.Mocha Expect idiom (matches EncoderMergeDedup.fs / Program.fs):
//   Expect.equal actual expected "message"      (message is the LAST arg)
//   Expect.isTrue value "message"
// ---------------------------------------------------------------------------

open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.ReactiveBuilders
open Fable.Mocha

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

let private tk (i: int) : TypeKey = TypeKey.createWith i

/// A filled optional slot.
let private someSlot (v: 'a) : Signal<'a voption> = Signal.source (ValueSome v)
/// An empty optional slot (resolve must DROP these).
let private noneSlot<'a> () : Signal<'a voption> = Signal.source (ValueNone: 'a voption)

let private modSig (s: string) : Signal<ModuleName> = Signal.source (ModuleName s)

// A representative parameter builder used inside slot-array tests.
let private paramBuilder (name: string) (typeKey: int) : SParameterBuilder =
    { Name = name
      IsOptional = false
      IsSpread = false
      Type = TypeSignal.ofKey (tk typeKey)
      Documentation = [] }

// ---------------------------------------------------------------------------
// SEnumCaseBuilder — Source: Signal<ModuleName>, Parent: TypeSignal.
// ---------------------------------------------------------------------------
let private enumCaseTests =
    testList "SEnumCaseBuilder" [

        testCase "Build resolves Parent + Source(ModuleName) into the TsEnumCase" <| fun _ ->
            let b: SEnumCaseBuilder =
                { Source = modSig "mymod"
                  Parent = TypeSignal.ofKey (tk 7)
                  FullyQualifiedName = [| "E"; "Red" |]
                  Name = "Red"
                  Value = TsLiteral.Int 0
                  Documentation = [] }
            let built = b.Build()
            Expect.equal built.Parent (tk 7) "Parent slot resolved"
            Expect.equal built.Source (Some "mymod") "Source(ModuleName) unwrapped to Some string"
            Expect.equal built.FullyQualifiedName [ "E"; "Red" ] "FQN array -> list"
            Expect.equal built.Name "Red" "Name carried"
            Expect.equal built.Value (TsLiteral.Int 0) "Value carried"

        testCase "late-bound Parent slot: Set AFTER construction is seen by Build" <| fun _ ->
            let parent = TypeSignal.ofKey (tk 1)
            let b: SEnumCaseBuilder =
                { Source = modSig "m"
                  Parent = parent
                  FullyQualifiedName = [||]
                  Name = "C"
                  Value = TsLiteral.Null
                  Documentation = [] }
            // dispatcher-style late fill of the slot before Build
            parent.Set (tk 42)
            Expect.equal (b.Build().Parent) (tk 42) "Build reads the late-bound Parent value"
    ]

// ---------------------------------------------------------------------------
// SEnumTypeBuilder — Members is a Signal<SEnumCaseBuilder voption> array.
// ---------------------------------------------------------------------------
let private enumTypeTests =
    testList "SEnumTypeBuilder" [

        let mkCase name : SEnumCaseBuilder =
            { Source = modSig "m"; Parent = TypeSignal.ofKey (tk 0)
              FullyQualifiedName = [||]; Name = name; Value = TsLiteral.Null; Documentation = [] }

        testCase "all member slots filled -> all cases built in order" <| fun _ ->
            let b: SEnumTypeBuilder =
                { Source = modSig "enums"
                  FullyQualifiedName = [| "Color" |]
                  Name = "Color"
                  Members = [| someSlot (mkCase "Red"); someSlot (mkCase "Green") |]
                  Documentation = [] }
            let built = b.Build()
            Expect.equal built.Source (Some "enums") "Source unwrapped"
            Expect.equal (built.Members |> List.map (fun m -> m.Name)) [ "Red"; "Green" ] "both cases built, in order"

        testCase "ValueNone member slots are DROPPED, ValueSome kept in order" <| fun _ ->
            let b: SEnumTypeBuilder =
                { Source = modSig "m"
                  FullyQualifiedName = [||]
                  Name = "E"
                  Members = [| someSlot (mkCase "A"); noneSlot (); someSlot (mkCase "B"); noneSlot () |]
                  Documentation = [] }
            Expect.equal (b.Build().Members |> List.map (fun m -> m.Name)) [ "A"; "B" ] "only ValueSome cases, in order"
    ]

// ---------------------------------------------------------------------------
// SVariableBuilder — Source + a TypeSignal slot.
// ---------------------------------------------------------------------------
let private variableTests =
    testList "SVariableBuilder" [

        testCase "Build resolves Type slot + Source" <| fun _ ->
            let b: SVariableBuilder =
                { Source = modSig "vars"
                  FullyQualifiedName = [| "answer" |]
                  Name = "answer"
                  Type = TypeSignal.ofKey (tk 3)
                  Documentation = [] }
            let built = b.Build()
            Expect.equal built.Type (tk 3) "Type slot resolved"
            Expect.equal built.Source (Some "vars") "Source unwrapped"
            Expect.equal built.Name "answer" "Name carried"

        testCase "pending Type slot defaults to Unknown.TypeKey until filled, then late-bind is seen" <| fun _ ->
            let ty = TypeSignal.pending ()
            let b: SVariableBuilder =
                { Source = modSig "m"; FullyQualifiedName = [||]; Name = "v"; Type = ty; Documentation = [] }
            // pending() seeds Unknown.TypeKey
            Expect.equal (b.Build().Type) TypeKindPrimitive.Unknown.TypeKey "pending defaults to Unknown.TypeKey"
            ty.Set (tk 99)
            Expect.equal (b.Build().Type) (tk 99) "late-bound Type value seen by a subsequent Build"
    ]

// ---------------------------------------------------------------------------
// SParameterBuilder — single TypeSignal slot + flags.
// ---------------------------------------------------------------------------
let private parameterTests =
    testList "SParameterBuilder" [

        testCase "Build carries flags + resolved Type" <| fun _ ->
            let b: SParameterBuilder =
                { Name = "x"; IsOptional = true; IsSpread = true
                  Type = TypeSignal.ofKey (tk 5); Documentation = [] }
            let built = b.Build()
            Expect.equal built.Name "x" "Name"
            Expect.isTrue built.IsOptional "IsOptional"
            Expect.isTrue built.IsSpread "IsSpread"
            Expect.equal built.Type (tk 5) "Type resolved"
    ]

// ---------------------------------------------------------------------------
// STypeParameterBuilder — Constraint/Default are TypeSignal voption (the slot
// ITSELF is optional, not its inner value).
// ---------------------------------------------------------------------------
let private typeParameterTests =
    testList "STypeParameterBuilder" [

        testCase "both Constraint and Default present -> Some resolved keys" <| fun _ ->
            let b: STypeParameterBuilder =
                { Name = "T"
                  Constraint = ValueSome (TypeSignal.ofKey (tk 2))
                  Default = ValueSome (TypeSignal.ofKey (tk 4))
                  Documentation = [] }
            let built = b.Build()
            Expect.equal built.Constraint (Some (tk 2)) "Constraint resolved"
            Expect.equal built.Default (Some (tk 4)) "Default resolved"

        testCase "absent Constraint/Default (ValueNone) -> None" <| fun _ ->
            let b: STypeParameterBuilder =
                { Name = "T"; Constraint = ValueNone; Default = ValueNone; Documentation = [] }
            let built = b.Build()
            Expect.equal built.Constraint None "no constraint"
            Expect.equal built.Default None "no default"

        testCase "InlinedSTypeParameterBuilder.Build returns (Type key, built parameter)" <| fun _ ->
            let inl: InlinedSTypeParameterBuilder =
                { Type = tk 11
                  TypeParameter = { Name = "U"; Constraint = ValueNone; Default = ValueNone; Documentation = [] } }
            let key, tp = inl.Build()
            Expect.equal key (tk 11) "inlined Type key"
            Expect.equal tp.Name "U" "inlined parameter built"
    ]

// ---------------------------------------------------------------------------
// SPropertyBuilder / SGetAccessorBuilder / SSetAccessorBuilder.
// ---------------------------------------------------------------------------
let private memberLeafTests =
    testList "Property/Get/Set accessors" [

        testCase "SPropertyBuilder.Build carries modifiers + accessor + Type" <| fun _ ->
            let b: SPropertyBuilder =
                { Name = "count"; Type = TypeSignal.ofKey (tk 6)
                  IsStatic = true; IsOptional = true; IsPrivate = true
                  Accessor = TsAccessor.ReadOnly; Documentation = [] }
            let built = b.Build()
            Expect.equal built.Type (tk 6) "Type resolved"
            Expect.isTrue built.IsStatic "IsStatic"
            Expect.isTrue built.IsOptional "IsOptional"
            Expect.isTrue built.IsPrivate "IsPrivate"
            Expect.equal built.Accessor TsAccessor.ReadOnly "Accessor"

        testCase "SGetAccessorBuilder.Build resolves Type" <| fun _ ->
            let b: SGetAccessorBuilder =
                { Name = "value"; Type = TypeSignal.ofKey (tk 8); IsStatic = false; IsPrivate = true }
            let built = b.Build()
            Expect.equal built.Name "value" "Name"
            Expect.equal built.Type (tk 8) "Type resolved"
            Expect.isTrue built.IsPrivate "IsPrivate"

        testCase "SSetAccessorBuilder.Build resolves ArgumentType" <| fun _ ->
            let b: SSetAccessorBuilder =
                { Name = "value"; ArgumentType = TypeSignal.ofKey (tk 9)
                  IsStatic = true; IsPrivate = false; Documentation = [] }
            let built = b.Build()
            Expect.equal built.ArgumentType (tk 9) "ArgumentType resolved"
            Expect.isTrue built.IsStatic "IsStatic"
    ]

// ---------------------------------------------------------------------------
// SMethodBuilder — Parameters is a Signal<SParameterBuilder voption> array.
// ---------------------------------------------------------------------------
let private methodTests =
    testList "SMethodBuilder" [

        testCase "all parameter slots filled -> all params built, Type resolved" <| fun _ ->
            let b: SMethodBuilder =
                { Name = "foo"
                  Parameters = [| someSlot (paramBuilder "a" 1); someSlot (paramBuilder "b" 2) |]
                  Type = TypeSignal.ofKey (tk 3)
                  IsOptional = false; IsStatic = true; Documentation = [] }
            let built = b.Build()
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "a"; "b" ] "params in order"
            Expect.equal built.Type (tk 3) "return Type resolved"
            Expect.isTrue built.IsStatic "IsStatic"

        testCase "ValueNone parameter slots are DROPPED (optional-param assembly contract)" <| fun _ ->
            let b: SMethodBuilder =
                { Name = "bar"
                  Parameters = [| noneSlot (); someSlot (paramBuilder "kept" 1); noneSlot () |]
                  Type = TypeSignal.ofKey (tk 0)
                  IsOptional = false; IsStatic = false; Documentation = [] }
            Expect.equal (b.Build().Parameters |> List.map (fun p -> p.Name)) [ "kept" ] "only the filled slot survives"

        testCase "late-bound parameter slot: pending slot Set after construction is seen" <| fun _ ->
            let slot: Signal<SParameterBuilder voption> = Signal.pending ()
            let b: SMethodBuilder =
                { Name = "baz"; Parameters = [| slot |]; Type = TypeSignal.ofKey (tk 0)
                  IsOptional = false; IsStatic = false; Documentation = [] }
            // empty before fill
            Expect.equal (b.Build().Parameters) [] "pending slot is empty before fill"
            slot.Set (ValueSome (paramBuilder "late" 7))
            let built = b.Build()
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "late" ] "late-filled param appears"
            Expect.equal (built.Parameters.[0].Type) (tk 7) "late param's nested Type resolved"
    ]

// ---------------------------------------------------------------------------
// SFunctionBuilder — Parameters + TypeParameters slot arrays + SignatureKey.
// ---------------------------------------------------------------------------
let private functionTests =
    testList "SFunctionBuilder" [

        let inlinedTp name key : InlinedSTypeParameterBuilder =
            { Type = tk key
              TypeParameter = { Name = name; Constraint = ValueNone; Default = ValueNone; Documentation = [] } }

        testCase "Build resolves Source, Type, SignatureKey, params, type-params" <| fun _ ->
            let b: SFunctionBuilder =
                { Source = modSig "fns"
                  FullyQualifiedName = [| "sum" |]
                  Name = "sum"
                  IsDeclared = true
                  Type = TypeSignal.ofKey (tk 3)
                  Parameters = [| someSlot (paramBuilder "a" 1); someSlot (paramBuilder "b" 1) |]
                  TypeParameters = [| someSlot (inlinedTp "T" 5) |]
                  Documentation = []
                  SignatureKey = TypeSignal.ofKey (tk 77) }
            let built = b.Build()
            Expect.equal built.Source (Some "fns") "Source unwrapped"
            Expect.equal built.Type (tk 3) "return Type resolved"
            Expect.equal built.SignatureKey (tk 77) "SignatureKey resolved"
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "a"; "b" ] "params"
            Expect.equal (built.TypeParameters |> List.map (fun (k, tp) -> k, tp.Name)) [ (tk 5, "T") ] "inlined type-params"

        testCase "empty param/type-param slot arrays -> empty lists" <| fun _ ->
            let b: SFunctionBuilder =
                { Source = modSig "m"; FullyQualifiedName = [||]; Name = "noargs"; IsDeclared = false
                  Type = TypeSignal.ofKey (tk 0); Parameters = [||]; TypeParameters = [||]
                  Documentation = []; SignatureKey = TypeSignal.ofKey (tk 0) }
            let built = b.Build()
            Expect.equal built.Parameters [] "no params"
            Expect.equal built.TypeParameters [] "no type-params"
    ]

// ---------------------------------------------------------------------------
// Signature-like builders: SCallSignature / SConstructSignature / SConstructor /
// SIndexSignature — all share the Parameters slot-array resolve contract.
// ---------------------------------------------------------------------------
let private signatureTests =
    testList "Call/Construct/Constructor/IndexSignature builders" [

        testCase "SCallSignatureBuilder drops ValueNone params, resolves Type" <| fun _ ->
            let b: SCallSignatureBuilder =
                { Parameters = [| someSlot (paramBuilder "x" 1); noneSlot () |]
                  Type = TypeSignal.ofKey (tk 2); Documentation = [] }
            let built = b.Build()
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "x" ] "filled param only"
            Expect.equal built.Type (tk 2) "Type resolved"

        testCase "SConstructSignatureBuilder resolves Type + params" <| fun _ ->
            let b: SConstructSignatureBuilder =
                { Type = TypeSignal.ofKey (tk 4)
                  Parameters = [| someSlot (paramBuilder "x" 1) |] }
            let built = b.Build()
            Expect.equal built.Type (tk 4) "Type resolved"
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "x" ] "param built"

        testCase "SConstructorBuilder resolves params" <| fun _ ->
            let b: SConstructorBuilder =
                { Parameters = [| someSlot (paramBuilder "v" 1); noneSlot () |]; Documentation = [] }
            Expect.equal (b.Build().Parameters |> List.map (fun p -> p.Name)) [ "v" ] "only filled param"

        testCase "SIndexSignatureBuilder resolves params + Type + IsReadOnly" <| fun _ ->
            let b: SIndexSignatureBuilder =
                { Parameters = [| someSlot (paramBuilder "k" 1) |]
                  Type = TypeSignal.ofKey (tk 3); IsReadOnly = true }
            let built = b.Build()
            Expect.equal (built.Parameters |> List.map (fun p -> p.Name)) [ "k" ] "param built"
            Expect.equal built.Type (tk 3) "Type resolved"
            Expect.isTrue built.IsReadOnly "IsReadOnly"
    ]

// ---------------------------------------------------------------------------
// SMemberBuilder DU + STypeLiteralBuilder — Build dispatches each case to the
// right TsMember; overloadable members are wrapped via TsOverloadableConstruct.
// ---------------------------------------------------------------------------
let private memberDuTests =
    testList "SMemberBuilder / STypeLiteralBuilder" [

        testCase "Method case builds to TsMember.Method (NoOverloads) carrying the method" <| fun _ ->
            let m: SMethodBuilder =
                { Name = "go"; Parameters = [||]; Type = TypeSignal.ofKey (tk 1)
                  IsOptional = false; IsStatic = false; Documentation = [] }
            match (SMemberBuilder.Method m).Build() with
            | TsMember.Method construct ->
                Expect.equal construct.Values.Length 1 "single NoOverloads value"
                Expect.equal construct.Values.[0].Name "go" "method name carried"
            | other -> failwithf "expected TsMember.Method, got %A" other

        testCase "Property case builds to TsMember.Property" <| fun _ ->
            let p: SPropertyBuilder =
                { Name = "p"; Type = TypeSignal.ofKey (tk 1); IsStatic = false; IsOptional = false
                  IsPrivate = false; Accessor = TsAccessor.ReadWrite; Documentation = [] }
            match (SMemberBuilder.Property p).Build() with
            | TsMember.Property prop -> Expect.equal prop.Name "p" "property name carried"
            | other -> failwithf "expected TsMember.Property, got %A" other

        testCase "STypeLiteralBuilder drops ValueNone member slots, keeps order" <| fun _ ->
            let prop name : SMemberBuilder =
                SMemberBuilder.Property
                    { Name = name; Type = TypeSignal.ofKey (tk 1); IsStatic = false; IsOptional = false
                      IsPrivate = false; Accessor = TsAccessor.ReadWrite; Documentation = [] }
            let b: STypeLiteralBuilder =
                { Members = [| someSlot (prop "a"); noneSlot (); someSlot (prop "b") |] }
            let names =
                b.Build().Members
                |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)
            Expect.equal names [ "a"; "b" ] "only filled members, in order"
    ]

// ---------------------------------------------------------------------------
// STypeReferenceBuilder — TypeArguments is a (non-optional) TypeSignal array;
// ResolvedType is a doubly-optional Signal<TypeKey voption> voption.
// ---------------------------------------------------------------------------
let private typeReferenceTests =
    testList "STypeReferenceBuilder" [

        testCase "resolves Type + all TypeArguments + a present ResolvedType" <| fun _ ->
            let b: STypeReferenceBuilder =
                { Type = TypeSignal.ofKey (tk 1)
                  TypeArguments = [| TypeSignal.ofKey (tk 2); TypeSignal.ofKey (tk 3) |]
                  ResolvedType = ValueSome (Signal.source (ValueSome (tk 9))) }
            let built = b.Build()
            Expect.equal built.Type (tk 1) "Type resolved"
            Expect.equal built.TypeArguments [ tk 2; tk 3 ] "all type-args resolved, in order"
            Expect.equal built.ResolvedType (Some (tk 9)) "ResolvedType present"

        testCase "absent ResolvedType slot (ValueNone outer) -> None" <| fun _ ->
            let b: STypeReferenceBuilder =
                { Type = TypeSignal.ofKey (tk 1); TypeArguments = [||]; ResolvedType = ValueNone }
            let built = b.Build()
            Expect.equal built.TypeArguments [] "no type args"
            Expect.equal built.ResolvedType None "no resolved type"

        testCase "ResolvedType slot present but inner ValueNone -> None (bind collapses)" <| fun _ ->
            let b: STypeReferenceBuilder =
                { Type = TypeSignal.ofKey (tk 1); TypeArguments = [||]
                  ResolvedType = ValueSome (Signal.source (ValueNone: TypeKey voption)) }
            Expect.equal (b.Build().ResolvedType) None "inner ValueNone collapses to None"
    ]

// ---------------------------------------------------------------------------
// Heritage builders — SInterfaceHeritageBuilder / SClassHeritageBuilder.
// ---------------------------------------------------------------------------
let private heritageTests =
    testList "Interface/Class heritage builders" [

        let typeRef key : STypeReferenceBuilder =
            { Type = TypeSignal.ofKey (tk key); TypeArguments = [||]; ResolvedType = ValueNone }

        testCase "SInterfaceHeritageBuilder drops ValueNone extends slots" <| fun _ ->
            let b: SInterfaceHeritageBuilder =
                { Extends = [| someSlot (typeRef 1); noneSlot (); someSlot (typeRef 2) |] }
            let built = b.Build()
            Expect.equal (built.Extends |> List.map (fun e -> e.Type)) [ tk 1; tk 2 ] "filled extends only, in order"

        testCase "SClassHeritageBuilder: present Implements + extends list" <| fun _ ->
            let b: SClassHeritageBuilder =
                { Implements = ValueSome (someSlot (typeRef 5))
                  Extends = [| someSlot (typeRef 6) |] }
            let built = b.Build()
            Expect.equal (built.Implements |> Option.map (fun i -> i.Type)) (Some (tk 5)) "Implements resolved"
            Expect.equal (built.Extends |> List.map (fun e -> e.Type)) [ tk 6 ] "Extends resolved"

        testCase "SClassHeritageBuilder: absent Implements -> None" <| fun _ ->
            let b: SClassHeritageBuilder =
                { Implements = ValueNone; Extends = [||] }
            let built = b.Build()
            Expect.equal built.Implements None "no implements"
            Expect.equal built.Extends [] "no extends"
    ]

// ---------------------------------------------------------------------------
// SInterfaceBuilder — Heritage is a non-optional Signal<...voption>; an empty
// (ValueNone) heritage slot defaults to { Extends = [] }.
// ---------------------------------------------------------------------------
let private interfaceTests =
    testList "SInterfaceBuilder" [

        let prop name : SMemberBuilder =
            SMemberBuilder.Property
                { Name = name; Type = TypeSignal.ofKey (tk 1); IsStatic = false; IsOptional = false
                  IsPrivate = false; Accessor = TsAccessor.ReadWrite; Documentation = [] }

        testCase "Build resolves Source, members, and a present Heritage" <| fun _ ->
            let heritage: SInterfaceHeritageBuilder =
                { Extends = [| someSlot { Type = TypeSignal.ofKey (tk 9); TypeArguments = [||]; ResolvedType = ValueNone } |] }
            let b: SInterfaceBuilder =
                { Source = modSig "iface"
                  FullyQualifiedName = [| "I" |]
                  Enumerable = true
                  Name = "I"
                  Members = [| someSlot (prop "a"); noneSlot () |]
                  TypeParameters = [||]
                  Documentation = []
                  Heritage = Signal.source (ValueSome heritage) }
            let built = b.Build()
            Expect.equal built.Source (Some "iface") "Source unwrapped (interface uses the direct ModuleName form)"
            Expect.isTrue built.Enumerable "Enumerable"
            Expect.equal (built.Members |> List.choose (function TsMember.Property p -> Some p.Name | _ -> None)) [ "a" ] "only filled member"
            Expect.equal (built.Heritage.Extends |> List.map (fun e -> e.Type)) [ tk 9 ] "heritage extends resolved"

        testCase "empty (ValueNone) Heritage slot defaults to { Extends = [] }" <| fun _ ->
            let b: SInterfaceBuilder =
                { Source = modSig "m"; FullyQualifiedName = [||]; Enumerable = false; Name = "Empty"
                  Members = [||]; TypeParameters = [||]; Documentation = []
                  Heritage = Signal.source ValueNone }
            Expect.equal (b.Build().Heritage.Extends) [] "default heritage has no extends"
    ]

// ---------------------------------------------------------------------------
// SClassBuilder — Constructors + Members + Heritage default to Implements=None.
// ---------------------------------------------------------------------------
let private classTests =
    testList "SClassBuilder" [

        testCase "Build resolves constructors, members, and defaults absent Heritage" <| fun _ ->
            let ctor: SConstructorBuilder =
                { Parameters = [| someSlot (paramBuilder "v" 1) |]; Documentation = [] }
            let b: SClassBuilder =
                { Source = modSig "classes"
                  FullyQualifiedName = [| "C" |]
                  Enumerable = false
                  Name = "C"
                  Constructors = [| someSlot ctor |]
                  Members = [||]
                  TypeParameters = [||]
                  Heritage = Signal.source ValueNone }
            let built = b.Build()
            Expect.equal built.Source (Some "classes") "Source unwrapped"
            Expect.equal built.Constructors.Length 1 "one constructor built"
            Expect.equal (built.Constructors.[0].Parameters |> List.map (fun p -> p.Name)) [ "v" ] "ctor param built"
            Expect.equal built.Heritage.Implements None "default heritage Implements = None"
            Expect.equal built.Heritage.Extends [] "default heritage Extends = []"
    ]

// ---------------------------------------------------------------------------
// Pure-TypeSignal builders that resolve scalar / array slots directly.
// ---------------------------------------------------------------------------
let private scalarTypeBuilderTests =
    testList "Index/Substitution/Conditional/Union/Intersection/Predicate/Query/TemplateLiteral/IndexAccess" [

        testCase "SIndexBuilder resolves its single Type slot" <| fun _ ->
            let b: SIndexBuilder = { Type = TypeSignal.ofKey (tk 5) }
            Expect.equal (b.Build().Type) (tk 5) "Type resolved"

        testCase "SIndexAccessTypeBuilder resolves Object + Index" <| fun _ ->
            let b: SIndexAccessTypeBuilder = { Object = TypeSignal.ofKey (tk 1); Index = TypeSignal.ofKey (tk 2) }
            let built = b.Build()
            Expect.equal built.Object (tk 1) "Object resolved"
            Expect.equal built.Index (tk 2) "Index resolved"

        testCase "SSubstitutionTypeBuilder resolves Base + Constraint" <| fun _ ->
            let b: SSubstitutionTypeBuilder = { Base = TypeSignal.ofKey (tk 3); Constraint = TypeSignal.ofKey (tk 4) }
            let built = b.Build()
            Expect.equal built.Base (tk 3) "Base resolved"
            Expect.equal built.Constraint (tk 4) "Constraint resolved"

        testCase "SConditionalTypeBuilder resolves all four slots" <| fun _ ->
            let b: SConditionalTypeBuilder =
                { Check = TypeSignal.ofKey (tk 1); Extends = TypeSignal.ofKey (tk 2)
                  True = TypeSignal.ofKey (tk 3); False = TypeSignal.ofKey (tk 4) }
            let built = b.Build()
            Expect.equal built.Check (tk 1) "Check"
            Expect.equal built.Extends (tk 2) "Extends"
            Expect.equal built.True (tk 3) "True"
            Expect.equal built.False (tk 4) "False"

        testCase "STypeUnionBuilder resolves the whole Types array in order" <| fun _ ->
            let b: STypeUnionBuilder =
                { Types = [| TypeSignal.ofKey (tk 1); TypeSignal.ofKey (tk 2); TypeSignal.ofKey (tk 3) |] }
            Expect.equal (b.Build().Types) [ tk 1; tk 2; tk 3 ] "all union members, in order"

        testCase "STypeIntersectionBuilder resolves the whole Types array in order" <| fun _ ->
            let b: STypeIntersectionBuilder =
                { Types = [| TypeSignal.ofKey (tk 4); TypeSignal.ofKey (tk 5) |] }
            Expect.equal (b.Build().Types) [ tk 4; tk 5 ] "all intersection members, in order"

        testCase "SPredicateBuilder resolves Type + carries name/assertion" <| fun _ ->
            let b: SPredicateBuilder =
                { ParameterName = "x"; Type = TypeSignal.ofKey (tk 7); IsAssertion = true }
            let built = b.Build()
            Expect.equal built.ParameterName "x" "name"
            Expect.equal built.Type (tk 7) "Type resolved"
            Expect.isTrue built.IsAssertion "IsAssertion"

        testCase "STypeQueryBuilder resolves Type + FQN" <| fun _ ->
            let b: STypeQueryBuilder = { FullyQualifiedName = [| "typeof"; "Foo" |]; Type = TypeSignal.ofKey (tk 8) }
            let built = b.Build()
            Expect.equal built.FullyQualifiedName [ "typeof"; "Foo" ] "FQN -> list"
            Expect.equal built.Type (tk 8) "Type resolved"

        testCase "STemplateLiteralTypeBuilder resolves Texts + interpolated Types in order" <| fun _ ->
            let b: STemplateLiteralTypeBuilder =
                { Texts = [| "on"; "" |]; Types = [| TypeSignal.ofKey (tk 1) |] }
            let built = b.Build()
            Expect.equal built.Texts [ "on"; "" ] "Texts -> list"
            Expect.equal built.Types [ tk 1 ] "interpolated Types resolved"
    ]

// ---------------------------------------------------------------------------
// Tuple builders — STupleElementTypeBuilder / STupleElementBuilder DU / STupleBuilder.
// ---------------------------------------------------------------------------
let private tupleTests =
    testList "Tuple builders" [

        testCase "STupleElementTypeBuilder resolves Type + flags" <| fun _ ->
            let b: STupleElementTypeBuilder =
                { Type = TypeSignal.ofKey (tk 1); IsOptional = true; IsRest = false }
            let built = b.Build()
            Expect.equal built.Type (tk 1) "Type resolved"
            Expect.isTrue built.IsOptional "IsOptional"

        testCase "STupleElementBuilder.FixedLabeled / Variadic / Fixed dispatch correctly" <| fun _ ->
            let elemTy = { Type = TypeSignal.ofKey (tk 2); IsOptional = false; IsRest = false }
            match (STupleElementBuilder.FixedLabeled ("x", elemTy)).Build() with
            | TsTupleElement.FixedLabeled (name, t) ->
                Expect.equal name "x" "label"
                Expect.equal t.Type (tk 2) "labeled element type resolved"
            | other -> failwithf "expected FixedLabeled, got %A" other

            match (STupleElementBuilder.Variadic (TypeSignal.ofKey (tk 3))).Build() with
            | TsTupleElement.Variadic key -> Expect.equal key (tk 3) "variadic key resolved"
            | other -> failwithf "expected Variadic, got %A" other

            match (STupleElementBuilder.Fixed elemTy).Build() with
            | TsTupleElement.Fixed t -> Expect.equal t.Type (tk 2) "fixed element resolved"
            | other -> failwithf "expected Fixed, got %A" other

        testCase "STupleBuilder builds all elements + carries shape metadata" <| fun _ ->
            let e1 = STupleElementBuilder.Fixed { Type = TypeSignal.ofKey (tk 1); IsOptional = false; IsRest = false }
            let e2 = STupleElementBuilder.Variadic (TypeSignal.ofKey (tk 2))
            let b: STupleBuilder =
                { IsReadOnly = true; FixedLength = 2; MinRequired = 1; Types = [| e1; e2 |] }
            let built = b.Build()
            Expect.isTrue built.IsReadOnly "IsReadOnly"
            Expect.equal built.FixedLength 2 "FixedLength"
            Expect.equal built.MinRequired 1 "MinRequired"
            Expect.equal (built.Types |> List.map (fun t -> t.Type)) [ tk 1; tk 2 ] "element types resolved, in order"
    ]

// ---------------------------------------------------------------------------
// SAliasBuilder — type-alias with inlined type parameters.
// ---------------------------------------------------------------------------
let private aliasTests =
    testList "SAliasBuilder" [

        testCase "Build resolves Source, Type, and inlined type-params" <| fun _ ->
            let inl: InlinedSTypeParameterBuilder =
                { Type = tk 5; TypeParameter = { Name = "T"; Constraint = ValueNone; Default = ValueNone; Documentation = [] } }
            let b: SAliasBuilder =
                { Source = modSig "aliases"
                  FullyQualifiedName = [| "MyAlias" |]
                  Name = "MyAlias"
                  Type = TypeSignal.ofKey (tk 3)
                  TypeParameters = [| someSlot inl; noneSlot () |]
                  Documentation = [] }
            let built = b.Build()
            Expect.equal built.Source (Some "aliases") "Source unwrapped"
            Expect.equal built.Type (tk 3) "aliased Type resolved"
            Expect.equal (built.TypeParameters |> List.map (fun (k, tp) -> k, tp.Name)) [ (tk 5, "T") ] "only filled inlined type-param"
    ]

// ---------------------------------------------------------------------------
// SModuleBuilder — Exports is a PendingSignal<STsExportDeclaration> array
// (= Signal<STsExportDeclaration voption>); resolve drops the unfulfilled ones.
// ---------------------------------------------------------------------------
let private moduleTests =
    testList "SModuleBuilder / STsExportDeclaration" [

        let variableExport name : STsExportDeclaration =
            STsExportDeclaration.Variable
                { Source = modSig "m"; FullyQualifiedName = [| name |]; Name = name
                  Type = TypeSignal.ofKey (tk 1); Documentation = [] }

        testCase "Build resolves Source, flags, and fulfilled exports (drops pending)" <| fun _ ->
            let exp1: PendingSignal<STsExportDeclaration> = Signal.source (ValueSome (variableExport "a"))
            let exp2: PendingSignal<STsExportDeclaration> = Signal.source ValueNone   // unfulfilled -> dropped
            let exp3: PendingSignal<STsExportDeclaration> = Signal.source (ValueSome (variableExport "b"))
            let b: SModuleBuilder =
                { Source = modSig "mymodule"
                  FullyQualifiedName = [| "M" |]
                  Name = "M"
                  IsNamespace = true
                  IsRecursive = false
                  Exports = [| exp1; exp2; exp3 |] }
            let built = b.Build()
            Expect.equal built.Source (Some "mymodule") "Source unwrapped"
            Expect.isTrue built.IsNamespace "IsNamespace"
            let names =
                built.Exports
                |> List.choose (function TsExportDeclaration.Variable v -> Some v.Name | _ -> None)
            Expect.equal names [ "a"; "b" ] "only fulfilled exports, pending dropped, order preserved"

        testCase "late-bound export: a pending export Set after construction appears in Build" <| fun _ ->
            let exp: PendingSignal<STsExportDeclaration> = Signal.pending ()
            let b: SModuleBuilder =
                { Source = modSig "m"; FullyQualifiedName = [||]; Name = "M"
                  IsNamespace = false; IsRecursive = false; Exports = [| exp |] }
            Expect.equal (b.Build().Exports) [] "empty before fulfill"
            exp.Set (ValueSome (variableExport "late"))
            let names =
                b.Build().Exports
                |> List.choose (function TsExportDeclaration.Variable v -> Some v.Name | _ -> None)
            Expect.equal names [ "late" ] "late-bound export appears after fulfill"
    ]

// ---------------------------------------------------------------------------
// SType top-level DU + STsExportDeclaration — dispatch to the right TsType /
// TsExportDeclaration case (spot-checks of the big match).
// ---------------------------------------------------------------------------
let private topLevelDuTests =
    testList "SType / STsExportDeclaration dispatch" [

        testCase "SType.Primitive passes the primitive through unchanged" <| fun _ ->
            match (SType.Primitive TypeKindPrimitive.String).Build() with
            | TsType.Primitive p -> Expect.equal p TypeKindPrimitive.String "primitive carried"
            | other -> failwithf "expected TsType.Primitive, got %A" other

        testCase "SType.Union builds a TsType.Union with resolved members" <| fun _ ->
            let u: STypeUnionBuilder = { Types = [| TypeSignal.ofKey (tk 1); TypeSignal.ofKey (tk 2) |] }
            match (SType.Union u).Build() with
            | TsType.Union union -> Expect.equal union.Types [ tk 1; tk 2 ] "union members resolved"
            | other -> failwithf "expected TsType.Union, got %A" other

        testCase "SType.Array wraps the built TypeReference in TsType.Array" <| fun _ ->
            let r: STypeReferenceBuilder =
                { Type = TypeSignal.ofKey (tk 5); TypeArguments = [||]; ResolvedType = ValueNone }
            match (SType.Array r).Build() with
            | TsType.Array (TsType.TypeReference tr) -> Expect.equal tr.Type (tk 5) "array element ref resolved"
            | other -> failwithf "expected TsType.Array(TypeReference _), got %A" other

        testCase "STsExportDeclaration.Function wraps in an Overloadable construct" <| fun _ ->
            let f: SFunctionBuilder =
                { Source = modSig "m"; FullyQualifiedName = [| "f" |]; Name = "f"; IsDeclared = true
                  Type = TypeSignal.ofKey (tk 1); Parameters = [||]; TypeParameters = [||]
                  Documentation = []; SignatureKey = TypeSignal.ofKey (tk 0) }
            match (STsExportDeclaration.Function f).Build() with
            | TsExportDeclaration.Function construct ->
                Expect.equal construct.Values.Length 1 "single NoOverloads function"
                Expect.equal construct.Values.[0].Name "f" "function name carried"
            | other -> failwithf "expected TsExportDeclaration.Function, got %A" other
    ]

// ---------------------------------------------------------------------------
// Cross-cutting LATE-BINDING demonstration: a nested TypeSignal shared between
// a builder and an external dispatcher, filled via FulfillWith AFTER the builder
// graph is constructed. This is the canonical signal-driven assembly pattern.
// ---------------------------------------------------------------------------
let private lateBindingTests =
    testList "late-binding via signals (FulfillWith)" [

        testCase "a parameter's Type pending-slot fulfilled via FulfillWith is seen by Build" <| fun _ ->
            // resolved key arrives from an upstream source signal (dispatcher).
            let upstream = Signal.source (tk 50)
            let paramType = TypeSignal.pending ()
            let p: SParameterBuilder =
                { Name = "x"; IsOptional = false; IsSpread = false; Type = paramType; Documentation = [] }
            let m: SMethodBuilder =
                { Name = "m"; Parameters = [| someSlot p |]; Type = TypeSignal.ofKey (tk 0)
                  IsOptional = false; IsStatic = false; Documentation = [] }
            // dispatcher fulfills the pending param type from upstream
            paramType |> Signal.fulfillWith (fun () -> upstream.Value)
            let built = m.Build()
            Expect.equal (built.Parameters.[0].Type) (tk 50) "Build sees the FulfillWith-resolved param type"
            // and a later upstream change is reflected on the NEXT Build (reactive slot)
            upstream.Set (tk 60)
            Expect.equal ((m.Build()).Parameters.[0].Type) (tk 60) "reactive: later upstream change seen on next Build"
    ]

// ---------------------------------------------------------------------------
// Aggregated suite — registered in Program.fs and the fsproj.
// ---------------------------------------------------------------------------
let reactiveBuildersTests =
    testList "ReactiveBuilders" [
        enumCaseTests
        enumTypeTests
        variableTests
        parameterTests
        typeParameterTests
        memberLeafTests
        methodTests
        functionTests
        signatureTests
        memberDuTests
        typeReferenceTests
        heritageTests
        interfaceTests
        classTests
        scalarTypeBuilderTests
        tupleTests
        aliasTests
        moduleTests
        topLevelDuTests
        lateBindingTests
    ]
