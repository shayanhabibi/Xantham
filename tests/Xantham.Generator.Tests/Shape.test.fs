/// The shape tier's nano-pass payoff: each pass exercised on a hand-built model, asserted on
/// the output model and its findings. No wire, no fixtures.
module Xantham.Generator.Tests.ShapeTests

open System.Text.Json.Nodes
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.Generator
open Xantham.Generator.Shape

/// A string-literal type carrying its payload.
let private stringLiteral (id: int) (text: string) =
    Build.facts
        { Build.typeResponse id TypeFlags.StringLiteral with
            Value = JsonValue.Create text }

/// A numeric-literal type carrying its payload.
let private numberLiteral (id: int) (value: float) =
    Build.facts
        { Build.typeResponse id TypeFlags.NumberLiteral with
            Value = JsonValue.Create value }

/// An intrinsic carrying nothing but its flag - `bigint`, `object`, `symbol`, and the
/// type-level computations (`` `on${string}` ``, `Uppercase<T>`) the checker hands back with no
/// structure to read.
let private intrinsic (id: int) (flags: TypeFlags) =
    Build.facts (Build.typeResponse id flags)

/// A tuple type over the given component ids, with one element flag each.
let private tuple (id: int) (components: int list) (flags: ElementFlags list) =
    { Build.facts
        { Build.typeResponse id TypeFlags.Object with
            IsTupleType = ValueSome true } with
        TypeArguments = components
        TupleElements = flags }

/// An object type carrying `Array`'s member set over a numeric index signature, the way an
/// interface extending `Array<T>` or an intersection over one reaches the shaper. `extra` names
/// members beyond the array's own.
let private arrayShaped (id: int) (name: string) (element: int) (extra: string list) =
    let member' index memberName =
        Build.resolvedMember (Build.symbol (id * 100 + index) memberName SymbolFlags.Property) element

    { Build.facts
          { Build.typeResponse id TypeFlags.Object with
              IsTupleType = ValueSome false } with
        SymbolName = Some name
        Members =
            [ "concat"
              "every"
              "filter"
              "forEach"
              "indexOf"
              "join"
              "lastIndexOf"
              "length"
              "map"
              "reduce"
              "reduceRight"
              "slice"
              "some" ]
            @ extra
            |> List.mapi member'
        IndexInfos =
            [ {
                  KeyTypeId = 2
                  ValueTypeId = element
                  IsReadonly = false
              } ] }

/// A type parameter type, named by its own symbol the way the resolve tier records it.
let private typeParam (id: int) (name: string) =
    { Build.facts (Build.typeResponse id TypeFlags.TypeParameter) with SymbolName = Some name }

/// `keyof X`: an index type carrying its operand as its target, the way the checker hands one
/// back when it cannot finish it.
let private keyOf (id: int) (operand: int) =
    Build.facts { Build.typeResponse id TypeFlags.Index with Target = ValueSome operand }

/// `X[K]`: an indexed access over an object type and an index type.
let private indexedAccess (id: int) (objectId: int) (keyId: int) =
    Build.facts
        { Build.typeResponse id TypeFlags.IndexedAccess with
            ObjectType = ValueSome objectId
            IndexType = ValueSome keyId }

/// A type the compiler's own lib declares, under the name and arguments it declares it with.
let private libType (id: int) (name: string) (arguments: int list) =
    { Build.facts (Build.typeResponse id TypeFlags.Object) with
        Origin = CompilerLib
        SymbolName = Some name
        TypeArguments = arguments }

/// `P & { marker }`: a branding intersection, given the ids of its constituents.
let private intersection (id: int) (members: int list) =
    { Build.facts (Build.typeResponse id TypeFlags.Intersection) with IntersectionMembers = members }

/// An object carrying one property, for the marker half of a brand.
let private marker (id: int) (name: string) (valueType: int) =
    { Build.facts (Build.typeResponse id TypeFlags.Object) with
        Members = [ Build.resolvedMember (Build.symbol (id * 10) name SymbolFlags.Property) valueType ] }

/// A generic declaration: its own target, holding its parameters as its arguments.
let private genericDecl (id: int) (parameters: int list) (members: ResolvedMember list) =
    { Build.facts
        { Build.typeResponse id TypeFlags.Object with
            Target = ValueSome id
            TypeParameters = ValueSome(List.toArray parameters) } with
        TypeArguments = parameters
        Members = members }

/// A generic alias over an intersection, paired with one application of it, under a shared
/// operand table (§11.4, `tests/fixtures/hoist-conditional-lab`):
///
/// * 20 - the alias's parameter, `TNodeType`
/// * 30 - `{ isNode: boolean }`, an operand instantiation leaves alone
/// * 31 - the conditional operand, deferred on 20
/// * 32 - `{ tag: TNodeType }`; 41 - `{ tag: number }`
/// * 40 - what the conditional resolved to
/// * 50 - the declaration, named `Node`; 60 - the application, exported as `seed`
///
/// The declaration carries no members and the application carries both, which is the shape the
/// checker hands over whenever an alias body intersects a conditional.
let private conditionalAliasModel (declaredOperands: int list) (appliedOperands: int list) =
    let table =
        [ Build.facts (Build.typeResponse 20 TypeFlags.TypeParameter)
          marker 30 "isNode" 3
          Build.facts (Build.typeResponse 31 TypeFlags.Conditional)
          marker 32 "tag" 20
          marker 40 "toVar" 1
          marker 41 "tag" 2
          { Build.facts { Build.typeResponse 50 TypeFlags.Intersection with AliasSymbol = ValueSome 100 } with
              AliasTypeArguments = [ 20 ]
              IntersectionMembers = declaredOperands }
          { Build.facts { Build.typeResponse 60 TypeFlags.Intersection with AliasSymbol = ValueSome 100 } with
              IntersectionMembers = appliedOperands
              Members =
                [ Build.resolvedMember (Build.symbol 300 "isNode" SymbolFlags.Property) 3
                  Build.resolvedMember (Build.symbol 301 "tag" SymbolFlags.Property) 2 ] } ]

    { Build.shapeModel (table @ Build.primitives) with
        Harvest = { Exports = [ Build.export "seed" (Build.symbol 400 "seed" SymbolFlags.BlockScopedVariable) ] }
        ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 60 } ]
        DeclNames = Map.ofList [ 50, "Node" ] }

[<Tests>]
let typeRefTests =
    testList "shape typeRef" [
        testCase "primitives map to F# primitives without findings" <| fun _ ->
            let model = Build.shapeModel Build.primitives

            for typeId, expected in [ 1, FsString; 2, FsFloat; 3, FsBool; 4, FsUnit ] do
                let reference, findings = Spec.typeRef Build.context model None "x" typeId
                Expect.equal reference expected $"type {typeId}"
                Expect.isEmpty findings $"type {typeId} findings"

        // The flags the tier used to answer with a bare `TypeFlagsNotMapped`. Each is asserted
        // on its key as well as its tier, because half the point of the work was that the
        // manifest stop overstating the damage: an exact mapping raises nothing at all, and a
        // lossy one names the construct and what was lost rather than a flag name.
        testCase "a template literal reads as the string it is at runtime (§4.11)" <| fun _ ->
            let model = Build.shapeModel (intrinsic 10 TypeFlags.TemplateLiteral :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsString "`on${string}` is a string"
            Expect.equal (findings |> List.map _.Key) [ "TR037" ] "the template-literal finding, not TR014"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened: it accepts strings the pattern does not"
            Expect.stringContains (List.head findings).Message "pattern" "and the message says which half was lost"

        testCase "an intrinsic string mapping reads as string too" <| fun _ ->
            let model = Build.shapeModel (intrinsic 10 TypeFlags.StringMapping :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsString "Uppercase<T> is a string"
            Expect.equal (findings |> List.map _.Key) [ "TR038" ] "named as the mapping it is"

        testCase "bigint maps exactly and raises nothing" <| fun _ ->
            // Fable 5 compiles F# `bigint` to the native JavaScript BigInt (the run gate reads
            // that off node). An exact mapping must not appear in the manifest at all.
            let model = Build.shapeModel (intrinsic 10 TypeFlags.BigInt :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsBigInt "bigint"
            Expect.isEmpty findings "nothing is lost, so nothing is reported"

        testCase "a bigint literal widens to bigint, as its string and number peers do" <| fun _ ->
            let model = Build.shapeModel (intrinsic 10 TypeFlags.BigIntLiteral :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsBigInt "2n"
            Expect.equal (findings |> List.map _.Key) [ "TR039" ] "the literal's own widening"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "TypeScript's object maps to obj, and says that is still a widening" <| fun _ ->
            let model = Build.shapeModel (intrinsic 10 TypeFlags.NonPrimitive :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "there is no closer F# type"
            Expect.equal (findings |> List.map _.Key) [ "TR040" ] "reported as the mapping it is, not as an unmapped flag"
            Expect.stringContains (List.head findings).Message "primitives" "obj admits what object excludes"

        testCase "symbol and unique symbol widen, each naming its own construct" <| fun _ ->
            // Fable.Core 5.2.0 declares no `JS.Symbol`, checked against the shipped assembly.
            let model =
                Build.shapeModel (
                    intrinsic 10 TypeFlags.ESSymbol
                    :: intrinsic 11 TypeFlags.UniqueESSymbol
                    :: Build.primitives
                )

            let plain, plainFindings = Spec.typeRef Build.context model None "x" 10
            let unique, uniqueFindings = Spec.typeRef Build.context model None "x" 11

            Expect.equal plain FsObj "symbol"
            Expect.equal unique FsObj "unique symbol"
            Expect.equal (plainFindings |> List.map _.Key) [ "TR041" ] "symbol"
            Expect.equal (uniqueFindings |> List.map _.Key) [ "TR042" ] "unique symbol"

            Expect.stringContains
                (List.head uniqueFindings).Message
                "unique"
                "a unique symbol loses its identity on top of its type"

        testCase "a union with undefined hoists to option with an ergonomic finding" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption FsString) "string | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "one ergonomic finding"

        testCase "a union of null and undefined alone maps to unit, widened" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 5; 6 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsUnit "null | undefined"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a union of several non-null members is erased (D4)" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 1; 2; 5 ] }

            let model = Build.shapeModel (union :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption(FsErasedUnion [ FsString; FsFloat ])) "string | number | undefined"

            Expect.equal
                (findings |> List.map _.Tier)
                [ Ergonomic ]
                "the hoist is reported; the erased union is not a widening"

        testCase "a union wider than the erased arity still widens to obj" <| fun _ ->
            // Five distinct arms: `U5` exists in Fable, but past four the consumer is doing
            // runtime tests the type no longer helps them write.
            let named id name =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with SymbolName = Some name }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with
                    UnionMembers = [ 1; 2; 3; 20; 21; 22 ] }

            let model =
                Build.shapeModel (union :: named 20 "A" :: named 21 "B" :: named 22 "C" :: Build.primitives)

            let model =
                { model with DeclNames = [ 20, "A"; 21, "B"; 22, "C" ] |> Map.ofList }

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "six members, five arms"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a fixed tuple maps to an F# tuple (D7)" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1; 2 ] [ ElementFlags.Required; ElementFlags.Required ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsTuple [ FsString; FsFloat ]) "[string, number]"
            Expect.isEmpty findings "Fable compiles both to the same JS array"

        testCase "an optional tail element arrives already hoisted to option" <| fun _ ->
            // The checker hands `[number, number?]` over as `number` and `number | undefined`,
            // so D1's hoist does the work and D7 imposes nothing of its own.
            let optionalTail =
                { Build.facts (Build.typeResponse 11 TypeFlags.Union) with UnionMembers = [ 2; 5 ] }

            let model =
                Build.shapeModel (
                    tuple 10 [ 2; 11 ] [ ElementFlags.Required; ElementFlags.Optional ]
                    :: optionalTail
                    :: Build.primitives
                )

            let reference, _ = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsTuple [ FsFloat; FsOption FsFloat ]) "[number, number?]"

        testCase "a rest element leaves no tuple form, so it widens to an array" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1; 2 ] [ ElementFlags.Required; ElementFlags.Rest ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsArray FsObj) "components disagree, so the element is obj"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a spread element contributes its own element to the widened array" <| fun _ ->
            // `[...Chapters]` spreads an array, so the widened array is that array, not one over it.
            let model =
                Build.shapeModel (
                    tuple 10 [ 30 ] [ ElementFlags.Variadic ]
                    :: arrayShaped 30 "Chapters" 1 []
                    :: Build.primitives
                )

            let reference, _ = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsArray FsString) "one array level, not two"

        testCase "a one-element tuple has no F# form either" <| fun _ ->
            let model =
                Build.shapeModel (tuple 10 [ 1 ] [ ElementFlags.Required ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsArray FsString) "widened to its element"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "a type parameter in scope names its variable (§4.9)" <| fun _ ->
            let model =
                { Build.shapeModel (typeParam 20 "T" :: Build.primitives) with
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 20

            Expect.equal reference (FsTypeVar "T") "'T"
            Expect.isEmpty findings "a bound variable costs nothing"

        testCase "a type parameter of some other declaration is not in scope" <| fun _ ->
            let model = Build.shapeModel (typeParam 20 "T" :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 20

            Expect.equal reference FsObj "nothing here binds T"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "an out-of-scope type parameter widens to its constraint, not to obj" <| fun _ ->
            // `Ai<obj>` does not compile against `'AiModelList :> AiModelListType`: where the
            // declaration bound a constraint, obj is not merely loose but wrong.
            let bounded = { typeParam 20 "T" with Constraint = Some 60 }
            let timer = Build.facts (Build.typeResponse 60 TypeFlags.Object)

            let model =
                { Build.shapeModel (bounded :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Timer" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 20

            Expect.equal reference (FsNamed "Timer") "the tightest thing still true of T"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "still a widening, just a smaller one"

        testCase "an out-of-scope parameter bound to a generic still widens to obj" <| fun _ ->
            // A generic constraint would need an arity this position cannot supply.
            let bounded = { typeParam 20 "T" with Constraint = Some 30 }

            let model =
                { Build.shapeModel (bounded :: genericDecl 30 [ 21 ] [] :: typeParam 21 "E" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 20

            Expect.equal reference FsObj "no arity to write Box at"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "keyof over an operand not in scope widens to obj" <| fun _ ->
            // The idiom needs a `'T` to be taken over; without one there is nothing to phantom
            // the key with, and an unphantomed key is just a string.
            let model = Build.shapeModel (keyOf 40 20 :: typeParam 20 "T" :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 40

            Expect.equal reference FsObj "no operand, no keyof"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened"

        testCase "keyof over an in-scope operand reads as keyof of it" <| fun _ ->
            let model =
                { Build.shapeModel (keyOf 40 20 :: typeParam 20 "T" :: Build.primitives) with
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 40

            Expect.equal reference (FsApp("keyof", [ FsTypeVar "T" ])) "keyof<'T>"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "the support idiom is ergonomic, not a widening"

        testCase "an indexed access no key variable selects has no F# form" <| fun _ ->
            // `T[keyof T]` - the value-of idiom. Nothing names the value type, so there is no
            // `'R` to write and no honest alternative to obj.
            let model =
                { Build.shapeModel (
                      indexedAccess 41 20 40 :: keyOf 40 20 :: typeParam 20 "T" :: Build.primitives
                  ) with
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 41

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "and said so"

        testCase "an instantiation of a declared generic is written as an application" <| fun _ ->
            // A reference, as the wire reports an instantiation of an interface or class; an
            // anonymous type instantiated elsewhere carries a target too, but no arguments.
            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ 1 ] }

            let model =
                { Build.shapeModel (genericDecl 30 [ 20 ] [] :: instantiation :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 31

            Expect.equal reference (FsApp("Box", [ FsString ])) "Box<string>, not the expansion"
            Expect.isEmpty findings "an application is exact"

        testCase "an argument that only structurally satisfies its bound is written as the bound" <| fun _ ->
            // F# subtyping is nominal where TypeScript's is structural. An argument that merely
            // has the constraint's members is FS0001 at the application, so it is written as the
            // constraint - the repair an argument that widened to obj already gets. This is the
            // hole §4.4's `inherit` opened: before it, no generated file applied such a type.
            let member' id name = Build.resolvedMember (Build.symbol id name SymbolFlags.Property) 2

            let marker =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with Members = [ member' 600 "at" ] }

            let lookalike =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with Members = [ member' 700 "at" ] }

            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ 70 ] }

            let model =
                { Build.shapeModel (
                      genericDecl 30 [ 20 ] []
                      :: instantiation
                      :: marker
                      :: lookalike
                      :: { typeParam 20 "T" with Constraint = Some 60 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Marker"; 70, "Lookalike" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 31

            Expect.equal reference (FsApp("Holder", [ FsNamed "Marker" ])) "the argument becomes the bound it cannot state"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Message))
                ("TR044",
                 Widened,
                 "Lookalike does not inherit Holder's constraint Marker; the argument is written as the constraint")
                "and the substitution is owned by name"

        testCase "an argument that inherits its bound is applied as itself" <| fun _ ->
            // The other side of the same gate: `satisfies` walks the bases `shape-interfaces`
            // turns into `inherit` lines, so a real subtype costs nothing at the application.
            let member' id name = Build.resolvedMember (Build.symbol id name SymbolFlags.Property) 2

            let marker =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with Members = [ member' 600 "at" ] }

            let subtype =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    BaseTypes = [ 60 ]
                    Members = [ member' 700 "at" ] }

            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ 70 ] }

            let model =
                { Build.shapeModel (
                      genericDecl 30 [ 20 ] []
                      :: instantiation
                      :: marker
                      :: subtype
                      :: { typeParam 20 "T" with Constraint = Some 60 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Marker"; 70, "Subtype" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 31

            Expect.equal reference (FsApp("Holder", [ FsNamed "Subtype" ])) "the argument stands"
            Expect.isEmpty findings "a nominal subtype needs no repair"

        // Wave three, lane K. The substitution reached named types and applications only, so a
        // primitive, a tuple or an array against a still-written constraint arrived as itself
        // and read FS0001 at the consumer. `constraint-arg-lab` pins the same three live.
        testCase "a sealed argument is written as the bound it cannot inherit" <| fun _ ->
            let member' id name = Build.resolvedMember (Build.symbol id name SymbolFlags.Property) 2

            let marker =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with Members = [ member' 600 "length" ] }

            let tuple =
                { Build.facts
                    { Build.typeResponse 71 TypeFlags.Object with
                        IsTupleType = ValueSome true } with
                    TypeArguments = [ 1; 2 ] }

            let instantiation argument typeId =
                { Build.facts
                    { Build.typeResponse typeId TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ argument ] }

            let model =
                { Build.shapeModel (
                      genericDecl 30 [ 20 ] []
                      :: instantiation 1 31
                      :: instantiation 71 32
                      :: marker
                      :: tuple
                      :: { typeParam 20 "T" with Constraint = Some 60 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Lengthy" ] }

            let primitive, primitiveFindings = Spec.typeRef Build.context model None "x" 31
            let tupled, tupleFindings = Spec.typeRef Build.context model None "x" 32

            Expect.equal primitive (FsApp("Holder", [ FsNamed "Lengthy" ])) "string is written as the bound"
            Expect.equal tupled (FsApp("Holder", [ FsNamed "Lengthy" ])) "and so is a tuple"

            Expect.contains
                (primitiveFindings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Message))
                ("TR044",
                 Widened,
                 "string does not inherit Holder's constraint Lengthy; the argument is written as the constraint")
                "the primitive is named by its F# spelling"

            Expect.contains
                (tupleFindings |> List.map _.Message)
                "string * float does not inherit Holder's constraint Lengthy; the argument is written as the constraint"
                "and so is the tuple"

        testCase "a variable bound by the same constraint is applied as itself" <| fun _ ->
            // The negative that keeps `EventListenerOrEventListenerObject<'EventType>` exact:
            // `'EventType :> Event` already satisfies the parameter it is passed to.
            let event = Build.facts (Build.typeResponse 60 TypeFlags.Object)
            let variable = { typeParam 21 "EventType" with Constraint = Some 60 }

            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ 21 ] }

            let model =
                { Build.shapeModel (
                      genericDecl 30 [ 20 ] []
                      :: instantiation
                      :: event
                      :: variable
                      :: { typeParam 20 "T" with Constraint = Some 60 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Listener"; 60, "Event" ]
                    TypeVars = Map.ofList [ 21, "EventType" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 31

            Expect.equal reference (FsApp("Listener", [ FsTypeVar "EventType" ])) "the variable stands"
            Expect.isEmpty findings "nothing to repair"

        testCase "a generic declaration named at a reference re-applies its parameters" <| fun _ ->
            // `map(next: T): Box<T>` refers to the declaration itself; F# has no bare `Box`.
            let model =
                { Build.shapeModel (genericDecl 30 [ 20 ] [] :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ]
                    TypeVars = Map.ofList [ 20, "T" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 30

            Expect.equal reference (FsApp("Box", [ FsTypeVar "T" ])) "Box<'T>"
            Expect.isEmpty findings "exact"

        testCase "a named literal union references its declaration, hoist intact" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 8; 5 ] }

            let model =
                { Build.shapeModel (union :: stringLiteral 7 "ms" :: stringLiteral 8 "s" :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "TimeUnit" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsOption(FsNamed "TimeUnit")) "the classified union's name"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "only the hoist"

        testCase "an array of a generated declaration reads as an F# array" <| fun _ ->
            let array =
                { Build.facts (Build.typeResponse 11 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "Array"
                    TypeArguments = [ 20 ] }

            let element = Build.facts (Build.typeResponse 20 TypeFlags.Object)

            let model =
                { Build.shapeModel [ array; element ] with DeclNames = Map.ofList [ 20, "Timer" ] }

            Expect.equal
                (Spec.typeRef Build.context model None "x" 11)
                (FsArray(FsNamed "Timer"), [])
                "Array<Timer> -> Timer[], whatever the lib group's disposition"

        testCase "an array under a name of its own reads as an F# array" <| fun _ ->
            // `interface Chapters extends Array<string> {}`: the checker hands the interface
            // `Array`'s member set and its numeric index signature, under the name the author
            // wrote.
            let model = Build.shapeModel (arrayShaped 30 "Chapters" 1 [] :: Build.primitives)

            Expect.equal
                (Spec.typeRef Build.context model None "x" 30)
                (FsArray FsString, [])
                "the element the index signature carries"

        testCase "an array intersected with a shape reports the members it drops" <| fun _ ->
            let other =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol 410 "kind" SymbolFlags.Property) 1
                          Build.resolvedMember (Build.symbol 411 "rank" SymbolFlags.Property) 2 ] }

            let intersected =
                { arrayShaped 40 "Tagged" 1 [ "kind"; "rank" ] with
                    Response = { Build.typeResponse 40 TypeFlags.Intersection with IsTupleType = ValueSome false }
                    IntersectionMembers = [ 42; 41 ] }

            let model =
                Build.shapeModel (intersected :: other :: arrayShaped 42 "ReadonlyArray" 1 [] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "Tagged" 40

            Expect.equal reference (FsArray FsString) "the element array"
            Expect.equal (findings |> List.map _.Key) [ "TR048" ] "one drop, counted"
            Expect.stringContains findings.Head.Message "2 member" "`kind` and `rank`"

        testCase "an indexable shape with none of Array's members is not an array" <| fun _ ->
            let register =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    SymbolName = Some "Register"
                    Members = [ Build.resolvedMember (Build.symbol 500 "length" SymbolFlags.Property) 2 ]
                    IndexInfos = [ { KeyTypeId = 2; ValueTypeId = 1; IsReadonly = false } ] }

            let model =
                { Build.shapeModel (register :: Build.primitives) with
                    DeclNames = Map.ofList [ 50, "Register" ] }

            Expect.equal
                (Spec.typeRef Build.context model None "x" 50)
                (FsNamed "Register", [])
                "a numeric index signature and `length` are not an array on their own"

        testCase "a member-less object reads as obj without claiming a declaration is missing"
        <| fun _ ->
            let anonymous =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with
                    SymbolName = Some "__type" }

            let named =
                { Build.facts (Build.typeResponse 61 TypeFlags.Object) with
                    SymbolName = Some "Env" }

            let model = Build.shapeModel (anonymous :: named :: Build.primitives)

            let _, anonymousFindings = Spec.typeRef Build.context model None "x" 60
            Expect.equal (anonymousFindings |> List.map _.Key) [ "TR047" ] "nothing was ever going to be named"

            let _, ownFindings = Spec.typeRef Build.context model None "Env" 61
            Expect.equal (ownFindings |> List.map _.Key) [ "TR047" ] "nor at the declaration of the name itself"

            // Read from somewhere else, the name is one the reader follows and this run owes
            // them a declaration for.
            let _, referenceFindings = Spec.typeRef Build.context model None "Holder.env" 61
            Expect.equal (referenceFindings |> List.map _.Key) [ "TR023" ] "a reference that leads nowhere"

        testCase "an anonymous callback reads as a delegate (D5)" <| fun _ ->
            let callback =
                { Build.facts (Build.typeResponse 12 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 300 "value" SymbolFlags.FunctionScopedVariable) 1 ]
                              4 ] }

            let model = Build.shapeModel (callback :: Build.primitives)

            Expect.equal
                (Spec.typeRef Build.context model None "x" 12)
                (FsDelegate([ FsString ], FsUnit), [])
                "(value: string) => void -> Action<string>"

        testCase "a polymorphic this return reads as the declaring type" <| fun _ ->
            let thisType =
                Build.facts
                    { Build.typeResponse 13 TypeFlags.TypeParameter with IsThisType = ValueSome true }

            let model = Build.shapeModel [ thisType ]
            let reference, findings = Spec.typeRef Build.context model (Some "Timer") "Timer.play()" 13

            Expect.equal reference (FsNamed "Timer") "chainable"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "ergonomic, not silent"

        testCase "an aliased object type references its generated declaration" <| fun _ ->
            let aliased = Build.facts (Build.typeResponse 20 TypeFlags.Object)

            let model =
                { Build.shapeModel [ aliased ] with DeclNames = Map.ofList [ 20, "Options" ] }

            Expect.equal (Spec.typeRef Build.context model None "x" 20) (FsNamed "Options", []) "alias reference"

        testCase "an external object type widens to obj and the finding names it" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with SymbolName = Some "RegExp" }

            let model = Build.shapeModel [ external ]
            let reference, findings = Spec.typeRef Build.context model None "x" 21

            Expect.equal reference FsObj "widened"

            match findings with
            | [ finding ] ->
                Expect.equal finding.Tier Widened "tier"
                Expect.stringContains finding.Message "RegExp" "the message says what was widened"
            | findings -> failtest $"expected one finding, got %A{findings}"

        testCase "a type id absent from the table is an escape, not an exception" <| fun _ ->
            let reference, findings = Spec.typeRef Build.context (Build.shapeModel []) None "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Escape ] "escape"

        testCase "a deliberately-not-followed type reports its reason" <| fun _ ->
            let model =
                { Build.shapeModel [] with NotFollowed = Map.ofList [ 99, "beyond the depth cutoff (12)" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 99

            Expect.equal reference FsObj "widened"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "widened, not escaped"
            Expect.stringContains findings.Head.Message "depth cutoff" "the reason is carried"

        testCase "a referenced group's type templates into its module, exact, no finding" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "RegExp" }

            let context =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups = Map.ofList [ "typescript/lib", Reference ] } }

            Expect.equal
                (Spec.typeRef context (Build.shapeModel [ external ]) None "x" 21)
                (FsNamed "TypeScript.Lib.RegExp", [])
                "the O7 template"

        testCase "an anonymous type in a referenced group still widens, with a finding" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 22 TypeFlags.Object) with Origin = Dependency "left-pad" }

            let context =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups = Map.ofList [ "left-pad", Reference ] } }

            let reference, findings = Spec.typeRef context (Build.shapeModel [ external ]) None "x" 22

            Expect.equal reference FsObj "nothing to template with"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "reported, not silent"

        // Wave five, lane R: O7's `map`. A group's table redirects the names it carries to a
        // binding somebody already wrote; every other name of the group keeps its widening.
        let mappedLib names =
            { Build.context with
                Config =
                    { GeneratorConfig.Default with
                        Groups = Map.ofList [ "typescript/lib", GroupDisposition.Map(Map.ofList names) ] } }

        testCase "a mapped group's type is written as the binding its table names" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "RegExp" }

            let context =
                mappedLib
                    [ "RegExp",
                      {
                          FSharpName = "System.Text.RegularExpressions.Regex"
                          Arity = 0
                      } ]

            Expect.equal
                (Spec.typeRef context (Build.shapeModel [ external ]) None "x" 21)
                (FsNamed "System.Text.RegularExpressions.Regex", [])
                "the destination, exact"

        testCase "a mapped generic is applied at the arity its destination takes" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "WeakRef"
                    TypeArguments = [ 1 ] }

            let context =
                mappedLib
                    [ "WeakRef",
                      {
                          FSharpName = "System.WeakReference"
                          Arity = 1
                      } ]

            let model = Build.shapeModel (Build.primitives @ [ external ])

            Expect.equal
                (Spec.typeRef context model None "x" 21)
                (FsApp("System.WeakReference", [ FsString ]), [])
                "the argument is shaped at its position"

        testCase "an arity the destination does not take widens rather than applying" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "Iterator"
                    TypeArguments = [ 1; 2; 4 ] }

            let context =
                mappedLib
                    [ "Iterator",
                      {
                          FSharpName = "System.Collections.Generic.IEnumerator"
                          Arity = 1
                      } ]

            let model = Build.shapeModel (Build.primitives @ [ external ])
            let reference, findings = Spec.typeRef context model None "x" 21

            Expect.equal reference FsObj "an application that would not compile is not written"
            Expect.equal (findings |> List.map _.Key) [ "TR053" ] "the arity mismatch is reported"
            Expect.stringContains findings.Head.Message "3 type arguments" "and says what the site applied"

        testCase "a name outside the table keeps the widening the group had" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 21 TypeFlags.Object) with
                    Origin = CompilerLib
                    SymbolName = Some "Response" }

            let context =
                mappedLib
                    [ "RegExp",
                      {
                          FSharpName = "System.Text.RegularExpressions.Regex"
                          Arity = 0
                      } ]

            let reference, findings = Spec.typeRef context (Build.shapeModel [ external ]) None "x" 21

            Expect.equal reference FsObj "mapping is per name"
            Expect.equal (findings |> List.map _.Key) [ "TR023" ] "reported as any unbound name is"

        testCase "an anonymous type in a mapped group widens, with a finding of its own" <| fun _ ->
            let external =
                { Build.facts (Build.typeResponse 22 TypeFlags.Object) with Origin = Dependency "left-pad" }

            let context =
                { Build.context with
                    Config =
                        { GeneratorConfig.Default with
                            Groups =
                                Map.ofList
                                    [ "left-pad",
                                      GroupDisposition.Map(
                                          Map.ofList
                                              [ "Padded",
                                                {
                                                    FSharpName = "Pad.Padded"
                                                    Arity = 0
                                                } ]
                                      ) ] } }

            let reference, findings = Spec.typeRef context (Build.shapeModel [ external ]) None "x" 22

            Expect.equal reference FsObj "the destination binds names and this type has none"
            Expect.equal (findings |> List.map _.Key) [ "TR052" ] "reported, not silent"

    ]

/// The Options-and-ansiRegex shape of the ansi-regex fixture, built by hand: an aliased
/// object type with one readonly boolean member, and a default-exported function taking it
/// optionally and returning an external type.
let private ansiRegexShaped () =
    let optionsSymbol = Build.symbol 100 "Options" SymbolFlags.TypeAlias
    let functionSymbol = Build.symbol 200 "ansiRegex" SymbolFlags.Function

    let optionsType =
        { Build.facts { Build.typeResponse 20 TypeFlags.Object with AliasSymbol = ValueSome 100 } with
            Members =
                [ { Build.resolvedMember (Build.symbol 101 "onlyFirst" SymbolFlags.Property) 3 with
                      ReadOnly = true } ] }

    let regExpType =
        { Build.facts (Build.typeResponse 21 TypeFlags.Object) with SymbolName = Some "RegExp" }

    let functionType =
        { Build.facts (Build.typeResponse 30 TypeFlags.Object) with
            CallSignatures =
                [ Build.signature
                      [ { Build.resolvedMember (Build.symbol 201 "options" SymbolFlags.FunctionScopedVariable) 20 with
                            Optional = true } ]
                      21 ] }

    { Build.shapeModel ([ optionsType; regExpType; functionType ] @ Build.primitives) with
        Harvest =
            { Exports =
                [ Build.export "Options" optionsSymbol
                  Build.export "default" functionSymbol ] }
        ExportTypes =
            Map.ofList
                [ 100, { Declared = Some 20; Value = None }
                  200, { Declared = None; Value = Some 30 } ] }

[<Tests>]
let shapePassTests =
    testList "shape passes" [
        testCase "name-exports names type-like exports by their declared type id" <| fun _ ->
            let model, findings = Build.runPass ExportNames.nameExports (ansiRegexShaped ())

            Expect.isEmpty findings "no findings"
            Expect.equal model.DeclNames (Map.ofList [ 20, "Options" ]) "the alias's type, not the function"

        testCase "shape-interfaces shapes the plain object alias" <| fun _ ->
            let named, _ = Build.runPass ExportNames.nameExports (ansiRegexShaped ())
            let model, findings = Build.runPass Interfaces.shapeInterfaces named

            Expect.isEmpty findings "nothing widened"

            match model.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Options" "name"

                match decl.Members with
                | [ FsProperty m ] ->
                    Expect.equal m.Name "onlyFirst" "member name"
                    Expect.equal m.Type FsBool "member type"
                    Expect.isTrue m.ReadOnly "readonly survives"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces declares a type whose only shape is an index signature" <| fun _ ->
            // `interface Bag { [key: string]: number }` has no properties at all, so before
            // §4.10's signatures were read it looked empty and abbreviated to obj.
            let bagSymbol = Build.symbol 100 "Bag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 2
                            IsReadonly = false } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "Bag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass ExportNames.nameExports model
            let shaped, _ = Build.runPass Interfaces.shapeInterfaces named

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.Name "Bag" "the index signature is shape enough to declare"

                match decl.Members with
                | [ FsIndexer indexer ] ->
                    Expect.equal indexer.Key FsString "key type"
                    Expect.equal indexer.Value FsFloat "value type"
                    Expect.isFalse indexer.ReadOnly "a writable signature keeps its setter"
                | members -> failtest $"expected one indexer, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces drops the setter for a readonly index signature" <| fun _ ->
            let bagSymbol = Build.symbol 100 "FrozenBag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 1
                            IsReadonly = true } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "FrozenBag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass ExportNames.nameExports model
            let shaped, _ = Build.runPass Interfaces.shapeInterfaces named

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsIndexer indexer ] -> Expect.isTrue indexer.ReadOnly "readonly survives to the emission"
                | members -> failtest $"expected one indexer, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "synthesize-paramobjects declines a type carrying an index signature" <| fun _ ->
            // An index signature has no name to bind a Create parameter to, so the type is
            // not plain data however many named members sit beside it.
            let bagSymbol = Build.symbol 100 "Bag" SymbolFlags.Interface

            let bagType =
                { Build.facts (Build.typeResponse 20 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 101 "label" SymbolFlags.Property) 1 ]
                    IndexInfos =
                        [ { KeyTypeId = 1
                            ValueTypeId = 2
                            IsReadonly = false } ] }

            let model =
                { Build.shapeModel (bagType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "Bag" bagSymbol ] }
                    ExportTypes = Map.ofList [ 100, { Declared = Some 20; Value = None } ] }

            let named, _ = Build.runPass ExportNames.nameExports model
            let shaped, _ = Build.runPass Interfaces.shapeInterfaces named
            let withCreate, _ = Build.runPass ParamObjects.synthesizeParamObjects shaped

            match withCreate.Decls with
            | [ FsInterface decl ] -> Expect.isEmpty decl.CreateOverloads "no Create for an indexed type"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-exports binds the default export under its declared name" <| fun _ ->
            let named, _ = Build.runPass ExportNames.nameExports (ansiRegexShaped ())
            let shaped, findings = Build.runPass Exports.shapeExports named
            let model, _ = Build.runPass Ordering.orderDeclarations shaped

            match model.Decls with
            | [ FsExports [ m ] ] ->
                Expect.equal m.Name "ansiRegex" "named after the declaring symbol, not 'default'"
                Expect.equal m.Binding ImportDefault "bound as the default import"

                match m.Body with
                | ExportFunction([ p ], FsObj) ->
                    Expect.isTrue p.Optional "optional"
                    Expect.equal p.Type (FsOption(FsNamed "Options")) "optional alias parameter"
                | body -> failtest $"expected a function with one parameter returning obj, got %A{body}"

                Expect.equal
                    (findings |> List.map (fun f -> f.Tier, f.Symbol))
                    [ Ergonomic, "ansiRegex(options)"; Widened, "ansiRegex()" ]
                    "the hoist and the widening are both findings"
            | decls -> failtest $"expected the Exports group, got %A{decls}"

        testCase "synthesize-anonymous names a parameter-position object literal by its path" <| fun _ ->
            let anonymous =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 401 "speed" SymbolFlags.Property) 2 ] }

            let makeType =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 402 "options" SymbolFlags.FunctionScopedVariable) 40 ]
                              4 ] }

            let model =
                { Build.shapeModel (anonymous :: makeType :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "make" (Build.symbol 400 "make" SymbolFlags.Function) ] }
                    ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 41 } ] }

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 40 named.DeclNames) (Some "MakeOptions") "path-derived name"
            Expect.equal (Map.tryFind 41 named.DeclNames) None "the callable itself stays inline"

        testCase "synthesize-anonymous prefers a non-exported type's own name" <| fun _ ->
            let internal' =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Globals"
                    Members = [ Build.resolvedMember (Build.symbol 401 "speed" SymbolFlags.Property) 2 ] }

            let model =
                { Build.shapeModel (internal' :: Build.primitives) with
                    Harvest =
                        { Exports = [ Build.export "globals" (Build.symbol 400 "globals" SymbolFlags.BlockScopedVariable) ] }
                    ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 40 } ] }

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 40 named.DeclNames) (Some "Globals") "its own name, not the path"

        testCase "synthesize-anonymous names the generic declaration behind an instantiation, not the instantiation" <| fun _ ->
            // `Ready<T>` is not exported and is reached only as `Ready<U>` from some generic
            // export. The declaration (30) is what gets the name; the instantiation (31) is
            // an application of it and claims nothing.
            let declaration =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 301 "latest" SymbolFlags.Property) 20 ]

            let declaration = { declaration with SymbolName = Some "Ready" }

            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    SymbolName = Some "Ready"
                    TypeArguments = [ 21 ]
                    Members = [ Build.resolvedMember (Build.symbol 301 "latest" SymbolFlags.Property) 21 ] }

            let model =
                { Build.shapeModel (declaration :: instantiation :: typeParam 20 "T" :: typeParam 21 "U" :: Build.primitives) with
                    Harvest = { Exports = [ Build.export "current" (Build.symbol 400 "current" SymbolFlags.BlockScopedVariable) ] }
                    ExportTypes = Map.ofList [ 400, { Declared = None; Value = Some 31 } ] }

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 30 named.DeclNames) (Some "Ready") "the declaration is named"
            Expect.equal (Map.tryFind 31 named.DeclNames) None "the instantiation is written as an application"

        testCase "synthesize-anonymous recognises an alias whose intersection body defers on a conditional" <| fun _ ->
            // `three`'s `Node<TNodeType>` (`docs/plans/generator-three-rung.md` §11.4):
            //
            //     type Node<TNodeType> = { isNode: true }
            //         & (unknown extends TNodeType ? {} : NodeExtensions<TNodeType>)
            //         & { tag: TNodeType };
            //
            // The conditional operand waits on the alias's own parameter, so the checker
            // surrenders no members at the declaration (50) and four at `Node<number>` (60).
            // Both carry alias symbol 100, and the argument comes back off the tag operand.
            let model = conditionalAliasModel [ 30; 31; 32 ] [ 30; 40; 41 ]

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60 named.DeclNames) (Some "Node") "the application reads back the declaration's name"
            Expect.equal (Map.tryFind 60 named.DeclParams) (Some [ 2 ]) "the argument comes back off the operands that resolved"
            Expect.equal (findings |> List.map _.Key) [ "SY001" ] "recognition, not a second hoist"

        testCase "synthesize-anonymous aligns the operands a vanished conditional leaves behind" <| fun _ ->
            // `Node<unknown>` takes the true branch, the checker keeps no `{}` in an
            // intersection, and the application arrives one operand short of its declaration.
            let model = conditionalAliasModel [ 30; 31; 32 ] [ 30; 41 ]

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60 named.DeclNames) (Some "Node") "the shorter application is still the same alias"
            Expect.equal (Map.tryFind 60 named.DeclParams) (Some [ 2 ]) "the surviving operands carry the argument"
            Expect.equal (findings |> List.map _.Key) [ "SY001" ] "recognition, not a second hoist"

        testCase "synthesize-anonymous widens an alias whose argument only the conditional carried" <| fun _ ->
            // Drop the tag operand and the parameter appears under the conditional alone. The
            // checker keeps neither branch nor argument there, so the application is
            // unwritable - and widening it is what bounds the chain (§9 blocker 1's second
            // option), because a hoist here mints a strictly larger type per generation.
            let model = conditionalAliasModel [ 30; 31 ] [ 30; 40 ]

            let named, findings = Build.runPass Anonymous.synthesizeAnonymous model

            Expect.equal (Map.tryFind 60 named.DeclNames) None "no second declaration under a minted name"
            Expect.equal (findings |> List.map _.Key) [ "SY002" ] "the widening is reported"

        testCase "bind-free-type-params declares a hoisted object over the parameters it reads" <| fun _ ->
            // `each<T, U>(props: { items: T[]; render: (item: T) => U })`: the hoisted
            // `EachProps` binds nothing of its own, so it is declared over `T` and `U` in
            // first-use order, and a reference under that scope applies them back.
            let render =
                { Build.facts (Build.typeResponse 42 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature [ Build.resolvedMember (Build.symbol 403 "item" SymbolFlags.FunctionScopedVariable) 20 ] 21 ] }

            let props =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol 401 "items" SymbolFlags.Property) 20
                          Build.resolvedMember (Build.symbol 402 "render" SymbolFlags.Property) 42 ] }

            let model =
                { Build.shapeModel (props :: render :: typeParam 20 "T" :: typeParam 21 "U" :: Build.primitives) with
                    DeclNames = Map.ofList [ 40, "EachProps" ] }

            let bound, _ = Build.runPass FreeTypeParams.bindFreeTypeParams model

            Expect.equal (Map.tryFind 40 bound.DeclParams) (Some [ 20; 21 ]) "T then U, as first read"

            let reference, findings =
                Spec.typeRef Build.context { bound with TypeVars = Map.ofList [ 20, "T"; 21, "U" ] } None "x" 40

            Expect.equal reference (FsApp("EachProps", [ FsTypeVar "T"; FsTypeVar "U" ])) "applied back where they are in scope"
            Expect.isEmpty findings "an application over in-scope variables is exact"

        testCase "bind-free-type-params leaves a signature's own parameters to the signature" <| fun _ ->
            // `interface Store { read<K>(key: K): string }` reads `K` only inside the method
            // that binds it - the declaration owes nothing to any outer scope.
            let read =
                { Build.facts (Build.typeResponse 42 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature [ Build.resolvedMember (Build.symbol 403 "key" SymbolFlags.FunctionScopedVariable) 20 ] 1 with
                              TypeParameters = [ 20 ] } ] }

            let store =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Store"
                    Members = [ Build.resolvedMember (Build.symbol 402 "read" SymbolFlags.Method) 42 ] }

            let model =
                { Build.shapeModel (store :: read :: typeParam 20 "K" :: Build.primitives) with
                    DeclNames = Map.ofList [ 40, "Store" ] }

            let bound, _ = Build.runPass FreeTypeParams.bindFreeTypeParams model

            Expect.equal (Map.tryFind 40 bound.DeclParams) None "nothing free"

        testCase "shape-interfaces flattens an object intersection and inherits its named operands" <| fun _ ->
            // `type NamedTimed = Named & Timed`: the resolve tier read both member sets off the
            // intersection itself, so it declares as one interface (§4.6) - inheriting the
            // operands this run declares, so it upcasts to either, and still declaring every
            // member, so `Create` and the member list stay exact.
            let name = Build.resolvedMember (Build.symbol 401 "name" SymbolFlags.Property) 1
            let at = Build.resolvedMember (Build.symbol 402 "at" SymbolFlags.Property) 2

            let named =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Named"
                    Members = [ name ] }

            let timed =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "Timed"
                    Members = [ at ] }

            let both =
                { Build.facts (Build.typeResponse 50 TypeFlags.Intersection) with
                    IntersectionMembers = [ 40; 41 ]
                    Members = [ name; at ] }

            let model =
                { Build.shapeModel (named :: timed :: both :: Build.primitives) with
                    DeclNames = Map.ofList [ 40, "Named"; 41, "Timed"; 50, "NamedTimed" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "NamedTimed" -> Some d
                    | _ -> None)

            Expect.equal decl.Inherits [ FsNamed "Named"; FsNamed "Timed" ] "the named operands are inherited"

            Expect.equal
                (decl.Members
                 |> List.map (function
                     | FsProperty p -> p.Name
                     | FsMethod m -> m.Name
                     | FsConstructor _ -> "Create"
                     | FsIndexer _ -> "Item"))
                [ "name"; "at" ]
                "both member sets, in the checker's order"

            Expect.contains
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                (Ergonomic, "NamedTimed")
                "the flattening is recorded on the declaration"

            let reference, refFindings = Spec.typeRef Build.context shaped None "x" 50
            Expect.equal reference (FsNamed "NamedTimed") "a reference names it"
            Expect.isEmpty refFindings "at no further cost"

        testCase "shape-interfaces inherits a declared base beside the members it redeclares" <| fun _ ->
            // `interface Derived extends Base`: the checker's property list on `Derived` already
            // carries `Base`'s members, so they are declared here in full - F# admits the
            // redeclaration, and it is what keeps `Create` and the member list exact. The base is
            // a type this run declares, so the is-a relation is emitted beside them (§4.4).
            let name = Build.resolvedMember (Build.symbol 401 "name" SymbolFlags.Property) 1
            let extra = Build.resolvedMember (Build.symbol 402 "extra" SymbolFlags.Property) 3

            let baseType =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Base"
                    Members = [ name ] }

            let derived =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "Derived"
                    BaseTypes = [ 40 ]
                    Members = [ extra; name ] }

            let model =
                { Build.shapeModel (baseType :: derived :: Build.primitives) with
                    DeclNames = Map.ofList [ 40, "Base"; 41, "Derived" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Derived" -> Some d
                    | _ -> None)

            Expect.equal decl.Inherits [ FsNamed "Base" ] "the is-a relation is stated"

            Expect.equal
                (decl.Members
                 |> List.map (function
                     | FsProperty p -> p.Name
                     | FsMethod m -> m.Name
                     | FsIndexer _ -> "Item"))
                [ "extra"; "name" ]
                "and the inherited member is still declared here"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Message))
                ("SI005", Exact, "base Base is inherited: the is-a relation is emitted (§4.4)")
                "an emitted is-a relation costs nothing"

        testCase "shape-interfaces applies a generic base's argument at the inherit" <| fun _ ->
            // `interface Tagged extends Box<string>`. F# has no bare `Box`: `inherit Box` is
            // FS0033, so the argument has to travel to the inherit or the edge cannot be drawn.
            let value = Build.resolvedMember (Build.symbol 401 "value" SymbolFlags.Property) 1
            let tag = Build.resolvedMember (Build.symbol 402 "tag" SymbolFlags.Property) 1

            let boxDecl = genericDecl 30 [ 20 ] [ value ]

            let instantiation =
                { Build.facts
                    { Build.typeResponse 31 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 30 } with
                    TypeArguments = [ 1 ] }

            let tagged =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "Tagged"
                    BaseTypes = [ 31 ]
                    Members = [ tag; value ] }

            let model =
                { Build.shapeModel (boxDecl :: instantiation :: tagged :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box"; 41, "Tagged" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Tagged" -> Some d
                    | _ -> None)

            Expect.equal decl.Inherits [ FsApp("Box", [ FsString ]) ] "Box<string>, not a bare Box"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Symbol))
                ("SI005", "Tagged")
                "recorded against the deriving declaration"

        testCase "shape-interfaces flattens a base this run does not declare, and names it" <| fun _ ->
            // `interface Deferred extends Promise<string>`: `Promise` resolves to the shipped
            // `JS.Promise` binding rather than to a type this run writes as an interface, and
            // `inherit` on something that is not an interface here is FS0887. The members
            // flatten, and the finding says which base was left behind - not just that one was.
            let tag = Build.resolvedMember (Build.symbol 401 "tag" SymbolFlags.Property) 1

            let deferred =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "Deferred"
                    BaseTypes = [ 40 ]
                    Members = [ tag ] }

            let model =
                { Build.shapeModel (libType 40 "Promise" [ 1 ] :: deferred :: Build.primitives) with
                    DeclNames = Map.ofList [ 41, "Deferred" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Deferred" -> Some d
                    | _ -> None)

            Expect.isEmpty decl.Inherits "nothing here is inheritable"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Message))
                ("SI006",
                 Ergonomic,
                 "base JS.Promise is not declared by this run as an interface; its members are flattened in and the is-a relation is not emitted (§4.4)")
                "the base that was not inherited is named"

        testCase "shape-interfaces flattens a base with no F# name at all" <| fun _ ->
            // `interface Failure extends Error`: nothing shipped binds `Error`, so the base has
            // no name at this position for an `inherit` to take. This is the undifferentiated
            // case the two named ones split out of, and it stays that way.
            let code = Build.resolvedMember (Build.symbol 401 "code" SymbolFlags.Property) 2

            let failure =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "Failure"
                    BaseTypes = [ 40 ]
                    Members = [ code ] }

            let model =
                { Build.shapeModel (libType 40 "Error" [] :: failure :: Build.primitives) with
                    DeclNames = Map.ofList [ 41, "Failure" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Failure" -> Some d
                    | _ -> None)

            Expect.isEmpty decl.Inherits "nothing here is inheritable"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier))
                ("SI002", Ergonomic)
                "the nameless case is still reported, distinctly from the named ones"

        testCase "shape-interfaces refuses an inherit that would close a cycle" <| fun _ ->
            // TypeScript admits no cyclic heritage, but an F# name is not a type id: two ids
            // hash-consed onto one name can close a loop the source never wrote, and F# rejects a
            // cyclic inheritance relation outright (FS0954). The second edge is the one that
            // would close it, so it is the one refused - the first still stands.
            let up = Build.resolvedMember (Build.symbol 401 "up" SymbolFlags.Property) 3
            let down = Build.resolvedMember (Build.symbol 402 "down" SymbolFlags.Property) 3

            let a =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "A"
                    BaseTypes = [ 41 ]
                    Members = [ up ] }

            let b =
                { Build.facts (Build.typeResponse 41 TypeFlags.Object) with
                    SymbolName = Some "B"
                    BaseTypes = [ 40 ]
                    Members = [ down ] }

            let model =
                { Build.shapeModel (a :: b :: Build.primitives) with DeclNames = Map.ofList [ 40, "A"; 41, "B" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            Expect.equal
                (shaped.Decls
                 |> List.choose (function
                     | FsInterface d -> Some(d.Name, d.Inherits)
                     | _ -> None))
                [ "A", [ FsNamed "B" ]; "B", [] ]
                "one edge, not two: the graph stays acyclic"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Symbol))
                ("SI007", Ergonomic, "B")
                "and the refusal is recorded where it happened"

        testCase "repair-arity drops an inherit whose base the widening just unnamed" <| fun _ ->
            // FS0887: `inherit obj` is not an interface type, and FS0033 forbids the bare `Box`
            // that got it there. A base the arity repair takes the name off has nothing left to
            // inherit, so the edge goes rather than the generated file failing to compile.
            let interfaceDecl name parameters inherits =
                FsInterface
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = parameters |> List.map (fun p -> { Name = p; Constraint = None })
                      Inherits = inherits
                      Members = []
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel [] with
                    Decls = [ interfaceDecl "Box" [ "T" ] []; interfaceDecl "Crate" [] [ FsNamed "Box" ] ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            Expect.equal
                (repaired.Decls
                 |> List.choose (function
                     | FsInterface d -> Some(d.Name, d.Inherits)
                     | _ -> None))
                [ "Box", []; "Crate", [] ]
                "the unnamed edge is dropped, not written as obj"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Symbol))
                ("RA003", "Crate")
                "the widening that removed it owns the loss"

        testCase "an intersection over a type-parameter operand has nothing to flatten" <| fun _ ->
            // `T & { id: number }`: the resolve tier reads no members off an intersection with
            // a non-object operand, so there is no shape to declare and the reference widens,
            // saying which case it is.
            let named =
                { Build.facts (Build.typeResponse 40 TypeFlags.Object) with
                    SymbolName = Some "Named"
                    Members = [ Build.resolvedMember (Build.symbol 401 "name" SymbolFlags.Property) 1 ] }

            let bare =
                { Build.facts (Build.typeResponse 51 TypeFlags.Intersection) with IntersectionMembers = [ 20; 40 ] }

            let model =
                { Build.shapeModel (named :: bare :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 40, "Named" ] }

            let named, _ = Build.runPass Anonymous.synthesizeAnonymous model
            Expect.equal (Map.tryFind 51 named.DeclNames) None "nothing to name"

            let reference, findings = Spec.typeRef Build.context named None "x" 51
            Expect.equal reference FsObj "widened"

            Expect.equal
                (findings |> List.map _.Message)
                [ "intersection over a non-object operand has no members to flatten; widened to obj (§4.6)" ]
                "and owned"

        testCase "an empty operand reduces away and the remaining operand is the type" <| fun _ ->
            // `string & {}`: the object operand declares nothing, so the intersection is
            // `string`, and a union carrying one keeps the literals beside it (§4.6).
            let empty = Build.facts (Build.typeResponse 80 TypeFlags.Object)

            let idiom =
                { Build.facts (Build.typeResponse 81 TypeFlags.Intersection) with IntersectionMembers = [ 1; 80 ] }

            let model = Build.shapeModel (empty :: idiom :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "Ease" 81
            Expect.equal reference FsString "the operand that declares something is the type"

            Expect.equal
                (findings |> List.map (fun finding -> finding.Key, finding.Symbol))
                [ "TR049", "Ease" ]
                "and the reduction is owned"

        testCase "an operand carrying a real member is not an empty operand" <| fun _ ->
            // `string & { count: number }`: a member is a shape the mapping owes the reader,
            // so the intersection stands and widens as it did.
            let counted =
                { Build.facts (Build.typeResponse 82 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 820 "count" SymbolFlags.Property) 2 ] }

            let branded =
                { Build.facts (Build.typeResponse 83 TypeFlags.Intersection) with IntersectionMembers = [ 1; 82 ] }

            let model = Build.shapeModel (counted :: branded :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "Counted" 83
            Expect.equal reference FsObj "widened"

            Expect.equal
                (findings |> List.map _.Key)
                [ "TR018" ]
                "under the case that describes it"

        testCase "an intersection of callable operands renders from its call signatures" <| fun _ ->
            // `typeof round & Chained`: both operands carry signatures and no properties, so
            // the checker hands the intersection both, and D5 writes the first as a delegate.
            let callable id parameters =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    CallSignatures = [ Build.signature parameters 2 ] }

            let value = Build.resolvedMember (Build.symbol 900 "value" SymbolFlags.Property) 2
            let length = Build.resolvedMember (Build.symbol 901 "length" SymbolFlags.Property) 2

            let overloaded =
                { Build.facts (Build.typeResponse 92 TypeFlags.Intersection) with
                    IntersectionMembers = [ 90; 91 ]
                    CallSignatures =
                        [ Build.signature [ value; length ] 2; Build.signature [ length ] 2 ] }

            let model =
                Build.shapeModel (callable 90 [ value; length ] :: callable 91 [ length ] :: overloaded :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "Utils.round" 92
            Expect.equal reference (FsDelegate([ FsFloat; FsFloat ], FsFloat)) "the first signature shapes the delegate"

            Expect.equal
                (findings |> List.map (fun finding -> finding.Key, finding.Message))
                [
                    "TR031", "callback with 2 overloads shaped from the first"
                    "TR050", "intersection of callable operands rendered from its 2 call signatures"
                ]
                "and both the overload loss and the flattening are owned"

        testCase "operands agreeing on a member's declared type give the member that type" <| fun _ ->
            // `{ to?: U } & { to: U }`: the checker types the flattened `to` as the two
            // declarations intersected and distributes that over `U`'s arms. The operands
            // declare one type, so that type is the answer (§4.6).
            let union id members =
                { Build.facts (Build.typeResponse id TypeFlags.Union) with UnionMembers = members }

            let operand id memberTypeId optional =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members =
                        [ { Build.resolvedMember (Build.symbol (id * 10) "to" SymbolFlags.Property) memberTypeId with
                              Optional = optional } ] }

            let flattened =
                { Build.facts (Build.typeResponse 76 TypeFlags.Intersection) with IntersectionMembers = [ 70; 71 ] }

            let destinations =
                { Build.facts (Build.typeResponse 77 TypeFlags.Intersection) with
                    IntersectionMembers = [ 72; 73 ]
                    Members = [ Build.resolvedMember (Build.symbol 770 "to" SymbolFlags.Property) 76 ] }

            let model =
                { Build.shapeModel (
                    [
                        union 70 [ 1; 2 ]
                        union 71 [ 1; 2; 5 ]
                        operand 72 71 true
                        operand 73 70 false
                        flattened
                        destinations
                    ]
                    @ Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 77, "Destinations" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Destinations" -> Some d
                    | _ -> None)

            Expect.equal
                (decl.Members
                 |> List.choose (function
                     | FsProperty property -> Some property.Type
                     | _ -> None))
                [ FsErasedUnion [ FsString; FsFloat ] ]
                "the member reads the type both operands declare"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Symbol))
                ("TR051", "Destinations.to")
                "and the agreement is owned"

        testCase "operands disagreeing on a member's declared type leave it flattened" <| fun _ ->
            let operand id memberTypeId =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol (id * 10) "to" SymbolFlags.Property) memberTypeId ] }

            let flattened =
                { Build.facts (Build.typeResponse 78 TypeFlags.Intersection) with IntersectionMembers = [ 1; 2 ] }

            let destinations =
                { Build.facts (Build.typeResponse 79 TypeFlags.Intersection) with
                    IntersectionMembers = [ 74; 75 ]
                    Members = [ Build.resolvedMember (Build.symbol 790 "to" SymbolFlags.Property) 78 ] }

            let model =
                { Build.shapeModel ([ operand 74 1; operand 75 2; flattened; destinations ] @ Build.primitives) with
                    DeclNames = Map.ofList [ 79, "Destinations" ] }

            let _, findings = Build.runPass Interfaces.shapeInterfaces model

            Expect.isEmpty
                (findings |> List.filter (fun finding -> finding.Key = "TR051"))
                "two declared types are two types"

        testCase "classify-literal-unions makes a StringEnum with CompiledName per case" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 8 ] }

            let model =
                { Build.shapeModel [ union; stringLiteral 7 "ms"; stringLiteral 8 "s" ] with
                    DeclNames = Map.ofList [ 10, "TimeUnit" ] }

            let shaped, findings = Build.runPass LiteralUnions.classifyLiteralUnions model

            Expect.isEmpty findings "exact"

            match shaped.Decls with
            | [ FsStringEnum decl ] ->
                Expect.equal decl.Name "TimeUnit" "name"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName))
                    [ "Ms", Some "ms"; "S", Some "s" ]
                    "PascalCased cases carrying their literals"
            | decls -> failtest $"expected one StringEnum, got %A{decls}"

        testCase "classify-literal-unions keeps mixed unions in one StringEnum via CompiledValue (D12)" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 9 ] }

            let model =
                { Build.shapeModel [ union; stringLiteral 7 "auto"; numberLiteral 9 1.5 ] with
                    DeclNames = Map.ofList [ 10, "Speed" ] }

            let shaped, findings = Build.runPass LiteralUnions.classifyLiteralUnions model

            Expect.equal (findings |> List.map _.Tier) [ Exact ] "D12 is exact, and says so"

            match shaped.Decls with
            | [ FsStringEnum decl ] ->
                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName, c.CompiledValue))
                    [ "Auto", Some "auto", None; "N1_5", None, Some(LitNumber 1.5) ]
                    "the numeric case carries CompiledValue"
            | decls -> failtest $"expected one StringEnum, got %A{decls}"

        testCase "classify-literal-unions makes an F# enum from an all-integer union" <| fun _ ->
            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 7; 9 ] }

            let model =
                { Build.shapeModel [ union; numberLiteral 7 0.0; numberLiteral 9 1.0 ] with
                    DeclNames = Map.ofList [ 10, "Flag" ] }

            let shaped, _ = Build.runPass LiteralUnions.classifyLiteralUnions model

            match shaped.Decls with
            | [ FsEnum decl ] -> Expect.equal decl.Cases [ "N0", 0; "N1", 1 ] "integer cases"
            | decls -> failtest $"expected one enum, got %A{decls}"

        testCase "shape-callbacks abbreviates a named callback to its delegate" <| fun _ ->
            let timer = Build.facts (Build.typeResponse 60 TypeFlags.Object)

            let callback =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 500 "timer" SymbolFlags.FunctionScopedVariable) 60 ]
                              4 ] }

            let model =
                { Build.shapeModel (callback :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 50, "TimerCallback"; 60, "Timer" ] }

            let shaped, findings = Build.runPass Callbacks.shapeCallbacks model

            Expect.isEmpty findings "exact"

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal decl.Name "TimerCallback" "name"
                Expect.equal decl.Target (FsDelegate([ FsNamed "Timer" ], FsUnit)) "Action<Timer>"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

        testCase "shape-interfaces emits methods with overloads, this-returns chained" <| fun _ ->
            let methodType =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ Build.signature [] 71
                          Build.signature
                              [ Build.resolvedMember (Build.symbol 700 "speed" SymbolFlags.FunctionScopedVariable) 2 ]
                              71 ] }

            let thisType =
                Build.facts
                    { Build.typeResponse 71 TypeFlags.TypeParameter with IsThisType = ValueSome true }

            let timer =
                { Build.facts (Build.typeResponse 60 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 601 "play" SymbolFlags.Method) 70 ] }

            let model =
                { Build.shapeModel (methodType :: thisType :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Timer" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsMethod first; FsMethod second ] ->
                    Expect.equal first.Parameters [] "first overload takes nothing"
                    Expect.equal first.Return (FsNamed "Timer") "this reads as Timer"
                    Expect.equal (second.Parameters |> List.map _.Name) [ "speed" ] "second overload's parameter"
                | members -> failtest $"expected two method overloads, got %A{members}"

                Expect.equal
                    (findings |> List.map _.Tier |> List.distinct)
                    [ Ergonomic ]
                    "this-chaining is the only finding"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-interfaces binds a declaration's parameters for its members (§4.9)" <| fun _ ->
            let box =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "value" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (box :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Box" ] }

            let shaped, _ = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Box<'T>"

                match decl.Members with
                | [ FsProperty property ] -> Expect.equal property.Type (FsTypeVar "T") "the member names the variable"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "a constraint naming a generated type survives" <| fun _ ->
            let bounded = { typeParam 20 "T" with Constraint = Some 60 }
            let timer = Build.facts (Build.typeResponse 60 TypeFlags.Object)

            let holder =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "held" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (holder :: bounded :: timer :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Timer" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = Some(FsNamed "Timer") } ] "'T :> Timer"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.isEmpty
                (findings |> List.filter (fun finding -> finding.Message.Contains "constraint"))
                "a bound F# can state costs nothing"

        testCase "a constraint against a sealed type is dropped, not written" <| fun _ ->
            // `T extends Renderable` where `Renderable = JSAnimation | Timeline`: the union
            // renders as an erased `U2`, and F# rejects a subtype constraint against a sealed
            // type outright (FS0698). A wrong constraint is worse than none.
            let bounded = { typeParam 20 "T" with Constraint = Some 60 }

            let renderable =
                { Build.facts (Build.typeResponse 60 TypeFlags.Union) with UnionMembers = [ 1; 2 ] }

            let holder =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "held" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (holder :: bounded :: renderable :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Holder"; 60, "Renderable" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "the variable stays, the bound goes"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "a dropped bound is ergonomic, not widening"

        testCase "a constraint with no F# form is dropped with a finding" <| fun _ ->
            // `K extends string`: an F# subtype constraint cannot name a primitive, and the
            // nearest approximation would reject code TypeScript accepts.
            let bounded = { typeParam 20 "K" with Constraint = Some 1 }

            let keyed =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "key" SymbolFlags.Property) 20 ]

            let model =
                { Build.shapeModel (keyed :: bounded :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Keyed" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "K"; Constraint = None } ] "the variable stays, the bound goes"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "a dropped bound is ergonomic, not widening"

        testCase "a generic method binds its own parameters over the declaration's" <| fun _ ->
            // `interface Accessor<T> { read<K>(key: K): T }` - the member's `K` is layered onto
            // the interface's `T` rather than replacing it, so the signature can read both.
            let read =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 700 "key" SymbolFlags.FunctionScopedVariable) 21 ]
                                20 with
                              TypeParameters = [ 21 ] } ] }

            let accessor =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "read" SymbolFlags.Method) 70 ]

            let model =
                { Build.shapeModel (accessor :: read :: typeParam 20 "T" :: typeParam 21 "K" :: Build.primitives) with
                    DeclNames = Map.ofList [ 30, "Accessor" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Accessor<'T>"

                match decl.Members with
                | [ FsMethod m ] ->
                    Expect.equal m.TypeParameters [ { Name = "K"; Constraint = None } ] "the method binds 'K itself"
                    Expect.equal (m.Parameters |> List.map _.Type) [ FsTypeVar "K" ] "its own parameter"
                    Expect.equal m.Return (FsTypeVar "T") "and the declaration's, still in scope"
                | members -> failtest $"expected one method, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.isEmpty findings "both variables are written, so nothing is lost"

        testCase "a signature parameter whose every use widens away is dropped" <| fun _ ->
            // `read<K extends keyof T>(key: keyof T): void` - `keyof T` has no F# form yet, so
            // every mention of `K` widens to obj. Writing `<'K>` over a signature that names no
            // `'K` would claim a generic member where nothing about it is generic.
            let keyofT = Build.facts (Build.typeResponse 40 TypeFlags.Index)

            let read =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 700 "key" SymbolFlags.FunctionScopedVariable) 40 ]
                                4 with
                              TypeParameters = [ 21 ] } ] }

            let accessor =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "read" SymbolFlags.Method) 70 ]

            let model =
                { Build.shapeModel (
                      accessor :: read :: keyofT :: typeParam 20 "T" :: typeParam 21 "K" :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Accessor" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsMethod m ] ->
                    Expect.isEmpty m.TypeParameters "the variable nothing names is not written"
                    Expect.equal (m.Parameters |> List.map _.Type) [ FsObj ] "its use widened"
                | members -> failtest $"expected one method, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.equal
                (findings |> List.map _.Tier |> List.distinct)
                [ Widened ]
                "the widening and the erasure are both recorded"

            Expect.isTrue
                (findings |> List.exists (fun f -> f.Message.Contains "'K' is erased"))
                "the dropped parameter is named in the manifest"

        testCase "a key variable with an indexed access reads as the typed accessor" <| fun _ ->
            // `interface Accessor<T> { read<K extends keyof T>(key: K): T[K] }`. F# cannot state
            // the bound, so `K` is not bound at all: the key is written as `typekeyof<'T,'R>`
            // and the access it selects as the `'R` that introduced.
            let read =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 700 "key" SymbolFlags.FunctionScopedVariable) 21 ]
                                41 with
                              TypeParameters = [ 21 ] } ] }

            let accessor =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "read" SymbolFlags.Method) 70 ]

            let model =
                { Build.shapeModel (
                      accessor
                      :: read
                      :: indexedAccess 41 20 21
                      :: keyOf 40 20
                      :: typeParam 20 "T"
                      :: { typeParam 21 "K" with Constraint = Some 40 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Accessor" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Accessor<'T>"

                match decl.Members with
                | [ FsMethod m ] ->
                    Expect.equal m.TypeParameters [ { Name = "R"; Constraint = None } ] "'K is gone, 'R is bound"

                    Expect.equal
                        (m.Parameters |> List.map _.Type)
                        [ FsApp("typekeyof", [ FsTypeVar "T"; FsTypeVar "R" ]) ]
                        "the key carries both"

                    Expect.equal m.Return (FsTypeVar "R") "and the access is exactly the result"
                | members -> failtest $"expected one method, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier |> List.distinct) [ Ergonomic ] "an idiom, not a widening"

        testCase "a key variable nothing indexes with reads as a bare keyof" <| fun _ ->
            // `read<K extends keyof T>(key: K): void` - no `T[K]`, so nothing needs the value
            // type and there is no reason to bind a variable for it.
            let read =
                { Build.facts (Build.typeResponse 70 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 700 "key" SymbolFlags.FunctionScopedVariable) 21 ]
                                4 with
                              TypeParameters = [ 21 ] } ] }

            let accessor =
                genericDecl 30 [ 20 ] [ Build.resolvedMember (Build.symbol 300 "read" SymbolFlags.Method) 70 ]

            let model =
                { Build.shapeModel (
                      accessor
                      :: read
                      :: keyOf 40 20
                      :: typeParam 20 "T"
                      :: { typeParam 21 "K" with Constraint = Some 40 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 30, "Accessor" ] }

            let shaped, _ = Build.runPass Interfaces.shapeInterfaces model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsMethod m ] ->
                    Expect.isEmpty m.TypeParameters "the member is not generic in its own right"

                    Expect.equal
                        (m.Parameters |> List.map _.Type)
                        [ FsApp("keyof", [ FsTypeVar "T" ]) ]
                        "keyof<'T> at the key"
                | members -> failtest $"expected one method, got %A{members}"
            | decls -> failtest $"expected one interface, got %A{decls}"

        testCase "shape-callbacks binds the parameters the alias carries" <| fun _ ->
            // `type Mapper<T> = (input: T) => T` leaves the function type parameterless; the
            // variable is only reachable through the alias's arguments.
            let mapper =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    AliasTypeArguments = [ 20 ]
                    CallSignatures =
                        [ Build.signature
                              [ Build.resolvedMember (Build.symbol 500 "input" SymbolFlags.FunctionScopedVariable) 20 ]
                              20 ] }

            let model =
                { Build.shapeModel (mapper :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 50, "Mapper" ] }

            let shaped, findings = Build.runPass Callbacks.shapeCallbacks model

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal decl.TypeParameters [ { Name = "T"; Constraint = None } ] "Mapper<'T>"
                Expect.equal decl.Target (FsDelegate([ FsTypeVar "T" ], FsTypeVar "T")) "Func<'T, 'T>"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

            Expect.isEmpty findings "a generic alias is exact"

        // Wave three, lane K. `solid-js`'s `Setter` is an object type whose four call
        // signatures each declare `U extends T`; the head wrote `Setter<'T, 'U, 'U, 'U, 'U>`
        // and F# refused it (FS0037). `setter-lab` pins the same shape live.
        testCase "shape-callbacks writes one variable per name a signature hoists" <| fun _ ->
            let signature =
                { Build.signature
                      [ Build.resolvedMember (Build.symbol 500 "value" SymbolFlags.FunctionScopedVariable) 21 ]
                      21 with
                    TypeParameters = [ 21 ] }

            let other =
                { Build.signature
                      [ Build.resolvedMember (Build.symbol 501 "value" SymbolFlags.FunctionScopedVariable) 22 ]
                      22 with
                    TypeParameters = [ 22 ] }

            let setter =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    AliasTypeArguments = [ 20 ]
                    CallSignatures = [ signature; other ] }

            let model =
                { Build.shapeModel (
                      setter
                      :: typeParam 20 "T"
                      :: { typeParam 21 "U" with Constraint = Some 20 }
                      :: { typeParam 22 "U" with Constraint = Some 20 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 50, "Setter" ] }

            let shaped, findings = Build.runPass Callbacks.shapeCallbacks model

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal (decl.TypeParameters |> List.map _.Name) [ "T"; "U" ] "Setter<'T, 'U>"

                // The second signature is discarded, but its `U` still had to resolve to the
                // variable the head wrote rather than widening.
                Expect.equal decl.Target (FsDelegate([ FsTypeVar "U" ], FsTypeVar "U")) "Func<'U, 'U>"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

            Expect.contains
                (findings |> List.map (fun finding -> finding.Key, finding.Tier, finding.Message))
                ("TP009", Ergonomic, "'U' is declared by 2 signatures of the same alias; the head writes one variable")
                "the collapse is reported"

        testCase "shape-callbacks keeps one name declared under two bounds apart" <| fun _ ->
            // One variable would retype a signature, so the head stays as declared and
            // `repair-arity` prices what F# refuses.
            let signature =
                { Build.signature
                      [ Build.resolvedMember (Build.symbol 500 "value" SymbolFlags.FunctionScopedVariable) 21 ]
                      21 with
                    TypeParameters = [ 21 ] }

            let other =
                { Build.signature
                      [ Build.resolvedMember (Build.symbol 501 "value" SymbolFlags.FunctionScopedVariable) 22 ]
                      22 with
                    TypeParameters = [ 22 ] }

            let divergent =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    AliasTypeArguments = [ 20 ]
                    CallSignatures = [ signature; other ] }

            let model =
                { Build.shapeModel (
                      divergent
                      :: typeParam 20 "T"
                      :: { typeParam 21 "U" with Constraint = Some 20 }
                      :: { typeParam 22 "U" with Constraint = Some 1 }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 50, "DivergentBound" ] }

            let shaped, findings = Build.runPass Callbacks.shapeCallbacks model

            match shaped.Decls with
            | [ FsAbbrev decl ] ->
                Expect.equal (decl.TypeParameters |> List.map _.Name) [ "T"; "U"; "U" ] "a slot per bound"
            | decls -> failtest $"expected one abbreviation, got %A{decls}"

            Expect.isEmpty (findings |> List.filter (fun finding -> finding.Key = "TP009")) "nothing was collapsed"

        // Wave three, lane K, wave two's second handback. `(...args: [value: T]) => R` is
        // TypeScript's spelling of `(value: T) => R`, and it arrived as `Func<obj[], R>`.
        testCase "a tuple-typed rest parameter reads as the parameters it stands for" <| fun _ ->
            let tuple flags elements typeId =
                { Build.facts
                    { Build.typeResponse typeId TypeFlags.Object with
                        IsTupleType = ValueSome true } with
                    TupleElements = flags
                    TypeArguments = elements }

            let callback rest typeId =
                { Build.facts (Build.typeResponse typeId TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 500 "args" SymbolFlags.FunctionScopedVariable) rest ]
                                4 with
                              HasRest = true } ] }

            let model =
                Build.shapeModel (
                    callback 71 50
                    :: callback 72 51
                    :: tuple [ ElementFlags.Required ] [ 1 ] 71
                    :: tuple [] [] 72
                    :: Build.primitives
                )

            let single, singleFindings = Spec.typeRef Build.context model None "x" 50
            let empty, emptyFindings = Spec.typeRef Build.context model None "x" 51

            Expect.equal single (FsDelegate([ FsString ], FsUnit)) "Action<string>"
            Expect.equal empty (FsDelegate([], FsUnit)) "Action"
            Expect.isEmpty singleFindings "a parameter list is exact"
            Expect.isEmpty emptyFindings "and so is an empty one"

        testCase "a rest parameter with a variadic tail keeps its array form" <| fun _ ->
            // The negative: `[A, ...B[]]` is the one shape F# has no parameter list for.
            let variadic =
                { Build.facts
                    { Build.typeResponse 71 TypeFlags.Object with
                        IsTupleType = ValueSome true } with
                    TupleElements = [ ElementFlags.Required; ElementFlags.Rest ]
                    TypeArguments = [ 1; 2 ] }

            let callback =
                { Build.facts (Build.typeResponse 50 TypeFlags.Object) with
                    CallSignatures =
                        [ { Build.signature
                                [ Build.resolvedMember (Build.symbol 500 "args" SymbolFlags.FunctionScopedVariable) 71 ]
                                4 with
                              HasRest = true } ] }

            let model = Build.shapeModel (callback :: variadic :: Build.primitives)
            let reference, findings = Spec.typeRef Build.context model None "x" 50

            Expect.equal reference (FsDelegate([ FsArray FsObj ], FsUnit)) "the rest tail still widens"
            Expect.equal (findings |> List.map _.Key) [ "TR028" ] "and says so"

        testCase "shape-classes emits a constructor member per construct signature" <| fun _ ->
            let instance =
                { Build.facts (Build.typeResponse 80 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 801 "progress" SymbolFlags.Property) 2 ] }

            let static' =
                { Build.facts (Build.typeResponse 81 TypeFlags.Object) with
                    ConstructSignatures =
                        [ Build.signature
                              [ { Build.resolvedMember (Build.symbol 802 "options" SymbolFlags.FunctionScopedVariable) 2 with
                                    Optional = true } ]
                              80 ] }

            let model =
                { Build.shapeModel (instance :: static' :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export "Timer" (Build.symbol 800 "Timer" (SymbolFlags.Class ||| SymbolFlags.Value)) ] }
                    ExportTypes = Map.ofList [ 800, { Declared = Some 80; Value = Some 81 } ]
                    DeclNames = Map.ofList [ 80, "Timer" ] }

            let shaped, findings = Build.runPass Classes.shapeClasses model

            Expect.isEmpty (findings |> List.filter (fun f -> f.Tier = Escape)) "no drops"

            match shaped.ExportMembers with
            | [ (0, m) ] ->
                Expect.equal m.Name "Timer" "constructor member name"

                match m.Body with
                | ExportConstructor([ p ], FsNamed "Timer") -> Expect.isTrue p.Optional "optional ctor parameter"
                | body -> failtest $"expected a constructor returning Timer, got %A{body}"
            | members -> failtest $"expected one constructor member, got %A{members}"

        testCase "shape-classes keeps only the static F# admits beside an instance member" <| fun _ ->
            // `json` is a method on both halves - the one collision F# allows, and the one
            // `Response` has. `status` is a property on both, which is FS0441.
            let jsonType =
                { Build.facts (Build.typeResponse 82 TypeFlags.Object) with
                    CallSignatures = [ Build.signature [] 2 ] }

            let instanceJson = Build.resolvedMember (Build.symbol 811 "json" SymbolFlags.Method) 82
            let instanceStatus = Build.resolvedMember (Build.symbol 812 "status" SymbolFlags.Property) 2

            let instance =
                { Build.facts (Build.typeResponse 80 TypeFlags.Object) with
                    Members = [ instanceJson; instanceStatus ] }

            let static' =
                { Build.facts (Build.typeResponse 81 TypeFlags.Object) with
                    ConstructSignatures = [ Build.signature [] 80 ]
                    Members =
                        [ instanceJson
                          instanceStatus
                          { Build.resolvedMember (Build.symbol 813 "MAX" SymbolFlags.Property) 2 with
                              ReadOnly = true } ] }

            // The instance interface shape-interfaces would have left behind: statics hang off
            // it, and it is what the collision is judged against.
            let declared =
                FsInterface
                    { Name = "Clash"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members =
                        [ FsMethod
                            { Name = "json"
                              Docs = ""
                              Tags = []
                              TypeParameters = []
                              Parameters = []
                              Return = FsFloat }
                          FsProperty
                              { Name = "status"
                                Docs = ""
                                Tags = []
                                ReadOnly = false
                                Type = FsFloat } ]
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel (instance :: static' :: jsonType :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export "Clash" (Build.symbol 810 "Clash" (SymbolFlags.Class ||| SymbolFlags.Value)) ] }
                    ExportTypes = Map.ofList [ 810, { Declared = Some 80; Value = Some 81 } ]
                    DeclNames = Map.ofList [ 80, "Clash" ]
                    Decls = [ declared ] }

            let shaped, findings = Build.runPass Classes.shapeClasses model

            match shaped.Decls with
            | [ FsInterface decl ] ->
                Expect.equal
                    (decl.Statics |> List.map _.Name)
                    [ "json"; "MAX" ]
                    "the method survives its instance twin, the property does not"

                match decl.Statics with
                | [ { Binding = ImportNamed "Clash.json" }; { Binding = ImportNamed "Clash.MAX" } ] -> ()
                | statics -> failtest $"expected dotted selectors off the class, got %A{statics}"
            | decls -> failtest $"expected one interface, got %A{decls}"

            Expect.contains
                (findings |> List.map (fun f -> f.Key, f.Symbol))
                ("SC002", "Clash.status")
                "the dropped static says which one and why"

        // -----------------------------------------------------------------------------------
        // Constructor objects (§4.4): the static side of a class, and what `typeof X` names.
        // -----------------------------------------------------------------------------------

        /// `class Gauge { constructor(size: number); readonly size: number; static readonly
        /// UNIT: string }` as the checker reports it: an instance type, and a static side
        /// carrying `prototype`, the statics, and the construct signatures.
        let gaugeInstance =
            { Build.facts (Build.typeResponse 60 TypeFlags.Object) with
                SymbolName = Some "Gauge"
                Members = [ Build.resolvedMember (Build.symbol 601 "size" SymbolFlags.Property) 2 ] }

        let gaugeStatic (symbolName: string option) =
            { Build.facts (Build.typeResponse 61 TypeFlags.Object) with
                SymbolName = symbolName
                Members =
                    [ Build.resolvedMember (Build.symbol 602 "prototype" SymbolFlags.Property) 60
                      { Build.resolvedMember (Build.symbol 603 "UNIT" SymbolFlags.Property) 1 with ReadOnly = true } ]
                ConstructSignatures =
                    [ Build.signature
                          [ Build.resolvedMember (Build.symbol 604 "size" SymbolFlags.FunctionScopedVariable) 2 ]
                          60 ] }

        testCase "name-constructor-objects names a typeof member after the class it constructs" <| fun _ ->
            // `interface Scope { readonly Gauge: typeof Gauge }` - the shape the whole
            // `ServiceWorkerGlobalScope` constructor table has.
            let scope =
                { Build.facts (Build.typeResponse 62 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 605 "Gauge" SymbolFlags.Property) 61 ] }

            let model =
                { Build.shapeModel (gaugeInstance :: gaugeStatic (Some "Gauge") :: scope :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Gauge"; 62, "Scope" ] }

            let named, _ = Build.runPass ConstructorObjects.nameConstructorObjects model

            Expect.equal (Map.tryFind 61 named.DeclNames) (Some "GaugeConstructor") "named after what it constructs"
            Expect.equal (Map.tryFind 60 named.DeclNames) (Some "Gauge") "the instance side keeps its own name"

        testCase "name-constructor-objects leaves an unreferenced class's static side alone" <| fun _ ->
            // Nothing names `typeof Gauge`, so the constructor object is `shape-classes`'s work
            // and only that: a second interface here would be referenced by nothing.
            let model =
                { Build.shapeModel (gaugeInstance :: gaugeStatic (Some "Gauge") :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export "Gauge" (Build.symbol 600 "Gauge" (SymbolFlags.Class ||| SymbolFlags.Value)) ] }
                    ExportTypes = Map.ofList [ 600, { Declared = Some 60; Value = Some 61 } ]
                    DeclNames = Map.ofList [ 60, "Gauge" ] }

            let named, _ = Build.runPass ConstructorObjects.nameConstructorObjects model

            Expect.equal (Map.tryFind 61 named.DeclNames) None "no declaration for a class's own static side"

        testCase "name-constructor-objects names a non-class export's value type after the export" <| fun _ ->
            // `declare const widgets: { new (size: number): Gauge }`: the checker calls the
            // object `__type`, and `Exports.widgets` is the position that wants the name.
            let model =
                { Build.shapeModel (gaugeInstance :: gaugeStatic (Some "__type") :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export "widgets" (Build.symbol 600 "widgets" SymbolFlags.BlockScopedVariable) ] }
                    ExportTypes = Map.ofList [ 600, { Declared = None; Value = Some 61 } ]
                    DeclNames = Map.ofList [ 60, "Gauge" ] }

            let named, _ = Build.runPass ConstructorObjects.nameConstructorObjects model

            Expect.equal (Map.tryFind 61 named.DeclNames) (Some "WidgetsConstructor") "named after the export"

        testCase "shape-interfaces reads construct signatures as EmitConstructor Create members" <| fun _ ->
            let model =
                { Build.shapeModel (gaugeInstance :: gaugeStatic (Some "Gauge") :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Gauge"; 61, "GaugeConstructor" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "GaugeConstructor" -> Some d
                    | _ -> None)

            match decl.Members with
            | [ FsProperty statics; FsConstructor create ] ->
                // `prototype` is the instance side, declared separately, and must not come back
                // as a member of its own static side.
                Expect.equal statics.Name "UNIT" "the statics are the constructor object's properties"
                Expect.equal (create.Parameters |> List.map _.Name) [ "size" ] "the construct signature's parameters"
                Expect.equal create.Return (FsNamed "Gauge") "returning the instance side"
            | members -> failtest $"expected a static and a Create, got %A{members}"

            Expect.contains
                (findings |> List.map (fun f -> f.Key, f.Symbol))
                ("SI004", "GaugeConstructor")
                "the idiom is recorded once, on the declaration"

        testCase "an undeclared constructor object names the class it stands for" <| fun _ ->
            // The honest fallback: nothing declared `typeof Gauge`, so the reference widens -
            // but the message says which constructor object, not `__type`.
            let holder =
                { Build.facts (Build.typeResponse 63 TypeFlags.Object) with
                    Members = [ Build.resolvedMember (Build.symbol 605 "Gauge" SymbolFlags.Property) 61 ] }

            let model =
                { Build.shapeModel (gaugeInstance :: gaugeStatic (Some "__type") :: holder :: Build.primitives) with
                    DeclNames = Map.ofList [ 60, "Gauge"; 63, "Holder" ] }

            let shaped, findings = Build.runPass Interfaces.shapeInterfaces model

            let decl =
                shaped.Decls
                |> List.pick (function
                    | FsInterface d when d.Name = "Holder" -> Some d
                    | _ -> None)

            match decl.Members with
            | [ FsProperty p ] -> Expect.equal p.Type FsObj "the reference widens"
            | members -> failtest $"expected one property, got %A{members}"

            Expect.contains
                (findings |> List.map (fun f -> f.Key, f.Message))
                ("TR043", "typeof Gauge is a constructor object this run does not declare; widened to obj (§4.4)")
                "the finding names the construct, not the checker's placeholder"

        testCase "shape-classes says a static method had no signatures, not that it was settable" <| fun _ ->
            // `DOMException.isError` is inherited from the lib's `ErrorConstructor`, which this
            // run resolves identity-only - so there are no call signatures to shape a method
            // from. It is not a settable static, and `StaticReadOnly` said it was.
            let isError =
                { Build.facts (Build.typeResponse 82 TypeFlags.Object) with Origin = CompilerLib }

            let instance = Build.facts (Build.typeResponse 80 TypeFlags.Object)

            let static' =
                { Build.facts (Build.typeResponse 81 TypeFlags.Object) with
                    ConstructSignatures = [ Build.signature [] 80 ]
                    Members = [ Build.resolvedMember (Build.symbol 813 "isError" SymbolFlags.Method) 82 ] }

            let declared =
                FsInterface
                    { Name = "DOMException"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members = []
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel (instance :: static' :: isError :: Build.primitives) with
                    Harvest =
                        { Exports =
                            [ Build.export
                                  "DOMException"
                                  (Build.symbol 810 "DOMException" (SymbolFlags.Class ||| SymbolFlags.Value)) ] }
                    ExportTypes = Map.ofList [ 810, { Declared = Some 80; Value = Some 81 } ]
                    DeclNames = Map.ofList [ 80, "DOMException" ]
                    Decls = [ declared ] }

            let _, findings = Build.runPass Classes.shapeClasses model
            let keyed = findings |> List.map (fun f -> f.Key, f.Symbol)

            Expect.contains keyed ("SC005", "DOMException.isError") "the finding says what actually happened"
            Expect.isFalse (keyed |> List.contains ("SC003", "DOMException.isError")) "and not that it was settable"

            Expect.contains
                (findings |> List.map (fun f -> f.Key, f.Message))
                ("SC005",
                 "static method emitted as a value: its type is declared in typescript/lib, which this run resolves identity-only, so there are no signatures to shape")
                "naming the group whose resolution lost the signatures"

        testCase "synthesize-paramobjects gives plain-data interfaces a Create overload (D3)" <| fun _ ->
            let decl =
                FsInterface
                    { Name = "Options"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Inherits = []
                      Members =
                        [ FsProperty
                              { Name = "delay"
                                Docs = ""
                                Tags = []
                                ReadOnly = false
                                Type = FsOption FsFloat }
                          FsProperty
                              { Name = "target"
                                Docs = ""
                                Tags = []
                                ReadOnly = false
                                Type = FsString } ]
                      CreateOverloads = []
                      Statics = [] }

            let model = { Build.shapeModel [] with Decls = [ decl ] }
            let shaped, findings = Build.runPass ParamObjects.synthesizeParamObjects model

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "reported"

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.CreateOverloads with
                | [ [ target; delay ] ] ->
                    Expect.equal (target.Name, target.Optional) ("target", false) "required first"
                    Expect.equal (delay.Name, delay.Optional) ("delay", true) "optional after"
                | overloads -> failtest $"expected one two-parameter overload, got %A{overloads}"
            | decls -> failtest $"expected the interface back, got %A{decls}"

        // Wave four lane O (docs/fable5-workarounds.md §3). A method reads as the delegate a
        // function-valued property of the same signature already carries.
        let paramObjectDecl name members =
            FsInterface
                { Name = name
                  Docs = ""
                  Tags = []
                  Order = None
                  TypeParameters = []
                  Inherits = []
                  Members = members
                  CreateOverloads = []
                  Statics = [] }

        let paramObjectProperty name reference =
            FsProperty
                { Name = name
                  Docs = ""
                  Tags = []
                  ReadOnly = false
                  Type = reference }

        let paramObjectMethod name parameters returns =
            FsMethod
                { Name = name
                  Docs = ""
                  Tags = []
                  TypeParameters = []
                  Parameters =
                    parameters
                    |> List.map (fun reference ->
                        { Name = "arg"
                          Optional = false
                          Rest = false
                          Type = reference })
                  Return = returns }

        testCase "a method member is carried into Create as a delegate-typed parameter" <| fun _ ->
            let decl =
                paramObjectDecl "Timer" [
                    paramObjectProperty "label" FsString
                    paramObjectMethod "play" [] FsUnit
                    paramObjectMethod "seek" [ FsFloat ] FsBool
                    paramObjectProperty "tag" (FsOption FsString)
                ]

            let model = { Build.shapeModel [] with Decls = [ decl ] }
            let shaped, findings = Build.runPass ParamObjects.synthesizeParamObjects model

            Expect.equal
                (findings |> List.map (fun f -> f.Key, f.Symbol))
                [ "SP001", "Timer"; "SP002", "Timer.play"; "SP002", "Timer.seek" ]
                "one finding per method carried in, after the synthesis itself"

            match shaped.Decls with
            | [ FsInterface decl ] ->
                match decl.CreateOverloads with
                | [ [ label; play; seek; tag ] ] ->
                    Expect.equal (label.Name, label.Type) ("label", FsString) "the property is unchanged"
                    Expect.equal play.Type (FsDelegate([], FsUnit)) "a void method binds an Action"
                    Expect.equal seek.Type (FsDelegate([ FsFloat ], FsBool)) "and a returning one a Func"
                    Expect.isFalse (play.Optional || seek.Optional) "a method is required, so it sorts first"
                    Expect.equal (tag.Name, tag.Optional) ("tag", true) "and the optional property comes last"
                | overloads -> failtest $"expected one four-parameter overload, got %A{overloads}"
            | decls -> failtest $"expected the interface back, got %A{decls}"

        testCase "an interface that gets no Create says which shape refused it" <| fun _ ->
            let indexed =
                paramObjectDecl "Bag" [
                    paramObjectProperty "label" FsString
                    FsIndexer
                        { Key = FsString
                          Value = FsObj
                          ReadOnly = false }
                ]

            let overloaded =
                paramObjectDecl "Formatter" [
                    paramObjectMethod "format" [ FsFloat ] FsString
                    paramObjectMethod "format" [ FsFloat; FsFloat ] FsString
                ]

            let wide =
                paramObjectDecl "Wide" [
                    for i in 1..24 do
                        yield paramObjectProperty $"a{i}" FsFloat

                    yield paramObjectMethod "go" [] FsUnit
                ]

            let model =
                { Build.shapeModel [] with
                    Decls = [ paramObjectDecl "Marker" []; indexed; overloaded; wide ] }

            let shaped, findings = Build.runPass ParamObjects.synthesizeParamObjects model

            Expect.equal (findings |> List.map (fun f -> f.Key, f.Tier) |> List.distinct) [ "SP003", Ergonomic ] "all ergonomic"

            let reasons = findings |> List.map (fun f -> f.Symbol, f.Message)

            Expect.equal (reasons |> List.map fst) [ "Marker"; "Bag"; "Formatter"; "Wide" ] "one per declaration"
            Expect.stringContains (snd reasons[0]) "no members" "an empty declaration"
            Expect.stringContains (snd reasons[1]) "index signature" "an index signature has no name"
            Expect.stringContains (snd reasons[2]) "overloaded method" "two parameters would share a name"
            Expect.stringContains (snd reasons[3]) "budget" "twenty-five members is one too many"

            Expect.isEmpty
                (shaped.Decls
                 |> List.choose (function
                     | FsInterface d when not d.CreateOverloads.IsEmpty -> Some d.Name
                     | _ -> None))
                "and none of them gained a Create"

        testCase "a constructor object keeps its own Create and reports nothing" <| fun _ ->
            let decl =
                paramObjectDecl "WidgetConstructor" [
                    paramObjectProperty "DEFAULT_LABEL" FsString
                    FsConstructor
                        { Docs = ""
                          Tags = []
                          TypeParameters = []
                          Parameters =
                            [ { Name = "label"
                                Optional = false
                                Rest = false
                                Type = FsString } ]
                          Return = FsNamed "Widget" }
                ]

            let model = { Build.shapeModel [] with Decls = [ decl ] }
            let shaped, findings = Build.runPass ParamObjects.synthesizeParamObjects model

            Expect.isEmpty findings "the construct signature already supplied Create members"

            match shaped.Decls with
            | [ FsInterface decl ] -> Expect.isEmpty decl.CreateOverloads "so no ParamObject overload joins them"
            | decls -> failtest $"expected the interface back, got %A{decls}"

        testCase "dedupe-overloads sees through abbreviations and drops the twin" <| fun _ ->
            let parameter name reference =
                { Name = name
                  Optional = false
                  Rest = false
                  Type = reference }

            let method' name parameters =
                FsMethod
                    { Name = name
                      Docs = ""
                      Tags = []
                      TypeParameters = []
                      Parameters = parameters
                      Return = FsUnit }

            let abbrev name =
                FsAbbrev
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = []
                      Target = FsObj }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ abbrev "DOMTargets"
                          abbrev "JSTargets"
                          FsInterface
                              { Name = "Scope"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                  [ method' "add" [ parameter "targets" (FsNamed "DOMTargets") ]
                                    method' "add" [ parameter "targets" (FsNamed "JSTargets") ]
                                    method' "add" [ parameter "targets" FsString ] ]
                                CreateOverloads = []
                                Statics = [] } ] }

            let deduped, findings = Build.runPass Overloads.dedupeOverloads model

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Widened, "Scope.add" ]
                "the twin is a finding"

            match deduped.Decls |> List.pick (function FsInterface d -> Some d | _ -> None) with
            | decl ->
                Expect.equal
                    (decl.Members
                     |> List.map (function
                         | FsMethod m -> m.Parameters.Head.Type
                         | FsProperty p -> p.Type
                         | FsConstructor c -> c.Return
                         | FsIndexer i -> i.Value))
                    [ FsNamed "DOMTargets"; FsString ]
                    "first of the obj pair survives; the string overload is distinct"

        testCase "detect-tagged-unions reads the arms' fields, not the arm types" <| fun _ ->
            // The arm properties become the case fields, because that is what Fable's erasure
            // writes: `Circle(radius = 2.0)` -> `{ kind: "circle", radius: 2 }`. The tag itself
            // is not a field - Fable writes it from the compiled name.
            let arm id tag extra =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol (id * 10) "kind" SymbolFlags.Property) tag
                          Build.resolvedMember (Build.symbol (id * 10 + 1) extra SymbolFlags.Property) 2 ] }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 20; 21 ] }

            let model =
                { Build.shapeModel (
                      union
                      :: arm 20 7 "radius"
                      :: arm 21 8 "width"
                      :: stringLiteral 7 "circle"
                      :: stringLiteral 8 "round-rect"
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Shape" ] }

            let shaped, findings = Build.runPass TaggedUnions.detectTaggedUnions model

            match shaped.Decls |> List.pick (function FsTaggedUnion d -> Some d | _ -> None) with
            | decl ->
                Expect.equal decl.Tag "kind" "the discriminant the checker proved"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Name, c.CompiledName))
                    [ "Circle", Some "circle"; "RoundRect", Some "round-rect" ]
                    "case names derive from the tag values, which keep a CompiledName"

                Expect.equal
                    (decl.Cases |> List.map (fun c -> c.Fields |> List.map (fun f -> f.Name, f.Type)))
                    [ [ "radius", FsFloat ]; [ "width", FsFloat ] ]
                    "the tag is not a field; everything else is"

            Expect.equal (findings |> List.map _.Tier) [ Exact ] "a tagged union costs no fidelity"

        testCase "detect-tagged-unions leaves an arm that is not plain data alone" <| fun _ ->
            let method' id =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    CallSignatures = [ Build.signature [] 4 ] }

            let arm id tag =
                { Build.facts (Build.typeResponse id TypeFlags.Object) with
                    Members =
                        [ Build.resolvedMember (Build.symbol (id * 10) "kind" SymbolFlags.Property) tag
                          Build.resolvedMember (Build.symbol (id * 10 + 1) "run" SymbolFlags.Method) 30 ] }

            let union =
                { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 20; 21 ] }

            let model =
                { Build.shapeModel (
                      union
                      :: method' 30
                      :: arm 20 7
                      :: arm 21 8
                      :: stringLiteral 7 "a"
                      :: stringLiteral 8 "b"
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Shape" ] }

            let shaped, findings = Build.runPass TaggedUnions.detectTaggedUnions model

            Expect.isEmpty
                (shaped.Decls |> List.choose (function FsTaggedUnion d -> Some d | _ -> None))
                "a method has no case-field form, so the erased union stands"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "the missed match is reported"

        testCase "shape-aliases twin unions chain to the smallest id, never cycle" <| fun _ ->
            // Two declared unions over the same member set: only the smaller id is canonical.
            // The larger abbreviates to it; the smaller resolves structurally. An A <-> B
            // abbreviation cycle here sends fsc into non-termination once a generic
            // instantiation references it, so the chain must strictly decrease.
            let twin id =
                { Build.facts (Build.typeResponse id TypeFlags.Union) with
                    UnionMembers = [ 1; 2 ] }

            let model =
                { Build.shapeModel (twin 10 :: twin 11 :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "ScrollThresholdValue"; 11, "TimelinePosition" ] }

            let shaped, findings = Build.runPass Aliases.shapeAliases model

            let targets =
                shaped.Decls
                |> List.choose (function
                    | FsAbbrev d -> Some(d.Name, d.Target)
                    | _ -> None)

            Expect.equal
                targets
                [ "ScrollThresholdValue", FsErasedUnion [ FsString; FsFloat ]
                  "TimelinePosition", FsNamed "ScrollThresholdValue" ]
                "the smaller twin resolves structurally, the larger references it"

            Expect.equal (findings |> List.map _.Tier) [] "an erased union costs no fidelity"

        testCase "a lib type Fable.Core binds is referenced, not widened" <| fun _ ->
            // O7 widens the compiler-lib group for want of a shipped binding. For `Promise`
            // there is one, every generated file opens it, and the argument is shaped at its
            // own position rather than disappearing with the wrapper.
            let model = Build.shapeModel (libType 10 "Promise" [ 1 ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsApp("JS.Promise", [ FsString ])) "the binding is written"
            Expect.isEmpty findings "and nothing is lost saying it that way"

        testCase "a lib type carrying more arguments than Fable's binding drops the extras, loudly" <| fun _ ->
            // TypeScript's lib made the typed arrays generic in their backing buffer; Fable's
            // abbreviation is not. Naming the type is still worth more than `obj`, and the
            // parameter that goes missing is exactly what a finding is for.
            let model = Build.shapeModel (libType 10 "Uint8Array" [ 1 ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsNamed "JS.Uint8Array") "the name survives the lib's drift"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "and the dropped argument is recorded"

        testCase "a lib type with too few arguments is some other type wearing the name" <| fun _ ->
            // A `Map` of one argument is not the `Map` this table is about. Guessing here would
            // emit code that does not compile, so it widens the way it always did.
            let model = Build.shapeModel (libType 10 "Map" [ 1 ] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "no binding is claimed"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "and the widening is the ordinary one"

        testCase "a lib name nothing shipped binds keeps widening" <| fun _ ->
            // The synchronous iteration protocol has no Fable.Core binding, and `seq<'T>` is not
            // one however alike the two look. `Response` is the DOM's version of the same
            // situation: it is a lib name, but `fetch` lives in `Fable.Fetch` rather than in the
            // `Fable.Browser.*` family this generator's table is built from.
            let model = Build.shapeModel (libType 10 "Iterable" [ 1 ] :: libType 11 "Response" [] :: Build.primitives)

            for id in [ 10; 11 ] do
                let reference, findings = Spec.typeRef Build.context model None "x" id
                Expect.equal reference FsObj "still obj"
                Expect.equal (findings |> List.map _.Tier) [ Widened ] "and still says so"

        testCase "a lib name a Fable.Browser package binds is referenced, not widened" <| fun _ ->
            // The DOM half of the same disposition. The table is generated from the family's
            // assemblies, so this asserts the rule that reads it, not the entry: a DOM name in
            // an ordinary position writes its `Browser.Types` spelling and loses nothing.
            let model = Build.shapeModel (libType 10 "EventTarget" [] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsNamed "Browser.Types.EventTarget") "the binding is written"
            Expect.isEmpty findings "and nothing is lost saying it that way"

        testCase "a DOM name bound at two arities takes the one the reference fits" <| fun _ ->
            // `CustomEvent` is in `Browser.Event` both bare and generic, so arity is part of the
            // table's key rather than a property of the name. A reference carrying an argument
            // reaches the generic binding; a bare one reaches the other.
            let model =
                Build.shapeModel (libType 10 "CustomEvent" [ 1 ] :: libType 11 "CustomEvent" [] :: Build.primitives)

            let generic, genericFindings = Spec.typeRef Build.context model None "x" 10
            let bare, bareFindings = Spec.typeRef Build.context model None "x" 11

            Expect.equal generic (FsApp("Browser.Types.CustomEvent", [ FsString ])) "the argument is carried"
            Expect.isEmpty genericFindings "exactly"
            Expect.equal bare (FsNamed "Browser.Types.CustomEvent") "and the bare form is the bare binding"
            Expect.isEmpty bareFindings "also exactly"

        testCase "a DOM name two packages of the family both define widens" <| fun _ ->
            // `Browser.Types.Range` is declared by `Browser.IndexedDB` and by
            // `Browser.MediaStream`, and no qualification picks one. The ambiguity is resolved
            // when the table is generated - by leaving the name out - so what reaches here is
            // an ordinary miss.
            let model = Build.shapeModel (libType 10 "Range" [] :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "an ambiguous name is not written"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "and the widening is the ordinary one"

        testCase "a package's own type named like a lib type is untouched" <| fun _ ->
            // The table is keyed by name, so what keeps it from hijacking a package's own
            // `Promise` is the group: this one is the entry package's, and it ships.
            let model =
                { Build.shapeModel (
                      { Build.facts (Build.typeResponse 10 TypeFlags.Object) with SymbolName = Some "Promise" }
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Promise" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsNamed "Promise") "the declaration this run generates wins"
            Expect.isEmpty findings "and no lib binding is invented over it"

        testCase "a primitive intersected with a marker object reads as a branded primitive" <| fun _ ->
            // `type UserId = string & { __brand: "UserId" }`. The marker exists only to make the
            // type nominal, which is exactly what a unit of measure is - so the reference is the
            // primitive carrying the measure the declaration emits (§4.6, D11).
            let model =
                { Build.shapeModel (intersection 10 [ 1; 11 ] :: marker 11 "__brand" 1 :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "UserId" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsBranded(FsString, "UserId")) "the brand is written at the use"
            Expect.isEmpty findings "and costs nothing: the measure says what the intersection said"

        testCase "a brand the run never declared falls back to its primitive, loudly" <| fun _ ->
            // The same intersection with no name of its own. A measure is a declaration, so an
            // anonymous brand has nothing to carry and the nominality is what is lost.
            let model = Build.shapeModel (intersection 10 [ 1; 11 ] :: marker 11 "__brand" 1 :: Build.primitives)

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsString "the primitive survives"
            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "the brand does not, and says so"

        testCase "an intersection whose object half carries a real member is not a brand" <| fun _ ->
            // `string & { count: number }` has a member a caller can actually read, so reading it
            // as a brand would throw that member away and call the result exact.
            let model =
                { Build.shapeModel (intersection 10 [ 1; 11 ] :: marker 11 "count" 2 :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "Counted" ] }

            let reference, findings = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference FsObj "no brand, and no shape either yet"
            Expect.equal (findings |> List.map _.Tier) [ Widened ] "the widening is recorded"

        testCase "a brand over a union distributes and is still one brand" <| fun _ ->
            // `boolean & Marker` reaches us as `(true & Marker) | (false & Marker)`: the checker
            // distributes, and the arms are its own working, carrying no names. One brand, not two.
            let model =
                { Build.shapeModel (
                      { Build.facts (Build.typeResponse 10 TypeFlags.Union) with UnionMembers = [ 12; 13 ] }
                      :: intersection 12 [ 3; 11 ]
                      :: intersection 13 [ 3; 11 ]
                      :: marker 11 "__brand" 1
                      :: Build.primitives
                  ) with
                    DeclNames = Map.ofList [ 10, "Verified" ] }

            let reference, _ = Spec.typeRef Build.context model None "x" 10

            Expect.equal reference (FsBranded(FsBool, "Verified")) "the distribution is undone"

        testCase "shape-aliases emits a measure, not an abbreviation, for a brand" <| fun _ ->
            // The name can only be spent once, and the measure is what spends it: uses read
            // `string<UserId>`, so there is no abbreviation left to write.
            let model =
                { Build.shapeModel (intersection 10 [ 1; 11 ] :: marker 11 "__brand" 1 :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "UserId" ] }

            let shaped, findings = Build.runPass Aliases.shapeAliases model

            Expect.equal
                (shaped.Decls
                 |> List.choose (function
                     | FsMeasure d -> Some(d.Name, d.Primitive)
                     | _ -> None))
                [ "UserId", FsString ]
                "one measure over the primitive it brands"

            Expect.isEmpty
                (shaped.Decls |> List.choose (function FsAbbrev d -> Some d.Name | _ -> None))
                "and no abbreviation competing for the name"

            Expect.equal (findings |> List.map _.Tier) [ Ergonomic ] "the idiom is recorded once, at the declaration"

        testCase "shape-aliases emits a phantom for a computation that names none of its parameters" <| fun _ ->
            // `type Unwrap<T> = T extends Array<infer E> ? E : T`: a conditional the checker
            // could not finish. It binds `T` through the alias, but there is nothing on the
            // right for `T` to appear in, and F# has no unused type variable in an
            // abbreviation - so the declaration keeps its name and arity as a phantom.
            let conditional =
                { Build.facts (Build.typeResponse 10 TypeFlags.Conditional) with
                    AliasTypeArguments = [ 20 ] }

            let model =
                { Build.shapeModel (conditional :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "Unwrap" ] }

            let shaped, findings = Build.runPass Aliases.shapeAliases model

            let phantoms =
                shaped.Decls
                |> List.choose (function
                    | FsPhantom d -> Some(d.Name, d.TypeParameters |> List.map _.Name, d.Carrier)
                    | _ -> None)

            Expect.equal phantoms [ "Unwrap", [ "T" ], FsObj ] "the arity survives, over an obj carrier"

            Expect.isEmpty
                (shaped.Decls |> List.choose (function FsAbbrev d -> Some d.Name | _ -> None))
                "and it is not also written as an abbreviation"

            Expect.isTrue
                (findings |> List.exists (fun f -> f.Tier = Widened && f.Message.Contains "erased phantom"))
                "the manifest says a phantom is what it got"

        testCase "shape-aliases carries a template literal's phantom on a string" <| fun _ ->
            // `` type Prefixed<T extends string> = `x-${T}` ``. Whatever it interpolates, the
            // value is a string at runtime, so the phantom says so rather than obj.
            let template =
                { Build.facts (Build.typeResponse 10 TypeFlags.TemplateLiteral) with
                    AliasTypeArguments = [ 20 ] }

            let model =
                { Build.shapeModel (template :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "Prefixed" ] }

            let shaped, _ = Build.runPass Aliases.shapeAliases model

            Expect.equal
                (shaped.Decls
                 |> List.choose (function
                     | FsPhantom d -> Some(d.Name, d.Carrier)
                     | _ -> None))
                [ "Prefixed", FsString ]
                "a template literal is a string at runtime"

        testCase "shape-aliases leaves an alias that does name its parameter an abbreviation" <| fun _ ->
            // The other side of the same test: `type Alias<T> = T[]` has somewhere for `T` to
            // appear, so it stays an ordinary abbreviation and no phantom is invented for it.
            let array' =
                { Build.facts
                    { Build.typeResponse 10 TypeFlags.Object with
                        ObjectFlags = ValueSome ObjectFlags.Reference
                        Target = ValueSome 90 } with
                    SymbolName = Some "Array"
                    TypeArguments = [ 20 ]
                    AliasTypeArguments = [ 20 ] }

            let model =
                { Build.shapeModel (array' :: typeParam 20 "T" :: Build.primitives) with
                    DeclNames = Map.ofList [ 10, "Alias" ] }

            let shaped, _ = Build.runPass Aliases.shapeAliases model

            Expect.isEmpty
                (shaped.Decls |> List.choose (function FsPhantom d -> Some d.Name | _ -> None))
                "nothing here is a phantom"

            Expect.equal
                (shaped.Decls
                 |> List.choose (function
                     | FsAbbrev d -> Some(d.Name, d.TypeParameters |> List.map _.Name, d.Target)
                     | _ -> None))
                [ "Alias", [ "T" ], FsArray(FsTypeVar "T") ]
                "the parameter is named on the right, so an abbreviation holds it"

        testCase "order-declarations puts declarations in source order, Exports last" <| fun _ ->
            let interface' name order =
                FsInterface
                    { Name = name
                      Docs = ""
                      Tags = []
                      Order = Some order
                      TypeParameters = []
                      Inherits = []
                      Members = []
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ interface' "B" { File = "b.ts"; NodeIndex = 5 }
                          interface' "A" { File = "a.ts"; NodeIndex = 9 } ]
                    ExportMembers =
                        [ 0,
                          { Name = "make"
                            Docs = ""
                            Tags = []
                            TypeParameters = []
                            Binding = ImportNamed "make"
                            Body = ExportValue FsFloat
                            Settable = false } ] }

            let ordered, _ = Build.runPass Ordering.orderDeclarations model

            let names =
                ordered.Decls
                |> List.map (function
                    | FsInterface decl -> decl.Name
                    | FsExports _ -> "<exports>"
                    | decl -> failtest $"unexpected decl %A{decl}")

            Expect.equal names [ "A"; "B"; "<exports>" ] "file order first, Exports last"
            Expect.isEmpty ordered.ExportMembers "consumed into the Exports decl"

        testCase "repair-arity keeps an alias whose target lost its parameters, as a phantom" <| fun _ ->
            // `type Params<'P> = obj` is FS0035. The erased phantom `shape-aliases` writes for a
            // computation it cannot reproduce takes the unused variable, so the name and the
            // arity survive and `Params<string>` still resolves.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsAbbrev
                              { Name = "Params"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = [ { Name = "P"; Constraint = None } ]
                                Target = FsObj }
                          FsInterface
                              { Name = "Context"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                    [ FsProperty
                                          { Name = "params"
                                            Docs = ""
                                            Tags = []
                                            ReadOnly = true
                                            Type = FsApp("Params", [ FsString ]) } ]
                                CreateOverloads = []
                                Statics = [] } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            match repaired.Decls with
            | [ FsPhantom phantom; FsInterface decl ] ->
                Expect.equal phantom.Name "Params" "the alias keeps its name"
                Expect.equal (phantom.TypeParameters |> List.map _.Name) [ "P" ] "and its arity"
                Expect.equal phantom.Carrier FsObj "the resolved target is what the private case carries"

                match decl.Members with
                | [ FsProperty p ] -> Expect.equal p.Type (FsApp("Params", [ FsString ])) "the reference stands"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected the phantom and the interface, got %A{decls}"

            Expect.equal
                (findings |> List.map (fun f -> f.Key, f.Tier, f.Symbol))
                [ "RA006", Widened, "Params" ]
                "the erasure is the only loss"

        testCase "repair-arity erases the surplus parameter and keeps the rest of the head" <| fun _ ->
            // `type ExcludeStrict<T, U extends T> = Exclude<T, U>` resolves to `T`: `U` is
            // declared and never reaches the target. Dropping the declaration took the whole
            // export with it; the phantom keeps both parameters, so `ExcludeStrict<A, B>`
            // applies at the arity it was written with.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsAbbrev
                              { Name = "ExcludeStrict"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters =
                                    [ { Name = "T"; Constraint = None }; { Name = "U"; Constraint = None } ]
                                Target = FsTypeVar "T" }
                          FsInterface
                              { Name = "Holder"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                    [ FsProperty
                                          { Name = "narrowed"
                                            Docs = ""
                                            Tags = []
                                            ReadOnly = true
                                            Type = FsApp("ExcludeStrict", [ FsString; FsFloat ]) } ]
                                CreateOverloads = []
                                Statics = [] } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            match repaired.Decls with
            | [ FsPhantom phantom; FsInterface decl ] ->
                Expect.equal (phantom.TypeParameters |> List.map _.Name) [ "T"; "U" ] "both parameters stay on the head"
                Expect.equal phantom.Carrier (FsTypeVar "T") "the one the target uses carries the value"

                match decl.Members with
                | [ FsProperty p ] ->
                    Expect.equal p.Type (FsApp("ExcludeStrict", [ FsString; FsFloat ])) "the application keeps its arity"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected the phantom and the interface, got %A{decls}"

            Expect.equal
                (findings |> List.map (fun f -> f.Key, f.Symbol))
                [ "RA006", "ExcludeStrict" ]
                "one finding, on the alias"

        testCase "repair-arity leaves an alias whose target uses every parameter" <| fun _ ->
            // The negative: `type EveryParameter<'T, 'R> = Func<'T, 'R>` is a legal abbreviation,
            // so it stays one.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsAbbrev
                              { Name = "EveryParameter"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters =
                                    [ { Name = "T"; Constraint = None }; { Name = "R"; Constraint = None } ]
                                Target = FsDelegate([ FsTypeVar "T" ], FsTypeVar "R") } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            Expect.equal repaired.Decls model.Decls "unchanged"
            Expect.isEmpty findings "and nothing to report"

        testCase "repair-arity drops an alias whose head names one variable twice" <| fun _ ->
            // Wave three, lane K collapsed the signatures that share a bound, so what still
            // arrives this way is one name declared under two bounds - `setter-lab`'s
            // `DivergentBound`. F# rejects the head at either arity, so the phantom is no
            // repair and the declaration goes.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsAbbrev
                              { Name = "DivergentBound"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters =
                                    [ { Name = "T"; Constraint = None }
                                      { Name = "U"; Constraint = None }
                                      { Name = "U"; Constraint = None } ]
                                Target = FsDelegate([ FsObj ], FsObj) } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            Expect.isEmpty repaired.Decls "the declaration goes"

            Expect.equal
                (findings |> List.map (fun f -> f.Key, f.Symbol))
                [ "RA001", "DivergentBound" ]
                "the drop is reported, not the phantom"

        testCase "repair-arity widens a generic named without its arguments" <| fun _ ->
            // FS0033: `PagesFunctionContext` takes three arguments and this position has none.
            let generic =
                FsInterface
                    { Name = "Ctx"
                      Docs = ""
                      Tags = []
                      Order = None
                      TypeParameters = [ { Name = "Env"; Constraint = None } ]
                      Inherits = []
                      Members = []
                      CreateOverloads = []
                      Statics = [] }

            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ generic
                          FsAbbrev
                              { Name = "Handler"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Target = FsDelegate([ FsNamed "Ctx" ], FsNamed "Other") } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            match repaired.Decls with
            | [ _; FsAbbrev decl ] ->
                Expect.equal
                    decl.Target
                    (FsDelegate([ FsObj ], FsNamed "Other"))
                    "the generic widened; a name this run does not declare is left alone (O7)"
            | decls -> failtest $"expected the interface and the alias, got %A{decls}"

            Expect.equal (findings |> List.map _.Tier) [ Widened ] "one widening, reported"

        testCase "repair-arity demotes a settable property that holds no value" <| fun _ ->
            // FS0252: `[__BRAND]: never` shapes to `unit`, which cannot be a setter's type.
            let model =
                { Build.shapeModel [] with
                    Decls =
                        [ FsInterface
                              { Name = "Branded"
                                Docs = ""
                                Tags = []
                                Order = None
                                TypeParameters = []
                                Inherits = []
                                Members =
                                    [ FsProperty
                                          { Name = "__BRAND"
                                            Docs = ""
                                            Tags = []
                                            ReadOnly = false
                                            Type = FsUnit } ]
                                CreateOverloads = []
                                Statics = [] } ] }

            let repaired, findings = Build.runPass Arity.repairArity model

            match repaired.Decls with
            | [ FsInterface decl ] ->
                match decl.Members with
                | [ FsProperty p ] -> Expect.isTrue p.ReadOnly "the setter is gone, the member reads"
                | members -> failtest $"expected one property, got %A{members}"
            | decls -> failtest $"expected the interface, got %A{decls}"

            Expect.equal (findings |> List.map (fun f -> f.Tier, f.Symbol)) [ Ergonomic, "Branded" ] "reported"

        testCase "audit-coverage reports an export nothing represented" <| fun _ ->
            let model =
                { Build.shapeModel [] with
                    Harvest = { Exports = [ Build.export "Gone" (Build.symbol 300 "Gone" SymbolFlags.TypeAlias) ] } }

            let _, findings = Build.runPass Coverage.auditCoverage model

            Expect.equal
                (findings |> List.map (fun f -> f.Tier, f.Symbol))
                [ Escape, "Gone" ]
                "the dropped export is a finding, not silence"

        testCase "the pipeline fold stamps findings with the pass that made them" <| fun _ ->
            let degrading: Pass<int> =
                { Name = "always-degrades"
                  Run = fun _ n -> async { return Degraded(n + 1, [ Finding.make "x" TypeReference.AnyToObj ]) } }

            let model, findings =
                Async.RunSynchronously(Pipeline.runTier Build.context [ degrading ] 0)

            Expect.equal model 1 "advanced"
            Expect.equal (findings |> List.map _.Pass) [ "always-degrades" ] "stamped"
    ]
