namespace Xantham.Generator

open System
open System.Collections.Concurrent
open FSharp.Reflection

/// How faithfully a generated construct represents its TypeScript source, per
/// `docs/plans/generator-type-mapping.md` §1. Declaration order is severity order: a symbol's
/// tier is the worst tier among its findings, and structural comparison on this type is that
/// "worst".
type Tier =
    /// The F# type accepts and rejects exactly what TypeScript does.
    | Exact
    /// Meaning preserved, spelling made idiomatic - e.g. `null | undefined` hoisted to `option`.
    | Ergonomic
    /// Information TypeScript had was dropped - e.g. a union collapsed to `obj`.
    | Widened
    /// The construct is not represented; the consumer is on their own.
    | Escape

// -------------------------------------------------------------------------------------------------
// The finding catalogue. Every kind of finding the generator can raise is a case of one of the
// unions below, grouped by the pass (or shared helper) that raises it. The manifest key is
// derived, not written: the union's `Prefix` plus the case's 1-based declaration position, so
// `SI001` is the first case of the `SI` union. That makes the catalogue exhaustive - a case
// without a tier attribute fails at first use, a case without a key cannot exist - at the price
// of one rule: **cases are append-only**. Inserting or reordering renumbers every key after the
// edit; `Findings.test.fs` snapshots the table so that cannot happen silently.
// -------------------------------------------------------------------------------------------------

/// The tier a finding case carries. Applied through the four sealed subclasses below, so the
/// choice is restricted to the `Tier` cases and never spelled as a string.
[<AbstractClass>]
type TierAttribute(tier: Tier) =
    inherit Attribute()
    member _.Tier = tier

[<Sealed>]
type ExactAttribute() =
    inherit TierAttribute(Exact)

[<Sealed>]
type ErgonomicAttribute() =
    inherit TierAttribute(Ergonomic)

[<Sealed>]
type WidenedAttribute() =
    inherit TierAttribute(Widened)

[<Sealed>]
type EscapeAttribute() =
    inherit TierAttribute(Escape)

/// The manifest key prefix of a finding union, and for a per-pass union the name of the pass it
/// belongs to. Explicit rather than derived from the type name so a rename never moves a key.
[<Sealed; AttributeUsage(AttributeTargets.Class)>]
type PrefixAttribute(prefix: string, pass: string) =
    inherit Attribute()
    /// A shared-helper union: raised under whichever pass is running.
    new(prefix: string) = PrefixAttribute(prefix, null)
    member _.Prefix = prefix
    member _.Pass = Option.ofObj pass

/// What every finding union provides beyond its attributes: the human-readable message, which
/// interpolates the case's payload and so cannot be an attribute.
type IFindingKind =
    abstract Message: string

/// A finding union read once through reflection: prefix, tag reader and per-case tiers, cached
/// per type. Reading is a dictionary hit and a tag read; the reflection happens once.
type Coder =
    {
        Prefix: string
        /// The pass a per-pass union belongs to; `None` for the shared helpers.
        Pass: string option
        Tag: obj -> int
        Tiers: Tier[]
        Names: string[]
    }

module Coder =
    let private cache = ConcurrentDictionary<Type, Coder>()

    let private precompute (kindType: Type) : Coder =
        if not (FSharpType.IsUnion(kindType, true)) then
            failwith $"finding kind {kindType.Name} is not a union"

        let prefix, pass =
            match kindType.GetCustomAttributes(typeof<PrefixAttribute>, false) with
            | [| :? PrefixAttribute as attribute |] -> attribute.Prefix, attribute.Pass
            | _ -> failwith $"finding union {kindType.Name} carries no [<Prefix>] attribute"

        let cases = FSharpType.GetUnionCases(kindType, true)

        let tiers =
            cases
            |> Array.map (fun case ->
                match case.GetCustomAttributes(typeof<TierAttribute>) with
                | [| :? TierAttribute as attribute |] -> attribute.Tier
                | _ -> failwith $"finding case {kindType.Name}.{case.Name} carries no tier attribute")

        let reader = FSharpValue.PreComputeUnionTagReader(kindType, true)

        {
            Prefix = prefix
            Pass = pass
            Tag = reader
            Tiers = tiers
            Names = cases |> Array.map _.Name
        }

    /// The coder for a finding union, computed on first use.
    let forType (kindType: Type) : Coder = cache.GetOrAdd(kindType, precompute)

    /// The manifest key of a case index under a coder: prefix plus 1-based position, three digits.
    let key (coder: Coder) (tag: int) = $"{coder.Prefix}{tag + 1:D3}"

    /// The union a case value belongs to. A case with fields is compiled to a nested subclass
    /// of its union, so the runtime type is one step below the type the attributes sit on.
    let private unionOf (kind: IFindingKind) =
        let runtime = kind.GetType()

        if not (isNull runtime.BaseType) && FSharpType.IsUnion(runtime.BaseType, true) then
            runtime.BaseType
        else
            runtime

    let private coderOf (kind: IFindingKind) = forType (unionOf kind)

    /// The manifest key of a finding kind.
    let code (kind: IFindingKind) =
        let coder = coderOf kind
        key coder (coder.Tag kind)

    /// The tier a finding kind was declared with.
    let tier (kind: IFindingKind) =
        let coder = coderOf kind
        coder.Tiers[coder.Tag kind]

/// One thing a pass had to say about a symbol: a widening, a drop, or an ergonomic rewrite.
/// Findings are the raw material of the fidelity manifest - a silent drop is a bug by
/// definition, so every non-Exact emission produces one of these.
type Finding =
    {
        /// The pass that produced the finding. Stamped by `Pipeline.runTier`, so passes leave it
        /// empty and cannot misreport themselves.
        Pass: string
        /// The symbol concerned, qualified from the exported name down: `Options.onlyFirst`.
        Symbol: string
        /// Which finding, with whatever detail it carries. Key, tier and message derive from it.
        Kind: IFindingKind
    }

    member this.Key = Coder.code this.Kind
    member this.Tier = Coder.tier this.Kind
    member this.Message = this.Kind.Message

module Finding =
    /// A finding not yet stamped with its pass; the pipeline fold fills `Pass` in.
    let make symbol (kind: #IFindingKind) =
        {
            Pass = ""
            Symbol = symbol
            Kind = kind
        }

// -------------------------------------------------------------------------------------------------
// Shared helpers: raised from wherever a type is written at a reference position, under
// whichever pass is running. Append-only.
// -------------------------------------------------------------------------------------------------

/// `Shape.typeRef` and the helpers under it: what happens to a type at a reference position.
[<Prefix "TR">]
type TypeReference =
    | [<Widened>] SelfReferenceThroughUnnamed
    | [<Widened>] TypeNotResolved of reason: string
    | [<Escape>] MissingFromTypeTable of typeId: int
    | [<Widened>] LoneEnumMemberToFloat
    | [<Widened>] LoneEnumMemberToString
    | [<Widened>] StringLiteralToString
    | [<Widened>] NumericLiteralToFloat
    | [<Escape>] AnyToObj
    | [<Widened>] UnknownToObj
    | [<Ergonomic>] PolymorphicThisAsDeclaringType
    | [<Widened>] ThisOutsideDeclaration
    | [<Widened>] TypeParameterOutOfScopeToConstraint of constraintName: string
    | [<Widened>] TypeParameterOutOfScope
    | [<Widened>] TypeFlagsNotMapped of flags: string
    | [<Ergonomic>] KeyOfOpenOperand of name: string
    | [<Widened>] KeyOfOperandOutOfScope
    | [<Ergonomic>] UnnamedBrandToPrimitive
    | [<Widened>] IntersectionOverNonObject
    | [<Widened>] IntersectionNotDeclared
    | [<Widened>] IndexedAccessNoForm
    | [<Widened>] AnonymousInReferencedGroup
    | [<Widened>] GlobalThisToObj
    | [<Widened>] NotAmongGeneratedDeclarations of shown: string
    | [<Ergonomic>] LibExtraTypeArgumentsDropped of name: string * given: int * fsharpName: string * arity: int
    | [<Ergonomic>] LibBindingLoss of note: string
    | [<Widened>] ConstrainedArgumentWidened of name: string * bound: string
    | [<Widened>] ArgumentNotBoundWithConstraint of variable: string * name: string * bound: string
    | [<Widened>] TupleRestToArray
    | [<Widened>] TupleArityNoForm of components: int
    | [<Widened>] CallableWithoutSignatures
    | [<Widened>] CallbackOverloadsFromFirst of overloads: int
    | [<Ergonomic>] NullableHoistedToOption
    | [<Widened>] OnlyNullUndefinedToUnit
    | [<Widened>] EmptyUnionToObj
    | [<Widened>] UnionWithObjArm
    | [<Widened>] UnionTooWide of arms: int * cap: int
    | [<Widened>] TemplateLiteralToString
    | [<Widened>] StringMappingToString
    | [<Widened>] BigIntLiteralToBigInt
    | [<Widened>] ObjectTypeToObj
    | [<Widened>] SymbolNoBinding
    | [<Widened>] UniqueSymbolNoBinding
    | [<Widened>] ConstructorObjectNotDeclared of constructs: string
    /// F# subtyping is nominal where TypeScript's is structural: an argument that merely has
    /// the constraint's members is FS0001 at the application, so it is written as the
    /// constraint, the way an argument that widened to `obj` already is.
    | [<Widened>] ArgumentNotASubtypeOfConstraint of argument: string * name: string * bound: string

    interface IFindingKind with
        member this.Message =
            match this with
            | SelfReferenceThroughUnnamed -> "type refers to itself through unnamed shapes; widened to obj"
            | TypeNotResolved reason -> $"type not resolved ({reason}); widened to obj"
            | MissingFromTypeTable typeId -> $"type#{typeId} missing from the type table; widened to obj"
            | LoneEnumMemberToFloat -> "lone enum member widened to float"
            | LoneEnumMemberToString -> "lone enum member widened to string"
            | StringLiteralToString -> "string literal type widened to string (doc-noted, §4.2)"
            | NumericLiteralToFloat -> "numeric literal type widened to float (doc-noted, §4.2)"
            | AnyToObj -> "any maps to obj"
            | UnknownToObj -> "unknown maps to obj (D8)"
            | PolymorphicThisAsDeclaringType -> "polymorphic this reads as the declaring type"
            | ThisOutsideDeclaration -> "this type outside a declaration; widened to obj"
            | TypeParameterOutOfScopeToConstraint name ->
                $"type parameter is not in scope here; widened to its constraint {name}"
            | TypeParameterOutOfScope -> "type parameter is not in scope here; widened to obj"
            | TypeFlagsNotMapped flags -> $"type flags {flags} not mapped yet; widened to obj"
            | KeyOfOpenOperand name -> $"keyof over an open operand reads as keyof<'{name}> (§4.10)"
            | KeyOfOperandOutOfScope -> "keyof over an operand not in scope here; widened to obj"
            | UnnamedBrandToPrimitive ->
                "an unnamed brand has no measure to carry; widened to the primitive it brands (§4.6)"
            | IntersectionOverNonObject ->
                "intersection over a non-object operand has no members to flatten; widened to obj (§4.6)"
            | IntersectionNotDeclared -> "intersection of object types not declared by this run; widened to obj (§4.6)"
            | IndexedAccessNoForm -> "indexed access has no F# form here; widened to obj"
            | AnonymousInReferencedGroup -> "anonymous type in a referenced group cannot be templated; widened to obj"
            | GlobalThisToObj -> "typeof globalThis is the whole global scope; widened to obj"
            | NotAmongGeneratedDeclarations shown -> $"{shown} is not among the generated declarations; widened to obj"
            | LibExtraTypeArgumentsDropped(name, given, fsharpName, arity) ->
                $"{name} carries {given} type arguments where {fsharpName} takes {arity}; the extras are dropped"
            | LibBindingLoss note -> note
            | ConstrainedArgumentWidened(name, bound) ->
                $"argument to {name}'s constrained parameter widened to obj; written as the constraint {bound}"
            | ArgumentNotBoundWithConstraint(variable, name, bound) ->
                $"'{variable} is not bound with {name}'s constraint; the argument is written as the constraint {bound}"
            | TupleRestToArray ->
                "tuple with a rest element widened to an array (§4.12 leaves the erased carrier to a fixture)"
            | TupleArityNoForm components -> $"{components}-element tuple has no F# tuple form; widened to an array"
            | CallableWithoutSignatures -> "callable type without signatures; widened to obj"
            | CallbackOverloadsFromFirst overloads -> $"callback with {overloads} overloads shaped from the first"
            | NullableHoistedToOption -> "null/undefined union members hoisted to option"
            | OnlyNullUndefinedToUnit -> "union of only null/undefined members maps to unit"
            | EmptyUnionToObj -> "empty union widened to obj"
            | UnionWithObjArm -> "union with an obj arm widened to obj (an erased union over obj is no safer)"
            | UnionTooWide(arms, cap) ->
                $"union of {arms} distinct types widened to obj (D4 caps the erased union at {cap})"
            | TemplateLiteralToString ->
                "template literal type reads as the string it is at runtime; the pattern is not carried (§4.11)"
            | StringMappingToString ->
                "intrinsic string mapping reads as string; the transform it applies is not carried (§4.11)"
            | BigIntLiteralToBigInt -> "bigint literal type widened to bigint (doc-noted, §4.2)"
            | ObjectTypeToObj ->
                "TypeScript's object maps to obj, which also admits the primitives object excludes (§4.1)"
            | SymbolNoBinding -> "symbol has no binding in Fable.Core 5.2.0; widened to obj (§4.1)"
            | UniqueSymbolNoBinding ->
                "unique symbol has no binding in Fable.Core 5.2.0 and no F# form for its identity; widened to obj"
            | ConstructorObjectNotDeclared constructs ->
                $"typeof {constructs} is a constructor object this run does not declare; widened to obj (§4.4)"
            | ArgumentNotASubtypeOfConstraint(argument, name, bound) ->
                $"{argument} does not inherit {name}'s constraint {bound}; the argument is written as the constraint"

/// Type parameter binding: `Shape.typeParamsOf`, `aliasTypeParams`, key variables and erasure.
[<Prefix "TP">]
type TypeParameters =
    | [<Widened>] UnnamedTypeParameter of id: int
    | [<Ergonomic>] ConstraintDropped of name: string
    | [<Ergonomic>] GenericFunctionHoisted
    | [<Ergonomic>] KeyWithIndexedAccess of operand: string * result: string
    | [<Ergonomic>] KeyOverOperand of operand: string
    | [<Widened>] TypeParameterErased of name: string

    interface IFindingKind with
        member this.Message =
            match this with
            | UnnamedTypeParameter id -> $"type parameter #{id} has no name to write; its uses widen to obj"
            | ConstraintDropped name -> $"constraint on '{name}' has no F# form and is dropped (§4.9)"
            | GenericFunctionHoisted -> "generic function type hoisted onto the alias; F# has no rank-2 form (§4.9)"
            | KeyWithIndexedAccess(operand, result) ->
                $"key over '{operand}' with its indexed access reads as typekeyof<'{operand},'{result}> (§4.10)"
            | KeyOverOperand operand -> $"key over '{operand}' reads as keyof<'{operand}> (§4.10)"
            | TypeParameterErased name -> $"type parameter '{name}' is erased: every use of it widened away"

/// Member and parameter shaping: `Shape.parametersOf` and `membersOf`.
[<Prefix "MB">]
type Members =
    | [<Ergonomic>] OptionalParameterAsOption
    | [<Widened>] SymbolKeyedMemberDropped
    | [<Ergonomic>] OptionalMemberAsOption
    | [<Ergonomic>] IndexSignatureAsIndexer

    interface IFindingKind with
        member this.Message =
            match this with
            | OptionalParameterAsOption -> "optional parameter reads as option"
            | SymbolKeyedMemberDropped -> "symbol-keyed member dropped (unrepresentable in F#)"
            | OptionalMemberAsOption -> "optional member reads as option"
            | IndexSignatureAsIndexer -> "index signature reads as an EmitIndexer Item member (§4.10)"

// -------------------------------------------------------------------------------------------------
// Per-pass unions, in pipeline order. Append-only.
// -------------------------------------------------------------------------------------------------

/// `harvest-globals`.
[<Prefix("HG", "harvest-globals")>]
type HarvestGlobals =
    | [<Escape>] AmbientModuleDropped
    | [<Escape>] UnwritableGlobalDropped
    | [<Escape>] NothingHarvested of entryFile: string

    interface IFindingKind with
        member this.Message =
            match this with
            | AmbientModuleDropped ->
                "global dropped - an ambient module declaration; its members are importable from that specifier, which the generator does not emit yet"
            | UnwritableGlobalDropped -> "global dropped - its name cannot be written as an F# declaration"
            | NothingHarvested entryFile ->
                $"{entryFile} declares neither a module nor any ambient global - nothing harvested"

/// `resolve-export-types`.
[<Prefix("RE", "resolve-export-types")>]
type ResolveExportTypes =
    | [<Escape>] FacetNotResolved of facet: string * reason: string

    interface IFindingKind with
        member this.Message =
            match this with
            | FacetNotResolved(facet, reason) -> $"{facet} not resolved: {reason}"

/// `resolve-type-table`.
[<Prefix("RT", "resolve-type-table")>]
type ResolveTypeTable =
    | [<Widened>] FrontierNotResolved of count: int * depth: int
    | [<Widened>] TypeNotResolved of reason: string

    interface IFindingKind with
        member this.Message =
            match this with
            | FrontierNotResolved(count, depth) ->
                $"{count} types not resolved: beyond the depth cutoff ({depth}) - the frontier of instantiations still growing after that many generations"
            | TypeNotResolved reason -> $"not resolved: {reason}"

/// `classify-literal-unions`.
[<Prefix("LU", "classify-literal-unions")>]
type ClassifyLiteralUnions =
    | [<Exact>] NonStringLiteralCase

    interface IFindingKind with
        member this.Message =
            match this with
            | NonStringLiteralCase -> "non-string literal case carries CompiledValue (D12)"

/// `detect-tagged-unions`.
[<Prefix("DT", "detect-tagged-unions")>]
type DetectTaggedUnions =
    | [<Ergonomic>] ArmNotPlainData of tag: string
    | [<Exact>] TaggedUnion of tag: string

    interface IFindingKind with
        member this.Message =
            match this with
            | ArmNotPlainData tag -> $"discriminated by '{tag}', but an arm is not plain data; left as an erased union"
            | TaggedUnion tag -> $"discriminated union on '{tag}' (D4)"

/// `shape-interfaces`.
[<Prefix("SI", "shape-interfaces")>]
type ShapeInterfaces =
    | [<Widened>] HybridLosesCallSignatures
    /// The undifferentiated case the three below split out of: a base with no F# name at this
    /// position at all. Its members are still flattened in, so nothing of the member set is
    /// lost - only the upcast.
    | [<Ergonomic>] BaseMembersFlattened
    | [<Ergonomic>] IntersectionFlattened of operands: int
    /// A constructor object declared as an interface of its own (§4.4): F# has no first-class
    /// type of a class object, so `typeof Request` at a member position reads as a named
    /// declaration whose construct signatures are `[<EmitConstructor>]` `Create` members and
    /// whose properties are the class's statics.
    | [<Ergonomic>] ConstructorObjectDeclared of signatures: int
    /// §4.4's heritage rule, emitted: the base is a declaration this run writes as an
    /// interface, so the derived type upcasts to it. Its members are declared again beside the
    /// `inherit` - F# admits the redeclaration - which is what keeps `Create` and the member
    /// list exact when a *second* base is not inheritable.
    | [<Exact>] BaseInherited of name: string
    /// A base that names something, but not something this run declares as an interface: a
    /// `Fable.Core.JS.*` or `Browser.Types.*` binding, a referenced group's templated name, an
    /// abbreviation over a non-object type. `inherit` on a non-interface is FS0887, and this
    /// run cannot prove which of those a foreign name is, so its members are flattened.
    | [<Ergonomic>] BaseNotDeclaredHere of name: string
    /// A base that reaches this declaration again through the inherit graph. TypeScript has no
    /// cyclic heritage, but two type ids can share one F# name, so the graph is checked rather
    /// than assumed: `inherit` here is FS0954, so the base stays flattened.
    | [<Ergonomic>] BaseWouldCycle of name: string

    interface IFindingKind with
        member this.Message =
            match this with
            | HybridLosesCallSignatures ->
                "callable-and-properties hybrid loses its call signatures (Invoke emission is future work)"
            | BaseMembersFlattened ->
                "base has no F# name at this position; its members are flattened in and the is-a relation is not emitted (§4.4)"
            | IntersectionFlattened operands ->
                $"intersection of {operands} object types flattened into one interface (the is-a relation to its operands is not emitted, §4.6)"
            | ConstructorObjectDeclared signatures ->
                $"constructor object declared as its own interface; {signatures} construct signature(s) read as EmitConstructor Create members (§4.4)"
            | BaseInherited name -> $"base {name} is inherited: the is-a relation is emitted (§4.4)"
            | BaseNotDeclaredHere name ->
                $"base {name} is not declared by this run as an interface; its members are flattened in and the is-a relation is not emitted (§4.4)"
            | BaseWouldCycle name ->
                $"base {name} reaches this declaration again; inheriting it is FS0954, so its members are flattened in instead (§4.4)"

/// `shape-aliases`.
[<Prefix("SA", "shape-aliases")>]
type ShapeAliases =
    | [<Ergonomic>] BrandAsMeasure
    | [<Widened>] PhantomComputation

    interface IFindingKind with
        member this.Message =
            match this with
            | BrandAsMeasure ->
                "branding intersection emitted as a unit of measure; uses read as the branded primitive (§4.6, D11)"
            | PhantomComputation ->
                "type-level computation over an unresolved operand; emitted as an erased phantom, which casts are the only use of"

/// `shape-classes`.
[<Prefix("SC", "shape-classes")>]
type ShapeClasses =
    | [<Escape>] ClassWithoutValueType
    /// A static F# will not let the class carry. The only case is a name an instance member
    /// already has: F# admits that between two methods and nowhere else - property over
    /// property is FS0441, method over property FS0434, and a static property under an abstract
    /// method is FS3214 at every use of it.
    | [<Widened>] StaticMemberDropped
    | [<Widened>] StaticReadOnly
    /// A class whose instance type declares nothing has no interface of its own for the statics
    /// to sit on, and F# has no free-standing static member.
    | [<Widened>] StaticWithoutDeclaration
    /// A static the checker calls a method, whose type the resolve tier read identity-only -
    /// the group it is declared in is not shipped, so there are no signatures to shape from.
    /// It is not a settable static, and saying so (`StaticReadOnly`) was misleading.
    | [<Widened>] StaticMethodWithoutSignatures of declaredIn: string

    interface IFindingKind with
        member this.Message =
            match this with
            | ClassWithoutValueType -> "class export without a value type; constructor dropped"
            | StaticMemberDropped ->
                "static member dropped: its name is an instance member's, which F# admits only between two methods"
            | StaticReadOnly ->
                "a settable static is emitted read-only: Fable compiles an assignment to an imported static as a call"
            | StaticWithoutDeclaration ->
                "static member dropped: the class declares no instance members, so this run emits no type to carry it"
            | StaticMethodWithoutSignatures declaredIn ->
                $"static method emitted as a value: its type is declared in {declaredIn}, which this run resolves identity-only, so there are no signatures to shape"

/// `shape-exports`.
[<Prefix("SE", "shape-exports")>]
type ShapeExports =
    | [<Escape>] NoValueType

    interface IFindingKind with
        member this.Message =
            match this with
            | NoValueType -> "no value type in the table; export dropped"

/// `synthesize-paramobjects`.
[<Prefix("SP", "synthesize-paramobjects")>]
type SynthesizeParamObjects =
    | [<Ergonomic>] ParamObjectSynthesized

    interface IFindingKind with
        member this.Message =
            match this with
            | ParamObjectSynthesized -> "ParamObject Create synthesized (D3)"

/// `dedupe-overloads`.
[<Prefix("DO", "dedupe-overloads")>]
type DedupeOverloads =
    | [<Widened>] OverloadDropped

    interface IFindingKind with
        member this.Message =
            match this with
            | OverloadDropped -> "overload dropped: identical to an earlier one after widening"

/// `repair-arity`.
[<Prefix("RA", "repair-arity")>]
type RepairArity =
    | [<Widened>] GenericAliasDropped
    | [<Widened>] ReferenceToDroppedAlias of name: string
    | [<Widened>] GenericWithoutArguments of name: string
    | [<Widened>] ArityMismatch of name: string * given: int * declared: int
    | [<Ergonomic>] ReadWithoutWrite of name: string

    interface IFindingKind with
        member this.Message =
            match this with
            | GenericAliasDropped ->
                "generic alias dropped: its target widened away every type parameter, and F# has no unused type variable in an abbreviation"
            | ReferenceToDroppedAlias name -> $"reference to the dropped generic alias {name} widened to obj"
            | GenericWithoutArguments name ->
                $"{name} is generic and this position has no arguments to apply; widened to obj"
            | ArityMismatch(name, given, declared) ->
                $"{name} applied to {given} arguments but declares {declared}; widened to obj"
            | ReadWithoutWrite name ->
                $"{name} reads but does not write: its type holds no value, and F# has no setter of type unit"

/// `audit-coverage`.
[<Prefix("AC", "audit-coverage")>]
type AuditCoverage =
    | [<Escape>] ExportNotRepresented

    interface IFindingKind with
        member this.Message =
            match this with
            | ExportNotRepresented -> "export not represented in the generated output"

module FindingCatalogue =
    /// Every finding union, in the order the manifest legend lists them. The snapshot test
    /// enumerates these; a union missing here has keys nothing guards.
    let unions: Type list =
        [
            typeof<TypeReference>
            typeof<TypeParameters>
            typeof<Members>
            typeof<HarvestGlobals>
            typeof<ResolveExportTypes>
            typeof<ResolveTypeTable>
            typeof<ClassifyLiteralUnions>
            typeof<DetectTaggedUnions>
            typeof<ShapeInterfaces>
            typeof<ShapeAliases>
            typeof<ShapeClasses>
            typeof<ShapeExports>
            typeof<SynthesizeParamObjects>
            typeof<DedupeOverloads>
            typeof<RepairArity>
            typeof<AuditCoverage>
        ]

    /// Pass name -> the key prefix of the union that pass owns. Passes without a union never
    /// raise findings of their own and are absent.
    let passPrefixes: Map<string, string> =
        unions
        |> List.choose (fun kindType ->
            let coder = Coder.forType kindType
            coder.Pass |> Option.map (fun pass -> pass, coder.Prefix))
        |> Map.ofList

    /// How the manifest labels a pass: its prefix and name, `SI - shape-interfaces`, or the bare
    /// name for a pass that owns no union.
    let passLabel (pass: string) =
        match Map.tryFind pass passPrefixes with
        | Some prefix -> $"{prefix} - {pass}"
        | None -> pass

    /// The whole key table: `(key, union case name, tier)` for every case of every union.
    let table () =
        [
            for kindType in unions do
                let coder = Coder.forType kindType

                for tag in 0 .. coder.Names.Length - 1 do
                    Coder.key coder tag, $"{kindType.Name}.{coder.Names[tag]}", coder.Tiers[tag]
        ]
