// =======================================================================
// Signal-based builder types for the TypeScript AST.
//
// Every ref<TypeKey> becomes Signal<TypeKey>; every ref<TsXxxBuilder voption>
// becomes Signal<SXxx voption> (where SXxx either holds its own signal slots
// or is already the final type when sub-types have no deferred resolution).
// Documentation arrays collapse to TsComment list — they are written once.
//
// .Build() on each S-type produces the final TsXxx / TsAstNode value with
// no intermediate Builder-suffixed stop.
//
// The active-pattern module Patterns.Builder provides inline case tests over
// the original TsAstNodeBuilder DU (kept for compatibility with existing
// reader code), while Patterns.SBuilder does the same for STsAstNodeBuilder.
// =======================================================================
[<AutoOpen>]
module Xantham.Fable.Types.ReactiveBuilders

open Xantham
open Xantham.Fable
open Xantham.Fable.Types.Signal

// -----------------------------------------------------------------------
// Core slot helpers
// -----------------------------------------------------------------------

/// <summary>
/// A reactive, mutable slot for a resolved <see cref="T:Xantham.TypeKey"/>.
/// Replaces <c>ref&lt;TypeKey&gt;</c> in every builder record.
/// </summary>
/// <remarks>
/// Create an unfilled slot with <c>TypeSignal.pending()</c>.
/// Fill it once with <c>.Set(key)</c>.
/// Any <c>Signal.auto</c> computation that reads <c>.Value</c> will
/// re-evaluate automatically when the slot is filled.
/// </remarks>
type TypeSignal = Signal<TypeKey>

module TypeSignal =
    /// An unfilled slot initialised to <c>TypeKey.Unknown</c>.
    let pending () : TypeSignal = Signal.source TypeKindPrimitive.Unknown.TypeKey
    /// A pre-filled, immutable slot.
    let ofKey (key: TypeKey) : TypeSignal = Signal.source key

/// Inline helper: collect filled optional slots into a list.
let inline private resolve (slots: Signal<'a voption> array) =
    slots
    |> Array.choose (_.Value >> ValueOption.toOption)
    |> Array.toList

// -----------------------------------------------------------------------
// S-builder record types
// (Ordered by dependency — leaf types first.)
// -----------------------------------------------------------------------

/// Signal-based equivalent of <c>TsEnumCaseBuilder</c>, builds to <see cref="T:Xantham.TsEnumCase"/>.
type SEnumCaseBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    Value: TsLiteral
    Documentation: TsComment list
} with
    member this.Build() : TsEnumCase =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          Value = this.Value
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsEnumTypeBuilder</c>, builds to <see cref="T:Xantham.TsEnumType"/>.
type SEnumTypeBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    /// Reactive slots; each is filled when an enum case is processed.
    Members: Signal<SEnumCaseBuilder voption> array
    Documentation: TsComment list
} with
    member this.Build() : TsEnumType =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          Members =
              this.Members
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsVariableBuilder</c>, builds to <see cref="T:Xantham.TsVariable"/>.
type SVariableBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    Type: TypeSignal
    Documentation: TsComment list
} with
    member this.Build() : TsVariable =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          Type = this.Type.Value
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsParameterBuilder</c>, builds to <see cref="T:Xantham.TsParameter"/>.
type SParameterBuilder = {
    Name: string
    IsOptional: bool
    IsSpread: bool
    Type: TypeSignal
    Documentation: TsComment list
} with
    member this.Build() : TsParameter =
        { Name = this.Name
          IsOptional = this.IsOptional
          IsSpread = this.IsSpread
          Type = this.Type.Value
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsTypeParameterBuilder</c>, builds to <see cref="T:Xantham.TsTypeParameter"/>.
type STypeParameterBuilder = {
    Name: string
    Constraint: TypeSignal voption
    Default: TypeSignal voption
    Documentation: TsComment list
} with
    member this.Build() : TsTypeParameter =
        { Name = this.Name
          Constraint = this.Constraint |> ValueOption.map _.Value |> ValueOption.toOption
          Default     = this.Default   |> ValueOption.map _.Value |> ValueOption.toOption
          Documentation = this.Documentation }

type InlinedSTypeParameterBuilder = {
    Type: TypeKey
    TypeParameter: STypeParameterBuilder
} with
    member this.Build(): TypeKey * TsTypeParameter =
        this.Type, this.TypeParameter.Build()

/// Signal-based equivalent of <c>TsPropertyBuilder</c>, builds to <see cref="T:Xantham.TsProperty"/>.
type SPropertyBuilder = {
    Name: string
    Type: TypeSignal
    IsStatic: bool
    IsOptional: bool
    IsPrivate: bool
    Accessor: TsAccessor
    Documentation: TsComment list
} with
    member this.Build() : TsProperty =
        { Name = this.Name
          Type = this.Type.Value
          IsStatic = this.IsStatic
          IsOptional = this.IsOptional
          IsPrivate = this.IsPrivate
          Accessor = this.Accessor
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsGetAccessorBuilder</c>, builds to <see cref="T:Xantham.TsGetAccessor"/>.
type SGetAccessorBuilder = {
    Name: string
    Type: TypeSignal
    IsStatic: bool
    IsPrivate: bool
} with
    member this.Build() : TsGetAccessor =
        { Name = this.Name; Type = this.Type.Value
          IsStatic = this.IsStatic; IsPrivate = this.IsPrivate }

/// Signal-based equivalent of <c>TsSetAccessorBuilder</c>, builds to <see cref="T:Xantham.TsSetAccessor"/>.
type SSetAccessorBuilder = {
    Name: string
    ArgumentType: TypeSignal
    IsStatic: bool
    IsPrivate: bool
    Documentation: TsComment list
} with
    member this.Build() : TsSetAccessor =
        { Name = this.Name
          ArgumentType = this.ArgumentType.Value
          IsStatic = this.IsStatic
          IsPrivate = this.IsPrivate
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsMethodBuilder</c>, builds to <see cref="T:Xantham.TsMethod"/>.
type SMethodBuilder = {
    Name: string
    /// Reactive parameter slots — filled when each parameter is processed.
    Parameters: Signal<SParameterBuilder voption> array
    Type: TypeSignal
    IsOptional: bool
    IsStatic: bool
    Documentation: TsComment list
} with
    member this.Build() : TsMethod =
        { Name = this.Name
          Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Type = this.Type.Value
          IsOptional = this.IsOptional
          IsStatic = this.IsStatic
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsCallSignatureBuilder</c>, builds to <see cref="T:Xantham.TsCallSignature"/>.
type SCallSignatureBuilder = {
    Parameters: Signal<SParameterBuilder voption> array
    Type: TypeSignal
    Documentation: TsComment list
} with
    member this.Build() : TsCallSignature =
        { Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Type = this.Type.Value
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsConstructSignatureBuilder</c>, builds to <see cref="T:Xantham.TsConstructSignature"/>.
type SConstructSignatureBuilder = {
    Type: TypeSignal
    Parameters: Signal<SParameterBuilder voption> array
} with
    member this.Build() : TsConstructSignature =
        { Type = this.Type.Value
          Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList }

/// Signal-based equivalent of <c>TsConstructorBuilder</c>, builds to <see cref="T:Xantham.TsConstructor"/>.
type SConstructorBuilder = {
    Parameters: Signal<SParameterBuilder voption> array
    Documentation: TsComment list
} with
    member this.Build() : TsConstructor =
        { Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsIndexSignatureBuilder</c>, builds to <see cref="T:Xantham.TsIndexSignature"/>.
type SIndexSignatureBuilder = {
    Parameters: Signal<SParameterBuilder voption> array
    Type: TypeSignal
    IsReadOnly: bool
} with
    member this.Build() : TsIndexSignature =
        { Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Type = this.Type.Value
          IsReadOnly = this.IsReadOnly }

/// <summary>
/// Signal-based DU equivalent of <c>TsMemberBuilder</c>, builds to <see cref="T:Xantham.TsMember"/>.
/// </summary>
type SMemberBuilder =
    | Method of SMethodBuilder
    | Property of SPropertyBuilder
    | GetAccessor of SGetAccessorBuilder
    | SetAccessor of SSetAccessorBuilder
    | CallSignature of SCallSignatureBuilder
    | IndexSignature of SIndexSignatureBuilder
    | ConstructSignature of SConstructSignatureBuilder
    member this.Build() : TsMember =
        match this with
        | Method m            -> m.Build() |> TsMember.Method
        | Property p          -> p.Build() |> TsMember.Property
        | GetAccessor g       -> g.Build() |> TsMember.GetAccessor
        | SetAccessor s       -> s.Build() |> TsMember.SetAccessor
        | CallSignature c     -> c.Build() |> TsMember.CallSignature
        | IndexSignature i    -> i.Build() |> TsMember.IndexSignature
        | ConstructSignature c -> c.Build() |> TsMember.ConstructSignature

/// Signal-based equivalent of <c>TsTypeLiteralBuilder</c>, builds to <see cref="T:Xantham.TsTypeLiteral"/>.
type STypeLiteralBuilder = {
    Members: Signal<SMemberBuilder voption> array
} with
    member this.Build() : TsTypeLiteral =
        { Members =
              this.Members
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList }

/// Signal-based equivalent of <c>TsTypeReferenceBuilder</c>, builds to <see cref="T:Xantham.TsTypeReference"/>.
type STypeReferenceBuilder = {
    Type: TypeSignal
    TypeArguments: TypeSignal array
    ResolvedType: Signal<TypeKey voption> voption
} with
    member this.Build() : TsTypeReference =
        { Type = this.Type.Value
          TypeArguments = this.TypeArguments |> Array.map _.Value |> Array.toList
          ResolvedType  = this.ResolvedType  |> ValueOption.bind _.Value |> ValueOption.toOption }

/// Signal-based equivalent of <c>TsInterfaceHeritageBuilder</c>, builds to <see cref="T:Xantham.TsInterfaceHeritage"/>.
type SInterfaceHeritageBuilder = {
    Extends: Signal<STypeReferenceBuilder voption> array
} with
    member this.Build() : TsInterfaceHeritage =
        { Extends =
              this.Extends
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList }

/// Signal-based equivalent of <c>TsClassHeritageBuilder</c>, builds to <see cref="T:Xantham.TsClassHeritage"/>.
type SClassHeritageBuilder = {
    Implements: Signal<STypeReferenceBuilder voption> voption
    Extends: Signal<STypeReferenceBuilder voption> array
} with
    member this.Build() : TsClassHeritage =
        { Implements =
              this.Implements
              |> ValueOption.bind _.Value
              |> ValueOption.toOption
              |> Option.map _.Build()
          Extends =
              this.Extends
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList }

/// Signal-based equivalent of <c>TsInterfaceBuilder</c>, builds to <see cref="T:Xantham.TsInterface"/>.
type SInterfaceBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Enumerable: bool
    Name: string
    Members: Signal<SMemberBuilder voption> array
    TypeParameters: Signal<InlinedSTypeParameterBuilder voption> array
    Documentation: TsComment list
    Heritage: Signal<SInterfaceHeritageBuilder voption>
} with
    member this.Build() : TsInterface =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Enumerable = this.Enumerable
          Name = this.Name
          Members =
              this.Members
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          TypeParameters =
              this.TypeParameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Documentation = this.Documentation
          Heritage =
              this.Heritage.Value
              |> ValueOption.toOption
              |> Option.map _.Build()
              |> Option.defaultValue { Extends = [] } }

/// Signal-based equivalent of <c>TsIndexAccessTypeBuilder</c>, builds to <see cref="T:Xantham.TsIndexAccessType"/>.
type SIndexAccessTypeBuilder = {
    Object: TypeSignal
    Index: TypeSignal
} with
    member this.Build() : TsIndexAccessType =
        { Object = this.Object.Value; Index = this.Index.Value }

/// Signal-based equivalent of <c>TsTypeAliasBuilder</c>, builds to <see cref="T:Xantham.TsTypeAlias"/>.
type SAliasBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    Type: TypeSignal
    TypeParameters: Signal<InlinedSTypeParameterBuilder voption> array
    Documentation: TsComment list
} with
    member this.Build() : TsTypeAlias =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          Type = this.Type.Value
          TypeParameters =
              this.TypeParameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsModuleBuilder</c>, builds to <see cref="T:Xantham.TsModule"/>.
type SModuleBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    IsNamespace: bool
    IsRecursive: bool
    /// Each slot is filled with the TypeKey of a resolved member type.
    Types: TypeSignal array
} with
    member this.Build() : TsModule =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          IsNamespace = this.IsNamespace
          IsRecursive = this.IsRecursive
          Types = this.Types |> Array.map _.Value |> Array.toList }

/// Signal-based equivalent of <c>TsSubstitutionTypeBuilder</c>, builds to <see cref="T:Xantham.TsSubstitutionType"/>.
type SSubstitutionTypeBuilder = {
    Base: TypeSignal
    Constraint: TypeSignal
} with
    member this.Build() : TsSubstitutionType =
        { Base = this.Base.Value; Constraint = this.Constraint.Value }

/// Signal-based equivalent of <c>TsClassBuilder</c>, builds to <see cref="T:Xantham.TsClass"/>.
type SClassBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Enumerable: bool
    Name: string
    Constructors: Signal<SConstructorBuilder voption> array
    Members: Signal<SMemberBuilder voption> array
    TypeParameters: Signal<InlinedSTypeParameterBuilder voption> array
    Heritage: Signal<SClassHeritageBuilder voption>
} with
    member this.Build() : TsClass =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Enumerable = this.Enumerable
          Name = this.Name
          Constructors =
              this.Constructors
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Members =
              this.Members
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          TypeParameters =
              this.TypeParameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Heritage =
              this.Heritage.Value
              |> ValueOption.toOption
              |> Option.map _.Build()
              |> Option.defaultValue { Implements = None; Extends = [] } }

/// Signal-based equivalent of <c>TsConditionalTypeBuilder</c>, builds to <see cref="T:Xantham.TsConditionalType"/>.
type SConditionalTypeBuilder = {
    Check: TypeSignal
    Extends: TypeSignal
    True: TypeSignal
    False: TypeSignal
} with
    member this.Build() : TsConditionalType =
        { Check = this.Check.Value; Extends = this.Extends.Value
          True = this.True.Value; False = this.False.Value }

/// Signal-based equivalent of <c>TsFunctionBuilder</c>, builds to <see cref="T:Xantham.TsFunction"/>.
type SFunctionBuilder = {
    Source: ModuleName voption
    FullyQualifiedName: string array
    Name: string
    IsDeclared: bool
    Type: TypeSignal
    Parameters: Signal<SParameterBuilder voption> array
    TypeParameters: Signal<InlinedSTypeParameterBuilder voption> array
    Documentation: TsComment list
} with
    member this.Build() : TsFunction =
        { Source = this.Source |> ValueOption.toOption |> Option.map (fun (ModuleName s) -> s)
          FullyQualifiedName = Array.toList this.FullyQualifiedName
          Name = this.Name
          IsDeclared = this.IsDeclared
          Type = this.Type.Value
          Parameters =
              this.Parameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          TypeParameters =
              this.TypeParameters
              |> Array.choose (_.Value >> ValueOption.toOption >> Option.map _.Build())
              |> Array.toList
          Documentation = this.Documentation }

/// Signal-based equivalent of <c>TsTypeUnionBuilder</c>, builds to <see cref="T:Xantham.TsTypeUnion"/>.
type STypeUnionBuilder = {
    Types: TypeSignal array
} with
    member this.Build() : TsTypeUnion =
        TsTypeUnion (this.Types |> Array.map _.Value |> Array.toList)

/// Signal-based equivalent of <c>TsTypeIntersectionBuilder</c>, builds to <see cref="T:Xantham.TsTypeIntersection"/>.
type STypeIntersectionBuilder = {
    Types: TypeSignal array
} with
    member this.Build() : TsTypeIntersection =
        TsTypeIntersection (this.Types |> Array.map _.Value |> Array.toList)

/// Signal-based equivalent of <c>TsTupleElementTypeBuilder</c>, builds to <see cref="T:Xantham.TsTupleElementType"/>.
type STupleElementTypeBuilder = {
    Type: TypeSignal
    IsOptional: bool
    IsRest: bool
} with
    member this.Build() : TsTupleElementType =
        { Type = this.Type.Value; IsOptional = this.IsOptional; IsRest = this.IsRest }

/// Signal-based equivalent of <c>TsTupleElementBuilder</c>, builds to <see cref="T:Xantham.TsTupleElement"/>.
type STupleElementBuilder =
    | FixedLabeled of string * STupleElementTypeBuilder
    | Variadic of TypeSignal
    | Fixed of STupleElementTypeBuilder
    member this.Build() : TsTupleElement =
        match this with
        | FixedLabeled (name, v) -> TsTupleElement.FixedLabeled (name, v.Build())
        | Variadic s             -> TsTupleElement.Variadic s.Value
        | Fixed v                -> TsTupleElement.Fixed (v.Build())

/// Signal-based equivalent of <c>TsTupleBuilder</c>, builds to <see cref="T:Xantham.TsTuple"/>.
type STupleBuilder = {
    IsReadOnly: bool
    FixedLength: int
    MinRequired: int
    Types: STupleElementBuilder array
} with
    member this.Build() : TsTuple =
        { IsReadOnly = this.IsReadOnly
          FixedLength = this.FixedLength
          MinRequired = this.MinRequired
          Types = this.Types |> Array.map _.Build() |> Array.toList }

/// Signal-based equivalent of <c>TsIndexBuilder</c>, builds to <see cref="T:Xantham.TsIndex"/>.
type SIndexBuilder = {
    Type: TypeSignal
} with
    member this.Build() : TsIndex = { Type = this.Type.Value }

/// Signal-based equivalent for template literal types, builds to <see cref="T:Xantham.TsTemplateLiteralType"/>.
type STemplateLiteralTypeBuilder = {
    Texts: string array
    Types: TypeSignal array
} with
    member this.Build() : TsTemplateLiteralType =
        { Texts = Array.toList this.Texts
          Types = this.Types |> Array.map _.Value |> Array.toList }

/// Signal-based equivalent of <c>TsTypePredicateBuilder</c>, builds to <see cref="T:Xantham.TsTypePredicate"/>.
type SPredicateBuilder = {
    ParameterName: string
    Type: TypeSignal
    IsAssertion: bool
} with
    member this.Build() : TsTypePredicate =
        { ParameterName = this.ParameterName
          Type = this.Type.Value
          IsAssertion = this.IsAssertion }

// -----------------------------------------------------------------------
// Top-level reactive builder DU
// -----------------------------------------------------------------------

/// <summary>
/// Signal-based equivalent of <c>TsAstNodeBuilder</c>.
/// Each case wraps the corresponding <c>SXxx</c> builder type.
/// Call <c>.Build()</c> to snapshot all signal values into a <see cref="T:PartasTypes.TsAstNode"/>.
/// </summary>
type STsAstNodeBuilder =
    | GlobalThis
    | Tuple of STupleBuilder
    | Interface of SInterfaceBuilder
    | Variable of SVariableBuilder
    | Primitive of TypeKindPrimitive
    | Predicate of SPredicateBuilder
    | Literal of TsLiteral
    | TypeLiteral of STypeLiteralBuilder
    | Property of SPropertyBuilder
    | Parameter of SParameterBuilder
    | TypeParameter of STypeParameterBuilder
    | IndexAccessType of SIndexAccessTypeBuilder
    | FunctionDeclaration of SFunctionBuilder
    | Method of SMethodBuilder
    | Alias of SAliasBuilder
    | Constructor of SConstructorBuilder
    | ConstructSignature of SConstructSignatureBuilder
    | IndexSignature of SIndexSignatureBuilder
    | Index of SIndexBuilder
    | TypeReference of STypeReferenceBuilder
    | Array of STypeReferenceBuilder
    | Enum of SEnumTypeBuilder
    | EnumCase of SEnumCaseBuilder
    | SubstitutionType of SSubstitutionTypeBuilder
    | Conditional of SConditionalTypeBuilder
    | Class of SClassBuilder
    | Union of STypeUnionBuilder
    | Intersection of STypeIntersectionBuilder
    | Optional of STypeReferenceBuilder
    | GetAccessor of SGetAccessorBuilder
    | SetAccessor of SSetAccessorBuilder
    | CallSignature of SCallSignatureBuilder
    | Module of SModuleBuilder
    | TemplateLiteral of STemplateLiteralTypeBuilder
    member this.Build() : TsAstNode =
        match this with
        | GlobalThis             -> TsAstNode.GlobalThis
        | Tuple v                -> v.Build() |> TsAstNode.Tuple
        | Interface v            -> v.Build() |> TsAstNode.Interface
        | Variable v             -> v.Build() |> TsAstNode.Variable
        | Primitive v            -> TsAstNode.Primitive v
        | Predicate v            -> v.Build() |> TsAstNode.Predicate
        | Literal v              -> TsAstNode.Literal v
        | TypeLiteral v          -> v.Build() |> TsAstNode.TypeLiteral
        | Property v             -> v.Build() |> TsAstNode.Property
        | Parameter v            -> v.Build() |> TsAstNode.Parameter
        | TypeParameter v        -> v.Build() |> TsAstNode.TypeParameter
        | IndexAccessType v      -> v.Build() |> TsAstNode.IndexAccessType
        | FunctionDeclaration v  -> v.Build() |> TsAstNode.FunctionDeclaration
        | Method v               -> v.Build() |> TsAstNode.Method
        | Alias v                -> v.Build() |> TsAstNode.Alias
        | Constructor v          -> v.Build() |> TsAstNode.Constructor
        | ConstructSignature v   -> v.Build() |> TsAstNode.ConstructSignature
        | IndexSignature v       -> v.Build() |> TsAstNode.IndexSignature
        | Index v                -> v.Build() |> TsAstNode.Index
        | TypeReference v        -> v.Build() |> TsAstNode.TypeReference
        | Array v                -> v.Build() |> TsAstNode.Array
        | Enum v                 -> v.Build() |> TsAstNode.Enum
        | EnumCase v             -> v.Build() |> TsAstNode.EnumCase
        | SubstitutionType v     -> v.Build() |> TsAstNode.SubstitutionType
        | Conditional v          -> v.Build() |> TsAstNode.Conditional
        | Class v                -> v.Build() |> TsAstNode.Class
        | Union v                -> v.Build() |> TsAstNode.Union
        | Intersection v         -> v.Build() |> TsAstNode.Intersection
        | Optional v             -> v.Build() |> TsAstNode.Optional
        | GetAccessor v          -> v.Build() |> TsAstNode.GetAccessor
        | SetAccessor v          -> v.Build() |> TsAstNode.SetAccessor
        | CallSignature v        -> v.Build() |> TsAstNode.CallSignature
        | Module v               -> v.Build() |> TsAstNode.Module
        | TemplateLiteral v      -> v.Build() |> TsAstNode.TemplateLiteral

// -----------------------------------------------------------------------
// Active patterns
// -----------------------------------------------------------------------

/// <summary>
/// Active patterns for <c>TsAstNodeBuilder</c> (the original ref-based DU)
/// and <c>STsAstNodeBuilder</c> (the signal-based DU).
/// </summary>
/// <example>
/// <code lang="fsharp">
/// match store.Glue with
/// | Patterns.Builder.Interface b -> b.Name
/// | Patterns.Builder.Method m    -> m.Name
/// | _                            -> "?"
///
/// match sstore.Glue with
/// | Patterns.SBuilder.Interface b -> b.Name
/// | Patterns.SBuilder.Method m    -> m.Name
/// | _                             -> "?"
/// </code>
/// </example>
[<AutoOpen>]
module Patterns =

    // --- STsAstNodeBuilder (signal-based) ---
    [<RequireQualifiedAccess>]
    module SBuilder =
        let inline (|GlobalThis|_|)         (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.GlobalThis       -> Some () | _ -> None
        let inline (|Interface|_|)          (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Interface      x -> Some x  | _ -> None
        let inline (|Class|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Class          x -> Some x  | _ -> None
        let inline (|Enum|_|)               (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Enum           x -> Some x  | _ -> None
        let inline (|EnumCase|_|)           (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.EnumCase       x -> Some x  | _ -> None
        let inline (|Alias|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Alias          x -> Some x  | _ -> None
        let inline (|Variable|_|)           (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Variable       x -> Some x  | _ -> None
        let inline (|FunctionDeclaration|_|)(b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.FunctionDeclaration x -> Some x | _ -> None
        let inline (|Module|_|)             (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Module         x -> Some x  | _ -> None
        let inline (|Primitive|_|)          (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Primitive      x -> Some x  | _ -> None
        let inline (|Literal|_|)            (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Literal        x -> Some x  | _ -> None
        let inline (|TypeLiteral|_|)        (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.TypeLiteral    x -> Some x  | _ -> None
        let inline (|Property|_|)           (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Property       x -> Some x  | _ -> None
        let inline (|Method|_|)             (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Method         x -> Some x  | _ -> None
        let inline (|Constructor|_|)        (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Constructor    x -> Some x  | _ -> None
        let inline (|Parameter|_|)          (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Parameter      x -> Some x  | _ -> None
        let inline (|TypeParameter|_|)      (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.TypeParameter  x -> Some x  | _ -> None
        let inline (|TypeReference|_|)      (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.TypeReference  x -> Some x  | _ -> None
        let inline (|Array|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Array          x -> Some x  | _ -> None
        let inline (|Optional|_|)           (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Optional       x -> Some x  | _ -> None
        let inline (|Union|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Union          x -> Some x  | _ -> None
        let inline (|Intersection|_|)       (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Intersection   x -> Some x  | _ -> None
        let inline (|Tuple|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Tuple          x -> Some x  | _ -> None
        let inline (|Conditional|_|)        (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Conditional    x -> Some x  | _ -> None
        let inline (|IndexAccessType|_|)    (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.IndexAccessType x -> Some x | _ -> None
        let inline (|IndexSignature|_|)     (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.IndexSignature  x -> Some x | _ -> None
        let inline (|Index|_|)              (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Index           x -> Some x | _ -> None
        let inline (|SubstitutionType|_|)   (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.SubstitutionType x -> Some x | _ -> None
        let inline (|CallSignature|_|)      (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.CallSignature   x -> Some x | _ -> None
        let inline (|ConstructSignature|_|) (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.ConstructSignature x -> Some x | _ -> None
        let inline (|GetAccessor|_|)        (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.GetAccessor     x -> Some x | _ -> None
        let inline (|SetAccessor|_|)        (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.SetAccessor     x -> Some x | _ -> None
        let inline (|Predicate|_|)          (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.Predicate       x -> Some x | _ -> None
        let inline (|TemplateLiteral|_|)    (b: STsAstNodeBuilder) = match b with STsAstNodeBuilder.TemplateLiteral  x -> Some x | _ -> None

// -----------------------------------------------------------------------
// Reactive TypeStore
// -----------------------------------------------------------------------

/// <summary>
/// A reactive store entry keyed by <c>IdentityKey</c> in <c>TypeScriptReader.SignalCache</c>.
/// </summary>
/// <remarks>
/// <para>
/// <c>Key</c> is the <c>TypeKey</c> (semantic <c>type.id</c>) of this entry — the value
/// that should be written into <c>TypeSignal</c> slots on other builders that reference
/// this type. It bridges the <c>IdentityKey</c>-keyed cache to the <c>TypeKey</c>-based
/// signal system.
/// </para>
/// <para>
/// <c>Glue</c> is the <c>STsAstNodeBuilder</c> whose internal <c>Signal</c> slots fire as
/// child nodes are resolved during traversal. Use <c>Signal.auto</c> at the call site to
/// derive reactive computations:
/// </para>
/// <code lang="fsharp">
/// // Derive a reactive name — auto-tracked over signal slots
/// let name = Signal.auto (fun () -> TypeStore.name store)
///
/// // Navigate the type reactively
/// let retType =
///     Signal.auto (fun () ->
///         match store.Glue with
///         | Patterns.SBuilder.Method m -> Some m.Type.Value
///         | _ -> None)
/// </code>
/// </remarks>
type TypeStore = {
    Key: TypeKey
    /// Pending signal for the builder. Fulfilled by the dispatcher; read via
    /// <c>.Value</c> to get the current <c>STsAstNodeBuilder voption</c>.
    Builder: PendingSignal<STsAstNodeBuilder>
}

[<RequireQualifiedAccess>]
module TypeStore =

    /// Snapshot the current builder to a plain <c>TsAstNode</c>, or <c>ValueNone</c>
    /// if the builder signal has not been filled yet.
    let build (s: TypeStore) : TsAstNode voption =
        s.Builder.Value |> ValueOption.map (fun b -> b.Build())

    // -----------------------------------------------------------------------
    // Reactive projections — read inside Signal.auto to auto-track.
    // -----------------------------------------------------------------------

    /// Display name if the builder carries one; <c>None</c> otherwise.
    /// Returns <c>None</c> when the builder signal is still pending.
    let name (s: TypeStore) : string option =
        s.Builder.Value
        |> ValueOption.toOption
        |> Option.bind (fun b ->
            match b with
            | Patterns.SBuilder.Interface b        -> Some b.Name
            | Patterns.SBuilder.Class b            -> Some b.Name
            | Patterns.SBuilder.Enum b             -> Some b.Name
            | Patterns.SBuilder.EnumCase b         -> Some b.Name
            | Patterns.SBuilder.Alias b            -> Some b.Name
            | Patterns.SBuilder.Variable b         -> Some b.Name
            | Patterns.SBuilder.FunctionDeclaration b -> Some b.Name
            | Patterns.SBuilder.Module b           -> Some b.Name
            | Patterns.SBuilder.Property b         -> Some b.Name
            | Patterns.SBuilder.Method b           -> Some b.Name
            | Patterns.SBuilder.GetAccessor b      -> Some b.Name
            | Patterns.SBuilder.SetAccessor b      -> Some b.Name
            | Patterns.SBuilder.Parameter b        -> Some b.Name
            | Patterns.SBuilder.TypeParameter b    -> Some b.Name
            | _                                    -> None)

    /// The primary resolved <c>TypeKey</c> if the builder carries one;
    /// reading this inside <c>Signal.auto</c> auto-tracks the slot.
    /// Returns <c>None</c> when the builder signal is still pending.
    let primaryType (s: TypeStore) : TypeKey option =
        s.Builder.Value
        |> ValueOption.toOption
        |> Option.bind (fun b ->
            match b with
            | Patterns.SBuilder.Alias b             -> Some b.Type.Value
            | Patterns.SBuilder.Variable b          -> Some b.Type.Value
            | Patterns.SBuilder.FunctionDeclaration b -> Some b.Type.Value
            | Patterns.SBuilder.Property b          -> Some b.Type.Value
            | Patterns.SBuilder.Method b            -> Some b.Type.Value
            | Patterns.SBuilder.CallSignature b     -> Some b.Type.Value
            | Patterns.SBuilder.ConstructSignature b -> Some b.Type.Value
            | Patterns.SBuilder.IndexSignature b    -> Some b.Type.Value
            | Patterns.SBuilder.GetAccessor b       -> Some b.Type.Value
            | Patterns.SBuilder.SetAccessor b       -> Some b.ArgumentType.Value
            | Patterns.SBuilder.Index b             -> Some b.Type.Value
            | Patterns.SBuilder.Predicate b         -> Some b.Type.Value
            | Patterns.SBuilder.TypeReference b
            | Patterns.SBuilder.Array b
            | Patterns.SBuilder.Optional b          -> Some b.Type.Value
            | Patterns.SBuilder.SubstitutionType b  -> Some b.Base.Value
            | _                                     -> None)

    /// Fully-qualified name segments if present.
    /// Returns <c>None</c> when the builder signal is still pending.
    let fullyQualifiedName (s: TypeStore) : string array option =
        s.Builder.Value
        |> ValueOption.toOption
        |> Option.bind (fun b ->
            match b with
            | Patterns.SBuilder.Interface b        -> Some b.FullyQualifiedName
            | Patterns.SBuilder.Class b            -> Some b.FullyQualifiedName
            | Patterns.SBuilder.Enum b             -> Some b.FullyQualifiedName
            | Patterns.SBuilder.Alias b            -> Some b.FullyQualifiedName
            | Patterns.SBuilder.Variable b         -> Some b.FullyQualifiedName
            | Patterns.SBuilder.FunctionDeclaration b -> Some b.FullyQualifiedName
            | Patterns.SBuilder.Module b           -> Some b.FullyQualifiedName
            | _                                    -> None)

    /// Source module string if present.
    /// Returns <c>None</c> when the builder signal is still pending.
    let source (s: TypeStore) : string option =
        s.Builder.Value
        |> ValueOption.toOption
        |> Option.bind (fun b ->
            match b with
            | Patterns.SBuilder.Interface b        -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.Class b            -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.Enum b             -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.Alias b            -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.Variable b         -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.FunctionDeclaration b -> b.Source |> ValueOption.toOption |> unbox
            | Patterns.SBuilder.Module b           -> b.Source |> ValueOption.toOption |> unbox
            | _                                    -> None)

    /// <c>true</c> when the builder is filled and its primary type slot still holds
    /// <c>TypeKey.Unknown</c>. Reading inside <c>Signal.auto</c> auto-tracks both the
    /// builder signal and the inner slot.
    let isPending (s: TypeStore) : bool =
        s.Builder.Value
        |> ValueOption.exists (fun b ->
            match b with
            | Patterns.SBuilder.Alias b            -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.Variable b         -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.Property b         -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.Method b           -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.CallSignature b    -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.ConstructSignature b -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | Patterns.SBuilder.Index b            -> b.Type.Value = TypeKindPrimitive.Unknown.TypeKey
            | _                                    -> false)
