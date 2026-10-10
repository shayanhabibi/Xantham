namespace Xantham.Generator.Customization

open Xantham.Generator

type SourceType
type SourceMember
type OutputTarget
type BindingType
type SemanticSnapshot
type ExtensionDiagnostic
type ResolvedSource
type ResolvedSnapshot
type ProjectionCompanion

/// The resolved value arms accepted by the first companion projection contract.
[<RequireQualifiedAccess>]
type ResolvedUnionArm =
    | StringLiteral of string
    | Number
    | Null
    | Undefined

/// Bounded input values captured before Shape; record property presence is separate from Undefined.
[<RequireQualifiedAccess>]
type ResolvedValueShape =
    | String
    | Number
    | Boolean
    | Null
    | Undefined
    | StringLiteral of string
    | Array of ResolvedValueShape
    | Record of ResolvedValueField list
    | Union of ResolvedValueShape list

and ResolvedValueField =
    {
        Name: string
        Optional: bool
        Shape: ResolvedValueShape
    }

type ResolvedOperation =
    {
        MethodName: string
        ParameterNames: string list
        ParameterOptional: bool list
        ParameterIndex: int
        FieldName: string option
    }

type AttributeValue =
    | String of string
    | Boolean of bool
    | Integer of int
    | Type of BindingType
    | Enum of typeName: string * caseName: string
    | Array of AttributeValue list

type AttributeSpec
type CompanionSpec
type InteropSpec
type ReplacementSpec
type ValidationCompiler
type EditBatch

type ExtensionIdentity =
    {
        Id: string
        Version: string
        Configuration: Map<string, string>
    }

/// Produces companions from compiler facts before F# shaping can widen those facts.
type ProjectionExtension =
    {
        Identity: ExtensionIdentity
        Transform: ResolvedSnapshot -> Result<ProjectionCompanion list, ExtensionDiagnostic list>
    }

type GeneratorExtension =
    {
        Identity: ExtensionIdentity
        Transform: SemanticSnapshot -> Result<EditBatch, ExtensionDiagnostic list>
    }

module Resolved =
    val tryFind: string -> string list -> ResolvedSnapshot -> ResolvedSource option
    /// Selects an instance-method parameter by package and TypeScript declaration names.
    val tryFindParameter: string -> string list -> string -> string -> ResolvedSnapshot -> ResolvedSource option

    /// Selects an optional field only when constructing that field alone omits no required siblings.
    val tryFindParameterField:
        string -> string list -> string -> string -> string -> ResolvedSnapshot -> ResolvedSource option

    val shape: ResolvedSource -> ResolvedSnapshot -> Result<ResolvedValueShape, ExtensionDiagnostic list>
    val operation: ResolvedSource -> ResolvedSnapshot -> Result<ResolvedOperation, ExtensionDiagnostic list>
    val package: ResolvedSource -> ResolvedSnapshot -> string
    val path: ResolvedSource -> ResolvedSnapshot -> string list
    val identity: ResolvedSource -> ResolvedSnapshot -> string
    val union: ResolvedSource -> ResolvedSnapshot -> Result<ResolvedUnionArm list, ExtensionDiagnostic list>
    val diagnostics: ResolvedSnapshot -> ExtensionDiagnostic list

module ProjectionCompanion =
    /// Seals the selected facts into a plan; a foreign source or unsupported union is rejected.
    val create: ResolvedSource -> string -> string list -> string -> ResolvedSnapshot -> ProjectionCompanion

    /// Seals the expected F# receiver name for validation against its finalized binding before emission.
    val forOperation:
        ResolvedSource -> string -> string -> string list -> string -> ResolvedSnapshot -> ProjectionCompanion

module Attribute =
    val create: string -> AttributeValue list -> AttributeSpec
    val onGetter: AttributeSpec -> AttributeSpec
    val onSetter: AttributeSpec -> AttributeSpec

module Interop =
    val property: string -> InteropSpec

module Replacement =
    val marker: ReplacementSpec
    val properties: SourceMember list -> SemanticSnapshot -> ReplacementSpec
    val withBases: BindingType list -> ReplacementSpec -> ReplacementSpec
    val raw: string -> string list -> BindingType list -> ReplacementSpec

module Compiler =
    val dotnet: string -> string list -> ValidationCompiler

module Edits =
    val empty: EditBatch
    val addAttribute: OutputTarget -> AttributeSpec -> EditBatch -> EditBatch
    val emitCompanion: CompanionSpec -> EditBatch -> EditBatch
    val replaceInterop: OutputTarget -> InteropSpec -> EditBatch -> EditBatch
    val replaceDeclaration: OutputTarget -> ReplacementSpec -> EditBatch -> EditBatch

module Companion =
    val create: string -> string -> SourceType -> SemanticSnapshot -> CompanionSpec
    val directProperties: CompanionSpec -> CompanionSpec
    val withProperties: SourceMember list -> CompanionSpec -> CompanionSpec
    val withBases: BindingType list -> CompanionSpec -> CompanionSpec
    val reference: CompanionSpec -> SemanticSnapshot -> BindingType

module Diagnostic =
    val code: ExtensionDiagnostic -> string
    val message: ExtensionDiagnostic -> string
    val source: ExtensionDiagnostic -> string option
    val create: string -> string -> string option -> ExtensionDiagnostic

module BindingType =
    val display: BindingType -> string

module Semantic =
    val members: SemanticSnapshot -> SourceMember list
    val types: SemanticSnapshot -> SourceType list
    val tryFind: string -> string list -> SemanticSnapshot -> SourceType option
    val descendants: SourceType -> SemanticSnapshot -> SourceType list
    val properties: SourceType -> SemanticSnapshot -> SourceMember list
    val bindingType: SourceMember -> SemanticSnapshot -> BindingType
    val declaringType: SourceMember -> SemanticSnapshot -> SourceType
    val jsName: SourceMember -> SemanticSnapshot -> string
    val isReadOnly: SourceMember -> SemanticSnapshot -> bool
    val isOptional: SourceMember -> SemanticSnapshot -> bool
    val outputTargets: SourceMember -> SemanticSnapshot -> OutputTarget list
    val declarationTarget: SourceType -> SemanticSnapshot -> OutputTarget option
    val package: SourceType -> SemanticSnapshot -> string
    val path: SourceType -> SemanticSnapshot -> string list
    val identity: SourceType -> SemanticSnapshot -> string
    val isSameDeclaration: SourceType -> SourceType -> SemanticSnapshot -> bool
    val implementedTypes: SourceType -> SemanticSnapshot -> SourceType list
    val diagnostics: SemanticSnapshot -> ExtensionDiagnostic list

type internal SemanticTypeInfo =
    {
        Key: string
        Package: string
        Paths: string list list
        Declaration: string
        Bases: string list
        Implemented: string list
        Members: string list
        Target: string option
        External: bool
        Findings: Finding list
        Excluded: string list
    }

type internal SemanticMemberInfo =
    {
        Key: string
        Receiver: string
        Declaring: string
        Name: string
        IsProperty: bool
        ReadOnly: bool
        Optional: bool
        Type: FsTypeRef
        Findings: Finding list
        Targets: (string * string * bool) list
    }

type internal ResolvedSourceInfo =
    {
        Package: string
        Path: string list
        Declaration: string option
        Fingerprint: string option
        Union: Result<ResolvedUnionArm list, ExtensionDiagnostic list>
    }

type internal ResolvedOperationQuery = string * string list * string * string * string option

type internal ResolvedOperationInfo =
    {
        Source: ResolvedSourceInfo
        Shape: Result<ResolvedValueShape, ExtensionDiagnostic list>
        Operation: Result<ResolvedOperation, ExtensionDiagnostic list>
        ReceiverTypeId: int<Measure.typeId> option
    }

type internal Edit =
    | AddAttribute of OutputTarget * AttributeSpec
    | EmitCompanion of CompanionSpec
    | ReplaceInterop of OutputTarget * InteropSpec
    | ReplaceDeclaration of OutputTarget * ReplacementSpec

module internal ContractData =
    val resolvedSnapshot: ResolvedSourceInfo list -> ResolvedSnapshot

    val withOperationLookup:
        (ResolvedOperationQuery -> ResolvedOperationInfo option) -> ResolvedSnapshot -> ResolvedSnapshot

    val projectionCompanionInfo: ProjectionCompanion -> string * string * string * string list * string
    val projectionCompanionIsCurrent: ResolvedSnapshot -> ProjectionCompanion -> bool
    val projectionReceiver: ProjectionCompanion -> (int<Measure.typeId> * string) option
    val withReferences: Map<string, string> -> SemanticSnapshot -> SemanticSnapshot
    val edits: EditBatch -> Edit list
    val attributeInfo: AttributeSpec -> string * AttributeValue list * string
    val companionInfo: CompanionSpec -> string * string * SourceType * SourceMember list * BindingType list * string
    val interopKey: InteropSpec -> string
    val replacementInfo: ReplacementSpec -> FsMember list * FsTypeRef list * (string * string list) option
    val compilerInfo: ValidationCompiler -> string * string list
    val snapshot: SemanticTypeInfo list -> SemanticMemberInfo list -> ExtensionDiagnostic list -> SemanticSnapshot
    val typeInfo: SourceType -> SemanticSnapshot -> SemanticTypeInfo
    val memberInfo: SourceMember -> SemanticSnapshot -> SemanticMemberInfo
    val binding: FsTypeRef -> BindingType
    val typeRef: BindingType -> FsTypeRef
    val targetInfo: OutputTarget -> string * string option * bool
    val sourceType: string -> SourceType
    val sourceMember: string -> SourceMember
