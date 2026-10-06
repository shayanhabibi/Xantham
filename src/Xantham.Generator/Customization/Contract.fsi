namespace Xantham.Generator.Customization

open Xantham.Generator

type SourceType
type SourceMember
type OutputTarget
type BindingType
type SemanticSnapshot
type ExtensionDiagnostic
type AttributeValue =
    | String of string
    | Boolean of bool
    | Integer of int
    | Type of BindingType
    | Enum of typeName: string * caseName: string
    | Array of AttributeValue list
type AttributeSpec
type CompanionSpec
type EditBatch
type ExtensionIdentity = { Id: string; Version: string; Configuration: Map<string, string> }
type GeneratorExtension =
    { Identity: ExtensionIdentity
      Transform: SemanticSnapshot -> Result<EditBatch, ExtensionDiagnostic list> }

module Attribute =
    val create: string -> AttributeValue list -> AttributeSpec
    val onGetter: AttributeSpec -> AttributeSpec
    val onSetter: AttributeSpec -> AttributeSpec

module Edits =
    val empty: EditBatch
    val addAttribute: OutputTarget -> AttributeSpec -> EditBatch -> EditBatch
    val emitCompanion: CompanionSpec -> EditBatch -> EditBatch

module Companion =
    val create: string -> string -> SourceType -> SemanticSnapshot -> CompanionSpec
    val directProperties: CompanionSpec -> CompanionSpec
    val withProperties: SourceMember list -> CompanionSpec -> CompanionSpec
    val withBases: BindingType list -> CompanionSpec -> CompanionSpec

module Diagnostic =
    val code: ExtensionDiagnostic -> string
    val message: ExtensionDiagnostic -> string
    val source: ExtensionDiagnostic -> string option
    val create: string -> string -> string option -> ExtensionDiagnostic

module BindingType =
    val display: BindingType -> string

module Semantic =
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
    { Key: string
      Package: string
      Paths: string list list
      Declaration: string
      Bases: string list
      Implemented: string list
      Members: string list
      Target: string option
      External: bool
      Findings: Finding list
      Excluded: string list }

type internal SemanticMemberInfo =
    { Key: string
      Receiver: string
      Declaring: string
      Name: string
      ReadOnly: bool
      Optional: bool
      Type: FsTypeRef
      Targets: (string * string * bool) list }

type internal Edit =
    | AddAttribute of OutputTarget * AttributeSpec
    | EmitCompanion of CompanionSpec
module internal ContractData =
    val edits: EditBatch -> Edit list
    val attributeInfo: AttributeSpec -> string * AttributeValue list * string
    val companionInfo: CompanionSpec -> string * string * SourceType * SourceMember list * BindingType list * string
    val snapshot: SemanticTypeInfo list -> SemanticMemberInfo list -> ExtensionDiagnostic list -> SemanticSnapshot
    val typeInfo: SourceType -> SemanticSnapshot -> SemanticTypeInfo
    val memberInfo: SourceMember -> SemanticSnapshot -> SemanticMemberInfo
    val binding: FsTypeRef -> BindingType
    val typeRef: BindingType -> FsTypeRef
    val targetInfo: OutputTarget -> string * string option * bool
    val sourceType: string -> SourceType
    val sourceMember: string -> SourceMember
