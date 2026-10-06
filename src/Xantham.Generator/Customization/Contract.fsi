namespace Xantham.Generator.Customization

open Xantham.Generator

type SourceType
type SourceMember
type OutputTarget
type BindingType
type SemanticSnapshot
type ExtensionDiagnostic

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

module internal ContractData =
    val snapshot: SemanticTypeInfo list -> SemanticMemberInfo list -> ExtensionDiagnostic list -> SemanticSnapshot
    val typeInfo: SourceType -> SemanticSnapshot -> SemanticTypeInfo
    val memberInfo: SourceMember -> SemanticSnapshot -> SemanticMemberInfo
    val binding: FsTypeRef -> BindingType
    val typeRef: BindingType -> FsTypeRef
    val targetInfo: OutputTarget -> string * string option * bool
    val sourceType: string -> SourceType
    val sourceMember: string -> SourceMember
