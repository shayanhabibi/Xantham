namespace Xantham.Generator.Customization

open Xantham.Generator

type SourceType = private SourceType of string
type SourceMember = private SourceMember of string
type OutputTarget = private OutputTarget of string * string option * bool
type BindingType = private BindingType of FsTypeRef
type ExtensionDiagnostic = private ExtensionDiagnostic of string * string * string option

type AttributeValue =
    | String of string
    | Boolean of bool
    | Integer of int
    | Type of BindingType
    | Enum of typeName: string * caseName: string
    | Array of AttributeValue list

type AttributeSpec = private AttributeSpec of string * AttributeValue list * string
type CompanionSpec = private CompanionSpec of string * string * SourceType * SourceMember list * BindingType list * string
type internal Edit =
    | AddAttribute of OutputTarget * AttributeSpec
    | EmitCompanion of CompanionSpec
type EditBatch = private EditBatch of Edit list
type ExtensionIdentity = { Id: string; Version: string; Configuration: Map<string, string> }

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

type SemanticSnapshot =
    private
        { Types: Map<string, SemanticTypeInfo>
          Members: Map<string, SemanticMemberInfo>
          Diagnostics: ExtensionDiagnostic list }

type GeneratorExtension =
    { Identity: ExtensionIdentity
      Transform: SemanticSnapshot -> Result<EditBatch, ExtensionDiagnostic list> }

module Attribute =
    let create name arguments = AttributeSpec(name, arguments, "member")
    let onGetter (AttributeSpec(name, arguments, _)) = AttributeSpec(name, arguments, "get")
    let onSetter (AttributeSpec(name, arguments, _)) = AttributeSpec(name, arguments, "set")

module Edits =
    let empty = EditBatch []
    let addAttribute target attribute (EditBatch edits) = EditBatch(edits @ [AddAttribute(target, attribute)])
    let emitCompanion companion (EditBatch edits) = EditBatch(edits @ [EmitCompanion companion])

module Diagnostic =
    let code (ExtensionDiagnostic(code, _, _)) = code
    let message (ExtensionDiagnostic(_, message, _)) = message
    let source (ExtensionDiagnostic(_, _, source)) = source
    let create code message source = ExtensionDiagnostic(code, message, source)

module BindingType =
    let display (BindingType value) = Render.printType value

module internal ContractData =
    let edits (EditBatch edits) = edits
    let attributeInfo (AttributeSpec(name, arguments, target)) = name, arguments, target
    let companionInfo (CompanionSpec(ns, name, source, members, bases, mode)) = ns, name, source, members, bases, mode
    let snapshot (types: SemanticTypeInfo list) (members: SemanticMemberInfo list) diagnostics =
        { Types = types |> List.map (fun t -> t.Key, t) |> Map.ofList
          Members = members |> List.map (fun m -> m.Key, m) |> Map.ofList
          Diagnostics = diagnostics }

    let typeInfo (SourceType key) model = model.Types[key]
    let memberInfo (SourceMember key) model = model.Members[key]
    let binding value = BindingType value
    let typeRef (BindingType value) = value
    let targetInfo (OutputTarget(declaration, memberName, external)) = declaration, memberName, external
    let sourceType key = SourceType key
    let sourceMember key = SourceMember key

module Semantic =
    let types model = model.Types |> Map.toList |> List.map (fst >> SourceType)
    let package source model = (ContractData.typeInfo source model).Package
    let path source model = (ContractData.typeInfo source model).Paths |> List.head
    let identity source model = (ContractData.typeInfo source model).Declaration

    let tryFind package path model =
        let matches =
            model.Types |> Map.toList
            |> List.filter (fun (_, info) -> info.Package = package && List.contains path info.Paths)
        match matches with
        | [] -> None
        | [(key, _)] -> Some(SourceType key)
        | _ ->
            let name = String.concat "." path
            invalidOp $"customization/ambiguous-declaration: {package}/{name}"

    let descendants (SourceType root) model =
        let rec reaches visited key =
            if key = root then true
            elif Set.contains key visited then false
            else
                let visited = Set.add key visited
                model.Types[key].Bases |> List.exists (reaches visited)
        types model |> List.filter (fun (SourceType key) -> reaches Set.empty key)

    let properties source model = (ContractData.typeInfo source model).Members |> List.map SourceMember
    let bindingType memberSource model = (ContractData.memberInfo memberSource model).Type |> BindingType
    let declaringType memberSource model = (ContractData.memberInfo memberSource model).Declaring |> SourceType
    let jsName memberSource model = (ContractData.memberInfo memberSource model).Name
    let isReadOnly memberSource model = (ContractData.memberInfo memberSource model).ReadOnly
    let isOptional memberSource model = (ContractData.memberInfo memberSource model).Optional
    let outputTargets memberSource model =
        (ContractData.memberInfo memberSource model).Targets
        |> List.map (fun (decl, name, external) -> OutputTarget(decl, Some name, external))
    let declarationTarget source model =
        let info = ContractData.typeInfo source model
        info.Target |> Option.map (fun target -> OutputTarget(target, None, info.External))
    let isSameDeclaration left right model = identity left model = identity right model
    let implementedTypes source model = (ContractData.typeInfo source model).Implemented |> List.map SourceType
    let diagnostics model = model.Diagnostics

module Companion =
    let create ns name source model = CompanionSpec(ns, name, source, Semantic.properties source model, [], "erase")
    let directProperties (CompanionSpec(ns, name, source, members, bases, _)) = CompanionSpec(ns, name, source, members, bases, "direct")
    let withProperties members (CompanionSpec(ns, name, source, _, bases, mode)) = CompanionSpec(ns, name, source, members, bases, mode)
    let withBases bases (CompanionSpec(ns, name, source, members, _, mode)) = CompanionSpec(ns, name, source, members, bases, mode)
