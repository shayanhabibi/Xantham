namespace Xantham.Generator.Customization

open Xantham.Generator

type SourceType = private SourceType of string
type SourceMember = private SourceMember of string
type OutputTarget = private OutputTarget of string * string * string option * bool
type BindingType = private BindingType of FsTypeRef
type ExtensionDiagnostic = private ExtensionDiagnostic of string * string * string option
type ResolvedSource = private ResolvedSource of System.Guid * int

/// The resolved value arms accepted by the first companion projection contract.
[<RequireQualifiedAccess>]
type ResolvedUnionArm =
    | StringLiteral of string
    | Number
    | Null
    | Undefined

type internal ResolvedSourceInfo =
    {
        Package: string
        Path: string list
        Declaration: string option
        Fingerprint: string option
        Union: Result<ResolvedUnionArm list, ExtensionDiagnostic list>
    }

type ResolvedSnapshot =
    private
        {
            Nonce: System.Guid
            Sources: ResolvedSourceInfo list
        }

type ProjectionCompanion = private ProjectionCompanion of System.Guid * string * string * string * string list * string

type AttributeValue =
    | String of string
    | Boolean of bool
    | Integer of int
    | Type of BindingType
    | Enum of typeName: string * caseName: string
    | Array of AttributeValue list

type AttributeSpec = private AttributeSpec of string * AttributeValue list * string

type CompanionSpec =
    private | CompanionSpec of string * string * SourceType * SourceMember list * BindingType list * string

type InteropSpec = private InteropSpec of string
type ReplacementSpec = private ReplacementSpec of FsMember list * FsTypeRef list * (string * string list) option
type ValidationCompiler = private ValidationCompiler of string * string list

type internal Edit =
    | AddAttribute of OutputTarget * AttributeSpec
    | EmitCompanion of CompanionSpec
    | ReplaceInterop of OutputTarget * InteropSpec
    | ReplaceDeclaration of OutputTarget * ReplacementSpec

type EditBatch = private EditBatch of Edit list

type ExtensionIdentity =
    {
        Id: string
        Version: string
        Configuration: Map<string, string>
    }

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

type SemanticSnapshot =
    private
        {
            Types: Map<string, SemanticTypeInfo>
            Members: Map<string, SemanticMemberInfo>
            Diagnostics: ExtensionDiagnostic list
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

module Attribute =
    let create name arguments =
        AttributeSpec(name, arguments, "member")

    let onGetter (AttributeSpec(name, arguments, _)) = AttributeSpec(name, arguments, "get")
    let onSetter (AttributeSpec(name, arguments, _)) = AttributeSpec(name, arguments, "set")

module Interop =
    let property jsName = InteropSpec jsName

module Edits =
    let empty = EditBatch []

    let addAttribute target attribute (EditBatch edits) =
        EditBatch(edits @ [ AddAttribute(target, attribute) ])

    let emitCompanion companion (EditBatch edits) =
        EditBatch(edits @ [ EmitCompanion companion ])

    let replaceInterop target interop (EditBatch edits) =
        EditBatch(edits @ [ ReplaceInterop(target, interop) ])

    let replaceDeclaration target replacement (EditBatch edits) =
        EditBatch(edits @ [ ReplaceDeclaration(target, replacement) ])

module Diagnostic =
    let code (ExtensionDiagnostic(code, _, _)) = code
    let message (ExtensionDiagnostic(_, message, _)) = message
    let source (ExtensionDiagnostic(_, _, source)) = source

    let create code message source =
        ExtensionDiagnostic(code, message, source)

module BindingType =
    let display (BindingType value) = Render.printType value

module Resolved =
    let private tryInfo (ResolvedSource(nonce, index)) snapshot =
        if nonce = snapshot.Nonce then
            List.tryItem index snapshot.Sources
        else
            None

    let private info source snapshot =
        tryInfo source snapshot
        |> Option.defaultWith (fun () ->
            invalidArg "source" "projection/stale-source: the source belongs to another snapshot")

    let tryFind package path snapshot =
        let matches =
            snapshot.Sources
            |> List.indexed
            |> List.filter (fun (_, source) -> source.Package = package && source.Path = path)

        match matches with
        | [] -> None
        | [ (index, _) ] -> Some(ResolvedSource(snapshot.Nonce, index))
        | _ ->
            let name = String.concat "." path
            invalidOp $"projection/ambiguous-declaration: {package}/{name}"

    let package source snapshot = (info source snapshot).Package
    let path source snapshot = (info source snapshot).Path

    let identity source snapshot =
        (info source snapshot).Declaration
        |> Option.defaultWith (fun () ->
            invalidOp "projection/missing-source-metadata: no declaration identity is available")

    let union source snapshot =
        match tryInfo source snapshot with
        | Some info -> info.Union
        | None ->
            Error
                [
                    Diagnostic.create "projection/stale-source" "The source belongs to another resolved snapshot" None
                ]

    let diagnostics snapshot =
        snapshot.Sources
        |> List.collect (fun source ->
            match source.Union with
            | Ok _ -> []
            | Error diagnostics -> diagnostics)

    let internal selected source snapshot = info source snapshot

module ProjectionCompanion =
    let create source fileName exportedTypeNames sourceText snapshot =
        let info = Resolved.selected source snapshot

        match info.Union, info.Declaration, info.Fingerprint with
        | Ok _, Some identity, Some fingerprint ->
            ProjectionCompanion(snapshot.Nonce, identity, fingerprint, fileName, exportedTypeNames, sourceText)
        | _ -> invalidArg "source" "projection/unsupported-source: the selected union has unresolved diagnostics"

module internal ContractData =
    let resolvedSnapshot sources =
        {
            Nonce = System.Guid.NewGuid()
            Sources = sources
        }

    let projectionCompanionInfo (ProjectionCompanion(_, identity, fingerprint, file, names, text)) =
        identity, fingerprint, file, names, text

    let projectionCompanionIsCurrent snapshot (ProjectionCompanion(nonce, _, _, _, _, _)) = snapshot.Nonce = nonce

    let edits (EditBatch edits) = edits
    let attributeInfo (AttributeSpec(name, arguments, target)) = name, arguments, target
    let companionInfo (CompanionSpec(ns, name, source, members, bases, mode)) = ns, name, source, members, bases, mode
    let interopKey (InteropSpec name) = name
    let replacementInfo (ReplacementSpec(members, bases, raw)) = members, bases, raw
    let compilerInfo (ValidationCompiler(directory, references)) = directory, references

    let withReferences (references: Map<string, string>) model =
        { model with
            Types =
                model.Types
                |> Map.map (fun _ info ->
                    { info with
                        External =
                            info.External
                            || info.Target |> Option.exists (fun name -> Map.containsKey name references)
                    })
            Members =
                model.Members
                |> Map.map (fun _ info ->
                    { info with
                        Targets =
                            info.Targets
                            |> List.map (fun (name, memberName, external) ->
                                name, memberName, external || Map.containsKey name references)
                    })
        }

    let snapshot (types: SemanticTypeInfo list) (members: SemanticMemberInfo list) diagnostics =
        {
            Types = types |> List.map (fun t -> t.Key, t) |> Map.ofList
            Members = members |> List.map (fun m -> m.Key, m) |> Map.ofList
            Diagnostics = diagnostics
        }

    let typeInfo (SourceType key) model = model.Types[key]
    let memberInfo (SourceMember key) model = model.Members[key]
    let binding value = BindingType value
    let typeRef (BindingType value) = value
    let targetInfo (OutputTarget(_, declaration, memberName, external)) = declaration, memberName, external
    let sourceType key = SourceType key
    let sourceMember key = SourceMember key

module Semantic =
    let members model =
        model.Members |> Map.toList |> List.map (fst >> SourceMember)

    let types model =
        model.Types |> Map.toList |> List.map (fst >> SourceType)

    let package source model =
        (ContractData.typeInfo source model).Package

    let path source model =
        (ContractData.typeInfo source model).Paths
        |> List.tryHead
        |> Option.defaultValue []

    let identity source model =
        (ContractData.typeInfo source model).Declaration

    let tryFind package path model =
        let matches =
            model.Types
            |> Map.toList
            |> List.filter (fun (_, info) -> info.Package = package && List.contains path info.Paths)

        match matches with
        | [] -> None
        | [ (key, _) ] -> Some(SourceType key)
        | _ ->
            let name = String.concat "." path
            invalidOp $"customization/ambiguous-declaration: {package}/{name}"

    let descendants (SourceType root) model =
        let rec reaches visited key =
            if key = root then
                true
            elif Set.contains key visited then
                false
            else
                let visited = Set.add key visited
                model.Types[key].Bases |> List.exists (reaches visited)

        types model |> List.filter (fun (SourceType key) -> reaches Set.empty key)

    let properties source model =
        (ContractData.typeInfo source model).Members
        |> List.filter (fun key -> model.Members[key].IsProperty)
        |> List.map SourceMember

    let bindingType memberSource model =
        (ContractData.memberInfo memberSource model).Type |> BindingType

    let declaringType memberSource model =
        (ContractData.memberInfo memberSource model).Declaring |> SourceType

    let jsName memberSource model =
        (ContractData.memberInfo memberSource model).Name

    let isReadOnly memberSource model =
        (ContractData.memberInfo memberSource model).ReadOnly

    let isOptional memberSource model =
        (ContractData.memberInfo memberSource model).Optional

    let outputTargets memberSource model =
        let info = ContractData.memberInfo memberSource model

        info.Targets
        |> List.map (fun (decl, name, external) -> OutputTarget(info.Key, decl, Some name, external))

    let declarationTarget source model =
        let info = ContractData.typeInfo source model

        info.Target
        |> Option.map (fun target -> OutputTarget(info.Key, target, None, info.External))

    let isSameDeclaration left right model =
        identity left model = identity right model

    let implementedTypes source model =
        (ContractData.typeInfo source model).Implemented |> List.map SourceType

    let diagnostics model = model.Diagnostics

module Companion =
    let create ns name source model =
        CompanionSpec(ns, name, source, Semantic.properties source model, [], "erase")

    let directProperties (CompanionSpec(ns, name, source, members, bases, _)) =
        CompanionSpec(ns, name, source, members, bases, "direct")

    let withProperties members (CompanionSpec(ns, name, source, _, bases, mode)) =
        CompanionSpec(ns, name, source, members, bases, mode)

    let withBases bases (CompanionSpec(ns, name, source, members, _, mode)) =
        CompanionSpec(ns, name, source, members, bases, mode)

    let rec private variables =
        function
        | FsTypeVar variable -> [ variable ]
        | FsOption inner
        | FsArray inner
        | FsBranded(inner, _) -> variables inner
        | FsTuple values
        | FsErasedUnion values
        | FsApp(_, values) -> List.collect variables values
        | FsFunc(a, b) -> variables a @ variables b
        | FsDelegate(args, result) -> List.collect variables (args @ [ result ])
        | _ -> []

    let reference (CompanionSpec(ns, name, _, members, bases, _)) model =
        let parameters =
            (members |> List.map (fun m -> (ContractData.memberInfo m model).Type))
            @ (bases |> List.map ContractData.typeRef)
            |> List.collect variables
            |> List.distinct
            |> List.map FsTypeVar

        BindingType(
            if parameters.IsEmpty then
                FsNamed(ns + "." + name)
            else
                FsApp(ns + "." + name, parameters)
        )

module Replacement =
    let marker = ReplacementSpec([], [], None)

    let properties members model =
        let values =
            members
            |> List.map (fun source ->
                let p = ContractData.memberInfo source model

                if not p.IsProperty then
                    invalidOp "customization/property-required"

                FsProperty
                    {
                        Name = p.Name
                        Docs = ""
                        Tags = []
                        ReadOnly = p.ReadOnly
                        Type = p.Type
                    })

        ReplacementSpec(values, [], None)

    let withBases bases (ReplacementSpec(members, _, raw)) =
        ReplacementSpec(members, bases |> List.map ContractData.typeRef, raw)

    let raw source exports dependencies =
        ReplacementSpec([], dependencies |> List.map ContractData.typeRef, Some(source, exports))

module Compiler =
    let dotnet workingDirectory assemblyReferences =
        ValidationCompiler(workingDirectory, assemblyReferences)
