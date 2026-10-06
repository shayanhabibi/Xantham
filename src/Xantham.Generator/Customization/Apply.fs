module internal Xantham.Generator.Customization.Apply

open System
open Xantham.Generator

let private qualified (name: string) =
    if
        String.IsNullOrWhiteSpace name
        || name.Split('.')
           |> Array.exists (fun part ->
               not (System.Text.RegularExpressions.Regex.IsMatch(part, "^[A-Za-z_][A-Za-z0-9_']*$")))
    then
        invalidOp $"customization/invalid-name: {name}"

    name

let rec private argument foreign =
    function
    | AttributeValue.String value -> Render.stringLit value
    | AttributeValue.Boolean value -> if value then "true" else "false"
    | AttributeValue.Integer value -> string value
    | AttributeValue.Type value ->
        "typeof<"
        + (ContractData.typeRef value |> Render.qualifyType foreign |> Render.printType)
        + ">"
    | AttributeValue.Enum(name, case) -> qualified name + "." + qualified case
    | AttributeValue.Array values -> "[|" + (values |> List.map (argument foreign) |> String.concat "; ") + "|]"

let private interop =
    set
        [
            "Emit"
            "EmitProperty"
            "EmitIndexer"
            "EmitConstructor"
            "Import"
            "Global"
            "Erase"
            "CompiledName"
            "ParamObject"
        ]

let evaluate foreign (extensions: GeneratorExtension list) snapshot =
    let validTargets =
        Semantic.types snapshot
        |> List.collect (fun source -> (Semantic.declarationTarget source snapshot |> Option.toList))
        |> fun targets ->
            targets
            @ (Semantic.members snapshot
               |> List.collect (fun m -> Semantic.outputTargets m snapshot))

    let validate target =
        if not (List.contains target validTargets) then
            invalidOp "customization/stale-target: target does not belong to this source snapshot"

    let mutable companions = []
    let mutable companionOwners = Map.empty
    let mutable replacementOwners = Map.empty
    let mutable declarationOwners = Map.empty
    let mutable declarations = Map.empty
    let mutable findings = []

    let duplicates =
        extensions
        |> List.groupBy _.Identity.Id
        |> List.filter (fun (_, values) -> values.Length > 1)

    if not duplicates.IsEmpty then
        invalidOp $"customization/duplicate-extension: {fst duplicates.Head}"

    extensions
    |> List.fold
        (fun annotations extension ->
            if
                String.IsNullOrWhiteSpace extension.Identity.Id
                || String.IsNullOrWhiteSpace extension.Identity.Version
            then
                invalidOp "customization/invalid-identity: extension id and version are required"

            let batch =
                try
                    match extension.Transform snapshot with
                    | Ok batch -> batch
                    | Error diagnostics ->
                        invalidOp (
                            diagnostics
                            |> List.map (fun d -> Diagnostic.code d + ": " + Diagnostic.message d)
                            |> String.concat "\n"
                        )
                with error ->
                    raise (
                        InvalidOperationException($"customization/extension-failed: {extension.Identity.Id}", error)
                    )

            ContractData.edits batch
            |> List.fold
                (fun annotations edit ->
                    match edit with
                    | ReplaceDeclaration(target, replacement) ->
                        validate target
                        let declaration, memberName, external = ContractData.targetInfo target

                        if external || memberName.IsSome then
                            invalidOp
                                $"customization/invalid-declaration-target: {extension.Identity.Id}: {declaration}"

                        match Map.tryFind declaration declarationOwners with
                        | Some owner ->
                            invalidOp
                                $"customization/replacement-conflict: {declaration}: {owner}, {extension.Identity.Id}"
                        | None -> ()

                        if annotations |> Map.exists (fun (name, _) _ -> name = declaration) then
                            invalidOp $"customization/removed-member-edit: {declaration}: {extension.Identity.Id}"

                        declarationOwners <- Map.add declaration extension.Identity.Id declarationOwners
                        declarations <- Map.add declaration replacement declarations

                        findings <-
                            findings
                            @ [
                                Finding.make declaration (CustomizeOutput.DeclarationReplaced extension.Identity.Id)
                            ]

                        annotations
                    | ReplaceInterop(target, interop) ->
                        validate target
                        let declaration, memberName, external = ContractData.targetInfo target

                        if external then
                            invalidOp $"customization/external-edit: {extension.Identity.Id}: {declaration}"

                        let memberName =
                            memberName
                            |> Option.defaultWith (fun () -> invalidOp "customization/member-required")

                        if Map.containsKey declaration declarationOwners then
                            invalidOp $"customization/removed-member-edit: {declaration}: {extension.Identity.Id}"

                        let key = declaration, memberName

                        match Map.tryFind key replacementOwners with
                        | Some owner ->
                            invalidOp
                                $"customization/replacement-conflict: {declaration}.{memberName}: {owner}, {extension.Identity.Id}"
                        | None -> replacementOwners <- Map.add key extension.Identity.Id replacementOwners

                        let source =
                            Semantic.members snapshot
                            |> List.find (fun p -> Semantic.outputTargets p snapshot |> List.contains target)

                        if not (ContractData.memberInfo source snapshot).IsProperty then
                            invalidOp $"customization/property-required: {declaration}.{memberName}"

                        let expression =
                            "$0["
                            + System.Text.Json.JsonSerializer.Serialize(ContractData.interopKey interop)
                            + "]"

                        let emit text = "Emit(" + Render.stringLit text + ")"

                        let attrs =
                            [
                                yield "get", emit expression
                                if not (Semantic.isReadOnly source snapshot) then
                                    yield "set", emit (expression + " = $1")
                            ]

                        let previous = Map.tryFind key annotations |> Option.defaultValue []

                        findings <-
                            findings
                            @ [
                                Finding.make
                                    (declaration + "." + memberName)
                                    (CustomizeOutput.InteropReplaced extension.Identity.Id)
                            ]

                        Map.add key (previous @ attrs) annotations
                    | EmitCompanion spec ->
                        let ns, name, _, _, _, _ = ContractData.companionInfo spec
                        let identity = ns + "." + name

                        for claimed in [ identity; identity + "Extensions" ] do
                            match Map.tryFind claimed companionOwners with
                            | Some owner ->
                                invalidOp $"customization/name-collision: {claimed}: {owner}, {extension.Identity.Id}"
                            | None -> companionOwners <- Map.add claimed extension.Identity.Id companionOwners

                        companions <- companions @ [ spec ]
                        let _, _, source, members, _, _ = ContractData.companionInfo spec
                        let info = ContractData.typeInfo source snapshot

                        findings <-
                            findings
                            @ [
                                Finding.make identity (CustomizeOutput.CompanionEmitted extension.Identity.Id)
                            ]

                        findings <-
                            findings
                            @ (info.Findings |> List.map (fun finding -> { finding with Symbol = identity }))

                        findings <-
                            findings
                            @ (members
                               |> List.collect (fun memberSource ->
                                   (ContractData.memberInfo memberSource snapshot).Findings)
                               |> List.map (fun finding -> { finding with Symbol = identity }))

                        let omitted =
                            info.Excluded
                            @ (Semantic.properties source snapshot
                               |> List.filter (fun p -> not (List.contains p members))
                               |> List.map (fun p -> Semantic.jsName p snapshot))

                        findings <-
                            findings
                            @ (omitted
                               |> List.map (fun memberName ->
                                   Finding.make
                                       identity
                                       (CustomizeOutput.MemberOmitted(extension.Identity.Id, memberName))))

                        annotations
                    | AddAttribute(target, attribute) ->
                        validate target
                        let declaration, memberName, external = ContractData.targetInfo target

                        if external then
                            invalidOp $"customization/external-edit: {extension.Identity.Id}: {declaration}"

                        let memberName =
                            memberName
                            |> Option.defaultWith (fun () -> invalidOp "customization/member-required")

                        if Map.containsKey declaration declarationOwners then
                            invalidOp $"customization/removed-member-edit: {declaration}: {extension.Identity.Id}"

                        let name, values, site = ContractData.attributeInfo attribute

                        if site <> "member" then
                            let property =
                                Semantic.members snapshot
                                |> List.find (fun p -> Semantic.outputTargets p snapshot |> List.contains target)

                            if not (ContractData.memberInfo property snapshot).IsProperty then
                                invalidOp $"customization/accessor-required: {declaration}.{memberName}"

                            if site = "set" && Semantic.isReadOnly property snapshot then
                                invalidOp $"customization/readonly-setter: {declaration}.{memberName}"

                        let name = qualified name

                        if
                            Set.contains
                                (name.Split('.') |> Array.last |> fun n -> n.Replace("Attribute", ""))
                                interop
                        then
                            invalidOp $"customization/interop-replacement-required: {extension.Identity.Id}: {name}"

                        let text =
                            if values.IsEmpty then
                                name
                            else
                                name + "(" + (values |> List.map (argument foreign) |> String.concat ", ") + ")"

                        let key = declaration, memberName
                        let previous = Map.tryFind key annotations |> Option.defaultValue []

                        findings <-
                            findings
                            @ [
                                Finding.make
                                    (declaration + "." + memberName)
                                    (CustomizeOutput.AttributeAdded extension.Identity.Id)
                            ]

                        Map.add key (previous @ [ site, text ] |> List.distinct) annotations)
                annotations)
        Map.empty
    |> fun annotations -> annotations, companions, declarations, findings

let replace declarations (shape: ShapeModel) =
    let rec variables =
        function
        | FsTypeVar name -> [ name ]
        | FsOption inner
        | FsArray inner
        | FsBranded(inner, _) -> variables inner
        | FsTuple refs
        | FsErasedUnion refs
        | FsApp(_, refs) -> List.collect variables refs
        | FsFunc(a, b) -> variables a @ variables b
        | FsDelegate(args, result) -> List.collect variables (result :: args)
        | _ -> []

    let changed =
        shape.Decls
        |> List.map (fun decl ->
            let name = Render.declName decl

            match Map.tryFind name declarations, decl with
            | None, _ -> decl
            | Some replacement, FsInterface original ->
                let members, bases, raw = ContractData.replacementInfo replacement

                let free =
                    (bases
                     @ (members
                        |> List.choose (function
                            | FsProperty p -> Some p.Type
                            | _ -> None)))
                    |> List.collect variables
                    |> List.filter (fun name -> original.TypeParameters |> List.exists (fun p -> p.Name = name) |> not)

                if not free.IsEmpty then
                    let variables = String.concat ", " free
                    invalidOp $"customization/free-type-variable: {name}: {variables}"

                match raw with
                | Some(_, exports) when exports <> [ name ] || not original.TypeParameters.IsEmpty ->
                    invalidOp $"customization/raw-contract: expected the non-generic declaration {name}"
                | _ -> ()

                let inherits =
                    shape.Decls
                    |> List.exists (function
                        | FsInterface d ->
                            d.Inherits
                            |> List.exists (function
                                | FsNamed n
                                | FsApp(n, _) -> n = name
                                | _ -> false)
                        | _ -> false)

                if inherits && (original.Members <> members || original.Inherits <> bases) then
                    invalidOp $"customization/dependent-heritage: {name}"

                if members |> List.distinct <> members then
                    invalidOp $"customization/duplicate-member: {name}"

                FsInterface
                    { original with
                        Members = members
                        Inherits = bases
                        CreateOverloads = []
                        Statics = []
                        Entrypoint = None
                    }
            | Some _, _ -> invalidOp $"customization/unsupported-declaration: {name}")

    { shape with Decls = changed }
