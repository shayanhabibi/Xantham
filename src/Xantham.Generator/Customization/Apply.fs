module internal Xantham.Generator.Customization.Apply

open System
open Xantham.Generator

let private qualified (name: string) =
    if String.IsNullOrWhiteSpace name || name.Split('.') |> Array.exists (fun part -> not (System.Text.RegularExpressions.Regex.IsMatch(part, "^[A-Za-z_][A-Za-z0-9_']*$"))) then
        invalidOp $"customization/invalid-name: {name}"
    name

let rec private argument = function
    | AttributeValue.String value -> Render.stringLit value
    | AttributeValue.Boolean value -> if value then "true" else "false"
    | AttributeValue.Integer value -> string value
    | AttributeValue.Type value -> $"typeof<{BindingType.display value}>"
    | AttributeValue.Enum(name, case) -> qualified name + "." + qualified case
    | AttributeValue.Array values -> "[|" + (values |> List.map argument |> String.concat "; ") + "|]"

let private interop = set ["Emit"; "EmitProperty"; "EmitIndexer"; "EmitConstructor"; "Import"; "Global"; "Erase"; "CompiledName"; "ParamObject"]

let evaluate (extensions: GeneratorExtension list) snapshot =
    let mutable companions = []
    let mutable companionOwners = Map.empty
    let duplicates = extensions |> List.groupBy _.Identity.Id |> List.filter (fun (_, values) -> values.Length > 1)
    if not duplicates.IsEmpty then invalidOp $"customization/duplicate-extension: {fst duplicates.Head}"
    extensions |> List.fold (fun annotations extension ->
        if String.IsNullOrWhiteSpace extension.Identity.Id || String.IsNullOrWhiteSpace extension.Identity.Version then
            invalidOp "customization/invalid-identity: extension id and version are required"
        let batch =
            try
                match extension.Transform snapshot with
                | Ok batch -> batch
                | Error diagnostics -> invalidOp (diagnostics |> List.map (fun d -> Diagnostic.code d + ": " + Diagnostic.message d) |> String.concat "\n")
            with error ->
                raise (InvalidOperationException($"customization/extension-failed: {extension.Identity.Id}", error))
        ContractData.edits batch |> List.fold (fun annotations edit ->
            match edit with
            | EmitCompanion spec ->
                let ns, name, _, _, _, _ = ContractData.companionInfo spec
                let identity = ns + "." + name
                match Map.tryFind identity companionOwners with
                | Some owner -> invalidOp $"customization/name-collision: {identity}: {owner}, {extension.Identity.Id}"
                | None -> companionOwners <- Map.add identity extension.Identity.Id companionOwners
                companions <- companions @ [spec]
                annotations
            | AddAttribute(target, attribute) ->
                let declaration, memberName, external = ContractData.targetInfo target
                if external then invalidOp $"customization/external-edit: {extension.Identity.Id}: {declaration}"
                let memberName = memberName |> Option.defaultWith (fun () -> invalidOp "customization/member-required")
                let name, values, site = ContractData.attributeInfo attribute
                let name = qualified name
                if Set.contains (name.Split('.') |> Array.last |> fun n -> n.Replace("Attribute", "")) interop then
                    invalidOp $"customization/interop-replacement-required: {extension.Identity.Id}: {name}"
                let text = if values.IsEmpty then name else name + "(" + (values |> List.map argument |> String.concat ", ") + ")"
                let key = declaration, memberName
                let previous = Map.tryFind key annotations |> Option.defaultValue []
                Map.add key (previous @ [site, text] |> List.distinct) annotations
        ) annotations
    ) Map.empty
    |> fun annotations -> annotations, companions
