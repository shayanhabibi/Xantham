module internal Xantham.Generator.Customization.Output

open System
open System.Text.Json
open Xantham.Generator

let private name (text: string) =
    if
        String.IsNullOrWhiteSpace text
        || text.Split('.')
           |> Array.exists (fun part ->
               not (System.Text.RegularExpressions.Regex.IsMatch(part, "^[A-Za-z_][A-Za-z0-9_]*$")))
    then
        invalidOp $"customization/invalid-name: {text}"

    text

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

let render foreign modules snapshot specs =
    let identity spec =
        let ns, marker, _, _, _, _ = ContractData.companionInfo spec in ns + "." + marker

    let names = specs |> List.map identity |> Set.ofList

    let rec dependencies =
        function
        | FsNamed n -> [ n ]
        | FsApp(n, args) -> n :: List.collect dependencies args
        | FsOption inner
        | FsArray inner
        | FsBranded(inner, _) -> dependencies inner
        | FsTuple refs
        | FsErasedUnion refs -> List.collect dependencies refs
        | FsFunc(a, b) -> dependencies a @ dependencies b
        | FsDelegate(args, result) -> List.collect dependencies (args @ [ result ])
        | _ -> []

    let deps spec =
        let _, _, _, members, bases, _ = ContractData.companionInfo spec

        (members |> List.map (fun m -> (ContractData.memberInfo m snapshot).Type))
        @ (bases |> List.map ContractData.typeRef)
        |> List.collect dependencies
        |> List.filter (fun n -> Set.contains n names)
        |> Set.ofList

    let rec order written pending =
        match pending with
        | [] -> []
        | _ ->
            match pending |> List.tryFind (fun spec -> Set.isSubset (deps spec) written) with
            | None -> invalidOp "customization/companion-dependency-cycle"
            | Some spec ->
                spec
                :: order (Set.add (identity spec) written) (pending |> List.filter ((<>) spec))

    order Set.empty specs
    |> List.map (fun spec ->
        let ns, marker, source, members, bases, mode = ContractData.companionInfo spec
        let ns, marker = name ns, name marker
        let ordinary = foreign |> Map.toList |> List.map snd |> Set.ofList

        let occupied = Set.union ordinary (Set.ofList modules)

        let under (parent: string) (child: string) =
            child = parent || child.StartsWith(parent + ".", StringComparison.Ordinal)

        if
            occupied
            |> Set.exists (fun path ->
                under path ns
                || under (ns + "." + marker) path
                || under (ns + "." + marker + "Extensions") path)
        then
            invalidOp $"customization/name-collision: {ns}.{marker}"

        if
            names
            |> Set.exists (fun path -> path <> ns + "." + marker && (under path ns || under (path + "Extensions") ns))
        then
            invalidOp $"customization/name-collision: {ns}.{marker}"

        if
            Set.contains (ns + "." + marker) ordinary
            || Set.contains (ns + "." + marker + "Extensions") ordinary
        then
            invalidOp $"customization/name-collision: {ns}.{marker}"

        if marker.Contains '.' then
            invalidOp "customization/invalid-marker: marker must be one identifier"

        let properties = members |> List.map (fun m -> ContractData.memberInfo m snapshot)

        if
            properties
            |> List.groupBy _.Name
            |> List.exists (fun (_, members) -> List.distinct members |> List.length > 1)
        then
            invalidOp $"customization/ambiguous-property: {ns}.{marker}"

        let properties = properties |> List.distinctBy _.Name
        let receiver = ContractData.typeInfo source snapshot

        if
            properties
            |> List.exists (fun p -> not p.IsProperty || not (List.contains p.Key receiver.Members))
        then
            invalidOp $"customization/foreign-property: {ns}.{marker}"

        let refs =
            (properties |> List.map _.Type) @ (bases |> List.map ContractData.typeRef)

        let parameters = refs |> List.collect variables |> List.distinct

        let head =
            if parameters.IsEmpty then
                marker
            else
                marker
                + "<"
                + (parameters
                   |> List.map (fun p -> "'" + Naming.typeVariable p)
                   |> String.concat ", ")
                + ">"

        let propertyType (property: SemanticMemberInfo) =
            property.Type |> Render.qualifyType foreign |> Render.printType

        let getter (property: SemanticMemberInfo) =
            "$0[" + JsonSerializer.Serialize(property.Name) + "]"

        let lines =
            [
                yield "// <auto-generated>"
                yield "// Generated by Xantham customization."
                yield "// </auto-generated>"
                yield "namespace " + ns
                yield ""
                yield "open System"
                yield "open Fable.Core"
                yield "open Fable.Core.JsInterop"
                yield "open Fable.Core.JS"
                yield "open Fable.Core.TS.Dom"
                yield ""
                yield "[<Interface>]"
                if bases.IsEmpty then
                    yield $"type {head} = interface end"
                else
                    yield $"type {head} ="

                    for baseType in bases do
                        yield
                            "    inherit "
                            + (ContractData.typeRef baseType |> Render.qualifyType foreign |> Render.printType)
                yield ""
                yield "[<AutoOpen>]"
                yield $"module {marker}Extensions ="
                yield $"    type {head} with"
                if properties.IsEmpty then
                    invalidOp $"customization/empty-companion: {ns}.{marker}"
                for property in properties do
                    let typ = propertyType property
                    let prop = Render.ident property.Name

                    if mode = "erase" then
                        yield "        [<Erase>]"

                    if property.ReadOnly then
                        if mode = "direct" then
                            yield "        [<Emit(" + Render.stringLit (getter property) + ")>]"

                        yield $"        member _.{prop}: {typ} = jsNative"
                    else
                        yield $"        member _.{prop}"

                        let getAttribute =
                            if mode = "direct" then
                                "[<Emit(" + Render.stringLit (getter property) + ")>] "
                            else
                                ""

                        let setAttribute =
                            if mode = "direct" then
                                "[<Emit(" + Render.stringLit (getter property + " = $1") + ")>] "
                            else
                                ""

                        yield $"            with {getAttribute}get (): {typ} = jsNative"
                        let body = if mode = "direct" then "jsNative" else "()"
                        yield $"            and {setAttribute}set (value: {typ}) = {body}"

                    yield ""
            ]

        $"customizations/{ns}.{marker}.fs", String.concat "\n" lines + "\n")
