/// Functions to allocate export paths and owners for declarations.
module Xantham.Generator.Shape.ExportLayout

open System
open System.Security.Cryptography
open System.Text
open Xantham.Generator
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto

/// Determine the export owner of a declaration from its export origin;
/// delineates between ambient modules from the runtime package and ambient modules from other packages.
let ownerOf (runtimePackage: string<importSpecifier>) =
    function
    | FromModule -> EntryModule
    | FromGlobal -> GlobalScope
    | FromAmbientModule specifier when specifier = runtimePackage -> EntryModule
    | FromAmbientModule specifier -> AmbientModule specifier

/// Stringify export owner for diagnostics
let private ownerKey =
    function
    | EntryModule -> "entry"
    | GlobalScope -> "global"
    | AmbientModule specifier -> $"ambient:{specifier / uom<importSpecifier>}"

/// Split a specifier into segments; apply pascal casing; remove empty segments
let private segments (specifier: string) =
    specifier.TrimStart('@').Split([| '/'; ':' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map Naming.pascalSegment
    |> Array.filter (String.IsNullOrWhiteSpace >> not)
    |> Array.toList

/// Split a specifier into segments; remove empty segments; no casing applied
let private rawSegments (specifier: string) =
    specifier.TrimStart('@').Split([| '/'; ':' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

/// Normalize an export owner; converts ambient modules from the runtime package to entry module owners
let private normalizeOwner runtimePackage =
    function
    | AmbientModule specifier when specifier = runtimePackage -> EntryModule
    | owner -> owner

let private rawPreferredPath (runtimePackage: string<importSpecifier>) hasEntryOwner =
    function
    | EntryModule -> []
    | GlobalScope -> if hasEntryOwner then [ "global" ] else []
    | AmbientModule specifier ->
        let runtime = runtimePackage / uom<importSpecifier>
        let ambient = specifier / uom<importSpecifier>
        let prefix = runtime + "/"

        if ambient.StartsWith(prefix, StringComparison.Ordinal) then
            rawSegments (ambient.Substring(prefix.Length))
        else
            rawSegments ambient

let preferredPath (runtimePackage: string<importSpecifier>) hasEntryOwner =
    function
    | EntryModule -> []
    | GlobalScope -> if hasEntryOwner then [ "Globals" ] else []
    | AmbientModule specifier ->
        let runtime = runtimePackage / uom<importSpecifier>
        let ambient = specifier / uom<importSpecifier>
        let prefix = runtime + "/"

        if ambient.StartsWith(prefix, StringComparison.Ordinal) then
            segments (ambient.Substring(prefix.Length))
        else
            segments ambient

let private digest owner length =
    SHA256.HashData(Encoding.UTF8.GetBytes(ownerKey owner))
    |> Convert.ToHexString
    |> _.ToLowerInvariant().Substring(0, length)

let private conflictsWithDeclaration (declared: Set<string>) path =
    path
    |> List.mapi (fun index _ -> path |> List.take (index + 1) |> String.concat ".")
    |> List.tryFindIndex (fun name -> Set.contains name declared)

let allocate (runtimePackage: string<importSpecifier>) declaredNames owners =
    let owners =
        owners
        |> List.map (normalizeOwner runtimePackage)
        |> List.distinct
        |> List.sortBy ownerKey

    let hasEntryOwner = owners |> List.contains EntryModule

    let requested =
        owners
        |> List.map (fun owner ->
            owner,
            preferredPath runtimePackage hasEntryOwner owner,
            rawPreferredPath runtimePackage hasEntryOwner owner)

    let requestedPaths = requested |> List.map (fun (_, path, _) -> path) |> Set.ofList

    let declared =
        declaredNames
        |> Set.ofList
        |> Set.union (
            requested
            |> List.map (fun (_, path, _) -> path @ [ "Exports" ] |> String.concat ".")
            |> Set.ofList
        )

    let presentationConflict owner path rawPath index =
        requested
        |> List.exists (fun (otherOwner, otherPath, otherRawPath) ->
            otherOwner <> owner
            && otherPath.Length > index
            && (otherPath |> List.take (index + 1)) = (path |> List.take (index + 1))
            && (path = otherPath
                || (otherRawPath |> List.take (index + 1)) <> (rawPath |> List.take (index + 1))))

    requested
    |> List.fold
        (fun (allocated, result) (owner, path, rawPath) ->
            let presentationIndex =
                path
                |> List.mapi (fun index _ -> index)
                |> List.tryFind (fun index -> presentationConflict owner path rawPath index)

            let conflictIndex =
                [ presentationIndex; conflictsWithDeclaration declared path ]
                |> List.choose id
                |> List.sort
                |> List.tryHead

            if conflictIndex.IsNone then
                Set.add path allocated, Map.add owner path result
            else
                let index = Option.get conflictIndex

                let rec claim length =
                    let candidate =
                        path
                        |> List.mapi (fun i segment ->
                            if i = index then
                                $"{segment}_{digest owner length}"
                            else
                                segment)

                    if
                        Set.contains candidate allocated
                        || Set.contains candidate requestedPaths
                        || conflictsWithDeclaration declared candidate |> Option.isSome
                    then
                        claim (length + 1)
                    else
                        candidate

                let candidate = claim 12
                Set.add candidate allocated, Map.add owner candidate result)
        (Set.empty, Map.empty)
    |> snd

let containerName declaredNames owner path =
    let requested = path @ [ "Exports" ]
    let dotted = String.concat "." requested

    if not (List.contains dotted declaredNames) then
        dotted
    else
        let rec claim length =
            let candidate = path @ [ $"Exports_{digest owner length}" ] |> String.concat "."

            if List.contains candidate declaredNames then
                claim (length + 1)
            else
                candidate

        claim 12

/// The owners whose value exports become bound members: every non-class value export with a
/// resolved value type.
let private memberOwners (model: ShapeModel) : ExportOwner list =
    model.Harvest.Exports
    |> List.filter (fun export ->
        export.HasValueExport
        && uint32 (export.Symbol.Flags &&& SymbolFlags.Class) = 0u
        && (Map.tryFind export.Symbol.SymbolId model.ExportTypes
            |> Option.bind _.Value
            |> Option.exists (fun typeId -> Map.containsKey typeId model.Types)))
    |> List.map (fun export -> ownerOf model.RuntimePackage export.Origin)
    |> List.distinct

/// Each owner's container name, `Exports`, `Strict.Exports` or `Globals.Exports`, for the owners
/// given. Deterministic for one model and owner set, so shaping, literal retention and ordering
/// share one map.
let containersFor (model: ShapeModel) (owners: ExportOwner list) : Map<ExportOwner, string> =
    let owners =
        owners |> List.map (normalizeOwner model.RuntimePackage) |> List.distinct

    let declared = model.DeclNames |> Map.toList |> List.map snd |> List.distinct

    // Declarations are types, which may share a name with a companion module in the same
    // generated file. Reserving them as module paths would split cloudflare:email and
    // cloudflare:workers into hashed parents merely because the package also declares a type
    // Cloudflare. Container leaves still reserve every declaration name.
    let allocated = allocate model.RuntimePackage [] owners

    owners
    |> List.map (fun owner ->
        let owner = normalizeOwner model.RuntimePackage owner

        owner,
        allocated
        |> Map.tryFind owner
        |> Option.defaultValue []
        |> containerName declared owner)
    |> Map.ofList

/// The container names of the owners whose harvested value exports become bound members.
let containers (model: ShapeModel) : Map<ExportOwner, string> =
    containersFor model (memberOwners model)
