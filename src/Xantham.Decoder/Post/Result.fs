module Xantham.Decoder.Post.Result
open Xantham
open Xantham.Decoder
open Xantham.Decoder.Post.Duplicates
open Xantham.Internal

type NodeRoute =
    | Type of TsType
    | Export of TsExportDeclaration
    | Both of TsType * TsExportDeclaration

let rec routeType = function
    | TsAstNode.Alias node -> TsType.TypeAlias node |> ValueSome
    | TsAstNode.TemplateLiteral tsTemplateLiteralType -> TsType.TemplateLiteral tsTemplateLiteralType |> ValueSome
    | TsAstNode.GlobalThis -> TsType.GlobalThis |> ValueSome
    | TsAstNode.Tuple tsTuple -> TsType.Tuple tsTuple |> ValueSome
    | TsAstNode.Interface tsInterface -> TsType.Interface tsInterface |> ValueSome
    | TsAstNode.Primitive typeKindPrimitive -> TsType.Primitive typeKindPrimitive |> ValueSome
    | TsAstNode.Predicate tsTypePredicate -> TsType.Predicate tsTypePredicate |> ValueSome
    | TsAstNode.Literal tsLiteral -> TsType.Literal tsLiteral |> ValueSome
    | TsAstNode.TypeLiteral tsTypeLiteral -> TsType.TypeLiteral tsTypeLiteral |> ValueSome
    | TsAstNode.TypeParameter tsTypeParameter -> TsType.TypeParameter tsTypeParameter |> ValueSome
    | TsAstNode.IndexAccessType tsIndexAccessType -> TsType.IndexedAccess tsIndexAccessType |> ValueSome
    | TsAstNode.Index tsIndex -> TsType.Index tsIndex |> ValueSome
    | TsAstNode.TypeReference tsTypeReference -> TsType.TypeReference tsTypeReference |> ValueSome
    | TsAstNode.Array tsTypeReference ->
        TsType.TypeReference tsTypeReference
        |> TsType.Array
        |> ValueSome
    | TsAstNode.Enum tsEnumType -> TsType.Enum tsEnumType |> ValueSome
    | TsAstNode.EnumCase tsEnumCase -> TsType.EnumCase tsEnumCase |> ValueSome
    | TsAstNode.SubstitutionType tsSubstitutionType -> TsType.Substitution tsSubstitutionType |> ValueSome
    | TsAstNode.Conditional tsConditionalType -> TsType.Conditional tsConditionalType |> ValueSome
    | TsAstNode.Class tsClass -> TsType.Class tsClass |> ValueSome
    | TsAstNode.Union tsTypeUnion -> TsType.Union tsTypeUnion |> ValueSome
    | TsAstNode.Intersection tsTypeIntersection -> TsType.Intersection tsTypeIntersection |> ValueSome
    | TsAstNode.Optional tsTypeReference -> TsType.Optional tsTypeReference |> ValueSome
    | TsAstNode.Module tsModule -> TsType.Module tsModule |> ValueSome
    | TsAstNode.FunctionDeclaration _ -> ValueNone
    | TsAstNode.Variable _ -> ValueNone

let routeExport = function
    | TsAstNode.FunctionDeclaration node -> TsExportDeclaration.Function node |> ValueSome
    | TsAstNode.Variable node -> TsExportDeclaration.Variable node |> ValueSome
    | TsAstNode.Interface node -> TsExportDeclaration.Interface node |> ValueSome
    | TsAstNode.Enum tsEnumType -> TsExportDeclaration.Enum tsEnumType |> ValueSome
    | TsAstNode.Class tsClass -> TsExportDeclaration.Class tsClass |> ValueSome
    | TsAstNode.Module tsModule -> TsExportDeclaration.Module tsModule |> ValueSome
    | _ -> ValueNone

let routeNode value =
    match routeExport value, routeType value with
    | ValueSome export, ValueSome type' -> Both (type', export)
    | ValueSome export, _ -> Export export
    | _, ValueSome type' -> Type type'
    | _ -> failwith "AstNodes are missing TsType or TsExportDeclaration matches"

let processResult (result: EncodedResult) =
    let topLevelExports = result.TopLevelExports
    let flattenedDuplicates =
        result.DuplicateNodes
        |> resolveDuplicates
    let duplicateTypes, duplicateExports =
        flattenedDuplicates
        |> Array.fold (fun (types, exports) mergeResult ->
            let addType key typ map = map |> Map.change key (function None -> Some typ | _ -> failwith "Collision")
            let addExport key export map = map |> Map.change key (function None -> Some export | _ -> failwith "Collision")
            match mergeResult with
            | MergeResult.Both(key, node, export) ->
                match routeNode node with
                | Both _
                | Export _ -> failwith "Collision on export node for typeKey"
                | Type typ ->
                    addType key typ types, addExport key export exports
            | MergeResult.ExportOnly(key, export) ->
                types, addExport key export exports
            | MergeResult.NodeOnly(key, node) ->
                match routeNode node with
                | Both(typ, export) -> addType key typ types, addExport key export exports
                | Export export -> types, addExport key export exports
                | Type typ -> addType key typ types, exports
            ) (Map.empty, Map.empty)
    let resultTypes, resultExports =
        result.NonDuplicateNodes
        |> Map.fold (fun (types, exports) key node ->
            let addType key typ = types |> Map.change key (function None -> Some typ | _ -> failwith "Collision")
            let addExport key export = exports |> Map.change key (function None -> Some export | _ -> failwith "Collision")
            match routeNode node with
            | Both(typ, export) -> addType key typ, addExport key export
            | Export export -> types, addExport key export
            | Type typ -> addType key typ, exports
            ) (duplicateTypes, duplicateExports)
    let topLevelExports, resultExports =
        resultExports
        |> Map.partition (fun key _ -> topLevelExports |> Array.contains key)
        ||> fun topLevelExports resultExports ->
            Map.values topLevelExports |> Seq.toArray,
            Map.values resultExports |> Seq.toArray
    {| Types = resultTypes; Exports = resultExports; TopLevelExports = topLevelExports |}
    
    