module Xantham.Generator.Generator.TypeRefRender

open Xantham.Generator.TypeRefRender
open Xantham.Generator.NamePath
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham
open Xantham.Generator

let makeGlobalThis nullable =
    TypeRefRender.create nullable Types.globalThis

let fromPrimitiveKind nullable =
    function
    | TypeKindPrimitive.String -> Types.string
    | TypeKindPrimitive.Any -> Types.obj
    | TypeKindPrimitive.NonPrimitive  -> Types.obj
    | TypeKindPrimitive.Unknown -> Types.objNull
    | TypeKindPrimitive.Never -> Types.objNull
    | TypeKindPrimitive.Null 
    | TypeKindPrimitive.Undefined 
    | TypeKindPrimitive.Void -> Types.unit
    | TypeKindPrimitive.Integer -> Types.int
    | TypeKindPrimitive.Number -> Types.float
    | TypeKindPrimitive.Boolean -> Types.bool
    | TypeKindPrimitive.BigInt -> Types.bigint
    | TypeKindPrimitive.ESSymbol -> Types.obj
    >> TypeRefRender.create nullable

let rec fromResolvedType nullable (resolvedType: ResolvedType) =
    match resolvedType with
    | ResolvedType.GlobalThis -> makeGlobalThis nullable
    | ResolvedType.Conditional conditionalType ->
        let renders = [
            fromResolvedType false conditionalType.True.Value
            fromResolvedType false conditionalType.False.Value
        ]
        if nullable || renders |> List.forall _.Nullable then
            renders
            |> List.map (fun render -> { render with Nullable = false })
            |> TypeRefRender.create true
        else
            TypeRefRender.create false renders
    | ResolvedType.Interface ``interface`` ->
        Path.fromInterface ``interface``
        |> TypeRefRender.create nullable
    | ResolvedType.Class ``class`` ->
        Path.fromClass ``class``
        |> TypeRefRender.create nullable
    | ResolvedType.Primitive typeKindPrimitive ->
        fromPrimitiveKind nullable
        |> funApply typeKindPrimitive
    | ResolvedType.Union union ->
        union.Types
        |> List.map (_.Value >> fromResolvedType false)
        |> TypeRefRender.create nullable
    | ResolvedType.Intersection intersection -> failwith "todo"
    | ResolvedType.Literal tsLiteral -> failwith "todo"
    | ResolvedType.IndexedAccess indexAccessType -> failwith "todo"
    | ResolvedType.Index index -> failwith "todo"
    | ResolvedType.TypeReference typeReference ->
        match typeReference with
        | { ResolvedType = Some (Resolve resolvedType) } 
        | { TypeArguments = []; Type = (Resolve resolvedType) } ->
            fromResolvedType nullable resolvedType
        | { Type = (Resolve resolvedType); TypeArguments = typeArguments } ->
            let prefix = fromResolvedType false resolvedType
            let args =
                typeArguments
                |> List.map (_.Value >> fromResolvedType false)
            (prefix, args)
            |> TypeRefRender.create nullable
    | ResolvedType.Array resolvedType ->
        (TypeRefRender.createWidget false Types.arrayType, [fromResolvedType false resolvedType])
        |> TypeRefRender.create nullable
    | ResolvedType.Enum enumType ->
        Path.fromEnum enumType
        |> TypeRefRender.create nullable
    | ResolvedType.EnumCase enumCase ->
        enumCase.Parent.Value
        |> Path.fromEnum
        |> TypeRefRender.create nullable
    | ResolvedType.TypeParameter typeParameter -> failwith "todo"
    | ResolvedType.ReadOnly resolvedType ->
        fromResolvedType nullable resolvedType
    | ResolvedType.Tuple tuple -> failwith "todo"
    | ResolvedType.Predicate predicate -> failwith "todo"
    | ResolvedType.TypeLiteral typeLiteral -> failwith "todo"
    | ResolvedType.TemplateLiteral templateLiteral -> failwith "todo"
    | ResolvedType.Optional typeReference ->
        fromResolvedType true (ResolvedType.TypeReference typeReference)
    | ResolvedType.Substitution substitutionType -> failwith "todo"