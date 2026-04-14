[<AutoOpen>]
module Xantham.Generator.Generator.Render_Primitive

open Xantham.Generator
open Xantham.Generator.TypeRenders
open Xantham

module Primitive =
    let render (typ: TypeKindPrimitive) =
        match typ with
        | TypeKindPrimitive.Unknown 
        | TypeKindPrimitive.Any ->
            Types.obj
            |> Render.createRefOnly true
        | TypeKindPrimitive.ESSymbol 
        | TypeKindPrimitive.NonPrimitive ->
            Types.obj |> Render.createRefOnly false
        | TypeKindPrimitive.Null 
        | TypeKindPrimitive.Void 
        | TypeKindPrimitive.Undefined 
        | TypeKindPrimitive.Never ->
            Types.unit
            |> Render.createRefOnly false
        | TypeKindPrimitive.String ->
            Types.string
            |> Render.createRefOnly false
        | TypeKindPrimitive.Integer ->
            Types.int
            |> Render.createRefOnly false
        | TypeKindPrimitive.Number ->
            Types.float
            |> Render.createRefOnly false
        | TypeKindPrimitive.Boolean ->
            Types.bool
            |> Render.createRefOnly false
        | TypeKindPrimitive.BigInt ->
            Types.bigint
            |> Render.createRefOnly false

module GlobalThis =
    let render =
        Types.globalThis
        |> Render.createRefOnly false