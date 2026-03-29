module Xantham.SimpleGenerator.Generator.EnumCaseRender

open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

let prerender (key: PatternContextHolder<KeyEnumCase>) =
    {
        Name =
            EnumCase.name key
            |> PatternContext.value
            |> Name.Pascal.create
        LiteralValue =
            EnumCase.literalValue key
            |> PatternContext.value
        IsEnum =
            match key with
            | EnumCase.IntCase _ -> true
            | _ -> false
    }

let prerenderFromLiteral (key: PatternContextHolder<TsLiteral>) =
    match key.Value with
    | TsLiteral.String value ->
        {
            Name = Name.Pascal.create value
            LiteralValue = key.Value
            IsEnum = false
        }
    | TsLiteral.Int value ->
        {
            Name = Name.Pascal.create (string value)
            LiteralValue = key.Value
            IsEnum = true
        }
    | TsLiteral.Float value when System.Double.IsInteger value ->
        {
            Name = Name.Pascal.create (string value)
            LiteralValue = int value |> TsLiteral.Int
            IsEnum = true
        }
    | TsLiteral.Float value ->
        {
            Name = Name.Pascal.create (string value)
            LiteralValue = key.Value
            IsEnum = false
        }
    | TsLiteral.Bool value ->
        {
            Name = Name.Pascal.create (string value)
            LiteralValue = key.Value
            IsEnum = false
        }
    | TsLiteral.BigInt value ->
        {
            Name = Name.Pascal.create (string value)
            LiteralValue = key.Value
            IsEnum = false
        }
    | TsLiteral.Null ->
        {
            Name = Name.Pascal.create "Null"
            LiteralValue = key.Value
            IsEnum = false
        }

let inline prerenderFromLiteralKey (key: PatternContextHolder<LiteralKey>) =
    LiteralKey.toLiteral key |> prerenderFromLiteral

let renderToUnion (prerender: UnionCaseRender) =
    match prerender.LiteralValue with
    | TsLiteral.String value ->
        let name = Name.Pascal.create value
        if Name.Case.valueOrSource name = Name.Case.valueOrSource prerender.Name then
            Ast.UnionCase(Name.Case.valueOrModified name).attributes(attributes {
                compiledName name
            })
        else
            Ast.UnionCase(Name.Case.valueOrModified prerender.Name).attributes(attributes {
                compiledName name
            })
    | TsLiteral.Int value ->
        Ast.UnionCase(Name.Case.valueOrModified prerender.Name)
            .attribute(Attributes.compiledValue value)
    | TsLiteral.Float value ->
        Ast.UnionCase(Name.Case.valueOrModified prerender.Name)
            .attribute(Attributes.compiledValue value)
    | TsLiteral.Bool value ->
        Ast.UnionCase(Name.Case.valueOrModified prerender.Name)
            .attribute(Attributes.compiledValue value)
    | TsLiteral.BigInt value ->
        Ast.UnionCase(Name.Case.valueOrModified prerender.Name)
            .attribute(Attributes.compiledValue (int value))
    | TsLiteral.Null ->
        Ast.UnionCase(Name.Case.valueOrModified prerender.Name)
            .attribute(Attributes.compiledName null)

let private literalToConstant = function
    | TsLiteral.String value -> Ast.String value
    | TsLiteral.Int value -> Ast.Int value
    | TsLiteral.Float value -> Ast.Float value
    | TsLiteral.Bool value -> Ast.Bool value
    | TsLiteral.BigInt value -> Ast.Constant(value.ToString())
    | TsLiteral.Null -> Ast.Constant "null"

let renderToEnum (prerender: UnionCaseRender) =
    let attributes = [
        if Name.Case.isModified prerender.Name then Attributes.compiledName (Name.Case.valueOrSource prerender.Name)
    ]
    Ast.EnumCase(Name.Case.valueOrModified prerender.Name, literalToConstant prerender.LiteralValue)
    |> Utils.EnumCase.attributesIfNotEmpty attributes