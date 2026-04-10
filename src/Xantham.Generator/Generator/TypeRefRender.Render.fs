module Xantham.Generator.Generator.TypeRefRender

open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Generator.TypeRefRender
open Xantham.Generator.NamePath
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder
open Xantham
open Xantham.Generator

module private Implementation =
    let rec isFunctionRender (render: TypeRefRender) =
        match render.Kind with
        | TypeRefKind.Molecule (TypeRefMolecule.Function _) -> true
        | _ -> false
    let renderAtom (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Widget widgetBuilder ->
            widgetBuilder
        | TypeRefAtom.AnchorPath typePath ->
            TypePath.flatten typePath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
        | TypeRefAtom.TransientPath transientTypePath ->
            TransientTypePath.toAnchored transientTypePath
            |> List.map Name.Case.valueOrModified
            |> Ast.LongIdent
    let rec renderMolecule (molecule: TypeRefMolecule) =
        match molecule with
        | TypeRefMolecule.Tuple typeRefRenders ->
            typeRefRenders
            |> List.map render
            |> Ast.Tuple
        | TypeRefMolecule.Union typeRefRenders ->
            typeRefRenders
            |> List.map render
            |> function
                | [] -> Ast.Unit()
                | [ widget ] -> widget
                | types ->
                    let length = List.length types
                    let prefix = Ast.Anon $"U{length}"
                    Ast.AppPrefix(prefix, types)
        // if we have no parameters, then we render a unit function
        | TypeRefMolecule.Function([], returnType) ->
            let returnType = render returnType
            Ast.Funs(Ast.Unit(), returnType)
        | TypeRefMolecule.Function(parameters, returnType) ->
            let parameters =
                parameters
                |> List.map (fun ref ->
                    render ref
                    |> if isFunctionRender ref
                        then Ast.Paren
                        else id)
            let returnType = render returnType
            Ast.Funs(parameters, returnType)
        | TypeRefMolecule.Prefix(prefix, args) ->
            let isNullable = prefix.Nullable
            let prefix = render { prefix with Nullable = false }
            let args = args |> List.map render
            Ast.AppPrefix(prefix, args)
            |> if isNullable then Ast.OptionPrefix else id

    and render (typeRefRender: TypeRefRender): WidgetBuilder<Type> =
        match typeRefRender.Kind with
        | TypeRefKind.Atom typeRefAtom ->
            renderAtom typeRefAtom
        | TypeRefKind.Molecule typeRefMolecule ->
            renderMolecule typeRefMolecule
        |> if typeRefRender.Nullable then
            Ast.OptionPrefix 
            else id
       
[<EditorBrowsable(EditorBrowsableState.Never)>]
module TestHelpers =
    let simpleRender (typeRefRender: TypeRefRender): WidgetBuilder<Type> = Implementation.render typeRefRender

module TypeRefAtom =
    let render value = Implementation.renderAtom value

module TypeRefMolecule =
    let render value = Implementation.renderMolecule value

module TypeRefRender =
    let render value = Implementation.render value