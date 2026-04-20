[<AutoOpen>]
module Xantham.Generator.Generator.TypeRefRender

open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Generator.Types
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Decoder

module private Implementation =
    let rec isFunctionRender (render: TypeRefRender) =
        match render.Kind with
        | TypeRefKind.Molecule (TypeRefMolecule.Function _) -> true
        | _ -> false
    let renderAtom (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Widget widgetBuilder ->
            widgetBuilder
        | TypeRefAtom.ConcretePath typePath ->
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
                    erasedUnion { yield! types }
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
    module Anchored =
        let renderAtom (atom: Anchored.TypeRefAtom) =
            match atom with
            | Anchored.TypeRefAtom.Widget widget -> widget
            | Anchored.TypeRefAtom.Path path ->
                TypePath.flatten path
                |> List.map Name.Case.valueOrModified
                |> Ast.LongIdent
        let rec renderMolecule (molecule: Anchored.TypeRefMolecule) =
            match molecule with
            | Anchored.TypeRefMolecule.Function([], returnType) ->
                let returnType = render returnType
                Ast.Funs(Ast.Unit(), returnType)
            | Anchored.TypeRefMolecule.Function(parameters, returnType) ->
                Ast.Funs(
                    parameters |> List.map render,
                    render returnType
                    )
            | Anchored.TypeRefMolecule.Prefix(prefix, args) ->
                let isNullable = prefix.Nullable
                let prefix = render { prefix with Nullable = false }
                let args = args |> List.map render
                Ast.AppPrefix(prefix, args)
                |> if isNullable then Ast.OptionPrefix else id
            | Anchored.TypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map render
                |> Ast.Tuple
            | Anchored.TypeRefMolecule.Union typeRefRenders ->
                typeRefRenders
                |> List.map render
                |> function
                    | [] -> Ast.Unit()
                    | [ widget ] -> widget
                    | types ->
                        erasedUnion { yield! types }
                
        and render (typeRefRender: Anchored.TypeRefRender): WidgetBuilder<Type> =
            match typeRefRender.Kind with
            | Anchored.TypeRefKind.Atom typeRefAtom ->
                renderAtom typeRefAtom
            | Anchored.TypeRefKind.Molecule typeRefMolecule ->
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
    type SRTPHelper =
        static member Render (value: TypeRefRender) = Implementation.render value
        static member Render (value: Anchored.TypeRefRender) = Implementation.Anchored.render value
    let inline render value = ((^T or SRTPHelper):(static member Render: ^T -> WidgetBuilder<Type>) value)
    module Anchored =
        let render value = Implementation.Anchored.render value