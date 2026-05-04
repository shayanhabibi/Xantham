[<AutoOpen>]
module Xantham.Generator.Generator.TypeRefRender

open System.ComponentModel
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.Types
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Decoder

module private Implementation =
    let inline mkRctxImpl stage (render: RenderMode voption) position owner =
        {
            Render = render
            Owner = owner
            Position = position
            Stage = stage
        }
    let inline mkBuildRctx render position owner = mkRctxImpl RenderStage.TypeRefBuild render position owner
    let inline mkEmitRctx render position owner = mkRctxImpl RenderStage.TypeRefEmit render position owner
    let rec isFunctionRender (render: TypeRefRender) =
        match render.Kind with
        | TypeRefKind.Molecule (TypeRefMolecule.Function _) -> true
        | _ -> false
    let renderAtom (atom: TypeRefAtom) =
        match atom with
        | TypeRefAtom.Intrinsic s ->
            Ast.LongIdent [ s ]
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
    let renderAtomHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (atom: TypeRefAtom) =
        let emission = renderAtom atom
        match HookSlot.run ctx.Customisation.TypeRefEmit ctx (mkEmitRctx renderMode position owner) emission with
        | HookResult.Pass -> emission
        | HookResult.Replace v -> v
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
    let rec renderMoleculeHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (molecule: TypeRefMolecule) =
        match molecule with
        | TypeRefMolecule.Function([], returnType) ->
            let returnType = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionReturn) returnType
            Ast.Funs(Ast.Unit(), returnType)
        | TypeRefMolecule.Function(parameters, returnType) ->
            let parameters =
                parameters
                |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionParameter))
            let returnType = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionReturn) returnType
            Ast.Funs(parameters, returnType)
        | TypeRefMolecule.Prefix(prefix, args) ->
            let isNullable = prefix.Nullable
            let prefix = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.Standalone) { prefix with Nullable = false }
            let args = args |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.TypeArg))
            Ast.AppPrefix(prefix, args)
            |> if isNullable then Ast.OptionPrefix else id
        | TypeRefMolecule.Tuple typeRefRenders ->
            typeRefRenders
            |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.TupleElement))
            |> Ast.Tuple
        | TypeRefMolecule.Union typeRefRenders ->
            typeRefRenders
            |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.UnionMember))
            |> function
                | [] -> Ast.Unit()
                | [ widget ] -> widget
                | types -> erasedUnion { yield! types }
    and renderHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (typeRefRender: TypeRefRender): WidgetBuilder<Type> =
        match HookSlot.run ctx.Customisation.TypeRefBuild ctx (mkBuildRctx renderMode position owner) typeRefRender with
        | HookResult.Pass ->
            match typeRefRender.Kind with
            | TypeRefKind.Atom atom -> renderAtomHook ctx renderMode owner position atom
            | TypeRefKind.Molecule molecule -> renderMoleculeHook ctx renderMode owner position molecule
            |> if typeRefRender.Nullable then
                   Ast.OptionPrefix
               else id
        | HookResult.Replace v ->
            match v.Kind with
            | TypeRefKind.Atom atom -> renderAtomHook ctx renderMode owner position atom
            | TypeRefKind.Molecule molecule -> renderMoleculeHook ctx renderMode owner position molecule
            |> if v.Nullable then
                   Ast.OptionPrefix
               else id
        |> fun widget ->
            match HookSlot.run ctx.Customisation.TypeRefEmit ctx (mkEmitRctx renderMode position owner) widget with
            | HookResult.Pass -> widget
            | HookResult.Replace v -> v
        
    module Anchored =
        let inline mkBuildRctx render position owner = mkRctxImpl RenderStage.Anchored render position owner
        let inline mkEmitRctx render position owner = mkRctxImpl RenderStage.TypeRefEmit render position owner
        let renderAtom (atom: Anchored.TypeRefAtom) =
            match atom with
            | Anchored.TypeRefAtom.Widget widget -> widget
            | Anchored.TypeRefAtom.Path path ->
                TypePath.flatten path
                |> List.map Name.Case.valueOrModified
                |> Ast.LongIdent
            | Anchored.TypeRefAtom.Intrinsic s ->
                Ast.LongIdent [ s ]
        let renderAtomHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (atom: Anchored.TypeRefAtom) =
            let emission = renderAtom atom
            match HookSlot.run ctx.Customisation.TypeRefEmit ctx (mkEmitRctx renderMode position owner) emission with
            | HookResult.Pass -> emission
            | HookResult.Replace v -> v
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
                
        let rec renderMoleculeHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (molecule: Anchored.TypeRefMolecule) =
            match molecule with
            | Anchored.TypeRefMolecule.Function([], returnType) ->
                let returnType = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionReturn) returnType
                Ast.Funs(Ast.Unit(), returnType)
            | Anchored.TypeRefMolecule.Function(parameters, returnType) ->
                let parameters =
                    parameters
                    |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionParameter))
                let returnType = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.FunctionReturn) returnType
                Ast.Funs(parameters, returnType)
            | Anchored.TypeRefMolecule.Prefix(prefix, args) ->
                let isNullable = prefix.Nullable
                let prefix = renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.Standalone) { prefix with Nullable = false }
                let args = args |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.TypeArg))
                Ast.AppPrefix(prefix, args)
                |> if isNullable then Ast.OptionPrefix else id
            | Anchored.TypeRefMolecule.Tuple typeRefRenders ->
                typeRefRenders
                |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.TupleElement))
                |> Ast.Tuple
            | Anchored.TypeRefMolecule.Union typeRefRenders ->
                typeRefRenders
                |> List.map (renderHook ctx renderMode owner (RenderPosition.RefPos TypeRefPosition.UnionMember))
                |> function
                    | [] -> Ast.Unit()
                    | [ widget ] -> widget
                    | types -> erasedUnion { yield! types }
        and renderHook (ctx: GeneratorContext) (renderMode: RenderMode voption) (owner: RenderOwner voption) (position: RenderPosition) (typeRefRender: Anchored.TypeRefRender): WidgetBuilder<Type> =
            match SkippableHookSlot.run ctx.Customisation.AnchoredRef ctx (mkBuildRctx renderMode position owner) typeRefRender with
            | SkippableHookResult.Pass ->
                match typeRefRender.Kind with
                | Anchored.TypeRefKind.Atom atom -> renderAtomHook ctx renderMode owner position atom
                | Anchored.TypeRefKind.Molecule molecule -> renderMoleculeHook ctx renderMode owner position molecule
                |> if typeRefRender.Nullable then
                       Ast.OptionPrefix
                   else id
                |> fun widget ->
                    match HookSlot.run ctx.Customisation.TypeRefEmit ctx (mkEmitRctx renderMode position owner) widget with
                    | HookResult.Pass -> widget
                    | HookResult.Replace v -> v
            | SkippableHookResult.Replace v ->
                match v.Kind with
                | Anchored.TypeRefKind.Atom atom -> renderAtomHook ctx renderMode owner position atom
                | Anchored.TypeRefKind.Molecule molecule -> renderMoleculeHook ctx renderMode owner position molecule
                |> if v.Nullable then
                       Ast.OptionPrefix
                   else id
                |> fun widget ->
                    match HookSlot.run ctx.Customisation.TypeRefEmit ctx (mkEmitRctx renderMode position owner) widget with
                    | HookResult.Pass -> widget
                    | HookResult.Replace v -> v
            | SkippableHookResult.Skip -> Ast.Unit()
            
     
        let localisedRender (anchorPath: AnchorPath) (typeRefRender: Anchored.TypeRefRender) =
            Anchored.TypeRefRender.localise anchorPath typeRefRender

[<EditorBrowsable(EditorBrowsableState.Never)>]
module TestHelpers =
    let simpleRender (typeRefRender: TypeRefRender): WidgetBuilder<Type> = Implementation.render typeRefRender

// Production code must not call these non-context renderers — they bypass the TypeRefBuild and
// TypeRefEmit hook slots. They remain available for tests (EditorBrowsable hides them from
// IntelliSense without restricting access). Production callers should use
// TypeRefRender.SRTPHelper.RenderWithContext via the renderTypeRef helper in TypeRender.Render.fs.
[<EditorBrowsable(EditorBrowsableState.Never)>]
module TypeRefAtom =
    let render value = Implementation.renderAtom value

[<EditorBrowsable(EditorBrowsableState.Never)>]
module TypeRefMolecule =
    let render value = Implementation.renderMolecule value

module TypeRefRender =
    type SRTPHelper =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        static member Render (value: TypeRefRender) = Implementation.render value
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        static member Render (value: Anchored.TypeRefRender) = Implementation.Anchored.render value
        static member RenderWithContext(ctx: GeneratorContext, renderMode: RenderMode voption, owner: RenderOwner voption, pos: RenderPosition voption, value: TypeRefRender) =
            let pos = defaultValueArg pos RenderPosition.NotApplicable
            Implementation.renderHook ctx renderMode owner pos value
        static member RenderWithContext(ctx: GeneratorContext, renderMode: RenderMode voption, owner: RenderOwner voption, pos: RenderPosition voption, value: Anchored.TypeRefRender) =
            let pos = defaultValueArg pos RenderPosition.NotApplicable
            Implementation.Anchored.renderHook ctx renderMode owner pos value

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let inline render value = ((^T or SRTPHelper):(static member Render: ^T -> WidgetBuilder<Type>) value)
    module Anchored =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let render value = Implementation.Anchored.render value
        let localiseRender (anchorPath: AnchorPath) (typeRefRender: Anchored.TypeRefRender) = Implementation.Anchored.localisedRender anchorPath typeRefRender