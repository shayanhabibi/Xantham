[<AutoOpen>]
module Xantham.Generator.Generator.Render_Collection

open System.Collections.Generic
open Fabulous.AST
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types

type Module = {
    Name: string
    Types: Dictionary<string, Anchored.TypeRender>
    Members: Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>
    Modules: Dictionary<string, Module>
}

type RootModule = {
    Types: Dictionary<string, Anchored.TypeRender>
    Members: Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>
    Modules: Dictionary<string, Module>
}

let collectModules (ctx: GeneratorContext) =
    let collectedRenders =
        ctx.AnchorRenders
        |> Seq.choose (function | KeyValue(_, Choice2Of2 x) -> Some x | _ -> None)
        |> Seq.toArray
        |> Array.collect (fun x ->
            [|
                x.Root, x.Render |> snd |> _.Value
                yield!
                    x.Anchors.Values
                    |> Seq.toArray
                    |> Array.map (fun (typePath, render) ->
                        Choice1Of2 typePath,
                        snd render |> _.Value
                    )
            |])
        |> Array.partition (fst >> _.IsChoice1Of2)
        ||> fun types members ->
            types
            |> Array.map (function
                | Choice1Of2 x, typeRender -> x, typeRender
                | _ -> failwith "unreachable guaranteed by guard in partition"
                )
            |> Array.map (fun (typePath, render) ->
                let path = TypePath.flatten typePath |> List.map Name.Case.valueOrModified
                path
                |> List.truncate (List.length path - 1), Choice1Of3 render
                )
            |> Array.append (
                members
                |> Array.map (function
                    | Choice2Of2 x, render ->
                        let path = AnchorPath.create x |> AnchorPath.flatten |> List.map Name.Case.valueOrModified
                        path
                        |> List.truncate (List.length path - 1),
                        match render with
                        | Anchored.TypeRender.Variable render -> Choice2Of3 render
                        | Anchored.TypeRender.Function render -> Choice3Of3 render
                        | _ -> failwith "unreachable guaranteed by guard in partition"
                    | _ -> failwith "unreachable guaranteed by guard in partition"
                    )
                )
            |> Array.sortBy fst
    let rootTypes = Dictionary<string, Anchored.TypeRender>()
    let rootMembers = Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>()
    let rootModules = Dictionary<string, Module>()
    let rootDummyModule = {
        Name = "ERROR_DUMMY_ROOT_SHOULD_NOT_EVER_RENDER"
        Types = rootTypes
        Members = rootMembers
        Modules = rootModules
    }
    let rec collect (module': Module)  (render: Choice<Anchored.TypeRender, Anchored.TypedNameRender, Anchored.FunctionLikeRender>) (path: string list) =
        match path with
        | [] ->
            match render with
            | Choice1Of3 typeRender ->
                let name =
                    match typeRender with
                    | TypeDefn { Name = name } 
                    | TypeAlias (
                        TypeAliasRender.Alias { Name = name }
                       | TypeAliasRender.StringUnion { Name = name }
                       | TypeAliasRender.EnumUnion { Name = name }
                       | TypeAliasRender.TypeDefn { Name = name }
                        ) 
                    | StringUnion { Name = name } 
                    | EnumUnion { Name = name } -> Name.Case.valueOrModified name
                    | TypeAlias (TypeAliasRender.Function { Name = name }) -> Name.Case.valueOrModified name
                    | _ | _ -> failwith "unreachable guaranteed by guard in partition"
                if module'.Types.TryAdd(name, typeRender) |> not then
                    printfn "Duplicate type name: %A" name
            | Choice2Of3 ({ Name = name } as typedNameRender) ->
                if module'.Members.TryAdd(Name.Case.valueOrModified name, Choice1Of2 typedNameRender) |> not then
                    printfn "Duplicate member name: %A" name
            | Choice3Of3 ({ Name = name } as functionLikeRender) ->
                if module'.Members.TryAdd(Name.Case.valueOrModified name, Choice2Of2 functionLikeRender) |> not then
                    printfn "Duplicate member name: %A" name
        | head :: tail ->
            match module'.Modules.TryGetValue head with
            | true, module' -> collect module' render tail
            | _ ->
                let next = {
                    Name = head
                    Types = Dictionary<string, Anchored.TypeRender>()
                    Members = Dictionary<string, Choice<Anchored.TypedNameRender, Anchored.FunctionLikeRender>>()
                    Modules = Dictionary<string, Module>()
                }
                module'.Modules.Add(head, next)
                collect next render tail
    collectedRenders
    |> Array.iter (fun (path, render) -> collect rootDummyModule render path)
    {
        Members = rootMembers
        Types = rootTypes
        Modules = rootModules
    }

let rec renderModule (ctx: GeneratorContext) (root: Module) =
    let nextModules =
        root.Modules.Values
        |> Seq.map (renderModule ctx)
    Ast.Module(root.Name) {
        for KeyValue(_, render) in root.Members do
            match render with
            | Choice1Of2 typedName ->
                TypedNameRender.renderBinding ctx typedName
            | Choice2Of2 functionLike ->
                FunctionLikeRender.renderDelegate ctx functionLike
        for KeyValue(_, render) in root.Types do
            match render with
            | TypeDefn typeLikeRender ->
                TypeLikeRender.renderInterface ctx typeLikeRender
            | TypeAlias typeAliasRender ->
                match typeAliasRender with
                | TypeAliasRender.Alias typeAliasRenderRef -> ()
                | TypeAliasRender.TypeDefn typeLikeRender ->
                    TypeLikeRender.renderInterface ctx typeLikeRender
                | TypeAliasRender.StringUnion literalUnionRender -> ()
                | TypeAliasRender.EnumUnion literalUnionRender -> ()
                | TypeAliasRender.Function functionLikeRender -> ()
            | StringUnion literalUnionRender -> ()
            | EnumUnion literalUnionRender -> ()
            | _ -> ()
        yield! nextModules
    }

let renderRoot (ctx: GeneratorContext) (root: RootModule) =
    let nextModules =
        root.Modules.Values
        |> Seq.map (renderModule ctx)
    Ast.AnonymousModule() {
        for KeyValue(_, render) in root.Members do
            match render with
            | Choice1Of2 typedName ->
                TypedNameRender.renderBinding ctx typedName
            | Choice2Of2 functionLike ->
                FunctionLikeRender.renderDelegate ctx functionLike
        for KeyValue(_, render) in root.Types do
            match render with
            | TypeDefn typeLikeRender ->
                TypeLikeRender.renderInterface ctx typeLikeRender
            | TypeAlias typeAliasRender ->
                match typeAliasRender with
                | TypeAliasRender.Alias typeAliasRenderRef -> ()
                | TypeAliasRender.TypeDefn typeLikeRender ->
                    TypeLikeRender.renderInterface ctx typeLikeRender
                | TypeAliasRender.StringUnion literalUnionRender -> ()
                | TypeAliasRender.EnumUnion literalUnionRender -> ()
                | TypeAliasRender.Function functionLikeRender -> ()
            | StringUnion literalUnionRender -> ()
            | EnumUnion literalUnionRender -> ()
            | _ -> ()
        yield! nextModules
    }