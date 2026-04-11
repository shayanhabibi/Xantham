namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.NamePath
open Xantham.Generator.TypeRefRender
open Xantham.Generator

/// Wrapper that chooses the type of dictionary (concurrent/normal)
/// depending on the presence of the CONCURRENT_DICT constant on compilation
type DictionaryImpl<'Key, 'Value> =
    #if CONCURRENT_DICT
    System.Collections.Concurrent.ConcurrentDictionary<'Key, 'Value>
    #else
    Dictionary<'Key, 'Value>
    #endif

type GeneratorHooks = {
    /// if you want default handling for the resolved type, you can return None
    TypeRefPrerender: (GeneratorContext -> ResolvedType -> TypeRefRender option) list
    ExportRefPrerender: (GeneratorContext -> ResolvedExport -> TypeRefRender option) list
    /// if you want default handling for the typeRefRender, you can return the original
    TypeRefPostrender: (GeneratorContext -> TypeRefRender -> TypeRefRender) list
    PathModifier: ( GeneratorContext -> AnchorPath -> AnchorPath ) list
    /// If yyou want to keep the default render, just return the argument
    TypeRenderModifier: ( GeneratorContext -> TypeRender -> TypeRender ) list
}

and GeneratorContext =
    {
        TypeRefRenders: DictionaryImpl<ResolvedType, TypeRefRender>
        ExportRefRenders: DictionaryImpl<ResolvedExport, TypeRefRender>
        TypeRenders: DictionaryImpl<ResolvedType, Render>
        ExportRenders: DictionaryImpl<ResolvedExport, Render>
        Hooks: GeneratorHooks option
    }
    static member Empty = {
        TypeRefRenders = DictionaryImpl<ResolvedType, TypeRefRender>()
        ExportRefRenders = DictionaryImpl<ResolvedExport, TypeRefRender>()
        TypeRenders = DictionaryImpl<ResolvedType, Render>()
        ExportRenders = DictionaryImpl<ResolvedExport, Render>()
        Hooks = None
    }
    override this.ToString() = $"GeneratorContext(%d{this.TypeRefRenders.Count})"

module GeneratorContext =
    /// <summary>
    /// Inlined implementations of the required functions with
    /// branching compile time logic depending on the type of dictionary
    /// we are working with due to the absence/presence of the CONCURRENT_DICT constant.
    /// </summary>
    module private Operation =
        let inline getOrAdd func key dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.tryItemOrAdd func key dict
            #else
            Dictionary.tryItem key dict
            |> ValueOption.defaultWith (fun () ->
                Dictionary.tryAddOrGet key (func key) dict)
            #endif
        let inline tryGet key dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.tryItem key dict
            #else
            Dictionary.tryItem key dict
            #endif
        let inline addOrReplace key value dict =
            #if CONCURRENT_DICT
            ConcurrentDictionary.addOrReplace key value dict
            #else
            Dictionary.addOrReplace key value dict
            #endif

    let private makeTypeRefRenderHooks (context: GeneratorContext) (fn: ResolvedType -> TypeRefRender) =
        let postFunc =
            match context.Hooks with
            | None | Some { TypeRefPostrender = [] } -> id
            | Some { TypeRefPostrender = hooks } ->
                fun typeRefRender ->
                    hooks
                    |> List.fold (fun typeRefRender fn -> fn context typeRefRender) typeRefRender
        match context.Hooks with
        | None | Some { TypeRefPrerender = [] } -> fn
        | Some { TypeRefPrerender = hooks } ->
            fun resolvedType ->
                hooks
                |> List.tryPick (fun hook -> hook context resolvedType)
                |> Option.defaultValue (fn resolvedType)
        >> postFunc
    let private makeExportRefRenderHooks (context: GeneratorContext) (fn: ResolvedExport -> TypeRefRender) =
        let postFunc =
            match context.Hooks with
            | None | Some { TypeRefPostrender = [] } -> id
            | Some { TypeRefPostrender = hooks } ->
                fun typeRefRender ->
                    hooks
                    |> List.fold (fun typeRefRender fn -> fn context typeRefRender) typeRefRender
        match context.Hooks with
        | None | Some { ExportRefPrerender = [] } -> fn
        | Some { ExportRefPrerender = hooks } ->
            fun resolvedExport ->
                hooks
                |> List.tryPick (fun hook -> hook context resolvedExport)
                |> Option.defaultValue (fn resolvedExport)
        >> postFunc
    module Hooks =
        let withHooks (hooks: GeneratorHooks) (context: GeneratorContext) =
            { context with Hooks = Some hooks }
        let empty = {
            GeneratorHooks.ExportRefPrerender = []
            TypeRefPrerender = []
            TypeRefPostrender = []
            PathModifier = []
            TypeRenderModifier = []
        }
        let forHooks (hookFn: GeneratorHooks -> GeneratorHooks) (context: GeneratorContext) =
            { context with Hooks = context.Hooks |> Option.defaultValue empty |> hookFn |> Some }
        module Path =
            /// <summary>
            /// Allows you to modify paths that are generated. The hooks are executed
            /// in order. Each subsequent hook will act on the output of the previous one.
            /// </summary>
            /// <param name="handler"></param>
            /// <param name="context"></param>
            let withHandler (handler: GeneratorContext -> AnchorPath -> AnchorPath) (context: GeneratorContext) =
                forHooks (fun hooks -> {
                    hooks with PathModifier = hooks.PathModifier @ [ handler ]
                }) context
        module TypeRef =
            /// <summary>
            /// Allows you to modify the typeRefRender that is generated for a ResolvedType.
            /// The hooks are executed in order. You can choose to allow the default behaviour by
            /// returning None. Hooks are executed until one of them returns <c>Some</c> value.
            /// </summary>
            /// <param name="handler"></param>
            /// <param name="context"></param>
            let forResolvedType (handler: GeneratorContext -> ResolvedType -> TypeRefRender option) (context: GeneratorContext) =
                forHooks (fun hooks -> {
                    hooks with TypeRefPrerender = hooks.TypeRefPrerender @ [ handler ]
                }) context
               
            /// <summary>
            /// Allows you to modify the typeRefRender that is generated for a ResolvedExport.
            /// The hooks are executed in order. You can choose to allow the default behaviour by
            /// returning None. Hooks are executed until one of them returns <c>Some</c> value.
            /// </summary>
            /// <param name="handler"></param>
            /// <param name="context"></param>
            let forExport (handler: GeneratorContext -> ResolvedExport -> TypeRefRender option) (context: GeneratorContext) =
                forHooks (fun hooks -> {
                    hooks with ExportRefPrerender = hooks.ExportRefPrerender @ [ handler ]
                }) context
            /// <summary>
            /// Allows you to modify type ref renders after they have been generated; agnostic of whether
            /// they were generated by a ResolvedType or ResolvedExport. The hooks are executed in order.
            /// Each subsequent hook will act on the output of the previous one.
            /// </summary>
            /// <param name="handler"></param>
            /// <param name="context"></param>
            let withPosthandler (handler: GeneratorContext -> TypeRefRender -> TypeRefRender) (context: GeneratorContext) =
                forHooks (fun hooks -> {
                    hooks with TypeRefPostrender = hooks.TypeRefPostrender @ [ handler ]
                }) context
                
    let getTypeRefWith (context: GeneratorContext) (resolvedType: ResolvedType) (f: ResolvedType -> TypeRefRender) =
        let pipeline = makeTypeRefRenderHooks context f
        Operation.getOrAdd pipeline resolvedType context.TypeRefRenders
    let getExportRefWith (context: GeneratorContext) (resolvedExport: ResolvedExport) (f: ResolvedExport -> TypeRefRender) =
        let pipeline = makeExportRefRenderHooks context f
        Operation.getOrAdd pipeline resolvedExport context.ExportRefRenders
    let getTypeRef (context: GeneratorContext) (resolvedType: ResolvedType) =
        Operation.tryGet resolvedType context.TypeRefRenders
    let getExportRef (context: GeneratorContext) (resolvedExport: ResolvedExport) =
        Operation.tryGet resolvedExport context.ExportRefRenders
    let addTypeRef (context: GeneratorContext) (resolvedType: ResolvedType) (typeRefRender: TypeRefRender) =
        Operation.addOrReplace resolvedType typeRefRender context.TypeRefRenders
    let addExportRef (context: GeneratorContext) (resolvedExport: ResolvedExport) (typeRefRender: TypeRefRender) =
        Operation.addOrReplace resolvedExport typeRefRender context.ExportRefRenders
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline TryGetRef(context: GeneratorContext, resolvedType: ResolvedType) = getTypeRef context resolvedType
        static member inline TryGetRef(context: GeneratorContext, resolvedExport: ResolvedExport) = getExportRef context resolvedExport
        static member inline TryAddRef(context: GeneratorContext, resolvedType: ResolvedType, typeRefRender: TypeRefRender) = addTypeRef context resolvedType typeRefRender
        static member inline TryAddRef(context: GeneratorContext, resolvedExport: ResolvedExport, typeRefRender: TypeRefRender) = addExportRef context resolvedExport typeRefRender
        static member inline GetOrAddRef(context: GeneratorContext, resolvedType: ResolvedType, func: ResolvedType -> TypeRefRender) = getTypeRefWith context resolvedType func
        static member inline GetOrAddRef(context: GeneratorContext, resolvedExport: ResolvedExport, func: ResolvedExport -> TypeRefRender) = getExportRefWith context resolvedExport func
    /// <summary>
    /// <para>SRTP Convenience function for <c>getTypeRefWith</c> and <c>getExportRefWith</c></para>
    /// </summary>
    /// <param name="context"></param>
    /// <param name="resolvedTypeOrExport"></param>
    /// <param name="func"></param>
    let inline getOrAddRef (context: GeneratorContext) (resolvedTypeOrExport: ^T) (func: ^T -> TypeRefRender) =
        ((^T or SRTPHelper) : (static member GetOrAddRef: GeneratorContext * ^T * (^T -> TypeRefRender) -> TypeRefRender) (context, resolvedTypeOrExport, func))
    /// <summary>
    /// <para>SRTP Convenience function for <c>getTypeRef</c> and <c>getExportRef</c></para>
    /// </summary>
    /// <param name="context"></param>
    /// <param name="resolvedTypeOrExport"></param>
    let inline getRef (context: GeneratorContext) (resolvedTypeOrExport: ^T) =
        ((^T or SRTPHelper) : (
            static member TryGetRef: GeneratorContext * ^T -> TypeRefRender voption
        ) (context, resolvedTypeOrExport))
    /// <summary>
    /// <para>SRTP Convenience function for <c>addTypeRef</c> and <c>addExportRef</c></para>
    /// </summary>
    /// <param name="context"></param>
    /// <param name="resolvedTypeOrExport"></param>
    /// <param name="typeRefRender"></param>
    let inline addRef (context: GeneratorContext) (resolvedTypeOrExport: ^T) (typeRefRender: TypeRefRender) =
        ((^T or SRTPHelper) : (
            static member TryAddRef: GeneratorContext * ^T * TypeRefRender -> unit
        ) (context, resolvedTypeOrExport, typeRefRender))
    let getTypeRender (context: GeneratorContext) (resolvedType: ResolvedType) =
        Operation.tryGet resolvedType context.TypeRenders
    let getExportRender (context: GeneratorContext) (resolvedExport: ResolvedExport) =
        Operation.tryGet resolvedExport context.ExportRenders
