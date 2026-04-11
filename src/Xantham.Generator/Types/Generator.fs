namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Xantham.Decoder.ArenaInterner
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

type GeneratorContext =
    {
        TypeRefRenders: DictionaryImpl<ResolvedType, TypeRefRender>
        ExportRefRenders: DictionaryImpl<ResolvedExport, TypeRefRender>
        TypeRenders: DictionaryImpl<ResolvedType, Render>
        ExportRenders: DictionaryImpl<ResolvedExport, Render>
    }
    static member Empty = {
        TypeRefRenders = DictionaryImpl<ResolvedType, TypeRefRender>()
        ExportRefRenders = DictionaryImpl<ResolvedExport, TypeRefRender>()
        TypeRenders = DictionaryImpl<ResolvedType, Render>()
        ExportRenders = DictionaryImpl<ResolvedExport, Render>()
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

    let getTypeRefWith (context: GeneratorContext) (resolvedType: ResolvedType) (f: ResolvedType -> TypeRefRender) =
        Operation.getOrAdd f resolvedType context.TypeRefRenders
    let getExportRefWith (context: GeneratorContext) (resolvedExport: ResolvedExport) (f: ResolvedExport -> TypeRefRender) =
        Operation.getOrAdd f resolvedExport context.ExportRefRenders
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
