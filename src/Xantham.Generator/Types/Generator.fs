namespace Xantham.Generator

open System.Collections.Generic
open System.ComponentModel
open Xantham.Decoder.ArenaInterner
open Xantham.Generator.TypeRefRender
open Xantham.Generator

type GeneratorContext =
    {
        TypeRefRenders: Dictionary<ResolvedType, TypeRefRender>
        ExportRefRenders: Dictionary<ResolvedExport, TypeRefRender>
        TypeRenders: Dictionary<ResolvedType, Render>
        ExportRenders: Dictionary<ResolvedExport, Render>
    }
    static member Empty = {
        TypeRefRenders = Dictionary<ResolvedType, TypeRefRender>()
        ExportRefRenders = Dictionary<ResolvedExport, TypeRefRender>()
        TypeRenders = Dictionary<ResolvedType, Render>()
        ExportRenders = Dictionary<ResolvedExport, Render>()
    }
    override this.ToString() = $"GeneratorContext(%d{this.TypeRefRenders.Count})"

module GeneratorContext =
    let getTypeRef (context: GeneratorContext) (resolvedType: ResolvedType) =
        Dictionary.tryItem resolvedType context.TypeRefRenders
    let getExportRef (context: GeneratorContext) (resolvedExport: ResolvedExport) =
        Dictionary.tryItem resolvedExport context.ExportRefRenders
    let addTypeRef (context: GeneratorContext) (resolvedType: ResolvedType) (typeRefRender: TypeRefRender) =
        Dictionary.addOrReplace resolvedType typeRefRender context.TypeRefRenders
    let addExportRef (context: GeneratorContext) (resolvedExport: ResolvedExport) (typeRefRender: TypeRefRender) =
        Dictionary.addOrReplace resolvedExport typeRefRender context.ExportRefRenders
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline TryGetRef(context: GeneratorContext, resolvedType: ResolvedType) = getTypeRef context resolvedType
        static member inline TryGetRef(context: GeneratorContext, resolvedExport: ResolvedExport) = getExportRef context resolvedExport
        static member inline TryAddRef(context: GeneratorContext, resolvedType: ResolvedType, typeRefRender: TypeRefRender) = addTypeRef context resolvedType typeRefRender
        static member inline TryAddRef(context: GeneratorContext, resolvedExport: ResolvedExport, typeRefRender: TypeRefRender) = addExportRef context resolvedExport typeRefRender
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
        Dictionary.tryItem resolvedType context.TypeRenders
    let getExportRender (context: GeneratorContext) (resolvedExport: ResolvedExport) =
        Dictionary.tryItem resolvedExport context.ExportRenders
