[<AutoOpen>]
module Xantham.Generator.Generator.RenderScope_Anchored

open System.Collections.Generic
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.Generator
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Anchored

// module Render =
//     let anchorMetadataPath (ctx: GeneratorContext) (anchorPath: AnchorPath) (path: Path) =
//         match path with
//         | Path.Transient transientPath ->
//             transientPath
//             |> TransientPath.anchor anchorPath
//         | Path.Anchor anchorPath -> anchorPath
//     let anchorMetadata (ctx: GeneratorContext) (anchorPath: AnchorPath) (metadata: RenderMetadata) =
//         if metadata.Path.IsAnchor then metadata else
//         { Path = anchorMetadataPath ctx anchorPath metadata.Path |> Path.create }
//     module Transient =
//         let inline anchorUnionCase (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralCaseRender<'T>) =
//             let anchoredPath =
//                 enumUnion.Metadata.Path
//                 |> anchorMetadataPath ctx parentPath
//             {
//                 LiteralCaseRender.Name =
//                     enumUnion.Name
//                     |> ValueOption.defaultValue (
//                         match anchoredPath with
//                         | AnchorPath.Type typePath -> typePath.Name
//                         | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
//                         | _ -> failwith "Unreachable branch"
//                         )
//                 Metadata = { Path = Path.create anchoredPath }
//                 Value = enumUnion.Value
//                 Documentation = enumUnion.Documentation
//             }
//         let inline anchorUnion (ctx: GeneratorContext) (parentPath: AnchorPath) (enumUnion: Transient.LiteralUnionRender<'T>) =
//             let anchoredPath =
//                 enumUnion.Metadata.Path
//                 |> anchorMetadataPath ctx parentPath
//             {
//                 Metadata = { Path = Path.create anchoredPath }
//                 LiteralUnionRender.Name =
//                     enumUnion.Name
//                     |> ValueOption.defaultWith (fun () ->
//                         match anchoredPath with
//                         | AnchorPath.Type typePath -> typePath.Name
//                         | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
//                         | _ -> failwith "Unreachable branch"
//                         )
//                 Cases =
//                     enumUnion.Cases
//                     |> List.map (anchorUnionCase ctx anchoredPath)
//                 Documentation = enumUnion.Documentation
//             }
//         let anchorTypeParameters (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeParameter: Transient.TypeParameterRender) =
//             let anchorPath =
//                 typeParameter.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 TypeParameterRender.Name =
//                     typeParameter.Name
//                 Constraint =
//                     typeParameter.Constraint
//                     |> ValueOption.map (TypeRefRender.anchor anchorPath)
//                 Default =
//                     typeParameter.Default
//                     |> ValueOption.map (TypeRefRender.anchor anchorPath)
//                 Documentation = typeParameter.Documentation
//             }
//         let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Transient.TypedNameRender) =
//             let anchorPath =
//                 typedName.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 TypedNameRender.Name = typedName.Name
//                 Type = typedName.Type |> TypeRefRender.anchor anchorPath
//                 Traits = typedName.Traits
//                 TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                 Documentation = typedName.Documentation
//             }
//         let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Transient.FunctionLikeSignature) =
//             {
//                 FunctionLikeSignature.Metadata = { Path = Path.create anchorPath }
//                 Parameters =
//                     functionSignature.Parameters
//                     |> List.map (anchorTypedNameRender ctx anchorPath)
//                 ReturnType =
//                     functionSignature.ReturnType
//                     |> TypeRefRender.anchor anchorPath
//                 Traits = functionSignature.Traits
//                 Documentation = functionSignature.Documentation
//                 TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//             }
//         let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Transient.FunctionLikeRender) =
//             let anchorPath =
//                 functionLike.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 FunctionLikeRender.Name = functionLike.Name
//                 Signatures = functionLike.Signatures |> List.map (anchorFunctionSignature ctx anchorPath)
//                 Traits = functionLike.Traits
//                 TypeParameters = functionLike.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                 Documentation = functionLike.Documentation
//             }
//         let anchorTypeDefn (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeDefn: Transient.TypeLikeRender) =
//             let anchorPath =
//                 typeDefn.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 TypeLikeRender.Metadata = { Path = Path.create anchorPath }
//                 Name =
//                     typeDefn.Name
//                     |> ValueOption.defaultWith (fun () ->
//                         match anchorPath with
//                         | AnchorPath.Type typePath -> typePath.Name
//                         | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
//                         | _ -> failwith "Unreachable branch"
//                         )
//                 TypeParameters =
//                     typeDefn.TypeParameters
//                     |> List.map (anchorTypeParameters ctx anchorPath)
//                 Inheritance =
//                     typeDefn.Inheritance
//                     |> List.map (TypeRefRender.anchor anchorPath)
//                 Members =
//                     typeDefn.Members
//                     |> List.map (anchorTypedNameRender ctx anchorPath)
//                 Functions =
//                     typeDefn.Functions
//                     |> List.map (anchorFunction ctx anchorPath)
//                 Constructors =
//                     typeDefn.Constructors
//                     |> List.map (List.map (anchorTypedNameRender ctx anchorPath))
//                 Documentation = typeDefn.Documentation
//             }
//         let anchorTypeAlias (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeAlias: Transient.TypeAliasRender) =
//             match typeAlias with
//             | Transient.TypeAliasRender.Alias alias ->
//                 let anchorPath =
//                     alias.Metadata.Path
//                     |> anchorMetadataPath ctx anchorPath
//                 {
//                     TypeAliasRenderRef.Metadata = { Path = Path.create anchorPath }
//                     Name = alias.Name |> ValueOption.defaultWith (fun () ->
//                         match anchorPath with
//                         | AnchorPath.Type typePath -> typePath.Name
//                         | AnchorPath.Member memberPath -> Name.Pascal.fromCase memberPath.Name
//                         | _ -> failwith "Unreachable branch"
//                         )
//                     TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                     Documentation = alias.Documentation
//                     Type = alias.Type |> TypeRefRender.anchor anchorPath
//                 }
//                 |> TypeAliasRender.Alias
//             | Transient.TypeAliasRender.TypeDefn typeLikeRender ->
//                 anchorTypeDefn ctx anchorPath typeLikeRender
//                 |> TypeAliasRender.TypeDefn
//             | Transient.TypeAliasRender.StringUnion literalUnionRender ->
//                 anchorUnion ctx anchorPath literalUnionRender
//                 |> TypeAliasRender.StringUnion
//             | Transient.TypeAliasRender.EnumUnion literalUnionRender ->
//                 anchorUnion ctx anchorPath literalUnionRender
//                 |> TypeAliasRender.EnumUnion
//             | Transient.TypeAliasRender.Function functionLikeRender ->
//                 anchorFunction ctx anchorPath functionLikeRender
//                 |> TypeAliasRender.Function
//
//         let anchor (ctx: GeneratorContext) (anchorPath: AnchorPath) (render: Transient.Render) =
//             match render with
//             | Transient.Render.RefOnly _ -> failwith "UNREACHABLE"
//             | Transient.Render.Render(ref, render) ->
//                 match render with
//                 | Transient.TypeRender.EnumUnion enumUnion ->
//                     anchorUnion ctx anchorPath enumUnion
//                     |> TypeRender.EnumUnion
//                 | Transient.TypeRender.StringUnion enumUnion ->
//                     anchorUnion ctx anchorPath enumUnion
//                     |> TypeRender.StringUnion
//                 | Transient.TypeRender.TypeDefn typeLikeRender ->
//                     anchorTypeDefn ctx anchorPath typeLikeRender
//                     |> TypeRender.TypeDefn
//                 | Transient.TypeRender.TypeAlias typeAliasRender ->
//                     anchorTypeAlias ctx anchorPath typeAliasRender
//                     |> TypeRender.TypeAlias
//                 | Transient.TypeRender.Function functionLikeRender ->
//                     anchorFunction ctx anchorPath functionLikeRender
//                     |> TypeRender.Function
//                 | Transient.TypeRender.Variable typedNameRender ->
//                     anchorTypedNameRender ctx anchorPath typedNameRender
//                     |> TypeRender.Variable
//                 |> fun typeRender ->
//                     Anchored.Render.Render( TypeRefRender.anchor anchorPath ref, typeRender )
//     module Concrete =
//         let inline anchorEnumCase (enumUnion: Concrete.LiteralCaseRender<'T>) =
//             {
//                 Metadata = enumUnion.Metadata
//                 LiteralCaseRender.Name = enumUnion.Name
//                 Value = enumUnion.Value
//                 Documentation = enumUnion.Documentation
//             }
//         let inline anchorEnum (enumUnion: Concrete.LiteralUnionRender<'T>) =
//             {
//                 Metadata = enumUnion.Metadata
//                 LiteralUnionRender.Name = enumUnion.Name
//                 Cases = enumUnion.Cases |> List.map anchorEnumCase
//                 Documentation = enumUnion.Documentation
//             }
//         let anchorTypeParameters (ctx: GeneratorContext) (anchorPath: AnchorPath) (typeParameter: Concrete.TypeParameterRender) =
//             let anchorPath =
//                 typeParameter.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 TypeParameterRender.Name =
//                     typeParameter.Name
//                 Constraint =
//                     typeParameter.Constraint
//                     |> ValueOption.map (TypeRefRender.anchor anchorPath)
//                 Default =
//                     typeParameter.Default
//                     |> ValueOption.map (TypeRefRender.anchor anchorPath)
//                 Documentation = typeParameter.Documentation
//             }
//         let anchorTypedNameRender (ctx: GeneratorContext) (anchorPath: AnchorPath) (typedName: Concrete.TypedNameRender) =
//             let anchorPath =
//                 typedName.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 TypedNameRender.Name = typedName.Name
//                 Type = typedName.Type |> TypeRefRender.anchor anchorPath
//                 Traits = typedName.Traits
//                 TypeParameters = typedName.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                 Documentation = typedName.Documentation
//             }
//         let anchorFunctionSignature (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionSignature: Concrete.FunctionLikeSignature) =
//             {
//                 FunctionLikeSignature.Metadata = { Path = Path.create anchorPath }
//                 Parameters =
//                     functionSignature.Parameters
//                     |> List.map (anchorTypedNameRender ctx anchorPath)
//                 ReturnType =
//                     functionSignature.ReturnType
//                     |> TypeRefRender.anchor anchorPath
//                 Traits = functionSignature.Traits
//                 Documentation = functionSignature.Documentation
//                 TypeParameters = functionSignature.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//             }
//         let anchorFunction (ctx: GeneratorContext) (anchorPath: AnchorPath) (functionLike: Concrete.FunctionLikeRender) =
//             let anchorPath =
//                 functionLike.Metadata.Path
//                 |> anchorMetadataPath ctx anchorPath
//             {
//                 Metadata = { Path = Path.create anchorPath }
//                 FunctionLikeRender.Name = functionLike.Name
//                 Signatures = functionLike.Signatures |> List.map (anchorFunctionSignature ctx anchorPath)
//                 Traits = functionLike.Traits
//                 TypeParameters = functionLike.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                 Documentation = functionLike.Documentation
//             }
//         let anchorTypeDefn (ctx: GeneratorContext) (typeDefn: Concrete.TypeLikeRender) =
//             let anchorPath =
//                 match typeDefn.Metadata.Path with
//                 | Path.Anchor anchorPath -> anchorPath
//                 | _ -> failwith "UNREACHABLE"
//             {
//                 TypeLikeRender.Metadata = { Path = Path.create anchorPath }
//                 Name = typeDefn.Name
//                 TypeParameters =
//                     typeDefn.TypeParameters
//                     |> List.map (anchorTypeParameters ctx anchorPath)
//                 Inheritance =
//                     typeDefn.Inheritance
//                     |> List.map (TypeRefRender.anchor anchorPath)
//                 Members =
//                     typeDefn.Members
//                     |> List.map (anchorTypedNameRender ctx anchorPath)
//                 Functions =
//                     typeDefn.Functions
//                     |> List.map (anchorFunction ctx anchorPath)
//                 Constructors =
//                     typeDefn.Constructors
//                     |> List.map (List.map (anchorTypedNameRender ctx anchorPath))
//                 Documentation = typeDefn.Documentation
//             }
//         let anchorTypeAlias (ctx: GeneratorContext) (typeAlias: Concrete.TypeAliasRender) =
//             match typeAlias with
//             | Concrete.TypeAliasRender.Alias alias ->
//                 let anchorPath =
//                     match alias.Metadata.Path with
//                     | Path.Anchor anchorPath -> anchorPath
//                     | _ -> failwith "UNREACHABLE"
//                 {
//                     TypeAliasRenderRef.Metadata = { Path = Path.create anchorPath }
//                     Name = alias.Name 
//                     TypeParameters = alias.TypeParameters |> List.map (anchorTypeParameters ctx anchorPath)
//                     Documentation = alias.Documentation
//                     Type = alias.Type |> TypeRefRender.anchor anchorPath
//                 }
//                 |> TypeAliasRender.Alias
//             | Concrete.TypeAliasRender.TypeDefn typeLikeRender ->
//                 anchorTypeDefn ctx typeLikeRender
//                 |> TypeAliasRender.TypeDefn
//             | Concrete.TypeAliasRender.StringUnion literalUnionRender ->
//                 anchorEnum literalUnionRender
//                 |> TypeAliasRender.StringUnion
//             | Concrete.TypeAliasRender.EnumUnion literalUnionRender ->
//                 anchorEnum literalUnionRender
//                 |> TypeAliasRender.EnumUnion
//             | Concrete.TypeAliasRender.Function functionLikeRender ->
//                 let anchorPath =
//                     match functionLikeRender.Metadata.Path with
//                     | Path.Anchor anchorPath -> anchorPath
//                     | _ -> failwith "UNREACHABLE"
//                 anchorFunction ctx anchorPath functionLikeRender
//                 |> TypeAliasRender.Function
//
//         let anchor (ctx: GeneratorContext) (render: Concrete.Render) =
//             match render with
//             | Concrete.Render.RefOnly _ -> failwith "UNREACHABLE"
//             | Concrete.Render.Render(ref, render) ->
//                 match render with
//                 | Concrete.TypeRender.EnumUnion enumUnion ->
//                     anchorEnum enumUnion
//                     |> TypeRender.EnumUnion
//                 | Concrete.TypeRender.StringUnion literalUnionRender ->
//                     anchorEnum literalUnionRender
//                     |> TypeRender.StringUnion
//                 | Concrete.TypeRender.TypeDefn typeLikeRender ->
//                     anchorTypeDefn ctx typeLikeRender
//                     |> TypeRender.TypeDefn
//                 | Concrete.TypeRender.TypeAlias typeAliasRender ->
//                     anchorTypeAlias ctx typeAliasRender
//                     |> TypeRender.TypeAlias
//                 | Concrete.TypeRender.Function functionLikeRender ->
//                     let anchorPath =
//                         match functionLikeRender.Metadata.Path with
//                         | Path.Anchor anchorPath -> anchorPath
//                         | _ -> failwith "UNREACHABLE"
//                     anchorFunction ctx anchorPath functionLikeRender
//                     |> TypeRender.Function
//                 | Concrete.TypeRender.Variable typedNameRender ->
//                     let anchorPath =
//                         match typedNameRender.Metadata.Path with
//                         | Path.Anchor anchorPath -> anchorPath
//                         | _ -> failwith "UNREACHABLE"
//                     anchorTypedNameRender ctx anchorPath typedNameRender
//                     |> TypeRender.Variable
//                 |> fun typeRender ->
//                     let ref =
//                         ModulePath.init ""
//                         |> AnchorPath.create
//                         |> TypeRefRender.anchor
//                         |> funApply ref
//                     Anchored.Render.Render(ref, typeRender)
//
// let inline addOrReplace ctx key value =
//     GeneratorContext.Anchored.addOrReplace ctx key value
//     
// let rec anchor (ctx: GeneratorContext) (resolvedExport: Concrete.RenderScope) =
//     let path = resolvedExport.Root
//     let anchorPath = AnchorPath.create path
//     let anchors = Dictionary<ResolvedType, TypePath * Render>()
//     let render = Render.Concrete.anchor ctx resolvedExport.Render.Value
//     {
//         RenderScope.Type = resolvedExport.Type
//         Root = path
//         TypeRef =
//             resolvedExport.TypeRef
//             |> TypeRefRender.anchor anchorPath
//         Render = render
//         Anchors = anchors
//     }
//     |> addOrReplace ctx resolvedExport.Type
//     resolvedExport.TransientChildren.Keys
//     |> Seq.iter (anchorResolvedType ctx anchors anchorPath)
// and anchorExport (ctx: GeneratorContext) (export: ResolvedExport) (resolvedExport: Concrete.RenderScope) =
//     let path = resolvedExport.Root
//     let anchorPath = AnchorPath.create path
//     let anchors = Dictionary<ResolvedType, TypePath * Render>()
//     let render = Render.Concrete.anchor ctx resolvedExport.Render.Value
//     {
//         RenderScope.Type = resolvedExport.Type
//         Root = path
//         TypeRef =
//             resolvedExport.TypeRef
//             |> TypeRefRender.anchor anchorPath
//         Render = render
//         Anchors = anchors
//     }
//     |> addOrReplace ctx export
//     resolvedExport.TransientChildren.Keys
//     |> Seq.iter (anchorResolvedType ctx anchors anchorPath)
// and anchorResolvedType ctx anchors anchorPath (resolvedType: ResolvedType) =
//     GeneratorContext.Prelude.tryGet ctx resolvedType
//     |> ValueOption.iter (function
//         | Choice1Of3 render -> anchorWidget ctx anchorPath render
//         | Choice2Of3 render -> anchorTransient ctx anchors anchorPath render
//         | Choice3Of3 render -> anchor ctx render
//         )
// and anchorWidget
//     (ctx: GeneratorContext)
//     (anchorPath: AnchorPath)
//     (widget: Widget.RenderScope) =
//     widget.TypeRef
//     |> TypeRefRender.anchor anchorPath
//     |> addOrReplace ctx widget.Type
// and anchorTransient
//     (ctx: GeneratorContext)
//     (anchors: Dictionary<ResolvedType, TypePath * Render>)
//     (anchorPath: AnchorPath)
//     (transientExport: Transient.RenderScope) =
//     let path =
//         transientExport.Root
//         |> TransientTypePath.anchor anchorPath
//     let render =
//         transientExport.Render.Value
//         |> Render.Transient.anchor ctx anchorPath
//     anchors
//     |> Dictionary.tryAdd transientExport.Type (path, render)