[<AutoOpen>]
module Xantham.Generator.Generator.TypeRefRenderPrerender
open Fabulous.AST
open Xantham
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator.Path
open Xantham.Generator.Generator.ResolvedTypeCategorization
open Xantham.Generator.NamePath
open Xantham.Generator.Generator
open Xantham.Decoder
open Xantham.Generator.Types

// module private Render =
//     let rec typeRender (ctx: GeneratorContext) (resolved: ResolvedType) =
//         let inline create value = TypeRefRender.create false value
//         let inline createOptional value = TypeRefRender.create true value
//         match resolved with
//         | ResolvedType.GlobalThis ->
//             ModulePath.init "Browser"
//             |> ModulePath.create "Dom"
//             |> TypePath.create "Window"
//             |> create
//         | ResolvedType.Conditional conditionalType ->
//             if [
//                 conditionalType.True.Value
//                 conditionalType.False.Value
//             ] |> List.contains resolved
//             then
//                 Types.obj
//                 |> createOptional
//             else
//             [
//                 refTypeRender ctx conditionalType.True.Value
//                 refTypeRender ctx conditionalType.False.Value
//             ]
//             |> create
//         | ResolvedType.Interface ``interface`` ->
//             Path.fromInterface ``interface``
//             |> create
//         | ResolvedType.Class ``class`` ->
//             Path.fromClass ``class``
//             |> create
//         | ResolvedType.Primitive typeKindPrimitive ->
//             match typeKindPrimitive with
//             | TypeKindPrimitive.Unknown 
//             | TypeKindPrimitive.Any ->
//                 Types.obj
//                 |> createOptional
//             | TypeKindPrimitive.ESSymbol 
//             | TypeKindPrimitive.NonPrimitive ->
//                 Types.obj
//                 |> create
//             | TypeKindPrimitive.Never 
//             | TypeKindPrimitive.Void 
//             | TypeKindPrimitive.Undefined 
//             | TypeKindPrimitive.Null -> create Types.unit 
//             | TypeKindPrimitive.String -> create Types.string 
//             | TypeKindPrimitive.Integer -> create Types.int 
//             | TypeKindPrimitive.Number -> create Types.float 
//             | TypeKindPrimitive.Boolean -> create Types.bool 
//             | TypeKindPrimitive.BigInt -> create Types.bigint 
//         | ResolvedType.Union _ ->
//             match ResolvedTypeCategories.create resolved with
//             | { Others = []; LiteralLike = []; EnumLike = []; Primitives = [] } ->
//                 Types.obj
//                 |> createOptional
//             | { Others = []; EnumLike = []; Primitives = [ ResolvedTypeCategories.AsResolvedType i ]; LiteralLike = []; Nullable = true } 
//             | { Others = []; EnumLike = [ ResolvedTypeCategories.AsResolvedType i ]; Primitives = []; LiteralLike = []; Nullable = true } 
//             | { Others = [ ResolvedTypeCategories.AsResolvedType i ]; EnumLike = []; Primitives = []; LiteralLike = []; Nullable = true } ->
//                 refTypeRender ctx i
//                 |> TypeRefRender.nullable
//             | { Others = []; EnumLike = []; Primitives = []; LiteralLike = _; Nullable = isNullable } ->
//                 TransientTypePath.Anchored
//                 |> TypeRefRender.create isNullable
//             | { Others = others; EnumLike = enums; Primitives = primitives; LiteralLike = []; Nullable = isNullable } ->
//                 seq {
//                     for primitive in primitives do primitive.AsResolvedType
//                     for other in others do other.AsResolvedType
//                     for enum in enums do enum.AsResolvedType
//                 }
//                 |> Seq.map (refTypeRender ctx >> if isNullable then TypeRefRender.nonNullable else id)
//                 |> Seq.toList
//                 |> TypeRefRender.create isNullable
//             | { Others = others; EnumLike = enums; Primitives = primitives; LiteralLike = literals; Nullable = isNullable } ->
//                 let refs =
//                     seq {
//                         for primitive in primitives do primitive.AsResolvedType
//                         for other in others do other.AsResolvedType
//                         for enum in enums do enum.AsResolvedType
//                         for literal in literals do literal.AsResolvedType
//                     }
//                     |> Seq.map (refTypeRender ctx >> if isNullable then TypeRefRender.nonNullable else id)
//                     |> Seq.toList
//                 create TransientTypePath.Anchored :: refs
//                 |> TypeRefRender.create isNullable
//         | ResolvedType.Literal _
//         | ResolvedType.IndexedAccess _
//         | ResolvedType.Intersection _ ->
//             create TransientTypePath.Anchored
//         | ResolvedType.Index index ->
//             (TypeRefRender.create false Types.keyofType, [ refTypeRender ctx index.Type.Value ])
//             |> TypeRefRender.create false
//         | ResolvedType.TypeReference { ResolvedType = Some (Resolve resolvedType) }
//         | ResolvedType.TypeReference { Type = Resolve resolvedType; TypeArguments = [] } ->
//             refTypeRender ctx resolvedType
//         | ResolvedType.TypeReference typeReference ->
//             let prefix = refTypeRender ctx typeReference.Type.Value
//             let args = typeReference.TypeArguments |> List.map (_.Value >> refTypeRender ctx)
//             create (prefix, args)
//         | ResolvedType.Array resolvedType ->
//             create (create Types.arrayType, [ refTypeRender ctx resolvedType ])
//         | ResolvedType.Enum enumType -> Path.fromEnum enumType |> create
//         | ResolvedType.EnumCase _ -> create TransientTypePath.Anchored
//         | ResolvedType.TypeParameter typeParameter ->
//             typeParameter.Name
//             |> Name.Case.valueOrModified
//             |> Ast.LongIdent
//             |> create
//         | ResolvedType.ReadOnly resolvedType -> refTypeRender ctx resolvedType
//         | ResolvedType.Tuple tuple ->
//             tuple.Types
//             |> List.map (fun tupleElement ->
//                 tupleElement.Type.Value
//                 |> refTypeRender ctx
//                 |> TypeRefRender.orNullable tupleElement.IsOptional
//                 )
//             |> List.toArray
//             |> create
//         | ResolvedType.Predicate _ ->
//             create Types.bool
//         | ResolvedType.TypeLiteral typeLiteral ->
//             // is func?
//             let callSignature, rest =
//                 typeLiteral.Members
//                 |> List.partition _.IsCallSignature
//                 ||> fun sigs rest ->
//                     sigs
//                     |> List.map (function
//                         Member.CallSignature callSignature -> callSignature
//                         | _ -> failwith "Expected call signature"),
//                     rest
//             let shouldInlineCallSignature (callSignature: CallSignature) =
//                 List.length callSignature.Parameters < 3
//                 &&
//                 callSignature.Parameters
//                 |> List.exists _.IsSpread
//                 |> not
//             match callSignature, rest with
//             | [], [] -> createOptional Types.obj
//             // single sig; no overloads
//             | [ [ singleSig ] ], [] when shouldInlineCallSignature singleSig ->
//                 let parameters =
//                     singleSig.Parameters
//                     |> List.map (fun parameter ->
//                         refTypeRender ctx parameter.Type.Value
//                         |> TypeRefRender.orNullable parameter.IsOptional
//                         )
//                 let returnValue = refTypeRender ctx singleSig.Type.Value
//                 create (parameters, returnValue)
//             | _, _ ->
//                 create TransientTypePath.Anchored
//         | ResolvedType.TemplateLiteral _ -> create Types.string
//         | ResolvedType.Optional { ResolvedType = Some (Resolve resolvedType) }
//         | ResolvedType.Optional { Type = Resolve resolvedType; TypeArguments = [] } ->
//             { refTypeRender ctx resolvedType with Nullable = true }
//         | ResolvedType.Optional { Type = Resolve resolvedType; TypeArguments = typeArguments } ->
//             let args = typeArguments |> List.map (_.Value >> refTypeRender ctx)
//             createOptional (refTypeRender ctx resolvedType, args)
//         | ResolvedType.Substitution _ -> create Types.obj
//
//     and refTypeRender (ctx: GeneratorContext) (resolved: ResolvedType) =
//         GeneratorContext.getTypeRefWith ctx resolved (typeRender ctx)
//
// module TestHelper =
//     let prerender (ctx: GeneratorContext) (resolved: ResolvedType) =
//         Render.refTypeRender ctx resolved
//
// module Prerender =
//     let rec private prerenderResolvedType ctx (resolvedType: LazyResolvedType) =
//         if resolvedType.IsValueCreated then () else
//         let resolvedType = resolvedType.Value
//         Render.refTypeRender ctx resolvedType
//         |> ignore
//         match resolvedType with
//         | ResolvedType.GlobalThis 
//         | ResolvedType.Primitive _
//         | ResolvedType.Literal _
//         | ResolvedType.Enum _
//         | ResolvedType.EnumCase _ -> ()
//         | ResolvedType.Conditional conditionalType ->
//             [
//                 conditionalType.Check
//                 conditionalType.Extends
//                 conditionalType.True
//                 conditionalType.False
//             ]
//             |> List.iter (prerenderResolvedType ctx)
//         | ResolvedType.Class classType ->
//             classType.Heritage.Extends
//             |> List.iter (ResolvedType.TypeReference >> Lazy.CreateFromValue >> prerenderResolvedType ctx)
//             classType.Heritage.Implements
//             |> Option.iter (ResolvedType.TypeReference >> Lazy.CreateFromValue >> prerenderResolvedType ctx)
//             classType.Constructors
//             |> List.iter (prerenderConstructor ctx)
//             classType.Members
//             |> List.iter (prerenderMember ctx)
//         | ResolvedType.Interface iface ->
//             iface.Heritage.Extends
//             |> List.iter (ResolvedType.TypeReference >> Lazy.CreateFromValue >> prerenderResolvedType ctx)
//             iface.TypeParameters
//             |> List.iter (prerenderTypeParameter ctx)
//             iface.Members
//             |> List.iter (prerenderMember ctx)
//         | ResolvedType.Union union ->
//             union.Types
//             |> List.iter (prerenderResolvedType ctx)
//         | ResolvedType.Intersection intersection ->
//             intersection.Types
//             |> List.iter (prerenderResolvedType ctx)
//         | ResolvedType.IndexedAccess indexAccessType ->
//             [
//                 indexAccessType.Index
//                 indexAccessType.Object
//             ]
//             |> List.iter (prerenderResolvedType ctx)
//         | ResolvedType.Index index -> index.Type |> prerenderResolvedType ctx
//         | ResolvedType.Optional typeReference 
//         | ResolvedType.TypeReference typeReference ->
//             typeReference.ResolvedType
//             |> Option.iter (prerenderResolvedType ctx)
//             typeReference.TypeArguments
//             |> List.iter (prerenderResolvedType ctx)
//             typeReference.Type
//             |> prerenderResolvedType ctx
//         | ResolvedType.ReadOnly resolvedType 
//         | ResolvedType.Array resolvedType -> prerenderResolvedType ctx (lazy resolvedType)
//         | ResolvedType.TypeParameter typeParameter -> prerenderTypeParameter ctx (lazy typeParameter)
//         | ResolvedType.Tuple tuple ->
//             tuple.Types
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//         | ResolvedType.Predicate predicate ->
//             predicate.Type |> prerenderResolvedType ctx
//         | ResolvedType.TypeLiteral typeLiteral ->
//             typeLiteral.Members |> List.iter (prerenderMember ctx)
//         | ResolvedType.TemplateLiteral templateLiteral ->
//             templateLiteral.Types
//             |> List.iter (prerenderResolvedType ctx)
//         | ResolvedType.Substitution substitutionType ->
//             [
//                 substitutionType.Base
//                 substitutionType.Constraint
//             ]
//             |> List.iter (prerenderResolvedType ctx)
//     and private prerenderConstructor ctx (constructor: Constructor) =
//         constructor.Parameters
//         |> List.iter (_.Type >> prerenderResolvedType ctx)
//     and private prerenderMember ctx = function
//         | Member.CallSignature callSignatures ->
//             callSignatures
//             |> List.collect _.Parameters
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//             callSignatures
//             |> List.map _.Type
//             |> List.iter (prerenderResolvedType ctx)
//         | Member.Method methods ->
//             methods
//             |> List.collect _.Parameters
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//             methods
//             |> List.map _.Type
//             |> List.iter (prerenderResolvedType ctx)
//         | Member.Property property -> property.Type |> prerenderResolvedType ctx
//         | Member.GetAccessor accessor -> accessor.Type |> prerenderResolvedType ctx
//         | Member.SetAccessor accessor -> accessor.ArgumentType |> prerenderResolvedType ctx
//         | Member.IndexSignature indexSignature ->
//             indexSignature.Type |> prerenderResolvedType ctx
//             indexSignature.Parameters
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//         | Member.ConstructSignature constructSignatures ->
//             constructSignatures
//             |> List.collect _.Parameters
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//             constructSignatures
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//
//     and private prerenderTypeParameter ctx (typParam: Lazy<TypeParameter>) =
//         if typParam.IsValueCreated then () else
//         let typParam = typParam.Value
//         typParam.Constraint
//         |> Option.iter (prerenderResolvedType ctx)
//         typParam.Default
//         |> Option.iter (prerenderResolvedType ctx)
//     and private prerenderExport ctx export =
//         match export with
//         | ResolvedExport.Interface export -> ResolvedType.Interface export |> Lazy.CreateFromValue |> prerenderResolvedType ctx
//         | ResolvedExport.Variable variable ->
//             variable.Type |> prerenderResolvedType ctx
//         | ResolvedExport.TypeAlias typeAlias ->
//             typeAlias.Type |> prerenderResolvedType ctx
//         | ResolvedExport.Class ``class`` -> Lazy.CreateFromValue (ResolvedType.Class ``class``) |> prerenderResolvedType ctx
//         | ResolvedExport.Enum enumType -> ResolvedType.Enum enumType |> Lazy.CreateFromValue |> prerenderResolvedType ctx
//         | ResolvedExport.Function functions ->
//             functions
//             |> List.collect _.TypeParameters
//             |> List.iter (prerenderTypeParameter ctx)
//             functions
//             |> List.collect _.Parameters
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//             functions
//             |> List.iter (_.Type >> prerenderResolvedType ctx)
//             functions
//             |> List.iter (_.SignatureKey.Value >> ResolvedType.TypeLiteral >> Lazy.CreateFromValue >> prerenderResolvedType ctx)
//         | ResolvedExport.Module ``module`` ->
//             ``module``.Exports
//             |> List.iter (prerenderExport ctx)
//
//     let prerenderTypeRefs (ctx: GeneratorContext) (decodedResults: ResolvedExport list) =
//         decodedResults
//         |> prepopulateTypeRefRendersForAliases ctx
//         decodedResults |> List.iter (prerenderExport ctx)