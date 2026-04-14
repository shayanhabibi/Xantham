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

module Prerender =
        | ResolvedType.Intersection intersection ->
            intersection.Types
            |> List.iter (prerenderResolvedType ctx)
        | ResolvedType.IndexedAccess indexAccessType ->
            [
                indexAccessType.Index
                indexAccessType.Object
            ]
            |> List.iter (prerenderResolvedType ctx)
        | ResolvedType.Index index -> index.Type |> prerenderResolvedType ctx
        | ResolvedType.Optional typeReference 
        | ResolvedType.TypeReference typeReference ->
            typeReference.ResolvedType
            |> Option.iter (prerenderResolvedType ctx)
            typeReference.TypeArguments
            |> List.iter (prerenderResolvedType ctx)
            typeReference.Type
            |> prerenderResolvedType ctx
        | ResolvedType.ReadOnly resolvedType 
        | ResolvedType.Array resolvedType -> prerenderResolvedType ctx (lazy resolvedType)
        | ResolvedType.TypeParameter typeParameter -> prerenderTypeParameter ctx (lazy typeParameter)
        | ResolvedType.Tuple tuple ->
            tuple.Types
            |> List.iter (_.Type >> prerenderResolvedType ctx)
        | ResolvedType.Predicate predicate ->
            predicate.Type |> prerenderResolvedType ctx
        | ResolvedType.TypeLiteral typeLiteral ->
            typeLiteral.Members |> List.iter (prerenderMember ctx)
        | ResolvedType.TemplateLiteral templateLiteral ->
            templateLiteral.Types
            |> List.iter (prerenderResolvedType ctx)
        | ResolvedType.Substitution substitutionType ->
            [
                substitutionType.Base
                substitutionType.Constraint
            ]
            |> List.iter (prerenderResolvedType ctx)
    and private prerenderConstructor ctx (constructor: Constructor) =
        constructor.Parameters
        |> List.iter (_.Type >> prerenderResolvedType ctx)
    and private prerenderMember ctx = function
        | Member.CallSignature callSignatures ->
            callSignatures
            |> List.collect _.Parameters
            |> List.iter (_.Type >> prerenderResolvedType ctx)
            callSignatures
            |> List.map _.Type
            |> List.iter (prerenderResolvedType ctx)
        | Member.Method methods ->
            methods
            |> List.collect _.Parameters
            |> List.iter (_.Type >> prerenderResolvedType ctx)
            methods
            |> List.map _.Type
            |> List.iter (prerenderResolvedType ctx)
        | Member.Property property -> property.Type |> prerenderResolvedType ctx
        | Member.GetAccessor accessor -> accessor.Type |> prerenderResolvedType ctx
        | Member.SetAccessor accessor -> accessor.ArgumentType |> prerenderResolvedType ctx
        | Member.IndexSignature indexSignature ->
            indexSignature.Type |> prerenderResolvedType ctx
            indexSignature.Parameters
            |> List.iter (_.Type >> prerenderResolvedType ctx)
        | Member.ConstructSignature constructSignatures ->
            constructSignatures
            |> List.collect _.Parameters
            |> List.iter (_.Type >> prerenderResolvedType ctx)
            constructSignatures
            |> List.iter (_.Type >> prerenderResolvedType ctx)

    and private prerenderTypeParameter ctx (typParam: Lazy<TypeParameter>) =
        if typParam.IsValueCreated then () else
        let typParam = typParam.Value
        typParam.Constraint
        |> Option.iter (prerenderResolvedType ctx)
        typParam.Default
        |> Option.iter (prerenderResolvedType ctx)
    and private prerenderExport ctx export =
        match export with
        | ResolvedExport.Interface export -> ResolvedType.Interface export |> Lazy.CreateFromValue |> prerenderResolvedType ctx
        | ResolvedExport.Variable variable ->
            variable.Type |> prerenderResolvedType ctx
        | ResolvedExport.TypeAlias typeAlias ->
            typeAlias.Type |> prerenderResolvedType ctx
        | ResolvedExport.Class ``class`` -> Lazy.CreateFromValue (ResolvedType.Class ``class``) |> prerenderResolvedType ctx
        | ResolvedExport.Enum enumType -> ResolvedType.Enum enumType |> Lazy.CreateFromValue |> prerenderResolvedType ctx
        | ResolvedExport.Function functions ->
            functions
            |> List.collect _.TypeParameters
            |> List.iter (prerenderTypeParameter ctx)
            functions
            |> List.collect _.Parameters
            |> List.iter (_.Type >> prerenderResolvedType ctx)
            functions
            |> List.iter (_.Type >> prerenderResolvedType ctx)
            functions
            |> List.iter (_.SignatureKey.Value >> ResolvedType.TypeLiteral >> Lazy.CreateFromValue >> prerenderResolvedType ctx)
        | ResolvedExport.Module ``module`` ->
            ``module``.Exports
            |> List.iter (prerenderExport ctx)

    let prerenderTypeRefs (ctx: GeneratorContext) (decodedResults: ResolvedExport list) =
        decodedResults
        |> prepopulateTypeRefRendersForAliases ctx
        decodedResults |> List.iter (prerenderExport ctx)