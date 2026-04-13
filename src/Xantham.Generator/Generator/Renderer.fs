module rec Xantham.Generator.Generator.Renderer

open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.TypeRefRender

let private getRef = function
    | Render.Render (ref, _)
    | Render.TransientRender(ref, _)
    | Render.RefOnly ref -> ref

let private renderTypeRefImpl (ctx: GeneratorContext) (typeRef: ResolvedType) =
    match typeRef with
    | ResolvedType.GlobalThis ->
        GlobalThis.render
    | ResolvedType.Primitive typeKindPrimitive ->
        Primitive.render typeKindPrimitive
    | ResolvedType.Conditional conditionalType ->
        let trueRender =
            ctx.render conditionalType.True.Value
        let falseRender =
            ctx.render conditionalType.False.Value
        let refs = [ getRef trueRender; getRef falseRender ]
            
        match trueRender with
        | Render.RefOnly trueRender -> trueRender
    | ResolvedType.Interface ``interface`` -> failwith "todo"
    | ResolvedType.Class ``class`` -> failwith "todo"
    | ResolvedType.Union union -> failwith "todo"
    | ResolvedType.Intersection intersection -> failwith "todo"
    | ResolvedType.Literal tsLiteral -> failwith "todo"
    | ResolvedType.IndexedAccess indexAccessType -> failwith "todo"
    | ResolvedType.Index index -> failwith "todo"
    | ResolvedType.TypeReference typeReference -> failwith "todo"
    | ResolvedType.Array resolvedType -> failwith "todo"
    | ResolvedType.Enum enumType -> failwith "todo"
    | ResolvedType.EnumCase enumCase -> failwith "todo"
    | ResolvedType.TypeParameter typeParameter -> failwith "todo"
    | ResolvedType.ReadOnly resolvedType -> failwith "todo"
    | ResolvedType.Tuple tuple -> failwith "todo"
    | ResolvedType.Predicate predicate -> failwith "todo"
    | ResolvedType.TypeLiteral typeLiteral -> failwith "todo"
    | ResolvedType.TemplateLiteral templateLiteral -> failwith "todo"
    | ResolvedType.Optional typeReference -> failwith "todo"
    | ResolvedType.Substitution substitutionType -> failwith "todo"

let renderTypeRef (ctx: GeneratorContext) (typeRef: ResolvedType) =
    GeneratorContext.getTypeRender ctx typeRef
    |> ValueOption.defaultWith (fun () -> renderTypeRefImpl ctx typeRef)