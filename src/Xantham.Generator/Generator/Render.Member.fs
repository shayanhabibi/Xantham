[<AutoOpen>]
module Xantham.Generator.Generator.RenderMember

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender
open Xantham

module CallSignature =
    let render (ctx: GeneratorContext) (callSignature: CallSignature) =
         let parameters =
             callSignature.Parameters
             |> List.map (Parameter.render ctx)
         let returnType =
             callSignature.Type.Value
             |> TypeRefRender.prerender ctx
         
         {
             FunctionLikeSignature.Parameters = parameters |> List.toArray
             ReturnType = returnType
             Traits = TypedNameTraits.None
             TypeParameters = [||]
             Documentation = callSignature.Documentation
         }
    let renderMember (ctx: GeneratorContext) (callSignatures: CallSignature list) =
        {
            FunctionLikeRender.Name =
                Name.create "Invoke"
                |> Case.addCamelMeasure
            Signatures = [|
                for callSignature in callSignatures do
                    yield render ctx callSignature
            |]
            Traits = TypedNameTraits.JSCallSignature
            TypeParameters = [||]
            Documentation = []
        }
        |> MemberRender.Method

module Method =
    let render (ctx: GeneratorContext) (method: Method) =
        {
            FunctionLikeRender.Name = method.Name
            Signatures = [|{
                 Parameters =
                     method.Parameters
                     |> List.map (Parameter.render ctx)
                     |> List.toArray
                 ReturnType =
                     method.Type.Value
                     |> TypeRefRender.prerender ctx
                 Traits = TypedNameTraits.None
                 TypeParameters = [||]
                 Documentation = []
             }|]
            Traits = 
                 if method.IsStatic then
                     TypedNameTraits.Static
                 else TypedNameTraits.None
                 ||| if method.IsOptional then TypedNameTraits.Optional
                    else TypedNameTraits.None
            TypeParameters = [||]
            Documentation = method.Documentation
        }
        |> MemberRender.Method

module GetAccessor =
    let private getTraits (getter: GetAccessor) =
        if getter.IsStatic then TypedNameTraits.Static
        else TypedNameTraits.None
        ||| TypedNameTraits.JSGetter
        ||| TypedNameTraits.Readable
    let render (ctx: GeneratorContext) (getter: GetAccessor) =
        match getter.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            let signatures =
                members
                |> List.collect (function
                    | Member.CallSignature callSignatures -> callSignatures
                    | _ -> [])
                |> List.map (CallSignature.render ctx)
                |> Array.ofList
            {
                FunctionLikeRender.Name = getter.Name
                Signatures = signatures
                Traits = getTraits getter
                TypeParameters = [||]
                Documentation = []
            }
            |> MemberRender.Method
        | resolvedType ->
            let ref = TypeRefRender.prerender ctx resolvedType
            {
                TypedNameRender.Name = getter.Name
                Type = ref
                Traits = getTraits getter
                TypeParameters = [||]
                Documentation = []
            }
            |> MemberRender.Property

module SetAccessor =
    let private getTraits (setter: SetAccessor) =
        if setter.IsStatic then TypedNameTraits.Static
        else TypedNameTraits.None
        ||| TypedNameTraits.JSSetter
        ||| TypedNameTraits.Writable
    
    let render (ctx: GeneratorContext) (setter: SetAccessor) =
        let ref = setter.ArgumentType.Value |> TypeRefRender.prerender ctx
        {
            TypedNameRender.Name = setter.Name
            Type = ref
            Traits = getTraits setter
            TypeParameters = [||]
            Documentation = setter.Documentation
        }
        |> MemberRender.Property

module IndexSignature =
    let render (ctx: GeneratorContext) (indexSignature: IndexSignature) =
        {
            FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Item"
                |> Case.addCamelMeasure
            Signatures = [|
                {
                    FunctionLikeSignature.Parameters =
                        indexSignature.Parameters
                        |> List.map (Parameter.render ctx)
                        |> List.toArray
                    ReturnType = indexSignature.Type.Value |> TypeRefRender.prerender ctx
                    Traits = TypedNameTraits.JSIndexer
                    TypeParameters = [||]
                    Documentation = []
                }
            |]
            Traits =
                if indexSignature.IsReadOnly then TypedNameTraits.Readable
                else TypedNameTraits.None
                ||| TypedNameTraits.JSIndexer
            TypeParameters = [||]
            Documentation = []
        }
        |> MemberRender.Method

module ConstructSignature =
    let render (ctx: GeneratorContext) (constructSignature: ConstructSignature list) =
        {
            FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Create"
                |> Case.addCamelMeasure
            Signatures = [|
                for constructSignature in constructSignature do
                    {
                        FunctionLikeSignature.Parameters =
                            constructSignature.Parameters
                            |> List.map (Parameter.render ctx)
                            |> List.toArray
                        ReturnType =
                            constructSignature.Type.Value
                            |> TypeRefRender.prerender ctx
                        Traits = TypedNameTraits.JSConstructor
                        TypeParameters = [||]
                        Documentation = []
                    }
            |]
            Traits = TypedNameTraits.JSConstructor
            TypeParameters = [||]
            Documentation = []
        }
        |> MemberRender.Method
        
module Property =
    let private getTraits (prop: Property) =
        match prop.Accessor with
        | TsAccessor.ReadOnly -> TypedNameTraits.Readable
        | TsAccessor.WriteOnly -> TypedNameTraits.Writable
        | TsAccessor.ReadWrite ->
            TypedNameTraits.Readable
            ||| TypedNameTraits.Writable
        ||| if prop.IsOptional then TypedNameTraits.Optional
            else TypedNameTraits.None
        ||| if prop.IsStatic then TypedNameTraits.Static
            else TypedNameTraits.None
    let renderMethodLike (ctx: GeneratorContext) (prop: Property) (callSignatures: CallSignature list) =
        {
            FunctionLikeRender.Name = prop.Name
            Signatures =
                callSignatures
                |> List.map (CallSignature.render ctx)
                |> Array.ofList
            Traits = getTraits prop
            TypeParameters = [||]
            Documentation = prop.Documentation
        }

    let render (ctx: GeneratorContext) (prop: Property) =
        match prop.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            members
            |> List.collect (function
                | Member.CallSignature callSignatures -> callSignatures
                | _ -> [])
            |> renderMethodLike ctx prop
            |> MemberRender.Method
        | resolvedType ->
            let ref = TypeRefRender.prerender ctx resolvedType
            {
                TypedNameRender.Name = prop.Name
                Type = ref
                Traits = getTraits prop
                TypeParameters = [||]
                Documentation = prop.Documentation
            }
            |> MemberRender.Property

module Member =
    let render (ctx: GeneratorContext) (member': Member) =
        match member' with
        | Member.Method methods ->
            methods
            |> List.map (Method.render ctx)
        | Member.Property property ->
            [ Property.render ctx property ]
        | Member.GetAccessor accessor ->
            [ GetAccessor.render ctx accessor ]
        | Member.SetAccessor accessor ->
            [ SetAccessor.render ctx accessor ]
        | Member.CallSignature callSignatures ->
            [ CallSignature.renderMember ctx callSignatures ]
        | Member.IndexSignature indexSignature ->
            [ IndexSignature.render ctx indexSignature ]
        | Member.ConstructSignature constructSignatures ->
            [ ConstructSignature.render ctx constructSignatures ]