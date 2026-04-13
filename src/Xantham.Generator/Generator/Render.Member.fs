[<AutoOpen>]
module Xantham.Generator.Generator.RenderMember

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.TypeRefRender
open Xantham

module CallSignature =
    let renderWithMetadata (ctx: GeneratorContext) (callSignature: CallSignature) (metadata: RenderMetadata) =
         let parameters =
             callSignature.Parameters
             |> List.map (Parameter.render ctx)
         let returnType = ctx.render callSignature.Type.Value
             
         
         {
             Metadata = metadata
             FunctionLikeSignature.Parameters = parameters |> List.toArray
             ReturnType = returnType
             Traits = TypedNameTraits.None
             TypeParameters = [||]
             Documentation = callSignature.Documentation
         }
    let render (ctx: GeneratorContext) (callSignature: CallSignature) =
        renderWithMetadata ctx callSignature RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (callSignature: CallSignature) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx callSignature
        
    let renderMemberWithMetadata (ctx: GeneratorContext) (callSignatures: CallSignature list) metadata =
        {
            Metadata = metadata
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
    
    let renderMember (ctx: GeneratorContext) (callSignatures: CallSignature list) =
        renderMemberWithMetadata ctx callSignatures RenderMetadata.empty
        
    let inline renderMemberWithPath (ctx: GeneratorContext) (callSignatures: CallSignature list) (path: ^T) =
        RenderMetadata.create path
        |> renderMemberWithMetadata ctx callSignatures

module Method =
    let renderWithMetadata (ctx: GeneratorContext) (method: Method) metadata =
        {
            Metadata = metadata
            FunctionLikeRender.Name = method.Name
            Signatures = [|{
                 Metadata = RenderMetadata.empty
                 Parameters =
                     method.Parameters
                     |> List.map (Parameter.render ctx)
                     |> List.toArray
                 ReturnType = ctx.render method.Type.Value
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
    
    let render (ctx: GeneratorContext) (method: Method) =
        renderWithMetadata ctx method RenderMetadata.empty
    
    let inline renderWithPath (ctx: GeneratorContext) (method: Method) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx method

module GetAccessor =
    let private getTraits (getter: GetAccessor) =
        if getter.IsStatic then TypedNameTraits.Static
        else TypedNameTraits.None
        ||| TypedNameTraits.JSGetter
        ||| TypedNameTraits.Readable
    let renderWithMetadata (ctx: GeneratorContext) (getter: GetAccessor) metadata =
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
                Metadata = metadata
                FunctionLikeRender.Name = getter.Name
                Signatures = signatures
                Traits = getTraits getter
                TypeParameters = [||]
                Documentation = []
            }
            |> MemberRender.Method
        | resolvedType ->
            {
                Metadata = metadata
                TypedNameRender.Name = getter.Name
                Type = ctx.render resolvedType
                Traits = getTraits getter
                TypeParameters = [||]
                Documentation = []
            }
            |> MemberRender.Property
    let render (ctx: GeneratorContext) (getter: GetAccessor) =
        renderWithMetadata ctx getter RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (getter: GetAccessor) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx getter
module SetAccessor =
    let private getTraits (setter: SetAccessor) =
        if setter.IsStatic then TypedNameTraits.Static
        else TypedNameTraits.None
        ||| TypedNameTraits.JSSetter
        ||| TypedNameTraits.Writable
    
    let renderWithMetadata (ctx: GeneratorContext) (setter: SetAccessor) metadata =
        {
            Metadata = metadata
            TypedNameRender.Name = setter.Name
            Type = ctx.render setter.ArgumentType.Value 
            Traits = getTraits setter
            TypeParameters = [||]
            Documentation = setter.Documentation
        }
        |> MemberRender.Property
    
    let render (ctx: GeneratorContext) (setter: SetAccessor) =
        renderWithMetadata ctx setter RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (setter: SetAccessor) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx setter

module IndexSignature =
    let renderWithMetadata (ctx: GeneratorContext) (indexSignature: IndexSignature) metadata =
        {
            Metadata = metadata
            FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Item"
                |> Case.addCamelMeasure
            Signatures = [|
                {
                    Metadata = metadata
                    FunctionLikeSignature.Parameters =
                        indexSignature.Parameters
                        |> List.map (Parameter.render ctx)
                        |> List.toArray
                    ReturnType = ctx.render indexSignature.Type.Value 
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
    
    let render (ctx: GeneratorContext) (indexSignature: IndexSignature) =
        renderWithMetadata ctx indexSignature RenderMetadata.empty
    
    let inline renderWithPath (ctx: GeneratorContext) (indexSignature: IndexSignature) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx indexSignature

module ConstructSignature =
    let renderWithMetadata (ctx: GeneratorContext) (constructSignature: ConstructSignature list) metadata =
        {
            Metadata = metadata
            FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Create"
                |> Case.addCamelMeasure
            Signatures = [|
                for constructSignature in constructSignature do
                    {
                        Metadata = RenderMetadata.empty
                        FunctionLikeSignature.Parameters =
                            constructSignature.Parameters
                            |> List.map (Parameter.render ctx)
                            |> List.toArray
                        ReturnType = ctx.render constructSignature.Type.Value
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
    
    let render (ctx: GeneratorContext) (constructSignature: ConstructSignature list) =
        renderWithMetadata ctx constructSignature RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (constructSignature: ConstructSignature list) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx constructSignature
        
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
    let renderMethodLikeWithMetadata (ctx: GeneratorContext) (prop: Property) (callSignatures: CallSignature list) metadata =
        {
            Metadata = metadata
            FunctionLikeRender.Name = prop.Name
            Signatures =
                callSignatures
                |> List.map (CallSignature.render ctx)
                |> Array.ofList
            Traits = getTraits prop
            TypeParameters = [||]
            Documentation = prop.Documentation
        }
    let renderMethodLike (ctx: GeneratorContext) (prop: Property) (callSignatures: CallSignature list) =
        renderMethodLikeWithMetadata ctx prop callSignatures RenderMetadata.empty
    let inline renderMethodLikeWithPath (ctx: GeneratorContext) (prop: Property) (callSignatures: CallSignature list) (path: ^T) =
        RenderMetadata.create path
        |> renderMethodLikeWithMetadata ctx prop callSignatures

    let renderWithMetadata (ctx: GeneratorContext) (prop: Property) metadata =
        match prop.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            members
            |> List.collect (function
                | Member.CallSignature callSignatures -> callSignatures
                | _ -> [])
            |> renderMethodLike ctx prop
            |> MemberRender.Method
        | resolvedType ->
            {
                Metadata = metadata
                TypedNameRender.Name = prop.Name
                Type = ctx.render resolvedType
                Traits = getTraits prop
                TypeParameters = [||]
                Documentation = prop.Documentation
            }
            |> MemberRender.Property
    
    let render (ctx: GeneratorContext) (prop: Property) =
        renderWithMetadata ctx prop RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (prop: Property) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx prop

module Member =
    let renderWithMetadata (ctx: GeneratorContext) (member': Member) metadata =
        match member' with
        | Member.Method methods ->
            methods
            |> List.map (Method.renderWithMetadata ctx >> funApply metadata)
        | Member.Property property ->
            [ Property.renderWithMetadata ctx property metadata ]
        | Member.GetAccessor accessor ->
            [ GetAccessor.renderWithMetadata ctx accessor metadata ]
        | Member.SetAccessor accessor ->
            [ SetAccessor.renderWithMetadata ctx accessor metadata ]
        | Member.CallSignature callSignatures ->
            [ CallSignature.renderMemberWithMetadata ctx callSignatures metadata ]
        | Member.IndexSignature indexSignature ->
            [ IndexSignature.renderWithMetadata ctx indexSignature metadata ]
        | Member.ConstructSignature constructSignatures ->
            [ ConstructSignature.renderWithMetadata ctx constructSignatures metadata ]
    let render (ctx: GeneratorContext) (member': Member) =
        renderWithMetadata ctx member' RenderMetadata.empty
    let inline renderWithPath (ctx: GeneratorContext) (member': Member) (path: ^T) =
        RenderMetadata.create path
        |> renderWithMetadata ctx member'
    let partitionRender ctx (members: Member list) =
        members
        |> Seq.collect (render ctx)
        |> Seq.fold (fun (members, functions) m ->
            match m with
            | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
            | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
            ) ([], [])
    let setReadOnly = function
        | Member.Property ({ Accessor = TsAccessor.ReadWrite | TsAccessor.WriteOnly } as prop) ->
            { prop with Accessor = TsAccessor.ReadOnly }
            |> Member.Property
        | Member.IndexSignature ({ IsReadOnly = false } as indexSignature) ->
            { indexSignature with IsReadOnly = true }
            |> Member.IndexSignature
        | m -> m
    let setOptional = function
        | Member.Property ({ IsOptional = false } as prop) ->
            { prop with IsOptional = true }
            |> Member.Property
        | Member.Method methods ->
            methods
            |> List.map (function
                | { IsOptional = false } as method -> { method with IsOptional = true }
                | m -> m
                )
            |> Member.Method
        | m -> m

    let rec collectAllRecursively (typ: ResolvedType) =
        let readOnly = collectAllRecursively >> List.map setReadOnly
        let optional = collectAllRecursively >> List.map setOptional
        match typ with
        | ResolvedType.Substitution { Base = Resolve resolved }
        | ResolvedType.TypeParameter { Default = Some (Resolve resolved) }
        | ResolvedType.TypeParameter { Constraint = Some (Resolve resolved) }
        | ResolvedType.TypeReference { ResolvedType = Some (Resolve resolved) }
        | ResolvedType.TypeReference { Type = Resolve resolved } ->
            collectAllRecursively resolved
        | ResolvedType.TypeLiteral { Members = members } -> members
        | ResolvedType.Intersection { Types = types } ->
            types
            |> List.collect (_.Value >> collectAllRecursively)
        | ResolvedType.Interface iface ->
            iface.Heritage.Extends
            |> List.collect (ResolvedType.TypeReference >> collectAllRecursively)
            |> List.append iface.Members
        | ResolvedType.Class classType ->
            classType.Heritage.Implements
            |> Option.toList
            |> List.append classType.Heritage.Extends
            |> List.collect (ResolvedType.TypeReference >> collectAllRecursively)
            |> List.append classType.Members
        | ResolvedType.Conditional conditionalValues ->
            [
                conditionalValues.True
                conditionalValues.False
            ]
            |> List.collect (_.Value >> collectAllRecursively)
            |> List.distinct
        | ResolvedType.Optional optionalType ->
            optional (ResolvedType.TypeReference optionalType)
        | ResolvedType.ReadOnly resolvedType ->
            readOnly resolvedType
        | ResolvedType.Union _ 
        | ResolvedType.GlobalThis 
        | ResolvedType.Primitive _
        | ResolvedType.Literal _
        | ResolvedType.IndexedAccess _
        | ResolvedType.Index _
        | ResolvedType.Array _
        | ResolvedType.Enum _
        | ResolvedType.EnumCase _
        | ResolvedType.Tuple _
        | ResolvedType.TypeParameter _
        | ResolvedType.Predicate _
        | ResolvedType.TemplateLiteral _ -> []