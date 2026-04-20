[<AutoOpen>]
module Xantham.Generator.Generator.RenderMember

open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham
open Xantham.Generator.Types
open Xantham.Generator.NamePath

module CallSignature =
    let renderWithMetadata
        (ctx: GeneratorContext)
        (scopeStore: RenderScopeStore)
        (callSignature: CallSignature)
        (metadata: RenderMetadata) =
        let parameters =
            callSignature.Parameters
            |> List.map (Parameter.render ctx scopeStore)
        let returnType = ctx.PreludeGetTypeRef ctx scopeStore callSignature.Type
        {
            Prelude.FunctionLikeSignature.Metadata = metadata
            Parameters = parameters
            ReturnType = returnType
            Traits = Set [ ]
            TypeParameters = []
            Documentation = callSignature.Documentation
        }
        
    let render (ctx: GeneratorContext) scopeStore (callSignature: CallSignature) =
        renderWithMetadata ctx scopeStore callSignature
            { Path = Path.create TransientMemberPath.Anchored
              Source = ValueNone
              FullyQualifiedName = ValueNone }
        
    let renderMemberWithMetadata (ctx: GeneratorContext) scopeStore (callSignatures: CallSignature list) metadata =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name =
                Name.create "Invoke"
                |> Case.addCamelMeasure
            Signatures = [
                for callSignature in callSignatures do
                    yield render ctx scopeStore callSignature
            ]
            Traits = Set [
                RenderTraits.JSCallSignature
            ]
            TypeParameters = []
            Documentation = []
        }
        |> MemberRender.Method
    
    let renderMember (ctx: GeneratorContext) scopeStore (callSignatures: CallSignature list) =
        renderMemberWithMetadata ctx scopeStore callSignatures {
            Path = Path.create TransientMemberPath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
        
module Method =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (method: Method) metadata =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name = method.Name
            Signatures = [{
                 Metadata = metadata
                 Parameters =
                     method.Parameters
                     |> List.map (Parameter.render ctx scopeStore)
                 ReturnType = ctx.PreludeGetTypeRef ctx scopeStore method.Type
                 Traits = Set []
                 TypeParameters = []
                 Documentation = []
             }]
            Traits = Set [
                if method.IsStatic then RenderTraits.Static
                if method.IsOptional then RenderTraits.Optional
            ]
            TypeParameters = []
            Documentation = method.Documentation
        }
        |> MemberRender.Method
    
    let render (ctx: GeneratorContext) scopeStore (method: Method) =
        renderWithMetadata ctx scopeStore method {
            Path = Path.create (TransientMemberPath.AnchoredAndMoored method.Name)
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
    
module GetAccessor =
    let private getTraits (getter: GetAccessor) = Set [
        if getter.IsStatic then RenderTraits.Static
        RenderTraits.Readable
        RenderTraits.JSGetter
    ]
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (getter: GetAccessor) metadata =
        match getter.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            let signatures =
                members
                |> List.collect (function
                    | Member.CallSignature callSignatures -> callSignatures
                    | _ -> [])
                |> List.map (CallSignature.render ctx scopeStore)
            {
                Metadata = metadata
                Prelude.FunctionLikeRender.Name = getter.Name
                Signatures = signatures
                Traits = getTraits getter
                TypeParameters = []
                Documentation = []
            }
            |> MemberRender.Method
        | _ ->
            {
                Metadata = metadata
                Prelude.TypedNameRender.Name = getter.Name
                Type = ctx.PreludeGetTypeRef ctx scopeStore getter.Type
                Traits = getTraits getter
                TypeParameters = []
                Documentation = []
            }
            |> MemberRender.Property
    let render (ctx: GeneratorContext) scopeStore (getter: GetAccessor) =
        renderWithMetadata ctx scopeStore getter {
            Path = Path.create (TransientMemberPath.AnchoredAndMoored getter.Name)
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
        
module SetAccessor =
    let private getTraits (setter: SetAccessor) = Set [
        if setter.IsStatic then RenderTraits.Static
        RenderTraits.Writable
        RenderTraits.JSSetter
    ]
    
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (setter: SetAccessor) metadata =
        {
            Metadata = metadata
            Prelude.TypedNameRender.Name = setter.Name
            Type = ctx.PreludeGetTypeRef ctx scopeStore setter.ArgumentType
            Traits = getTraits setter
            TypeParameters = []
            Documentation = setter.Documentation
        }
        |> MemberRender.Property
    
    let render (ctx: GeneratorContext) scopeStore (setter: SetAccessor) =
        renderWithMetadata ctx scopeStore setter {
            Path = Path.create (TransientMemberPath.AnchoredAndMoored setter.Name)
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }

module IndexSignature =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (indexSignature: IndexSignature) metadata =
        let traits = Set [
            RenderTraits.JSIndexer
            RenderTraits.Readable
            if not indexSignature.IsReadOnly then RenderTraits.Writable
        ]
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Item"
                |> Case.addCamelMeasure
            Signatures = [
                {
                    Metadata = metadata
                    Prelude.FunctionLikeSignature.Parameters =
                        indexSignature.Parameters
                        |> List.map (Parameter.render ctx scopeStore)
                    ReturnType = ctx.PreludeGetTypeRef ctx scopeStore indexSignature.Type
                    Traits = traits
                    TypeParameters = []
                    Documentation = []
                }
            ]
            Traits = traits
            TypeParameters = []
            Documentation = []
        }
        |> MemberRender.Method
    
    let render (ctx: GeneratorContext) scopeStore (indexSignature: IndexSignature) =
        renderWithMetadata ctx scopeStore indexSignature {
            Path = Path.create TransientMemberPath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
    
module ConstructSignature =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (constructSignature: ConstructSignature list) metadata =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name =
                // force pascal case for this member
                Name.create "Create"
                |> Case.addCamelMeasure
            Signatures = [
                for constructSignature in constructSignature do
                    {
                        Metadata = metadata
                        Prelude.FunctionLikeSignature.Parameters =
                            constructSignature.Parameters
                            |> List.map (Parameter.render ctx scopeStore)
                        ReturnType = ctx.PreludeGetTypeRef ctx scopeStore constructSignature.Type
                        Traits = Set [ RenderTraits.JSConstructor ]
                        TypeParameters = []
                        Documentation = []
                    }
            ]
            Traits = Set [ RenderTraits.JSConstructor ]
            TypeParameters = []
            Documentation = []
        }
        |> MemberRender.Method
    
    let render (ctx: GeneratorContext) scopeStore (constructSignature: ConstructSignature list) =
        renderWithMetadata ctx scopeStore constructSignature {
            Path = Path.create TransientMemberPath.Anchored
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }
        
module Property =
    let private getTraits (prop: Property) = Set [
        match prop.Accessor with
        | TsAccessor.ReadOnly -> RenderTraits.Readable
        | TsAccessor.WriteOnly -> RenderTraits.Writable
        | TsAccessor.ReadWrite ->
            RenderTraits.Readable
            RenderTraits.Writable
        if prop.IsOptional then RenderTraits.Optional
        if prop.IsStatic then RenderTraits.Static
    ]
    let renderMethodLikeWithMetadata (ctx: GeneratorContext) scopeStore (prop: Property) (callSignatures: CallSignature list) metadata =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name = prop.Name
            Signatures =
                callSignatures
                |> List.map (CallSignature.render ctx scopeStore)
            Traits = getTraits prop
            TypeParameters = []
            Documentation = prop.Documentation
        }
    let renderMethodLike (ctx: GeneratorContext) scopeStore (prop: Property) (callSignatures: CallSignature list) =
        renderMethodLikeWithMetadata ctx scopeStore prop callSignatures {
            Path = TransientMemberPath.AnchoredAndMoored prop.Name |> Path.create
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }

    let renderWithMetadata (ctx: GeneratorContext) scopeStore (prop: Property) metadata =
        match prop.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            members
            |> List.collect (function
                | Member.CallSignature callSignatures -> callSignatures
                | _ -> [])
            |> renderMethodLike ctx scopeStore prop
            |> MemberRender.Method
        | _ ->
            {
                Metadata = metadata
                Prelude.TypedNameRender.Name = prop.Name
                Type = ctx.PreludeGetTypeRef ctx scopeStore prop.Type
                Traits = getTraits prop
                TypeParameters = []
                Documentation = prop.Documentation
            }
            |> MemberRender.Property
    
    let render (ctx: GeneratorContext) scopeStore (prop: Property) =
        renderWithMetadata ctx scopeStore prop {
            Path = TransientMemberPath.AnchoredAndMoored prop.Name |> Path.create
            Source = ValueNone
            FullyQualifiedName = ValueNone
        }

module Member =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (member': Member) metadata =
        match member' with
        | Member.Method methods ->
            methods
            |> List.map (Method.renderWithMetadata ctx scopeStore >> funApply metadata)
        | Member.Property property ->
            [ Property.renderWithMetadata ctx scopeStore property metadata ]
        | Member.GetAccessor accessor ->
            [ GetAccessor.renderWithMetadata ctx scopeStore accessor metadata ]
        | Member.SetAccessor accessor ->
            [ SetAccessor.renderWithMetadata ctx scopeStore accessor metadata ]
        | Member.CallSignature callSignatures ->
            [ CallSignature.renderMemberWithMetadata ctx scopeStore callSignatures metadata ]
        | Member.IndexSignature indexSignature ->
            [ IndexSignature.renderWithMetadata ctx scopeStore indexSignature metadata ]
        | Member.ConstructSignature constructSignatures ->
            [ ConstructSignature.renderWithMetadata ctx scopeStore constructSignatures metadata ]
    let render (ctx: GeneratorContext) scopeStore (member': Member) =
        match member' with
        | Member.Method methods ->
            methods
            |> List.map (Method.render ctx scopeStore)
        | Member.Property property ->
            [ Property.render ctx scopeStore property ]
        | Member.GetAccessor accessor ->
            [ GetAccessor.render ctx scopeStore accessor ]
        | Member.SetAccessor accessor ->
            [ SetAccessor.render ctx scopeStore accessor ]
        | Member.CallSignature callSignatures ->
            [ CallSignature.renderMember ctx scopeStore callSignatures ]
        | Member.IndexSignature indexSignature ->
            [ IndexSignature.render ctx scopeStore indexSignature ]
        | Member.ConstructSignature constructSignatures ->
            [ ConstructSignature.render ctx scopeStore constructSignatures ]
        
    let partitionRender ctx scopeStore (members: Member list) =
        members
        |> Seq.collect (render ctx scopeStore)
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