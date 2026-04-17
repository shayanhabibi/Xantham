[<AutoOpen>]
module Xantham.Generator.Generator.RenderMember

open SignalsDotnet
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Xantham.Generator

module CallSignature =
    let renderWithMetadata
        (ctx: GeneratorContext)
        (scopeStore: RenderScopeStore)
        (metadata: RenderMetadata)
        (callSignature: CallSignature): FunctionLikeSignature =
        let parameters =
            callSignature.Parameters
            |> List.map (Parameter.render ctx scopeStore)
        let returnType =
            callSignature.Type.Value
            |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
        {
            Prelude.FunctionLikeSignature.Metadata = metadata
            Parameters = parameters
            ReturnType = returnType
            Traits = Set [ ]
            TypeParameters = []
            Documentation = callSignature.Documentation
        }
    let render
        (ctx: GeneratorContext)
        (scopeStore: RenderScopeStore)
        (callSignature: CallSignature): FunctionLikeSignature =
        TransientMemberPath.Anchored
        |> RenderMetadata.create
        |> renderWithMetadata ctx scopeStore
        |> funApply callSignature
    
    let renderMemberWithMetadata
        (ctx: GeneratorContext)
        (scopeStore: RenderScopeStore)
        metadata
        (callSignatures: CallSignature list): MemberRender =
        let name =
            match metadata with
            | RenderMetadata.Transient path ->
                TransientPath.toAnchored path
                |> List.tryLast
                |> Option.map Name.Camel.fromCase
                |> Option.defaultValue (Case.addCamelMeasure (Name.create "Invoke"))
            | RenderMetadata.Concrete concrete ->
                AnchorPath.traceToParentModule concrete
                |> snd |> List.tryLast
                |> Option.map Name.Camel.fromCase
                |> Option.defaultValue (Case.addCamelMeasure (Name.create "Invoke"))
        {
            Metadata = metadata
            Name = name
            Signatures = [
                for callSignature in callSignatures do
                    renderWithMetadata ctx scopeStore metadata callSignature
            ]
            Traits = Set [ RenderTraits.JSCallSignature ]
            TypeParameters = []
            Documentation = []
        }
        |> MemberRender.Method
    
    let renderMember (ctx: GeneratorContext) scopeStore (callSignatures: CallSignature list): MemberRender =
        TransientMemberPath.AnchoredAndMoored (Name.create "Invoke" |> Case.addCamelMeasure)
        |> RenderMetadata.create
        |> renderMemberWithMetadata ctx scopeStore
        |> funApply callSignatures
        
module Method =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore metadata (method: Method) : MemberRender =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name = method.Name
            Signatures = [{
                 Metadata = metadata
                 Parameters =
                     method.Parameters
                     |> List.map (Parameter.render ctx scopeStore)
                 ReturnType = GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value) method.Type.Value
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
        let metadata = RenderMetadata.create (TransientMemberPath.AnchoredAndMoored method.Name)
        renderWithMetadata ctx scopeStore metadata method
    
module GetAccessor =
    let private getTraits (getter: GetAccessor) = Set [
        if getter.IsStatic then RenderTraits.Static
        RenderTraits.Readable
        RenderTraits.JSGetter
    ]
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (getter: GetAccessor) metadata: MemberRender =
        match getter.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            let signatures =
                members
                |> List.collect (function
                    | Member.CallSignature callSignatures -> callSignatures
                    | _ -> [])
                |> List.map (CallSignature.renderWithMetadata ctx scopeStore metadata)
            {
                Metadata = metadata
                Prelude.FunctionLikeRender.Name = getter.Name
                Signatures = signatures
                Traits = getTraits getter
                TypeParameters = []
                Documentation = []
            }
            |> MemberRender.Method
        | resolvedType ->
            {
                Metadata = metadata
                Prelude.TypedNameRender.Name = getter.Name
                Type =
                    resolvedType
                    |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
                Traits = getTraits getter
                TypeParameters = []
                Documentation = []
            }
            |> MemberRender.Property
    let render (ctx: GeneratorContext) scopeStore (getter: GetAccessor): MemberRender =
        let metadata = TransientMemberPath.AnchoredAndMoored getter.Name |> RenderMetadata.create
        renderWithMetadata ctx scopeStore getter metadata
        
module SetAccessor =
    let private getTraits (setter: SetAccessor) = Set [
        if setter.IsStatic then RenderTraits.Static
        RenderTraits.Writable
        RenderTraits.JSSetter
    ]
    
    let renderWithMetadata (ctx: GeneratorContext) scopeStore (setter: SetAccessor) metadata: MemberRender =
        {
            Metadata = metadata
            Prelude.TypedNameRender.Name = setter.Name
            Type =
                setter.ArgumentType.Value
                |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
            Traits = getTraits setter
            TypeParameters = []
            Documentation = setter.Documentation
        }
        |> MemberRender.Property
    
    let render (ctx: GeneratorContext) scopeStore (setter: SetAccessor): MemberRender =
        let metadata = TransientMemberPath.AnchoredAndMoored setter.Name |> RenderMetadata.create
        renderWithMetadata ctx scopeStore setter metadata
    
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
                        |> List.map (Parameter.renderWithMetadata ctx scopeStore metadata)
                    ReturnType =
                        indexSignature.Type.Value
                        |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
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
    let render (ctx: GeneratorContext) scopeStore (indexSignature: IndexSignature): MemberRender =
        let metadata = TransientMemberPath.AnchoredAndMoored (Name.create "Item" |> Case.addCamelMeasure) |> RenderMetadata.create
        renderWithMetadata ctx scopeStore indexSignature metadata
    
module ConstructSignature =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore metadata  (constructSignature: ConstructSignature list) =
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
                        ReturnType =
                            constructSignature.Type.Value
                            |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
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
    
    let render (ctx: GeneratorContext) scopeStore (constructSignature: ConstructSignature list): MemberRender =
        let metadata = TransientMemberPath.AnchoredAndMoored (Name.create "Create" |> Case.addCamelMeasure) |> RenderMetadata.create
        renderWithMetadata ctx scopeStore metadata constructSignature
    
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
    let renderMethodLikeWithMetadata (ctx: GeneratorContext) scopeStore (prop: Property) metadata  (callSignatures: CallSignature list) =
        {
            Metadata = metadata
            Prelude.FunctionLikeRender.Name = prop.Name
            Signatures =
                callSignatures
                |> List.map (CallSignature.renderWithMetadata ctx scopeStore metadata)
            Traits = getTraits prop
            TypeParameters = []
            Documentation = prop.Documentation
        }
    let renderMethodLike (ctx: GeneratorContext) scopeStore (prop: Property) (callSignatures: CallSignature list) =
        let metadata = TransientMemberPath.AnchoredAndMoored prop.Name |> RenderMetadata.create
        renderMethodLikeWithMetadata ctx scopeStore prop metadata callSignatures
    
    let renderWithMetadata (ctx: GeneratorContext) scopeStore metadata (prop: Property) =
        match prop.Type.Value with
        | ResolvedType.TypeLiteral { Members = members } when members |> List.forall _.IsCallSignature ->
            members
            |> List.collect (function
                | Member.CallSignature callSignatures -> callSignatures
                | _ -> [])
            |> renderMethodLikeWithMetadata ctx scopeStore prop metadata
            |> MemberRender.Method
        | _ ->
            {
                Metadata = metadata
                Prelude.TypedNameRender.Name = prop.Name
                Type =
                    prop.Type.Value
                    |> GeneratorContext.Prelude.getRenderWithScope ctx scopeStore (fun _ -> _.TypeRef.Value)
                Traits = getTraits prop
                TypeParameters = []
                Documentation = prop.Documentation
            }
            |> MemberRender.Property
    
    let render (ctx: GeneratorContext) scopeStore (prop: Property) =
        let metadata = TransientMemberPath.AnchoredAndMoored prop.Name |> RenderMetadata.create
        renderWithMetadata ctx scopeStore metadata prop
    
module Member =
    let renderWithMetadata (ctx: GeneratorContext) scopeStore metadata (member': Member) =
        match member' with
        | Member.Method methods ->
            methods
            |> List.map (Method.renderWithMetadata ctx scopeStore metadata)
        | Member.Property property ->
            [ Property.renderWithMetadata ctx scopeStore metadata  property ]
        | Member.GetAccessor accessor ->
            [ GetAccessor.renderWithMetadata ctx scopeStore accessor metadata ]
        | Member.SetAccessor accessor ->
            [ SetAccessor.renderWithMetadata ctx scopeStore accessor metadata ]
        | Member.CallSignature callSignatures ->
            [ CallSignature.renderMemberWithMetadata ctx scopeStore metadata callSignatures ]
        | Member.IndexSignature indexSignature ->
            [ IndexSignature.renderWithMetadata ctx scopeStore indexSignature metadata ]
        | Member.ConstructSignature constructSignatures ->
            [ ConstructSignature.renderWithMetadata ctx scopeStore metadata constructSignatures ]
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

    let inline private partitionMembers value =
        value
        |> Seq.fold (fun (members, functions) m ->
            match m with
            | MemberRender.Property typedNameRender -> typedNameRender :: members, functions
            | MemberRender.Method functionLikeRender -> members, functionLikeRender :: functions
            ) ([], [])
    let partitionRenderWithMetadata ctx scopeStore metadata (members: Member list) =
        members
        |> Seq.collect (renderWithMetadata ctx scopeStore metadata)
        |> partitionMembers
    let partitionRender ctx scopeStore (members: Member list) =
        members
        |> Seq.collect (render ctx scopeStore)
        |> partitionMembers
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