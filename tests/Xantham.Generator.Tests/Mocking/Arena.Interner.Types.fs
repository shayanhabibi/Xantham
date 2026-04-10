module Mocking.ArenaInterner

open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner

module ResolvedType =
    let globalThis = ResolvedType.GlobalThis
    let primitive = ResolvedType.Primitive
    module Conditional =
        let create (trueType: ResolvedType) (falseType: ResolvedType) =
            {
                Check = lazy primitive TypeKindPrimitive.Any
                Extends = lazy primitive TypeKindPrimitive.Any
                True = lazy trueType
                False = lazy falseType
            }
        let extends typ conditional = { conditional with ConditionalType.Extends = lazy typ }
        let check typ conditional = { conditional with ConditionalType.Check = lazy typ }
        let trueType typ conditional = { conditional with ConditionalType.True = lazy typ }
        let falseType typ conditional = { conditional with ConditionalType.False = lazy typ }
        let wrap = ResolvedType.Conditional
    module Tuple =
        let private makeTupleElementType typ = {
            TupleElementType.Type = lazy typ
            IsOptional = false
            IsRest = false
        }
        let wrap = ResolvedType.Tuple
        let createElement (typ: ResolvedType) =
            makeTupleElementType typ
            |> TupleElement.Fixed
        module Element =
            let private makeOptional tupleElement = { tupleElement with TupleElementType.IsOptional = true }
            let private makeRest tupleElement = { tupleElement with TupleElementType.IsRest = true }
            let optional = function
                | TupleElement.Fixed tupleElement ->
                    makeOptional tupleElement
                    |> TupleElement.Fixed
                | TupleElement.FixedLabel (label, tupleElement) ->
                    TupleElement.FixedLabel (label, makeOptional tupleElement)
                | tele -> tele
            let rest = function
                | TupleElement.Fixed tupleElement ->
                    makeRest tupleElement
                    |> TupleElement.Fixed
                | TupleElement.FixedLabel (label, tupleElement) ->
                    TupleElement.FixedLabel (label, makeRest tupleElement)
                | tele -> tele
            let variadic = function
                | TupleElement.FixedLabel(_, tupleElement) 
                | TupleElement.Fixed tupleElement -> TupleElement.Variadic tupleElement.Type
                | tele -> tele
            let label text = function
                | TupleElement.FixedLabel (_, tupleElement) 
                | TupleElement.Fixed tupleElement -> TupleElement.FixedLabel (text, tupleElement)
                | tele -> tele
        let create (elements: TupleElement list) =
            {
                IsReadOnly = false
                FixedLength = elements.Length
                MinRequired = elements.Length
                Tuple.Types = elements
            }
    module Array =
        let create = ResolvedType.Array
    module Union =
        let create types = ResolvedType.Union { Types = types |> List.map Lazy.CreateFromValue }
    module Interface =
        let wrap = ResolvedType.Interface
        let create name =
            { IsLibEs = false
              Source = None
              Interface.FullyQualifiedName = [ QualifiedNamePart.Normal name ]
              Name = Name.Pascal.create name
              Members = []
              TypeParameters = []
              Documentation = []
              Heritage = { Extends = [] } }
        let withPath modules iface =
            { iface with Interface.FullyQualifiedName = (List.map QualifiedNamePart.Normal modules) @ iface.FullyQualifiedName }
        let withSource source iface = { iface with Interface.Source = Some (QualifiedNamePart.Normal source) }
        let esLib iface = { iface with Interface.IsLibEs = true }
    module Enum =
        let wrap = ResolvedType.Enum
        let create name = { IsLibEs = false
                            Source = None
                            FullyQualifiedName = [ QualifiedNamePart.Normal name ]
                            EnumType.Name = Name.Pascal.create name
                            Members = []
                            Documentation = [] }
        let withPath modules enum =
            { enum with EnumType.FullyQualifiedName = modules @ enum.FullyQualifiedName }
        let withSource source enum = { enum with EnumType.Source = Some source }
        let withMembers members enum = { enum with EnumType.Members = members }
    module EnumCase =
        let wrap = ResolvedType.EnumCase
        let create name value enum = { EnumCase.Name = Name.Pascal.create name
                                       EnumCase.Value = value
                                       EnumCase.Parent = Lazy.CreateFromValue enum
                                       Source = None
                                       FullyQualifiedName = [ yield! enum.FullyQualifiedName ; QualifiedNamePart.Normal name ]
                                       Documentation = [] }
    module Literal =
        let wrap = ResolvedType.Literal
        let createString value = TsLiteral.String value
        let createInt value = TsLiteral.Int value
        let createFloat value = TsLiteral.Float value
        let createBool value = TsLiteral.Bool value
        let createNull = TsLiteral.Null
        type SRTPHelper =
            static member inline Create(value: string) = createString value
            static member inline Create(value: int) = createInt value
            static member inline Create(value: float) = createFloat value
            static member inline Create(value: bool) = createBool value
        let inline create (value: ^T) =
            ((^T or SRTPHelper):(static member Create: ^T -> TsLiteral) value)
    
    module TypeParameter =
        let wrap = ResolvedType.TypeParameter
        let create name = {
            TypeParameter.Name = Name.Typar.create name
            Default = None
            Constraint = None
            Documentation = [] }
        let withDefault defaultType typar = { typar with TypeParameter.Default = Some defaultType }
        let withConstraint constraintType typar = { typar with TypeParameter.Constraint = Some constraintType }
    
    module TypeAlias =
        let create innerType name = {
            IsLibEs = false
            Source = None
            FullyQualifiedName = [ QualifiedNamePart.Normal name ]
            Name = Name.Pascal.create name
            TypeAlias.Type = Lazy.CreateFromValue innerType
            TypeParameters = []
            Documentation = []
        }
        
        let withSource source typeAlias = { typeAlias with TypeAlias.Source = Some source }
        let withPath modules typeAlias = { typeAlias with TypeAlias.FullyQualifiedName = modules @ typeAlias.FullyQualifiedName }
        let addTypeParameter typeParameter typeAlias = { typeAlias with TypeAlias.TypeParameters = Lazy.CreateFromValue typeParameter :: typeAlias.TypeParameters }
        let addTypeParameters typeParameters typeAlias = { typeAlias with TypeAlias.TypeParameters = (typeParameters |> List.map Lazy.CreateFromValue) @ typeAlias.TypeParameters }
        let withTypeParameters typeParameters typeAlias = { typeAlias with TypeAlias.TypeParameters = typeParameters |> List.map Lazy.CreateFromValue }
        let withType innerType typeAlias = { typeAlias with TypeAlias.Type = Lazy.CreateFromValue innerType }
    module TypeLiteral =
        let wrap = ResolvedType.TypeLiteral
        let empty = { TypeLiteral.Members = [] }
        let withMembers members typeLiteral = { typeLiteral with TypeLiteral.Members = members }
        let addMember member' typeLiteral = { typeLiteral with TypeLiteral.Members = member' :: typeLiteral.Members }
        let addMembers members typeLiteral = { typeLiteral with TypeLiteral.Members = members @ typeLiteral.Members }

    module Property =
        let wrap = Member.Property
        let create name typ = {
            Property.Name = Name.Camel.create name
            Property.Type = Lazy.CreateFromValue typ
            Documentation = []
            IsOptional = false
            IsStatic = false
            IsPrivate = false
            Accessor = TsAccessor.ReadWrite
        }
        let optional prop = { prop with Property.IsOptional = true }
        let static' prop = { prop with Property.IsStatic = true }
        let private' prop = { prop with Property.IsPrivate = true }
        let readOnly prop = { prop with Property.Accessor = TsAccessor.ReadOnly }
        let writeOnly prop = { prop with Property.Accessor = TsAccessor.WriteOnly }
        let readWrite prop = { prop with Property.Accessor = TsAccessor.ReadWrite }
        let withType typ prop = { prop with Property.Type = Lazy.CreateFromValue typ }
    module Parameter =
        let create name typ = {
            Parameter.Name = Name.Camel.create name
            IsOptional = false
            IsSpread = false
            Parameter.Type = Lazy.CreateFromValue typ
            Documentation = []
        }
        let optional param = { param with Parameter.IsOptional = true }
        let spread param = { param with Parameter.IsSpread = true }
        let withType typ param = { param with Parameter.Type = Lazy.CreateFromValue typ }
    module CallSignature =
        let wrap = Member.CallSignature
        let create returnType = {
            Documentation = []
            Parameters = []
            CallSignature.Type = Lazy.CreateFromValue returnType
        }
        let withParameters parameters callSignature = { callSignature with CallSignature.Parameters = parameters }
        let withReturnType returnType callSignature = { callSignature with CallSignature.Type = Lazy.CreateFromValue returnType }
    module TypeReference =
        let wrap = ResolvedType.TypeReference
        let create typ = {
            TypeReference.Type = Lazy.CreateFromValue typ
            TypeArguments = []
            ResolvedType = None
        }
        let withTypeArguments typeArguments typ = { typ with TypeReference.TypeArguments = typeArguments |> List.map Lazy.CreateFromValue }
        let withResolvedType resolvedType typ = { typ with TypeReference.ResolvedType = Lazy.CreateFromValue resolvedType |> Some }
        let withType innerType typ = { typ with TypeReference.Type = Lazy.CreateFromValue innerType }
    