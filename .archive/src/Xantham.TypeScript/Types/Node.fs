module Xantham.TypeScript.Node

open Fable.Core
open TypeScript
open Xantham.Fable
open Fable.Core.JsInterop

[<RequireQualifiedAccess>]
type JSDoc =
    | Tag of Ts.JSDocTag
    | Deprecated of Ts.JSDocDeprecatedTag
    | Type of Ts.JSDocTypeTag
    | Parameter of Ts.JSDocParameterTag
    | Private of Ts.JSDocPrivateTag
    | Return of Ts.JSDocReturnTag
    | See of Ts.JSDocSeeTag
    | Readonly of Ts.JSDocReadonlyTag
    | Override of Ts.JSDocOverrideTag
    | Public of Ts.JSDocPublicTag
    | Callback of Ts.JSDocCallbackTag
    | Throws of Ts.JSDocThrowsTag
    | Typedef of Ts.JSDocTypedefTag
    | Template of Ts.JSDocTemplateTag
    | Import of Ts.JSDocImportTag
    | Overload of Ts.JSDocOverloadTag
    | Augments of Ts.JSDocAugmentsTag
    | Class of Ts.JSDocClassTag
    interface IFastUnionUnwrappable<Ts.JSDocTag>
    interface IInlinedProgram
    
type CommentPartLink = {
    Text: string option
    Name: Choice<Symbol.Kind, string> option
}
    
[<RequireQualifiedAccess>]
type CommentPart =
    | Text of string array
    | Link of CommentPartLink
    | LinkCode of CommentPartLink
    | LinkPlain of CommentPartLink

[<RequireQualifiedAccess>]
type ModifierKeyword =
    | Export
    | Declare
    | Default
    | Abstract
    | ReadOnly
    | Static
    | Override
    | Public
    | Protected
    | Private
    | Out
    | In
    | Const

[<RequireQualifiedAccess>]
type TypeKeyword =
    | String
    | Number
    | Boolean
    | Null
    | Undefined
    | Void
    | Never
    | Any
    | Unknown
    | Object
    | Symbol
    | BigInt
    | Intrinsic
    interface IAlwaysType

type NumericLiteral =
    inherit IErasedWrapper<Ts.NumericLiteral>
    inherit IEmbedded<INeverSymbol>
type StringLiteral =
    inherit IErasedWrapper<Ts.StringLiteral>
    inherit IEmbedded<INeverSymbol>
type BooleanLiteral =
    inherit IErasedWrapper<Ts.BooleanLiteral>
    inherit IEmbedded<INeverSymbol>
type BigIntLiteral =
    inherit IErasedWrapper<Ts.BigIntLiteral>
    inherit IEmbedded<INeverSymbol>
type NoSubstitutionTemplateLiteral =
    inherit IErasedWrapper<Ts.NoSubstitutionTemplateLiteral>
    inherit IEmbedded<INeverSymbol>
type UnionType =
    inherit IErasedWrapper<Ts.UnionTypeNode>
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
type PrefixUnaryExpression =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.PrefixUnaryExpression>

type [<RequireQualifiedAccess>] Literal =
    | Numeric of NumericLiteral 
    | String of StringLiteral 
    | Boolean of BooleanLiteral
    | Null
    | BigInt of BigIntLiteral
    | NoSubstitutionTemplateLiteral of NoSubstitutionTemplateLiteral
    | PrefixUnary of PrefixUnaryExpression
    interface IInlinedProgram
    interface IEmbedded<INeverSymbol>

type IntersectionType =
    inherit IErasedWrapper<Ts.IntersectionTypeNode>
    inherit IEmbedded<INeverSymbol>
    inherit INeverSymbol
    inherit IAlwaysType

type ArrayType =
    inherit IErasedWrapper<Ts.ArrayTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type TupleType =
    inherit IErasedWrapper<Ts.TupleTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type NamedTupleMember =
    inherit IErasedWrapper<Ts.NamedTupleMember>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type RestType =
    inherit IErasedWrapper<Ts.RestTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type OptionalType =
    inherit IErasedWrapper<Ts.OptionalTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type ParenthesizedType =
    inherit IErasedWrapper<Ts.ParenthesizedTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type TypeParameterDeclaration =
    interface IErasedWrapper<Ts.TypeParameterDeclaration>
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IAlwaysType
    static member inline Create(program, node): TypeParameterDeclaration = IErasedWrapper.create program node
    static member inline Create(program: IInlinedProgram, node): TypeParameterDeclaration = IErasedWrapper.create program.Program node
    
type InferType =
    inherit IErasedWrapper<Ts.InferTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type TypePredicate =
    inherit IErasedWrapper<Ts.TypePredicateNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type TypeQuery =
    inherit IErasedWrapper<Ts.TypeQueryNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    
type IndexedAccessType =
    inherit IErasedWrapper<Ts.IndexedAccessTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type MappedType =
    inherit IErasedWrapper<Ts.MappedTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IAlwaysType

type ConditionalType =
    inherit IErasedWrapper<Ts.ConditionalTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type TemplateLiteralType =
    inherit IErasedWrapper<Ts.TemplateLiteralTypeNode>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type TemplateLiteralTypeSpan =
    inherit IErasedWrapper<Ts.TemplateLiteralTypeSpan>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType

type ImportType =
    inherit IAlwaysSymbol
    inherit IAlwaysType
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.ImportTypeNode>

type FunctionType =
    interface INeverSymbol
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface IAlwaysType
    interface IErasedWrapper<Ts.FunctionTypeNode>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            this.Value
            |> _.typeParameters
            |> Option.bind NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type ConstructorType =
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IAlwaysType
    interface IInlinedProgram
    interface IErasedWrapper<Ts.ConstructorTypeNode>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            this.Value
            |> _.typeParameters
            |> Option.bind NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type TypeLiteralType =
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IAlwaysType
    inherit IErasedWrapper<Ts.TypeLiteralNode>

type LiteralType =
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    inherit IErasedWrapper<Ts.LiteralTypeNode>

type ThisType =
    inherit IAlwaysSymbol
    inherit IAlwaysType
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.ThisTypeNode>

type TypeReference = 
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.TypeReferenceNode>

type ExpressionWithTypeArguments = 
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IAlwaysType
    inherit IErasedWrapper<Ts.ExpressionWithTypeArguments>

type TypeNode =
    inherit IInlinedProgram
    inherit IEmbedded<ICanHaveSymbol>
    inherit IErasedWrapper<Ts.TypeNode>

type [<RequireQualifiedAccess>] KeyOf =
    | Bounded of Type 
    | Unbounded of Type 
    | Generic of Type 
    | ConstrainedGeneric of Type 
    interface INeverSymbol
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Type>


and [<RequireQualifiedAccess>] TypeOperator =
    | KeyOf of KeyOf
    | Readonly of Type 
    | Unique of Type 
    interface INeverSymbol
    interface IEmbedded<INeverSymbol>
    interface IInlinedProgram
    
/// <summary>
/// </summary>
/// <remarks>Percentage of cases based on the corpus of tests in Xantham.TypeScript. Percentages
/// do not differentiate from whether they were a nested type node or not.</remarks>
and [<RequireQualifiedAccess>] Type =
    /// ~39% of cases
    | Reference of TypeReference * TypeNode
    /// ~29% of cases
    | Keyword of TypeKeyword * TypeNode
    /// ~10% of cases
    | Literal of Literal * TypeNode
    /// ~7% of cases
    | Union of UnionType * TypeNode
    /// ~3% of cases
    | Function of FunctionType * TypeNode
    /// ~2% of cases
    | TypeLiteral of TypeLiteralType * TypeNode
    /// ~2% of cases
    | ExpressionWithTypeArguments of ExpressionWithTypeArguments * TypeNode
    /// ~1.5% of cases
    | Array of ArrayType * TypeNode
    /// ~1% of cases
    | TypeOperator of TypeOperator * TypeNode
    /// ~1% of cases
    | IndexedAccess of IndexedAccessType * TypeNode
    /// ~1% of cases
    | Parenthesized of ParenthesizedType * TypeNode
    /// ~1% of cases
    | This of ThisType * TypeNode
    /// ~0.5% of cases
    | Tuple of TupleType * TypeNode
    /// ~0.25% of cases
    | Conditional of ConditionalType * TypeNode
    /// ~0.25% of cases
    | Intersection of IntersectionType * TypeNode
    /// ~0.25% of cases
    | TypeQuery of TypeQuery * TypeNode
    /// ~0.1% of cases
    | Infer of InferType * TypeNode
    /// ~0.1% of cases
    | TypePredicate of TypePredicate * TypeNode
    /// ~0.1% of cases
    | TemplateLiteralSpan of TemplateLiteralTypeSpan * TypeNode
    /// ~0.1% of cases
    | Mapped of MappedType * TypeNode
    /// &lt;0.1% of cases
    | NamedTuple of NamedTupleMember * TypeNode
    /// &lt;0.1% of cases
    | TemplateLiteral of TemplateLiteralType * TypeNode
    /// &lt;0.1% of cases
    | Rest of RestType * TypeNode
    /// &lt;0.1% of cases
    | Import of ImportType * TypeNode
    /// &lt;0.1% of cases
    | Constructor of ConstructorType * TypeNode
    /// &lt;0.1% of cases
    | Optional of OptionalType * TypeNode
    interface IInlinedProgram
    interface ICanHaveSymbol
    interface IEmbedded<ICanHaveSymbol>
    interface IAlwaysType
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            match this with
            | Type.Constructor (c, _) -> c :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | Type.Function (f, _) -> f :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | _ -> None

type ParameterDeclaration = 
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IErasedWrapper<Ts.ParameterDeclaration>
    
type BindingElement = 
    inherit INeverSymbol
    inherit IEmbedded<ICanHaveSymbol>
    inherit IErasedWrapper<Ts.BindingElement>

[<RequireQualifiedAccess>]
type ParameterKind =
    | Simple of ParameterDeclaration 
    | Binding of BindingElement 
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<ICanHaveSymbol>
    interface IFastUnionUnwrappable<Ts.NamedDeclaration>
    

type IndexSignature =
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface INeverSymbol
    interface IErasedWrapper<Ts.IndexSignatureDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !^(!^this.Value : Ts.DeclarationWithTypeParameterChildren)
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type CallSignature =
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface INeverSymbol
    interface IErasedWrapper<Ts.CallSignatureDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !^(!^this.Value : Ts.DeclarationWithTypeParameterChildren)
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type ConstructSignature =
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface INeverSymbol
    interface IErasedWrapper<Ts.ConstructSignatureDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !^(!^this.Value : Ts.DeclarationWithTypeParameterChildren)
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

[<RequireQualifiedAccess>]
type SignatureKind =
    | Index of IndexSignature
    | Call of CallSignature
    | Construct of ConstructSignature
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IFastUnionUnwrappable<Ts.SignatureDeclarationBase>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !^(!^this.Value : Ts.DeclarationWithTypeParameterChildren)
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type PropertyDeclaration =
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IErasedWrapper<Ts.PropertyDeclaration>

type PropertySignature =
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IErasedWrapper<Ts.PropertySignature>

[<RequireQualifiedAccess>]
type PropertyKind =
    | Class of PropertyDeclaration
    | Type of PropertySignature
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IFastUnionUnwrappable<Ts.Node>

type MethodDeclaration =
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IErasedWrapper<Ts.MethodDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type MethodSignature =
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IErasedWrapper<Ts.MethodSignature>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

[<RequireQualifiedAccess>]
type MethodKind =
    | Class of MethodDeclaration 
    | Type of MethodSignature 
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IFastUnionUnwrappable<Ts.Node>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type GetAccessorDeclaration =
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IErasedWrapper<Ts.GetAccessorDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type SetAccessorDeclaration =
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IErasedWrapper<Ts.SetAccessorDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type ConstructorDeclaration =
    interface IInlinedProgram
    interface IErasedWrapper<Ts.ConstructorDeclaration>
    interface IEmbedded<IAlwaysSymbol>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

[<RequireQualifiedAccess>]
type ClassMemberKind =
    | Method of MethodDeclaration 
    | Property of PropertyDeclaration 
    | GetAccessor of GetAccessorDeclaration
    | SetAccessor of SetAccessorDeclaration
    | Constructor of ConstructorDeclaration
    interface IInlinedProgram
    interface INeverSymbol
    interface IEmbedded<IAlwaysSymbol>
    interface IFastUnionUnwrappable<Ts.ClassElement>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

[<RequireQualifiedAccess>]
type TypeMemberKind =
    | Method of MethodSignature
    | Call of CallSignature
    | Construct of ConstructSignature
    | Property of PropertySignature
    | GetAccessor of GetAccessorDeclaration
    | SetAccessor of SetAccessorDeclaration
    | Index of IndexSignature
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface IFastUnionUnwrappable<Ts.TypeElement>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            !!this.Value
            |> ts.getEffectiveTypeParameterDeclarations
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type Script = 
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.SourceFile>

type ExternalModule =
    inherit IInlinedProgram
    inherit IAlwaysSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IErasedWrapper<Ts.SourceFile>

[<RequireQualifiedAccess>]
type SourceKind =
    | Script of Script
    | ExternalModule of ExternalModule
    interface IInlinedProgram
    interface ICanHaveSymbol
    interface IEmbedded<ICanHaveSymbol>
    interface IFastUnionUnwrappable<Ts.SourceFile>

type ModuleDeclaration = 
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ModuleDeclaration>
    inherit IEmbedded<IAlwaysSymbol>
    inherit INeverSymbol

[<RequireQualifiedAccess>]
type ModuleKind =
    | Declaration of ModuleDeclaration
    | Source of SourceKind
    interface IInlinedProgram
    interface IEmbedded<ICanHaveSymbol>
    interface ICanHaveSymbol

type FunctionDeclaration =
    interface IInlinedProgram
    interface IEmbedded<IAlwaysSymbol>
    interface IErasedWrapper<Ts.FunctionDeclaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type Variable =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.VariableDeclaration>
    inherit IEmbedded<IAlwaysSymbol>

type ClassDeclaration =
    interface IInlinedProgram
    interface IErasedWrapper<Ts.ClassDeclaration>
    interface IEmbedded<IAlwaysSymbol>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type InterfaceDeclaration =
    interface IInlinedProgram
    interface IErasedWrapper<Ts.InterfaceDeclaration>
    interface IEmbedded<IAlwaysSymbol>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type EnumDeclaration =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.EnumDeclaration>
    inherit IEmbedded<IAlwaysSymbol>
    
type TypeAliasDeclaration =
    interface IInlinedProgram
    interface IErasedWrapper<Ts.TypeAliasDeclaration>
    interface IEmbedded<IAlwaysSymbol>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))
            
[<RequireQualifiedAccess>]
type ModuleMemberKind =
    | Variable of Variable
    | Function of FunctionDeclaration 
    | Class of ClassDeclaration 
    | Interface of InterfaceDeclaration 
    | Enum of EnumDeclaration 
    | Module of ModuleDeclaration 
    | TypeAlias of TypeAliasDeclaration 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Declaration>
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            ts.getEffectiveTypeParameterDeclarations !!this.Value
            |> NonEmptyArray.create
            |> Option.map (NonEmptyArray.map (fun tp -> TypeParameterDeclaration.Create(this, tp)))

type EnumMember = 
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>
    inherit IErasedWrapper<Ts.EnumMember>

/// <summary>
/// <c>import { a as b }</c>
/// <para><c>propertyName</c> - original name before <c>as</c></para>
/// <para><c>name</c> - local binding</para>
/// </summary>
type ImportSpecifier =
    inherit IErasedWrapper<Ts.ImportSpecifier>
    inherit IInlinedProgram
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// <c>export { a as b }</c>
/// <para><c>propertyName</c> - original name before <c>as</c></para>
/// <para><c>name</c> - exported name</para>
/// </summary>
type ExportSpecifier =
    inherit IErasedWrapper<Ts.ExportSpecifier>
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// <c>import * as ns</c>
/// <para>Represents the <c>ns</c> binding.</para>
/// <para><c>name</c> - the namespace identifier.</para>
/// </summary>
type NamespaceImport =
    inherit IErasedWrapper<Ts.NamespaceImport>
    inherit IInlinedProgram
    inherit IEmbedded<IAlwaysSymbol>
    inherit INeverSymbol

/// <summary>
/// Represents either <c>export = x</c> (<c>isExportEquals = true</c>) or <c>export default x</c> (<c>isExportEquals = false</c>).
/// <para>If expression is <c>EntityNameExpression</c> or class expression, then will have alias symbol flag. Otherwise will have property symbol flag.</para>
/// <para>Symbol is declared in <c>container.symbol.exports</c>. For <c>export =</c> <c>setValueDeclaration</c> is also
/// called by the binder.</para>
/// </summary>
type ExportAssignment =
    inherit IErasedWrapper<Ts.ExportAssignment>
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// <c>ns</c> in <c>export * as ns from "mod"</c>. A child of <c>ExportDeclaration</c>.
/// <para><c>SymbolFlags.Alias</c>.</para>
/// </summary>
type NamespaceExport =
    inherit IErasedWrapper<Ts.NamespaceExport>
    inherit IInlinedProgram
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// Represents entire binding clause of an import: <c>import d, * as ns from "mod"</c> - the <c>d, * as ns</c> part.
/// <code>
/// import d from "mod"           => name = d, namedBindings = undefined  
/// import * as ns from "mod"     => name = undefined, namedBindings: NamespaceImport  
/// import d, { a, b as x } from  => name = d, namedBindings: NamedImports
/// </code>
/// <para>The binder only creates a symbol for the default binding <c>name</c>.
/// The <c>NamespaceImport</c> or <c>ImportSpecifier</c> children are bound separately when traversed.</para>
/// </summary>
type ImportClause =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ImportClause>
    inherit IEmbedded<ICanHaveSymbol>

/// <summary>
/// <c>import x = require("mod")</c> or <c>import x = M.x</c>.
/// <para><c>name</c>, <c>moduleReference</c> (either <c>ExternalModuleReference</c> or <c>EntityName</c>)</para>
/// </summary>
type ImportEqualsDeclaration =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ImportEqualsDeclaration>
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// <c>export as namespace Foo</c> in the UMD global export syntax. Only valid in <c>.d.ts</c> files.
/// Symbol is declared in <c>file.symbol.globalExports</c> with <c>SymbolFlags.Alias</c>.
/// </summary>
type NamespaceExportDeclaration =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.NamespaceExportDeclaration>
    inherit INeverSymbol
    inherit IEmbedded<IAlwaysSymbol>

/// <summary>
/// Represents <c>export { ... }</c>, <c>export * from "mod"</c> or <c>export * as ns from "mod"</c>.
/// Three cases:
/// <para>1. No container symbol (eg: inside a block), anonymous <c>SymbolFlags.ExportStar</c></para>
/// <para>2. No <c>exportClause</c> (<c>export * from "mod"</c>): <c>SymbolFlags.ExportStar</c> in <c>container.symbol.exports</c> under the internal
/// <c>__export</c> symbol name.</para>
/// <para>3. <c>NamespaceExport</c> clause (<c>export * as ns from "mod"</c>). <c>exportClause</c> gets the
/// <c>SymbolFlags.Alias</c> in <c>container.symbol.exports</c>. The parent is manually set first.</para>
/// </summary>
type ExportDeclaration =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ExportDeclaration>
    inherit IEmbedded<ICanHaveSymbol>
    inherit INeverSymbol

type NamedExports =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.NamedExports>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>

type ImportDeclaration =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ImportDeclaration>
    inherit INeverSymbol
    inherit IEmbedded<INeverSymbol>

type NamedImports =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.NamedImports>
    inherit IEmbedded<INeverSymbol>
    inherit INeverSymbol

type ExternalModuleReference =
    inherit IInlinedProgram
    inherit IErasedWrapper<Ts.ExternalModuleReference>

[<RequireQualifiedAccess>]
type ImportDeclarationKind =
    | Specifier of ImportSpecifier
    | Namespace of NamespaceImport
    | Clause of ImportClause
    | ImportEquals of ImportEqualsDeclaration
    | Named of NamedImports
    | Declaration of ImportDeclaration
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Node>
    interface IEmbedded<ICanHaveSymbol>

[<RequireQualifiedAccess>]
type ExportDeclarationKind =
    | Specifier of ExportSpecifier
    | Namespace of NamespaceExport
    | Assignment of ExportAssignment
    | NamespaceDeclaration of NamespaceExportDeclaration
    | Declaration of ExportDeclaration
    | Named of NamedExports
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Node>
    interface IEmbedded<ICanHaveSymbol>

[<RequireQualifiedAccess>]
type ImportExportDeclarationKind =
    | Import of ImportDeclarationKind
    | Export of ExportDeclarationKind
    interface IInlinedProgram

/// <summary>
/// Wrapper for <c>Ts.Declaration</c>
/// </summary>
[<RequireQualifiedAccess>]
type DeclarationKind =
    | Variable of Variable 
    | TypeAlias of TypeAliasDeclaration 
    | Function of FunctionDeclaration 
    | Parameter of ParameterKind
    | Interface of InterfaceDeclaration 
    | Property of PropertyKind
    | Signature of SignatureKind 
    | Method of MethodKind 
    | TypeParameter of TypeParameterDeclaration
    | Module of ModuleKind
    | GetAccessor of GetAccessorDeclaration
    | SetAccessor of SetAccessorDeclaration
    | Class of ClassDeclaration
    | Constructor of ConstructorDeclaration 
    | Enum of EnumDeclaration 
    | EnumMember of EnumMember
    | ImportExport of ImportExportDeclarationKind
    /// <summary>
    /// These only appear in situations where the type node declaration site IS the canonical declaration site.
    /// This includes things like type literals, and anonymous functions.
    /// </summary>
    | Type of Type // can have typars
    interface IInlinedProgram
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            match this with
            | DeclarationKind.TypeAlias t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.Function t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.Interface t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.Signature t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.Method t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.Class t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.GetAccessor t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | DeclarationKind.SetAccessor t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | _ -> None

[<RequireQualifiedAccess>]
type SemanticToken =
    | Spread
    | EoF
    | Minus
    | Optional
    | Asserts
    | PrivateField

type ObjectBindingPattern =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.ObjectBindingPattern>

type ArrayBindingPattern =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.ArrayBindingPattern>
    
[<RequireQualifiedAccess>]
type BindingPattern =
    | Object of ObjectBindingPattern 
    | Array of ArrayBindingPattern 
    interface IInlinedProgram
    interface IEmbedded<INeverSymbol>
    interface IFastUnionUnwrappable<Ts.Node>

type PropertyAccessExpression =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.PropertyAccessExpression>

[<RequireQualifiedAccess>]
type Expression =
    // | PrefixUnary of PrefixUnaryExpression 
    | PropertyAccess of PropertyAccessExpression 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Expression>

type TemplateHead =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.TemplateHead>

type TemplateMiddle =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.TemplateMiddle>

type TemplateTail =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.TemplateTail>

[<RequireQualifiedAccess>]
type TemplatePart =
    | Head of TemplateHead 
    | Middle of TemplateMiddle 
    | Tail of TemplateTail 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Node>

[<Class>]
type ComputedPropertyName =
    interface INode
    interface IErasedWrapper<Ts.ComputedPropertyName>
    interface IEmbedded<INeverSymbol>
    interface IInlinedProgram

type Identifier =
    inherit INode
    inherit IErasedWrapper<Ts.Identifier>
    inherit ICanHaveSymbol
    inherit IEmbedded<INeverSymbol>
    inherit IInlinedProgram

type QualifiedName =
    inherit INode
    inherit IErasedWrapper<Ts.QualifiedName>
    inherit IEmbedded<INeverSymbol>
    inherit IInlinedProgram

[<RequireQualifiedAccess>]
type IdentifierKind =
    | ComputedPropertyName of ComputedPropertyName
    | Identifier of Identifier 
    | QualifiedName of QualifiedName
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Node>

[<RequireQualifiedAccess>]
type IdentifierLiteralKind =
    | Identifier of IdentifierKind
    | StringLiteral of StringLiteral
    interface IInlinedProgram

type JSDocMemberName =
    inherit IErasedWrapper<Ts.JSDocMemberName>
    inherit IInlinedProgram

[<RequireQualifiedAccess>]
type JSDocIdentifier =
    | Identifier of IdentifierKind
    | JSDoc of JSDocMemberName 
    interface IInlinedProgram

type VariableStatement =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.VariableStatement>

type VariableDeclarationList =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.VariableDeclarationList>

type ModuleBlock =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.ModuleBlock>

[<RequireQualifiedAccess>]
type Container =
    | VariableStatement of VariableStatement 
    | VariableDeclarationList of VariableDeclarationList 
    | ModuleBlock of ModuleBlock 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<Ts.Node>

type HeritageClause =
    inherit IInlinedProgram
    inherit IEmbedded<INeverSymbol>
    inherit IErasedWrapper<Ts.HeritageClause>

[<RequireQualifiedAccess>]
type HeritageClauseKind =
    | Implements of NonEmptyArray<ExpressionWithTypeArguments> * HeritageClause 
    | Extends of NonEmptyArray<ExpressionWithTypeArguments> * HeritageClause 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<NonEmptyArray<ExpressionWithTypeArguments>>

[<RequireQualifiedAccess>]
type TypeHeritageClause =
    | Extends of NonEmptyArray<ExpressionWithTypeArguments> * HeritageClause 
    interface IInlinedProgram
    interface IFastUnionUnwrappable<NonEmptyArray<ExpressionWithTypeArguments>>

[<RequireQualifiedAccess>]
type ClassLikeHeritageClause =
    | Implements of NonEmptyArray<ExpressionWithTypeArguments> * HeritageClause 
    | Extends of ExpressionWithTypeArguments * HeritageClause 
    | ImplementsAndExtends of
        implements: NonEmptyArray<ExpressionWithTypeArguments> *
        extends: ExpressionWithTypeArguments *
        implementsClause: HeritageClause *
        extendsClause: HeritageClause
    interface IInlinedProgram


[<RequireQualifiedAccess>]
type Kind =
    | DeclarationOrType of DeclarationKind // can have typars
    | Modifier of ModifierKeyword
    | JSDoc of JSDoc
    | Semantic of SemanticToken
    | BindingPattern of BindingPattern
    | Expression of Expression
    | Identifier of IdentifierKind
    | TemplatePart of TemplatePart
    | Container of Container
    | HeritageClause of HeritageClauseKind
    // Duplication of type node literal branch in a way
    | Literal of Literal
    | ExternalModuleReference of ExternalModuleReference
    | JSDocIdentifier of JSDocIdentifier
    interface IInlinedProgram
    interface ICanHaveTypeParameters<TypeParameterDeclaration> with
        member this.TypeParameters =
            match this with
            | Kind.DeclarationOrType t -> t :> ICanHaveTypeParameters<TypeParameterDeclaration> |> _.TypeParameters
            | _ -> None

[<RequireQualifiedAccess>]
type TopLevelExportSymbolKind =
    | Interface of InterfaceDeclaration
    | TypeAlias of TypeAliasDeclaration
    | Class of ClassDeclaration
    | Enum of EnumDeclaration
    | Variable of Variable
    | Module of ModuleKind
    | Function of FunctionDeclaration
    | NamespaceExportDeclaration of NamespaceExportDeclaration
    | ExportDeclaration of ExportDeclaration
    | ImportEquals of ImportEqualsDeclaration
    | ImportDeclaration of ImportDeclaration
    | ExportAssignment of ExportAssignment
    | ExportSpecifier of ExportSpecifier
    | NamespaceExport of NamespaceExport
    interface IFastUnionUnwrappable<INode>

[<RequireQualifiedAccess>]
type TopLevelLocalSymbolKind =
    | Interface of InterfaceDeclaration
    | TypeAlias of TypeAliasDeclaration
    | Class of ClassDeclaration
    | Enum of EnumDeclaration
    | Variable of Variable
    | Module of ModuleKind
    | Function of FunctionDeclaration
    interface IFastUnionUnwrappable<INode>

[<RequireQualifiedAccess>]
type IdentifierExpressionKind =
    | Identifier of IdentifierKind
    | Expression of PropertyAccessExpression
    interface IInlinedProgram

[<RequireQualifiedAccess>]
type TopLevelStatementsKind =
    | Interface of InterfaceDeclaration
    | TypeAlias of TypeAliasDeclaration
    | Class of ClassDeclaration
    | Enum of EnumDeclaration
    | Variable of VariableStatement
    | VariableDeclaration of Variable
    | Function of FunctionDeclaration
    | ExportDeclaration of ExportDeclaration
    | NamespaceExportDeclaration of NamespaceExportDeclaration
    | ImportDeclaration of ImportDeclaration
    | ImportEqualsDeclaration of ImportEqualsDeclaration
    | ExportAssignment of ExportAssignment
    | Module of ModuleKind
    interface IFastUnionUnwrappable<INode>