[<AutoOpen>]
module rec Xantham.TypeScript.Misc

open Xantham.Fable
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open TypeScript


[<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
module Internal =
    let knownSymbolNodeCombinations =
        let inline (>=>) a b = a |> List.map (fun a -> KeyValuePair(a, Set b))
        List.collect id [
            [ Ts.SymbolFlags.Function ||| Ts.SymbolFlags.Interface ] >=> [
                Ts.SyntaxKind.FunctionDeclaration
                Ts.SyntaxKind.InterfaceDeclaration
            ]
            [ Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.TypeParameter ] >=> [
                Ts.SyntaxKind.Parameter
                Ts.SyntaxKind.TypeParameter
            ]
            [ Ts.SymbolFlags.GetAccessor ||| Ts.SymbolFlags.SetAccessor ] >=> [
                Ts.SyntaxKind.GetAccessor
                Ts.SyntaxKind.SetAccessor
            ]
            [ Ts.SymbolFlags.Method ||| Ts.SymbolFlags.Transient ] >=> [
                Ts.SyntaxKind.MethodDeclaration
                Ts.SyntaxKind.MethodSignature
            ]
            [ Ts.SymbolFlags.Property ||| Ts.SymbolFlags.Transient ] >=> [
                Ts.SyntaxKind.PropertyDeclaration
                Ts.SyntaxKind.PropertySignature
            ]
            [ Ts.SymbolFlags.BlockScopedVariable ||| Ts.SymbolFlags.Interface ||| Ts.SymbolFlags.NamespaceModule ] >=> [
                Ts.SyntaxKind.InterfaceDeclaration
                Ts.SyntaxKind.ModuleDeclaration
                Ts.SyntaxKind.VariableDeclaration
            ]
            [ Ts.SymbolFlags.Class ||| Ts.SymbolFlags.Interface ||| Ts.SymbolFlags.ValueModule ] >=> [
                Ts.SyntaxKind.ClassDeclaration
                Ts.SyntaxKind.InterfaceDeclaration
                Ts.SyntaxKind.ModuleDeclaration
            ]
            [ Ts.SymbolFlags.NamespaceModule ||| Ts.SymbolFlags.Transient ||| Ts.SymbolFlags.ValueModule ] >=> [
                Ts.SyntaxKind.ModuleDeclaration
                Ts.SyntaxKind.SourceFile
            ]
            [
                Ts.SymbolFlags.BlockScopedVariable ||| Ts.SymbolFlags.TypeAlias
                Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.TypeAlias
            ] >=> [
                Ts.SyntaxKind.TypeAliasDeclaration
                Ts.SyntaxKind.VariableDeclaration
            ]
            [
                Ts.SymbolFlags.Class ||| Ts.SymbolFlags.Interface
                Ts.SymbolFlags.Class ||| Ts.SymbolFlags.Interface ||| Ts.SymbolFlags.Transient
            ] >=> [
                Ts.SyntaxKind.ClassDeclaration
                Ts.SyntaxKind.InterfaceDeclaration
            ]
            [
                Ts.SymbolFlags.Class ||| Ts.SymbolFlags.NamespaceModule
                Ts.SymbolFlags.Class ||| Ts.SymbolFlags.ValueModule
            ] >=> [
                Ts.SyntaxKind.ClassDeclaration
                Ts.SyntaxKind.ModuleDeclaration
            ]
            [
                Ts.SymbolFlags.Interface ||| Ts.SymbolFlags.NamespaceModule
                Ts.SymbolFlags.Interface ||| Ts.SymbolFlags.ValueModule
            ] >=> [
                Ts.SyntaxKind.InterfaceDeclaration
                Ts.SyntaxKind.ModuleDeclaration
            ]
            [
                Ts.SymbolFlags.NamespaceModule ||| Ts.SymbolFlags.TypeAlias
                Ts.SymbolFlags.TypeAlias ||| Ts.SymbolFlags.ValueModule
            ] >=> [
                Ts.SyntaxKind.ModuleDeclaration
                Ts.SyntaxKind.TypeAliasDeclaration
            ]
            [
                Ts.SymbolFlags.Function ||| Ts.SymbolFlags.NamespaceModule
                Ts.SymbolFlags.Function ||| Ts.SymbolFlags.ValueModule
                Ts.SymbolFlags.Function ||| Ts.SymbolFlags.Transient ||| Ts.SymbolFlags.ValueModule
            ] >=> [
                Ts.SyntaxKind.FunctionDeclaration
                Ts.SyntaxKind.ModuleDeclaration
            ]
            [
                Ts.SymbolFlags.BlockScopedVariable ||| Ts.SymbolFlags.Interface
                Ts.SymbolFlags.ExportValue ||| Ts.SymbolFlags.Interface
                Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.Interface
                Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.Transient ||| Ts.SymbolFlags.Interface
            ] >=> [
                Ts.SyntaxKind.InterfaceDeclaration
                Ts.SyntaxKind.VariableDeclaration
            ]
            [
                Ts.SymbolFlags.BlockScopedVariable ||| Ts.SymbolFlags.NamespaceModule
                Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.NamespaceModule
                Ts.SymbolFlags.BlockScopedVariable ||| Ts.SymbolFlags.NamespaceModule ||| Ts.SymbolFlags.Transient
                Ts.SymbolFlags.FunctionScopedVariable ||| Ts.SymbolFlags.NamespaceModule ||| Ts.SymbolFlags.Transient
            ] >=> [
                Ts.SyntaxKind.ModuleDeclaration
                Ts.SyntaxKind.VariableDeclaration
            ]
        ]
        |> List.distinct
        |> Dictionary
    let declarationFileNodes: Dictionary<Ts.SyntaxKind, obj -> DeclarationFileNodes> =
        Dictionary [
            let inline (>=>) a b = KeyValuePair(a, unbox >> b)
            Ts.SyntaxKind.BigIntLiteral >=> DeclarationFileNodes.BigIntLiteral
            Ts.SyntaxKind.NamespaceExportDeclaration >=> DeclarationFileNodes.NamespaceExportDeclaration
            Ts.SyntaxKind.PrivateIdentifier >=> DeclarationFileNodes.PrivateIdentifier
            Ts.SyntaxKind.OptionalType >=> DeclarationFileNodes.OptionalType
            Ts.SyntaxKind.NoSubstitutionTemplateLiteral >=> DeclarationFileNodes.NoSubstitutionTemplateLiteral
            Ts.SyntaxKind.ImportEqualsDeclaration >=> DeclarationFileNodes.ImportEqualsDeclaration
            Ts.SyntaxKind.NamespaceExport >=> DeclarationFileNodes.NamespaceExport
            Ts.SyntaxKind.OutKeyword >=> DeclarationFileNodes.OutKeyword
            Ts.SyntaxKind.ExternalModuleReference >=> DeclarationFileNodes.ExternalModuleReference
            Ts.SyntaxKind.PublicKeyword >=> DeclarationFileNodes.PublicKeyword
            Ts.SyntaxKind.InKeyword >=> DeclarationFileNodes.InKeyword
            Ts.SyntaxKind.ObjectBindingPattern >=> DeclarationFileNodes.ObjectBindingPattern
            Ts.SyntaxKind.ImportType >=> DeclarationFileNodes.ImportType
            Ts.SyntaxKind.AssertsKeyword >=> DeclarationFileNodes.AssertsKeyword
            Ts.SyntaxKind.AbstractKeyword >=> DeclarationFileNodes.AbstractKeyword
            Ts.SyntaxKind.AnyKeyword >=> DeclarationFileNodes.AnyKeyword
            Ts.SyntaxKind.ArrayBindingPattern >=> DeclarationFileNodes.ArrayBindingPattern
            Ts.SyntaxKind.ArrayType >=> DeclarationFileNodes.ArrayType
            Ts.SyntaxKind.BigIntKeyword >=> DeclarationFileNodes.BigIntKeyword
            Ts.SyntaxKind.BindingElement >=> DeclarationFileNodes.BindingElement
            Ts.SyntaxKind.BooleanKeyword >=> DeclarationFileNodes.BooleanKeyword
            Ts.SyntaxKind.CallSignature >=> DeclarationFileNodes.CallSignature
            Ts.SyntaxKind.ClassDeclaration >=> DeclarationFileNodes.ClassDeclaration
            Ts.SyntaxKind.ComputedPropertyName >=> DeclarationFileNodes.ComputedPropertyName
            Ts.SyntaxKind.ConditionalType >=> DeclarationFileNodes.ConditionalType
            Ts.SyntaxKind.ConstKeyword >=> DeclarationFileNodes.ConstKeyword
            Ts.SyntaxKind.ConstructSignature >=> DeclarationFileNodes.ConstructSignature
            Ts.SyntaxKind.Constructor >=> DeclarationFileNodes.Constructor
            Ts.SyntaxKind.ConstructorType >=> DeclarationFileNodes.ConstructorType
            Ts.SyntaxKind.DeclareKeyword >=> DeclarationFileNodes.DeclareKeyword
            Ts.SyntaxKind.DefaultKeyword >=> DeclarationFileNodes.DefaultKeyword
            Ts.SyntaxKind.DotDotDotToken >=> DeclarationFileNodes.DotDotDotToken
            Ts.SyntaxKind.EndOfFileToken >=> DeclarationFileNodes.EndOfFileToken
            Ts.SyntaxKind.EnumDeclaration >=> DeclarationFileNodes.EnumDeclaration
            Ts.SyntaxKind.EnumMember >=> DeclarationFileNodes.EnumMember
            Ts.SyntaxKind.ExportAssignment >=> DeclarationFileNodes.ExportAssignment
            Ts.SyntaxKind.ExportDeclaration >=> DeclarationFileNodes.ExportDeclaration
            Ts.SyntaxKind.ExportKeyword >=> DeclarationFileNodes.ExportKeyword
            Ts.SyntaxKind.ExportSpecifier >=> DeclarationFileNodes.ExportSpecifier
            Ts.SyntaxKind.ExpressionWithTypeArguments >=> DeclarationFileNodes.ExpressionWithTypeArguments
            Ts.SyntaxKind.FalseKeyword >=> DeclarationFileNodes.FalseKeyword
            Ts.SyntaxKind.FunctionDeclaration >=> DeclarationFileNodes.FunctionDeclaration
            Ts.SyntaxKind.FunctionType >=> DeclarationFileNodes.FunctionType
            Ts.SyntaxKind.GetAccessor >=> DeclarationFileNodes.GetAccessor
            Ts.SyntaxKind.HeritageClause >=> DeclarationFileNodes.HeritageClause
            Ts.SyntaxKind.Identifier >=> DeclarationFileNodes.Identifier
            Ts.SyntaxKind.ImportClause >=> DeclarationFileNodes.ImportClause
            Ts.SyntaxKind.ImportDeclaration >=> DeclarationFileNodes.ImportDeclaration
            Ts.SyntaxKind.ImportSpecifier >=> DeclarationFileNodes.ImportSpecifier
            Ts.SyntaxKind.IndexSignature >=> DeclarationFileNodes.IndexSignature
            Ts.SyntaxKind.IndexedAccessType >=> DeclarationFileNodes.IndexedAccessType
            Ts.SyntaxKind.InferType >=> DeclarationFileNodes.InferType
            Ts.SyntaxKind.InterfaceDeclaration >=> DeclarationFileNodes.InterfaceDeclaration
            Ts.SyntaxKind.IntersectionType >=> DeclarationFileNodes.IntersectionType
            Ts.SyntaxKind.IntrinsicKeyword >=> DeclarationFileNodes.IntrinsicKeyword
            Ts.SyntaxKind.LiteralType >=> DeclarationFileNodes.LiteralType
            Ts.SyntaxKind.MappedType >=> DeclarationFileNodes.MappedType
            Ts.SyntaxKind.MethodDeclaration >=> DeclarationFileNodes.MethodDeclaration
            Ts.SyntaxKind.MethodSignature >=> DeclarationFileNodes.MethodSignature
            Ts.SyntaxKind.MinusToken >=> DeclarationFileNodes.MinusToken
            Ts.SyntaxKind.ModuleBlock >=> DeclarationFileNodes.ModuleBlock
            Ts.SyntaxKind.ModuleDeclaration >=> DeclarationFileNodes.ModuleDeclaration
            Ts.SyntaxKind.NamedExports >=> DeclarationFileNodes.NamedExports
            Ts.SyntaxKind.NamedImports >=> DeclarationFileNodes.NamedImports
            Ts.SyntaxKind.NamedTupleMember >=> DeclarationFileNodes.NamedTupleMember
            Ts.SyntaxKind.NamespaceImport >=> DeclarationFileNodes.NamespaceImport
            Ts.SyntaxKind.NeverKeyword >=> DeclarationFileNodes.NeverKeyword
            Ts.SyntaxKind.NullKeyword >=> DeclarationFileNodes.NullKeyword
            Ts.SyntaxKind.NumberKeyword >=> DeclarationFileNodes.NumberKeyword
            Ts.SyntaxKind.NumericLiteral >=> DeclarationFileNodes.NumericLiteral
            Ts.SyntaxKind.ObjectKeyword >=> DeclarationFileNodes.ObjectKeyword
            Ts.SyntaxKind.OverrideKeyword >=> DeclarationFileNodes.OverrideKeyword
            Ts.SyntaxKind.Parameter >=> DeclarationFileNodes.Parameter
            Ts.SyntaxKind.ParenthesizedType >=> DeclarationFileNodes.ParenthesizedType
            Ts.SyntaxKind.PrefixUnaryExpression >=> DeclarationFileNodes.PrefixUnaryExpression
            Ts.SyntaxKind.PrivateKeyword >=> DeclarationFileNodes.PrivateKeyword
            Ts.SyntaxKind.PropertyAccessExpression >=> DeclarationFileNodes.PropertyAccessExpression
            Ts.SyntaxKind.PropertyDeclaration >=> DeclarationFileNodes.PropertyDeclaration
            Ts.SyntaxKind.PropertySignature >=> DeclarationFileNodes.PropertySignature
            Ts.SyntaxKind.ProtectedKeyword >=> DeclarationFileNodes.ProtectedKeyword
            Ts.SyntaxKind.QualifiedName >=> DeclarationFileNodes.QualifiedName
            Ts.SyntaxKind.QuestionToken >=> DeclarationFileNodes.QuestionToken
            Ts.SyntaxKind.ReadonlyKeyword >=> DeclarationFileNodes.ReadonlyKeyword
            Ts.SyntaxKind.RestType >=> DeclarationFileNodes.RestType
            Ts.SyntaxKind.SetAccessor >=> DeclarationFileNodes.SetAccessor
            Ts.SyntaxKind.StaticKeyword >=> DeclarationFileNodes.StaticKeyword
            Ts.SyntaxKind.StringKeyword >=> DeclarationFileNodes.StringKeyword
            Ts.SyntaxKind.StringLiteral >=> DeclarationFileNodes.StringLiteral
            Ts.SyntaxKind.SymbolKeyword >=> DeclarationFileNodes.SymbolKeyword
            Ts.SyntaxKind.TemplateHead >=> DeclarationFileNodes.TemplateHead
            Ts.SyntaxKind.TemplateLiteralType >=> DeclarationFileNodes.TemplateLiteralType
            Ts.SyntaxKind.TemplateLiteralTypeSpan >=> DeclarationFileNodes.TemplateLiteralTypeSpan
            Ts.SyntaxKind.TemplateMiddle >=> DeclarationFileNodes.TemplateMiddle
            Ts.SyntaxKind.TemplateTail >=> DeclarationFileNodes.TemplateTail
            Ts.SyntaxKind.ThisType >=> DeclarationFileNodes.ThisType
            Ts.SyntaxKind.TrueKeyword >=> DeclarationFileNodes.TrueKeyword
            Ts.SyntaxKind.TupleType >=> DeclarationFileNodes.TupleType
            Ts.SyntaxKind.TypeAliasDeclaration >=> DeclarationFileNodes.TypeAliasDeclaration
            Ts.SyntaxKind.TypeLiteral >=> DeclarationFileNodes.TypeLiteral
            Ts.SyntaxKind.TypeOperator >=> DeclarationFileNodes.TypeOperator
            Ts.SyntaxKind.TypeParameter >=> DeclarationFileNodes.TypeParameter
            Ts.SyntaxKind.TypePredicate >=> DeclarationFileNodes.TypePredicate
            Ts.SyntaxKind.TypeQuery >=> DeclarationFileNodes.TypeQuery
            Ts.SyntaxKind.TypeReference >=> DeclarationFileNodes.TypeReference
            Ts.SyntaxKind.UndefinedKeyword >=> DeclarationFileNodes.UndefinedKeyword
            Ts.SyntaxKind.UnionType >=> DeclarationFileNodes.UnionType
            Ts.SyntaxKind.UnknownKeyword >=> DeclarationFileNodes.UnknownKeyword
            Ts.SyntaxKind.VariableDeclaration >=> DeclarationFileNodes.VariableDeclaration
            Ts.SyntaxKind.VariableDeclarationList >=> DeclarationFileNodes.VariableDeclarationList
            Ts.SyntaxKind.VariableStatement >=> DeclarationFileNodes.VariableStatement
            Ts.SyntaxKind.VoidKeyword >=> DeclarationFileNodes.VoidKeyword
            Ts.SyntaxKind.SourceFile >=> DeclarationFileNodes.SourceFile
        ]
    let topLevelStatements: Dictionary<Ts.SyntaxKind, obj -> TopLevelStatements> =
        Dictionary [
            let inline (>=>) a b = KeyValuePair(a, unbox >> b)
            let inline (>->) a b = a, (unbox >> b)
            Ts.SyntaxKind.InterfaceDeclaration >=> TopLevelStatements.Interface
            Ts.SyntaxKind.TypeAliasDeclaration >=> TopLevelStatements.TypeAlias
            Ts.SyntaxKind.ClassDeclaration >=> TopLevelStatements.Class
            Ts.SyntaxKind.EnumDeclaration >=> TopLevelStatements.Enum
            Ts.SyntaxKind.VariableStatement >=> TopLevelStatements.Variable
            Ts.SyntaxKind.VariableDeclaration >=> TopLevelStatements.VariableDeclaration
            Ts.SyntaxKind.ModuleDeclaration >=> TopLevelStatements.Module
            Ts.SyntaxKind.FunctionDeclaration >=> TopLevelStatements.Function
            Ts.SyntaxKind.ExportDeclaration >=> TopLevelStatements.ExportDeclaration
            Ts.SyntaxKind.NamespaceExportDeclaration >=> TopLevelStatements.NamespaceExportDeclaration
            Ts.SyntaxKind.ImportEqualsDeclaration >=> TopLevelStatements.ImportEqualsDeclaration
            Ts.SyntaxKind.ImportDeclaration >=> TopLevelStatements.ImportDeclaration
            Ts.SyntaxKind.ExportAssignment >=> TopLevelStatements.ExportAssignment
        ]
    let topLevelExportDeclarations: Dictionary<Ts.SyntaxKind, obj -> TopLevelExportSymbolDeclarations> =
        Dictionary [
            let inline (>=>) a b = KeyValuePair(a, unbox >> b)
            let inline (>->) a b = a, (unbox >> b)
            Ts.SyntaxKind.NamespaceExportDeclaration >=> TopLevelExportSymbolDeclarations.NamespaceExportDeclaration
            Ts.SyntaxKind.InterfaceDeclaration >=> TopLevelExportSymbolDeclarations.Interface
            Ts.SyntaxKind.TypeAliasDeclaration >=> TopLevelExportSymbolDeclarations.TypeAlias
            Ts.SyntaxKind.ClassDeclaration >=> TopLevelExportSymbolDeclarations.Class
            Ts.SyntaxKind.EnumDeclaration >=> TopLevelExportSymbolDeclarations.Enum
            Ts.SyntaxKind.VariableDeclaration >=> TopLevelExportSymbolDeclarations.VariableDeclaration
            Ts.SyntaxKind.ModuleDeclaration >=> TopLevelExportSymbolDeclarations.Module
            Ts.SyntaxKind.FunctionDeclaration >=> TopLevelExportSymbolDeclarations.Function
            Ts.SyntaxKind.ExportDeclaration >=> TopLevelExportSymbolDeclarations.ExportDeclaration
            Ts.SyntaxKind.ImportEqualsDeclaration >=> TopLevelExportSymbolDeclarations.ImportEqualsDeclaration
            Ts.SyntaxKind.ImportDeclaration >=> TopLevelExportSymbolDeclarations.ImportDeclaration
            Ts.SyntaxKind.ExportAssignment >=> TopLevelExportSymbolDeclarations.ExportAssignment
            Ts.SyntaxKind.ExportSpecifier >=> TopLevelExportSymbolDeclarations.ExportSpecifier
            Ts.SyntaxKind.NamespaceExport >=> TopLevelExportSymbolDeclarations.NamespaceExport
        ]
    let topLevelLocalDeclarations: Dictionary<Ts.SyntaxKind, obj -> TopLevelLocalSymbolDeclarations> =
        Dictionary [
            let inline (>=>) a b = KeyValuePair(a, unbox >> b)
            let inline (>->) a b = a, (unbox >> b)
            Ts.SyntaxKind.InterfaceDeclaration >=> TopLevelLocalSymbolDeclarations.Interface
            Ts.SyntaxKind.TypeAliasDeclaration >=> TopLevelLocalSymbolDeclarations.TypeAlias
            Ts.SyntaxKind.ClassDeclaration >=> TopLevelLocalSymbolDeclarations.Class
            Ts.SyntaxKind.EnumDeclaration >=> TopLevelLocalSymbolDeclarations.Enum
            Ts.SyntaxKind.VariableDeclaration >=> TopLevelLocalSymbolDeclarations.VariableDeclaration
            Ts.SyntaxKind.ModuleDeclaration >=> TopLevelLocalSymbolDeclarations.Module
            Ts.SyntaxKind.FunctionDeclaration >=> TopLevelLocalSymbolDeclarations.Function
            Ts.SyntaxKind.ExportDeclaration >=> TopLevelLocalSymbolDeclarations.ExportDeclaration
            Ts.SyntaxKind.ImportEqualsDeclaration >=> TopLevelLocalSymbolDeclarations.ImportEqualsDeclaration
            Ts.SyntaxKind.ImportDeclaration >=> TopLevelLocalSymbolDeclarations.ImportDeclaration
            Ts.SyntaxKind.ExportAssignment >=> TopLevelLocalSymbolDeclarations.ExportAssignment
            Ts.SyntaxKind.ExportSpecifier >=> TopLevelLocalSymbolDeclarations.ExportSpecifier
            Ts.SyntaxKind.NamespaceExport >=> TopLevelLocalSymbolDeclarations.NamespaceExport
            Ts.SyntaxKind.NamespaceImport >=> TopLevelLocalSymbolDeclarations.NamespaceImport
            Ts.SyntaxKind.ImportSpecifier >=> TopLevelLocalSymbolDeclarations.ImportSpecifier
            Ts.SyntaxKind.ImportClause >=> TopLevelLocalSymbolDeclarations.ImportClause
        ]

[<RequireQualifiedAccess>]
type DeclarationFileNodes =
    // Keywords
    | AssertsKeyword of Ts.AssertsKeyword
    | AbstractKeyword of Ts.AbstractKeyword
    | AnyKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | OutKeyword of Ts.OutKeyword
    | PublicKeyword of Ts.PublicKeyword
    | InKeyword of Ts.InKeyword
    | BigIntKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | BooleanKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | DeclareKeyword of Ts.DeclareKeyword
    | DefaultKeyword of Ts.DefaultKeyword
    | ConstKeyword of Ts.ConstKeyword
    | ExportKeyword of Ts.ExportKeyword
    | IntrinsicKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | TrueKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | ReadonlyKeyword of Ts.ReadonlyKeyword
    | FalseKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | NeverKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | NullKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | NumberKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | ObjectKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | OverrideKeyword of Ts.OverrideKeyword
    | PrivateKeyword of Ts.PrivateKeyword
    | ProtectedKeyword of Ts.ProtectedKeyword
    | StaticKeyword of Ts.StaticKeyword
    | StringKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | SymbolKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | UndefinedKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | UnknownKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    | VoidKeyword of Ts.KeywordToken<Ts.SyntaxKind>
    // Other tokens
    | DotDotDotToken of Ts.DotDotDotToken
    | EndOfFileToken of Ts.EndOfFileToken
    | MinusToken of Ts.MinusToken
    | QuestionToken of Ts.QuestionToken
    // Type nodes
    | OptionalType of Ts.OptionalTypeNode
    | ArrayType of Ts.ArrayTypeNode
    | ConditionalType of Ts.ConditionalTypeNode
    | FunctionType of Ts.FunctionTypeNode
    | ConstructorType of Ts.ConstructorTypeNode
    | IndexedAccessType of Ts.IndexedAccessTypeNode
    | InferType of Ts.InferTypeNode
    | IntersectionType of Ts.IntersectionTypeNode
    | LiteralType of Ts.LiteralTypeNode
    | MappedType of Ts.MappedTypeNode
    | ParenthesizedType of Ts.ParenthesizedTypeNode
    | ThisType of Ts.ThisTypeNode
    | TupleType of Ts.TupleTypeNode
    | TypeLiteral of Ts.TypeLiteralNode
    | TypeOperator of Ts.TypeOperatorNode
    | TypePredicate of Ts.TypePredicateNode
    | TemplateLiteralType of Ts.TemplateLiteralTypeNode
    | TypeQuery of Ts.TypeQueryNode
    | TypeReference of Ts.TypeReferenceNode
    | UnionType of Ts.UnionTypeNode
    // Literals
    | StringLiteral of Ts.StringLiteral
    | NoSubstitutionTemplateLiteral of Ts.NoSubstitutionTemplateLiteral
    | ObjectBindingPattern of Ts.ObjectBindingPattern
    | ArrayBindingPattern of Ts.ArrayBindingPattern
    | BindingElement of Ts.BindingElement
    // Top Declarations
    | ClassDeclaration of Ts.ClassDeclaration
    | EnumDeclaration of Ts.EnumDeclaration
    | FunctionDeclaration of Ts.FunctionDeclaration
    | InterfaceDeclaration of Ts.InterfaceDeclaration
    | TypeAliasDeclaration of Ts.TypeAliasDeclaration
    | VariableDeclaration of Ts.VariableDeclaration
    | VariableDeclarationList of Ts.VariableDeclarationList
    | VariableStatement of Ts.VariableStatement
    | ModuleDeclaration of Ts.ModuleDeclaration
    // Members
    | CallSignature of Ts.CallSignatureDeclaration
    | ConstructSignature of Ts.ConstructSignatureDeclaration
    | Constructor of Ts.ConstructorDeclaration
    | EnumMember of Ts.EnumMember
    | GetAccessor of Ts.GetAccessorDeclaration
    | TypeParameter of Ts.TypeParameterDeclaration
    | IndexSignature of Ts.IndexSignatureDeclaration
    | MethodDeclaration of Ts.MethodDeclaration
    | MethodSignature of Ts.MethodSignature
    | ModuleBlock of Ts.ModuleBlock
    | NumericLiteral of Ts.NumericLiteral
    | ComputedPropertyName of Ts.ComputedPropertyName
    | NamedTupleMember of Ts.NamedTupleMember
    | Parameter of Ts.ParameterDeclaration
    | HeritageClause of Ts.HeritageClause
    | PrefixUnaryExpression of Ts.PrefixUnaryExpression
    | PropertyAccessExpression of Ts.PropertyAccessExpression
    | PropertyDeclaration of Ts.PropertyDeclaration
    | PropertySignature of Ts.PropertySignature
    | RestType of Ts.RestTypeNode
    | SetAccessor of Ts.SetAccessorDeclaration
    | TemplateHead of Ts.TemplateHead
    | TemplateLiteralTypeSpan of Ts.TemplateLiteralTypeSpan
    | TemplateMiddle of Ts.TemplateMiddle
    | TemplateTail of Ts.TemplateTail
    | BigIntLiteral of Ts.BigIntLiteral
    | ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments
    
    | PrivateIdentifier of Ts.PrivateIdentifier
    | Identifier of Ts.Identifier
    | QualifiedName of Ts.QualifiedName
    
    | ExternalModuleReference of Ts.ExternalModuleReference
    | ExportAssignment of Ts.ExportAssignment
    | ExportDeclaration of Ts.ExportDeclaration
    | ExportSpecifier of Ts.ExportSpecifier
    | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration
    | ImportClause of Ts.ImportClause
    | ImportDeclaration of Ts.ImportDeclaration
    | ImportSpecifier of Ts.ImportSpecifier
    | NamespaceImport of Ts.NamespaceImport
    | NamedExports of Ts.NamedExports
    | NamedImports of Ts.NamedImports
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    | ImportType of Ts.ImportTypeNode
    | NamespaceExport of Ts.NamespaceExport
    | SourceFile of Ts.SourceFile

[<RequireQualifiedAccess>]
type TopLevelStatements =
    | Interface of Ts.InterfaceDeclaration
    | TypeAlias of Ts.TypeAliasDeclaration
    | Class of Ts.ClassDeclaration
    | Enum of Ts.EnumDeclaration
    | Variable of Ts.VariableStatement
    | VariableDeclaration of Ts.VariableDeclaration
    | Function of Ts.FunctionDeclaration
    | ExportDeclaration of Ts.ExportDeclaration
    | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration
    | ImportDeclaration of Ts.ImportDeclaration
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    | ExportAssignment of Ts.ExportAssignment
    | Module of Ts.ModuleDeclaration

[<RequireQualifiedAccess>]
type TopLevelExportSymbolDeclarations =
    | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration
    | Interface of Ts.InterfaceDeclaration
    | TypeAlias of Ts.TypeAliasDeclaration
    | Class of Ts.ClassDeclaration
    | Enum of Ts.EnumDeclaration
    | VariableDeclaration of Ts.VariableDeclaration
    | Function of Ts.FunctionDeclaration
    | ExportDeclaration of Ts.ExportDeclaration
    | ImportDeclaration of Ts.ImportDeclaration
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    | ExportAssignment of Ts.ExportAssignment
    | ExportSpecifier of Ts.ExportSpecifier
    | Module of Ts.ModuleDeclaration
    | NamespaceExport of Ts.NamespaceExport

[<RequireQualifiedAccess>]
type TopLevelLocalSymbolDeclarations =
    | Interface of Ts.InterfaceDeclaration
    | TypeAlias of Ts.TypeAliasDeclaration
    | Class of Ts.ClassDeclaration
    | Enum of Ts.EnumDeclaration
    | VariableDeclaration of Ts.VariableDeclaration
    | Function of Ts.FunctionDeclaration
    | ExportDeclaration of Ts.ExportDeclaration
    | ImportDeclaration of Ts.ImportDeclaration
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    | ExportAssignment of Ts.ExportAssignment
    | ExportSpecifier of Ts.ExportSpecifier
    | Module of Ts.ModuleDeclaration
    | NamespaceExport of Ts.NamespaceExport
    | ImportSpecifier of Ts.ImportSpecifier
    | NamespaceImport of Ts.NamespaceImport
    | ImportClause of Ts.ImportClause

type TopLevelStatements with
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    /// <summary>Maps a source-file top-level statement <c>Ts.Node</c> to its <c>TopLevelStatements</c> case.</summary>
    /// <remarks>
    /// Partial: throws on an unmapped <c>kind</c>. Totality over real top-level statements is proven by
    /// <b>XTK-7</b> (Program.test.fs); guard with
    /// <see cref="M:TypeScript.TopLevelStatements.IsTopLevelStatementKind(TypeScript.Ts.Node)"/> first.
    /// </remarks>
    static member Create(decl: Ts.Node) = Internal.topLevelStatements[decl.kind] decl
    /// <summary>True when <paramref name="decl"/> is a top-level statement kind that <c>TopLevelStatements.Create</c> can map.</summary>
    /// <remarks>Proof <b>XTK-7</b> (Program.test.fs) asserts this returns <c>true</c> for every top-level statement in the corpus.</remarks>
    static member IsTopLevelStatementKind(decl: Ts.Node) = Internal.topLevelStatements.ContainsKey decl.kind
type TopLevelExportSymbolDeclarations with
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    /// <summary>Maps an exported-symbol declaration <c>Ts.Node</c> to its <c>TopLevelExportSymbolDeclarations</c> case.</summary>
    /// <remarks>
    /// Partial: throws on an unmapped <c>kind</c>. Totality over real exported-symbol declarations is proven by
    /// <b>XTK-8</b> (Program.test.fs); guard with
    /// <see cref="M:TypeScript.TopLevelExportSymbolDeclarations.IsTopLevelExportDeclarationKind(TypeScript.Ts.Node)"/> first.
    /// </remarks>
    static member Create(decl: Ts.Node) = Internal.topLevelExportDeclarations[decl.kind] decl
    /// <summary>True when <paramref name="decl"/> is an export declaration kind that <c>TopLevelExportSymbolDeclarations.Create</c> can map.</summary>
    /// <remarks>Proof <b>XTK-8</b> (Program.test.fs) asserts this returns <c>true</c> for every exported-symbol declaration in the corpus.</remarks>
    static member IsTopLevelExportDeclarationKind(decl: Ts.Node) = Internal.topLevelExportDeclarations.ContainsKey decl.kind

type TopLevelLocalSymbolDeclarations with
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    /// <summary>Maps a local-symbol declaration <c>Ts.Node</c> to its <c>TopLevelLocalSymbolDeclarations</c> case.</summary>
    /// <remarks>
    /// Partial: throws on an unmapped <c>kind</c>. Totality over real local-symbol declarations is proven by
    /// <b>XTK-9</b> (Program.test.fs); guard with
    /// <see cref="M:TypeScript.TopLevelLocalSymbolDeclarations.IsTopLevelLocalDeclarationKind(TypeScript.Ts.Node)"/> first.
    /// </remarks>
    static member Create(decl: Ts.Node) = Internal.topLevelLocalDeclarations[decl.kind] decl
    /// <summary>True when <paramref name="decl"/> is a local declaration kind that <c>TopLevelLocalSymbolDeclarations.Create</c> can map.</summary>
    /// <remarks>Proof <b>XTK-9</b> (Program.test.fs) asserts this returns <c>true</c> for every local-symbol declaration in the corpus.</remarks>
    static member IsTopLevelLocalDeclarationKind(decl: Ts.Node) = Internal.topLevelLocalDeclarations.ContainsKey decl.kind

type Ts.Type with
    [<EmitProperty "checker">]
    member inline this.checker: Ts.TypeChecker = jsNative
    /// <summary>
    /// Retrieves the <c>symbol</c> field of the type (or falls back to the <c>aliasSymbol</c> field if present).
    /// and traverses aliases/merges to the canonical symbol for the type. Throws if no symbol is present.
    /// </summary>
    /// <seealso cref="M:Xantham.TypeScript.Misc.getCanonicalSymbol"/>
    member this.unsafeGetCanonicalSymbol() =
        let checker = this.checker
        let symbol =
            this.getSymbol()
            |> Option.orElse this.aliasSymbol
            |> Option.defaultWith (fun () ->
                [
                    "Attempted unsafe canonical symbol retrieval on a non-symbol type."
                    sprintf "Type flags: %A" <| this.flags.ToStringArray()
                    if this.flags.HasFlag Ts.TypeFlags.Object then
                        sprintf "Object flags: %A" <| (this :?> Ts.ObjectType).objectFlags.ToStringArray()
                    sprintf "Raw type: %A" (Utils.inspectTo 0 this)
                ]
                |> String.concat "\n"
                |> failwith
                )
        if symbol.flags.HasFlag Ts.SymbolFlags.Alias then
            checker.getAliasedSymbol symbol
        else symbol
        |> checker.getMergedSymbol
    /// <summary>
    /// Retrieves the <c>symbol</c> field of the type (or falls back to the <c>aliasSymbol</c> field if present).
    /// Follows any aliases/merges to the canonical symbol for the type. Returns <c>None</c> if no symbol is present.
    /// </summary>
    member this.getCanonicalSymbol() =
        if this.getSymbol().IsSome || this.aliasSymbol.IsSome
        then this.unsafeGetCanonicalSymbol() |> Some
        else None
    /// <summary>
    /// Retrieves the <c>symbol</c> field of the type, following any merges/aliases to the canonical symbol, without
    /// falling back to the alias symbol. Throws if no symbol is present.
    /// </summary>
    member this.unsafeGetNonAliasSymbol() =
        let checker = this.checker
        let symbol =
            this.getSymbol()
            |> Option.defaultWith (fun () ->
                [
                    "Attempted unsafe canonical symbol retrieval on a non-symbol type."
                    sprintf "Type flags: %A" <| this.flags.ToStringArray()
                    if this.flags.HasFlag Ts.TypeFlags.Object then
                        sprintf "Object flags: %A" <| (this :?> Ts.ObjectType).objectFlags.ToStringArray()
                    sprintf "Raw type: %A" (Utils.inspectTo 0 this)
                ]
                |> String.concat "\n"
                |> failwith
                )
        if symbol.flags.HasFlag Ts.SymbolFlags.Alias then
            checker.getAliasedSymbol symbol
        else symbol
        |> checker.getMergedSymbol
    /// <summary>
    /// Retrieves the <c>symbol</c> field of the type, following any merges/aliases to the canonical symbol, without
    /// falling back to the alias symbol. Returns <c>None</c> if no symbol is present.
    /// </summary>
    member this.getNonAliasSymbol() =
        if this.getSymbol().IsSome
        then this.unsafeGetNonAliasSymbol() |> Some
        else None

