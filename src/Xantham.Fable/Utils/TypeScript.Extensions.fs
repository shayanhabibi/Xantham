[<AutoOpen>]
module TypeScriptExtensions

open Fable.Core
open TypeScript
open Fable.Core.JsInterop
open Xantham


type Ts.Type with
    [<EmitProperty "id">]
    member inline this.TypeKey: TypeKey = jsNative
    
type HasTypeArguments =
    | CallExpression of Ts.CallExpression
    | NewExpression of Ts.NewExpression
    | TaggedTemplateExpression of Ts.TaggedTemplateExpression
    | JsxOpeningElement of Ts.JsxOpeningElement
    | JsxSelfClosingElement of Ts.JsxSelfClosingElement

type HasExpressionInitializer =
    | VariableDeclaration of Ts.VariableDeclaration
    | ParameterDeclaration of Ts.ParameterDeclaration
    | BindingElement of Ts.BindingElement
    | PropertyDeclaration of Ts.PropertyDeclaration
    | PropertyAssignment of Ts.PropertyAssignment
    | EnumMember of Ts.EnumMember
    
type HasInitializer =
    | HasExpressionInitializer of HasExpressionInitializer
    | ForStatement of Ts.ForStatement
    | ForInStatement of Ts.ForInStatement
    | ForOfStatement of Ts.ForOfStatement
    | JsxAttribute of Ts.JsxAttribute

type HasDecorators =
    | ParameterDeclaration of Ts.ParameterDeclaration
    | PropertyDeclaration of Ts.PropertyDeclaration
    | MethodDeclaration of Ts.MethodDeclaration
    | GetAccessorDeclaration of Ts.GetAccessorDeclaration
    | SetAccessorDeclaration of Ts.SetAccessorDeclaration
    | ClassExpression of Ts.ClassExpression
    | ClassDeclaration of Ts.ClassDeclaration

type AssertionExpression =
    | TypeAssertion of Ts.TypeAssertion
    | AsExpression of Ts.AsExpression

type HasType =
    | SignatureDeclaration of Ts.SignatureDeclaration
    | VariableDeclaration of Ts.VariableDeclaration
    | ParameterDeclaration of Ts.ParameterDeclaration
    | PropertySignature of Ts.PropertySignature
    | PropertyDeclaration of Ts.PropertyDeclaration
    | TypePredicateNode of Ts.TypePredicateNode
    | ParenthesizedTypeNode of Ts.ParenthesizedTypeNode
    | TypeOperatorNode of Ts.TypeOperatorNode
    | MappedTypeNode of Ts.MappedTypeNode
    | AssertionExpression of AssertionExpression
    | TypeAliasDeclaration of Ts.TypeAliasDeclaration
    | JSDocTypeExpression of Ts.JSDocTypeExpression
    | JSDocNonNullableType of Ts.JSDocNonNullableType
    | JSDocNullableType of Ts.JSDocNullableType
    | JSDocOptionalType of Ts.JSDocOptionalType
    | JSDocVariadicType of Ts.JSDocVariadicType

type AccessorDeclaration =
    | GetAccessorDeclaration of Ts.GetAccessorDeclaration
    | SetAccessorDeclaration of Ts.SetAccessorDeclaration

type ClassLikeDeclaration =
    | ClassDeclaration of Ts.ClassDeclaration
    | ClassExpression of Ts.ClassExpression

type ModifierLike =
    | Modifier of Ts.Modifier
    | Decorator of Ts.Decorator

type EntityName =
    | Identifier of Ts.Identifier
    | QualifiedName of Ts.QualifiedName

type PropertyName =
    | Identifier of Ts.Identifier
    | StringLiteral of Ts.StringLiteral
    | NoSubtitutionTemplateLiteral of Ts.NoSubstitutionTemplateLiteral
    | NumericLiteral of Ts.NumericLiteral
    | ComputedPropertyName of Ts.ComputedPropertyName
    | PrivateIdentifier of Ts.PrivateIdentifier
    | BigIntLiteral of Ts.BigIntLiteral

type PropertyNameLiteral =
    | Identifier of Ts.Identifier
    | StringLiteral of Ts.StringLiteral
    | NumericLiteral of Ts.NumericLiteral
    | JsxNamespacedName of Ts.JsxNamespacedName
    | BigIntLiteral of Ts.BigIntLiteral

type BooleanLiteral =
    | TrueLiteral of Ts.TrueLiteral
    | FalseLiteral of Ts.FalseLiteral

type TypeVariable =
    | TypeParameter of Ts.TypeParameter
    | IndexedAccessType of Ts.IndexedAccessType

type BaseType =
    | ObjectType of Ts.ObjectType
    | IntersectionType of Ts.IntersectionType
    | TypeVariable of TypeVariable
type StructuredType =
    | ObjectType of Ts.ObjectType
    | UnionType of Ts.UnionType
    | IntersectionType of Ts.IntersectionType
type ObjectTypeDeclaration =
    | ClassLikeDeclaration of ClassLikeDeclaration
    | InterfaceDeclaration of Ts.InterfaceDeclaration
    | TypeLiteralNode of Ts.TypeLiteralNode

type TsModuleName =
    | Identifier of Ts.Identifier
    | StringLiteral of Ts.StringLiteral

type JSDocComment =
    | JSDocText of Ts.JSDocText
    | JSDocLink of Ts.JSDocLink
    | JSDocLinkCode of Ts.JSDocLinkCode
    | JSDocLinkPlain of Ts.JSDocLinkPlain

type MemberName =
    | Identifier of Ts.Identifier
    | PrivateIdentifier of Ts.PrivateIdentifier

type JsxAttributeName =
    | Identifier of Ts.Identifier
    | JsxNamespacedName of Ts.JsxNamespacedName
type StringLiteralLike =
    | StringLiteral of Ts.StringLiteral
    | NoSubstitutionTemplateLiteral of Ts.NoSubstitutionTemplateLiteral
type BindingPattern =
    | ObjectBindingPattern of Ts.ObjectBindingPattern
    | ArrayBindingPattern of Ts.ArrayBindingPattern
type BindingName =
    | Identifier of Ts.Identifier
    | BindingPattern of BindingPattern
type ObjectLiteralElementLike =
    | PropertyAssignment of Ts.PropertyAssignment
    | ShorthandPropertyAssignment of Ts.ShorthandPropertyAssignment
    | SpreadAssignment of Ts.SpreadAssignment
    | MethodDeclaration of Ts.MethodDeclaration
    | AccessorDeclaration of AccessorDeclaration
type FunctionLikeDeclaration =
    | FunctionDeclaration of Ts.FunctionDeclaration
    | MethodDeclaration of Ts.MethodDeclaration
    | AccessorDeclaration of AccessorDeclaration
    | ConstructorDeclaration of Ts.ConstructorDeclaration
    | FunctionExpression of Ts.FunctionExpression
    | ArrowFunction of Ts.ArrowFunction
type FunctionOrConstructorTypeNode =
    | FunctionTypeNode of Ts.FunctionTypeNode
    | ConstructorTypeNode of Ts.ConstructorTypeNode
type TypeReferenceType =
    | TypeReferenceNode of Ts.TypeReferenceNode
    | ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments
type UnionOrIntersectionTypeNode =
    | UnionTypeNode of Ts.UnionTypeNode
    | IntersectionTypeNode of Ts.IntersectionTypeNode
type EntityNameExpression =
    | Identifier of Ts.Identifier
    | PropertyAccessEntityNameExpression of Ts.PropertyAccessEntityNameExpression
type DeclarationName =
    | PropertyName of PropertyName
    | JsxAttributeName of JsxAttributeName
    | StringLiteralLike of StringLiteralLike
    | ElementAccessExpression of Ts.ElementAccessExpression
    | BindingPattern of BindingPattern
    | EntityNameExpression of EntityNameExpression

type HasJSDoc =
    | AccessorDeclaration of AccessorDeclaration
    | ArrowFunction of Ts.ArrowFunction
    | BinaryExpression of Ts.BinaryExpression
    | Block of Ts.Block
    | BreakStatement of Ts.BreakStatement
    | CallSignatureDeclaration of Ts.CallSignatureDeclaration
    | CaseClause of Ts.CaseClause
    | ClassLikeDeclaration of ClassLikeDeclaration
    | ClassStaticBlockDeclaration of Ts.ClassStaticBlockDeclaration
    | ConstructorDeclaration of Ts.ConstructorDeclaration
    | ConstructorTypeNode of Ts.ConstructorTypeNode
    | ConstructSignatureDeclaration of Ts.ConstructSignatureDeclaration
    | ContinueStatement of Ts.ContinueStatement
    | DebuggerStatement of Ts.DebuggerStatement
    | DoStatement of Ts.DoStatement
    | ElementAccessExpression of Ts.ElementAccessExpression
    | EmptyStatement of Ts.EmptyStatement
    | EndOfFileToken of Ts.EndOfFileToken
    | EnumDeclaration of Ts.EnumDeclaration
    | EnumMember of Ts.EnumMember
    | ExportAssignment of Ts.ExportAssignment
    | ExportDeclaration of Ts.ExportDeclaration
    | ExportSpecifier of Ts.ExportSpecifier
    | ExpressionStatement of Ts.ExpressionStatement
    | ForInStatement of Ts.ForInStatement
    | ForOfStatement of Ts.ForOfStatement
    | ForStatement of Ts.ForStatement
    | FunctionDeclaration of Ts.FunctionDeclaration
    | FunctionExpression of Ts.FunctionExpression
    | FunctionTypeNode of Ts.FunctionTypeNode
    | Identifier of Ts.Identifier
    | IfStatement of Ts.IfStatement
    | ImportDeclaration of Ts.ImportDeclaration
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    | IndexSignatureDeclaration of Ts.IndexSignatureDeclaration
    | InterfaceDeclaration of Ts.InterfaceDeclaration
    | JSDocFunctionType of Ts.JSDocFunctionType
    | JSDocSignature of Ts.JSDocSignature
    | LabeledStatement of Ts.LabeledStatement
    | MethodDeclaration of Ts.MethodDeclaration
    | MethodSignature of Ts.MethodSignature
    | ModuleDeclaration of Ts.ModuleDeclaration
    | NamedTupleMember of Ts.NamedTupleMember
    | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration
    | ObjectLiteralExpression of Ts.ObjectLiteralExpression
    | ParameterDeclaration of Ts.ParameterDeclaration
    | ParenthesizedExpression of Ts.ParenthesizedExpression
    | PropertyAccessExpression of Ts.PropertyAccessExpression
    | PropertyAssignment of Ts.PropertyAssignment
    | PropertyDeclaration of Ts.PropertyDeclaration
    | PropertySignature of Ts.PropertySignature
    | ReturnStatement of Ts.ReturnStatement
    | SemicolonClassElement of Ts.SemicolonClassElement
    | ShorthandPropertyAssignment of Ts.ShorthandPropertyAssignment
    | SpreadAssignment of Ts.SpreadAssignment
    | SwitchStatement of Ts.SwitchStatement
    | ThrowStatement of Ts.ThrowStatement
    | TryStatement of Ts.TryStatement
    | TypeAliasDeclaration of Ts.TypeAliasDeclaration
    | TypeParameterDeclaration of Ts.TypeParameterDeclaration
    | VariableDeclaration of Ts.VariableDeclaration
    | VariableStatement of Ts.VariableStatement
    | WhileStatement of Ts.WhileStatement
    | WithStatement of Ts.WithStatement
    
type Ts.SyntaxKind with
    /// String of Enum
    member this.Name =
        match this with
        | Ts.SyntaxKind.Unknown -> "Unknown"
        | Ts.SyntaxKind.EndOfFileToken -> "EndOfFileToken"
        | Ts.SyntaxKind.SingleLineCommentTrivia -> "SingleLineCommentTrivia"
        | Ts.SyntaxKind.MultiLineCommentTrivia -> "MultiLineCommentTrivia"
        | Ts.SyntaxKind.NewLineTrivia -> "NewLineTrivia"
        | Ts.SyntaxKind.WhitespaceTrivia -> "WhitespaceTrivia"
        | Ts.SyntaxKind.ShebangTrivia -> "ShebangTrivia"
        | Ts.SyntaxKind.ConflictMarkerTrivia -> "ConflictMarkerTrivia"
        | Ts.SyntaxKind.NonTextFileMarkerTrivia -> "NonTextFileMarkerTrivia"
        | Ts.SyntaxKind.NumericLiteral -> "NumericLiteral"
        | Ts.SyntaxKind.BigIntLiteral -> "BigIntLiteral"
        | Ts.SyntaxKind.StringLiteral -> "StringLiteral"
        | Ts.SyntaxKind.JsxText -> "JsxText"
        | Ts.SyntaxKind.JsxTextAllWhiteSpaces -> "JsxTextAllWhiteSpaces"
        | Ts.SyntaxKind.RegularExpressionLiteral -> "RegularExpressionLiteral"
        | Ts.SyntaxKind.NoSubstitutionTemplateLiteral -> "NoSubstitutionTemplateLiteral"
        | Ts.SyntaxKind.TemplateHead -> "TemplateHead"
        | Ts.SyntaxKind.TemplateMiddle -> "TemplateMiddle"
        | Ts.SyntaxKind.TemplateTail -> "TemplateTail"
        | Ts.SyntaxKind.OpenBraceToken -> "OpenBraceToken"
        | Ts.SyntaxKind.CloseBraceToken -> "CloseBraceToken"
        | Ts.SyntaxKind.OpenParenToken -> "OpenParenToken"
        | Ts.SyntaxKind.CloseParenToken -> "CloseParenToken"
        | Ts.SyntaxKind.OpenBracketToken -> "OpenBracketToken"
        | Ts.SyntaxKind.CloseBracketToken -> "CloseBracketToken"
        | Ts.SyntaxKind.DotToken -> "DotToken"
        | Ts.SyntaxKind.DotDotDotToken -> "DotDotDotToken"
        | Ts.SyntaxKind.SemicolonToken -> "SemicolonToken"
        | Ts.SyntaxKind.CommaToken -> "CommaToken"
        | Ts.SyntaxKind.QuestionDotToken -> "QuestionDotToken"
        | Ts.SyntaxKind.LessThanToken -> "LessThanToken"
        | Ts.SyntaxKind.LessThanSlashToken -> "LessThanSlashToken"
        | Ts.SyntaxKind.GreaterThanToken -> "GreaterThanToken"
        | Ts.SyntaxKind.LessThanEqualsToken -> "LessThanEqualsToken"
        | Ts.SyntaxKind.GreaterThanEqualsToken -> "GreaterThanEqualsToken"
        | Ts.SyntaxKind.EqualsEqualsToken -> "EqualsEqualsToken"
        | Ts.SyntaxKind.ExclamationEqualsToken -> "ExclamationEqualsToken"
        | Ts.SyntaxKind.EqualsEqualsEqualsToken -> "EqualsEqualsEqualsToken"
        | Ts.SyntaxKind.ExclamationEqualsEqualsToken -> "ExclamationEqualsEqualsToken"
        | Ts.SyntaxKind.EqualsGreaterThanToken -> "EqualsGreaterThanToken"
        | Ts.SyntaxKind.PlusToken -> "PlusToken"
        | Ts.SyntaxKind.MinusToken -> "MinusToken"
        | Ts.SyntaxKind.AsteriskToken -> "AsteriskToken"
        | Ts.SyntaxKind.AsteriskAsteriskToken -> "AsteriskAsteriskToken"
        | Ts.SyntaxKind.SlashToken -> "SlashToken"
        | Ts.SyntaxKind.PercentToken -> "PercentToken"
        | Ts.SyntaxKind.PlusPlusToken -> "PlusPlusToken"
        | Ts.SyntaxKind.MinusMinusToken -> "MinusMinusToken"
        | Ts.SyntaxKind.LessThanLessThanToken -> "LessThanLessThanToken"
        | Ts.SyntaxKind.GreaterThanGreaterThanToken -> "GreaterThanGreaterThanToken"
        | Ts.SyntaxKind.GreaterThanGreaterThanGreaterThanToken ->
            "GreaterThanGreaterThanGreaterThanToken"
        | Ts.SyntaxKind.AmpersandToken -> "AmpersandToken"
        | Ts.SyntaxKind.BarToken -> "BarToken"
        | Ts.SyntaxKind.CaretToken -> "CaretToken"
        | Ts.SyntaxKind.ExclamationToken -> "ExclamationToken"
        | Ts.SyntaxKind.TildeToken -> "TildeToken"
        | Ts.SyntaxKind.AmpersandAmpersandToken -> "AmpersandAmpersandToken"
        | Ts.SyntaxKind.BarBarToken -> "BarBarToken"
        | Ts.SyntaxKind.QuestionToken -> "QuestionToken"
        | Ts.SyntaxKind.ColonToken -> "ColonToken"
        | Ts.SyntaxKind.AtToken -> "AtToken"
        | Ts.SyntaxKind.QuestionQuestionToken -> "QuestionQuestionToken"
        | Ts.SyntaxKind.BacktickToken -> "BacktickToken"
        | Ts.SyntaxKind.HashToken -> "HashToken"
        | Ts.SyntaxKind.EqualsToken -> "EqualsToken"
        | Ts.SyntaxKind.PlusEqualsToken -> "PlusEqualsToken"
        | Ts.SyntaxKind.MinusEqualsToken -> "MinusEqualsToken"
        | Ts.SyntaxKind.AsteriskEqualsToken -> "AsteriskEqualsToken"
        | Ts.SyntaxKind.AsteriskAsteriskEqualsToken -> "AsteriskAsteriskEqualsToken"
        | Ts.SyntaxKind.SlashEqualsToken -> "SlashEqualsToken"
        | Ts.SyntaxKind.PercentEqualsToken -> "PercentEqualsToken"
        | Ts.SyntaxKind.LessThanLessThanEqualsToken -> "LessThanLessThanEqualsToken"
        | Ts.SyntaxKind.GreaterThanGreaterThanEqualsToken -> "GreaterThanGreaterThanEqualsToken"
        | Ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken ->
            "GreaterThanGreaterThanGreaterThanEqualsToken"
        | Ts.SyntaxKind.AmpersandEqualsToken -> "AmpersandEqualsToken"
        | Ts.SyntaxKind.BarEqualsToken -> "BarEqualsToken"
        | Ts.SyntaxKind.BarBarEqualsToken -> "BarBarEqualsToken"
        | Ts.SyntaxKind.AmpersandAmpersandEqualsToken -> "AmpersandAmpersandEqualsToken"
        | Ts.SyntaxKind.QuestionQuestionEqualsToken -> "QuestionQuestionEqualsToken"
        | Ts.SyntaxKind.CaretEqualsToken -> "CaretEqualsToken"
        | Ts.SyntaxKind.Identifier -> "Identifier"
        | Ts.SyntaxKind.PrivateIdentifier -> "PrivateIdentifier"
        | Ts.SyntaxKind.BreakKeyword -> "BreakKeyword"
        | Ts.SyntaxKind.CaseKeyword -> "CaseKeyword"
        | Ts.SyntaxKind.CatchKeyword -> "CatchKeyword"
        | Ts.SyntaxKind.ClassKeyword -> "ClassKeyword"
        | Ts.SyntaxKind.ConstKeyword -> "ConstKeyword"
        | Ts.SyntaxKind.ContinueKeyword -> "ContinueKeyword"
        | Ts.SyntaxKind.DebuggerKeyword -> "DebuggerKeyword"
        | Ts.SyntaxKind.DefaultKeyword -> "DefaultKeyword"
        | Ts.SyntaxKind.DeleteKeyword -> "DeleteKeyword"
        | Ts.SyntaxKind.DoKeyword -> "DoKeyword"
        | Ts.SyntaxKind.ElseKeyword -> "ElseKeyword"
        | Ts.SyntaxKind.EnumKeyword -> "EnumKeyword"
        | Ts.SyntaxKind.ExportKeyword -> "ExportKeyword"
        | Ts.SyntaxKind.ExtendsKeyword -> "ExtendsKeyword"
        | Ts.SyntaxKind.FalseKeyword -> "FalseKeyword"
        | Ts.SyntaxKind.FinallyKeyword -> "FinallyKeyword"
        | Ts.SyntaxKind.ForKeyword -> "ForKeyword"
        | Ts.SyntaxKind.FunctionKeyword -> "FunctionKeyword"
        | Ts.SyntaxKind.IfKeyword -> "IfKeyword"
        | Ts.SyntaxKind.ImportKeyword -> "ImportKeyword"
        | Ts.SyntaxKind.InKeyword -> "InKeyword"
        | Ts.SyntaxKind.InstanceOfKeyword -> "InstanceOfKeyword"
        | Ts.SyntaxKind.NewKeyword -> "NewKeyword"
        | Ts.SyntaxKind.NullKeyword -> "NullKeyword"
        | Ts.SyntaxKind.ReturnKeyword -> "ReturnKeyword"
        | Ts.SyntaxKind.SuperKeyword -> "SuperKeyword"
        | Ts.SyntaxKind.SwitchKeyword -> "SwitchKeyword"
        | Ts.SyntaxKind.ThisKeyword -> "ThisKeyword"
        | Ts.SyntaxKind.ThrowKeyword -> "ThrowKeyword"
        | Ts.SyntaxKind.TrueKeyword -> "TrueKeyword"
        | Ts.SyntaxKind.TryKeyword -> "TryKeyword"
        | Ts.SyntaxKind.TypeOfKeyword -> "TypeOfKeyword"
        | Ts.SyntaxKind.VarKeyword -> "VarKeyword"
        | Ts.SyntaxKind.VoidKeyword -> "VoidKeyword"
        | Ts.SyntaxKind.WhileKeyword -> "WhileKeyword"
        | Ts.SyntaxKind.WithKeyword -> "WithKeyword"
        | Ts.SyntaxKind.ImplementsKeyword -> "ImplementsKeyword"
        | Ts.SyntaxKind.InterfaceKeyword -> "InterfaceKeyword"
        | Ts.SyntaxKind.LetKeyword -> "LetKeyword"
        | Ts.SyntaxKind.PackageKeyword -> "PackageKeyword"
        | Ts.SyntaxKind.PrivateKeyword -> "PrivateKeyword"
        | Ts.SyntaxKind.ProtectedKeyword -> "ProtectedKeyword"
        | Ts.SyntaxKind.PublicKeyword -> "PublicKeyword"
        | Ts.SyntaxKind.StaticKeyword -> "StaticKeyword"
        | Ts.SyntaxKind.YieldKeyword -> "YieldKeyword"
        | Ts.SyntaxKind.AbstractKeyword -> "AbstractKeyword"
        | Ts.SyntaxKind.AccessorKeyword -> "AccessorKeyword"
        | Ts.SyntaxKind.AsKeyword -> "AsKeyword"
        | Ts.SyntaxKind.AssertsKeyword -> "AssertsKeyword"
        | Ts.SyntaxKind.AssertKeyword -> "AssertKeyword"
        | Ts.SyntaxKind.AnyKeyword -> "AnyKeyword"
        | Ts.SyntaxKind.AsyncKeyword -> "AsyncKeyword"
        | Ts.SyntaxKind.AwaitKeyword -> "AwaitKeyword"
        | Ts.SyntaxKind.BooleanKeyword -> "BooleanKeyword"
        | Ts.SyntaxKind.ConstructorKeyword -> "ConstructorKeyword"
        | Ts.SyntaxKind.DeclareKeyword -> "DeclareKeyword"
        | Ts.SyntaxKind.GetKeyword -> "GetKeyword"
        | Ts.SyntaxKind.InferKeyword -> "InferKeyword"
        | Ts.SyntaxKind.IntrinsicKeyword -> "IntrinsicKeyword"
        | Ts.SyntaxKind.IsKeyword -> "IsKeyword"
        | Ts.SyntaxKind.KeyOfKeyword -> "KeyOfKeyword"
        | Ts.SyntaxKind.ModuleKeyword -> "ModuleKeyword"
        | Ts.SyntaxKind.NamespaceKeyword -> "NamespaceKeyword"
        | Ts.SyntaxKind.NeverKeyword -> "NeverKeyword"
        | Ts.SyntaxKind.OutKeyword -> "OutKeyword"
        | Ts.SyntaxKind.ReadonlyKeyword -> "ReadonlyKeyword"
        | Ts.SyntaxKind.RequireKeyword -> "RequireKeyword"
        | Ts.SyntaxKind.NumberKeyword -> "NumberKeyword"
        | Ts.SyntaxKind.ObjectKeyword -> "ObjectKeyword"
        | Ts.SyntaxKind.SatisfiesKeyword -> "SatisfiesKeyword"
        | Ts.SyntaxKind.SetKeyword -> "SetKeyword"
        | Ts.SyntaxKind.StringKeyword -> "StringKeyword"
        | Ts.SyntaxKind.SymbolKeyword -> "SymbolKeyword"
        | Ts.SyntaxKind.TypeKeyword -> "TypeKeyword"
        | Ts.SyntaxKind.UndefinedKeyword -> "UndefinedKeyword"
        | Ts.SyntaxKind.UniqueKeyword -> "UniqueKeyword"
        | Ts.SyntaxKind.UnknownKeyword -> "UnknownKeyword"
        | Ts.SyntaxKind.UsingKeyword -> "UsingKeyword"
        | Ts.SyntaxKind.FromKeyword -> "FromKeyword"
        | Ts.SyntaxKind.GlobalKeyword -> "GlobalKeyword"
        | Ts.SyntaxKind.BigIntKeyword -> "BigIntKeyword"
        | Ts.SyntaxKind.OverrideKeyword -> "OverrideKeyword"
        | Ts.SyntaxKind.OfKeyword -> "OfKeyword"
        | Ts.SyntaxKind.QualifiedName -> "QualifiedName"
        | Ts.SyntaxKind.ComputedPropertyName -> "ComputedPropertyName"
        | Ts.SyntaxKind.TypeParameter -> "TypeParameter"
        | Ts.SyntaxKind.Parameter -> "Parameter"
        | Ts.SyntaxKind.Decorator -> "Decorator"
        | Ts.SyntaxKind.PropertySignature -> "PropertySignature"
        | Ts.SyntaxKind.PropertyDeclaration -> "PropertyDeclaration"
        | Ts.SyntaxKind.MethodSignature -> "MethodSignature"
        | Ts.SyntaxKind.MethodDeclaration -> "MethodDeclaration"
        | Ts.SyntaxKind.ClassStaticBlockDeclaration -> "ClassStaticBlockDeclaration"
        | Ts.SyntaxKind.Constructor -> "Constructor"
        | Ts.SyntaxKind.GetAccessor -> "GetAccessor"
        | Ts.SyntaxKind.SetAccessor -> "SetAccessor"
        | Ts.SyntaxKind.CallSignature -> "CallSignature"
        | Ts.SyntaxKind.ConstructSignature -> "ConstructSignature"
        | Ts.SyntaxKind.IndexSignature -> "IndexSignature"
        | Ts.SyntaxKind.TypePredicate -> "TypePredicate"
        | Ts.SyntaxKind.TypeReference -> "TypeReference"
        | Ts.SyntaxKind.FunctionType -> "FunctionType"
        | Ts.SyntaxKind.ConstructorType -> "ConstructorType"
        | Ts.SyntaxKind.TypeQuery -> "TypeQuery"
        | Ts.SyntaxKind.TypeLiteral -> "TypeLiteral"
        | Ts.SyntaxKind.ArrayType -> "ArrayType"
        | Ts.SyntaxKind.TupleType -> "TupleType"
        | Ts.SyntaxKind.OptionalType -> "OptionalType"
        | Ts.SyntaxKind.RestType -> "RestType"
        | Ts.SyntaxKind.UnionType -> "UnionType"
        | Ts.SyntaxKind.IntersectionType -> "IntersectionType"
        | Ts.SyntaxKind.ConditionalType -> "ConditionalType"
        | Ts.SyntaxKind.InferType -> "InferType"
        | Ts.SyntaxKind.ParenthesizedType -> "ParenthesizedType"
        | Ts.SyntaxKind.ThisType -> "ThisType"
        | Ts.SyntaxKind.TypeOperator -> "TypeOperator"
        | Ts.SyntaxKind.IndexedAccessType -> "IndexedAccessType"
        | Ts.SyntaxKind.MappedType -> "MappedType"
        | Ts.SyntaxKind.LiteralType -> "LiteralType"
        | Ts.SyntaxKind.NamedTupleMember -> "NamedTupleMember"
        | Ts.SyntaxKind.TemplateLiteralType -> "TemplateLiteralType"
        | Ts.SyntaxKind.TemplateLiteralTypeSpan -> "TemplateLiteralTypeSpan"
        | Ts.SyntaxKind.ImportType -> "ImportType"
        | Ts.SyntaxKind.ObjectBindingPattern -> "ObjectBindingPattern"
        | Ts.SyntaxKind.ArrayBindingPattern -> "ArrayBindingPattern"
        | Ts.SyntaxKind.BindingElement -> "BindingElement"
        | Ts.SyntaxKind.ArrayLiteralExpression -> "ArrayLiteralExpression"
        | Ts.SyntaxKind.ObjectLiteralExpression -> "ObjectLiteralExpression"
        | Ts.SyntaxKind.PropertyAccessExpression -> "PropertyAccessExpression"
        | Ts.SyntaxKind.ElementAccessExpression -> "ElementAccessExpression"
        | Ts.SyntaxKind.CallExpression -> "CallExpression"
        | Ts.SyntaxKind.NewExpression -> "NewExpression"
        | Ts.SyntaxKind.TaggedTemplateExpression -> "TaggedTemplateExpression"
        | Ts.SyntaxKind.TypeAssertionExpression -> "TypeAssertionExpression"
        | Ts.SyntaxKind.ParenthesizedExpression -> "ParenthesizedExpression"
        | Ts.SyntaxKind.FunctionExpression -> "FunctionExpression"
        | Ts.SyntaxKind.ArrowFunction -> "ArrowFunction"
        | Ts.SyntaxKind.DeleteExpression -> "DeleteExpression"
        | Ts.SyntaxKind.TypeOfExpression -> "TypeOfExpression"
        | Ts.SyntaxKind.VoidExpression -> "VoidExpression"
        | Ts.SyntaxKind.AwaitExpression -> "AwaitExpression"
        | Ts.SyntaxKind.PrefixUnaryExpression -> "PrefixUnaryExpression"
        | Ts.SyntaxKind.PostfixUnaryExpression -> "PostfixUnaryExpression"
        | Ts.SyntaxKind.BinaryExpression -> "BinaryExpression"
        | Ts.SyntaxKind.ConditionalExpression -> "ConditionalExpression"
        | Ts.SyntaxKind.TemplateExpression -> "TemplateExpression"
        | Ts.SyntaxKind.YieldExpression -> "YieldExpression"
        | Ts.SyntaxKind.SpreadElement -> "SpreadElement"
        | Ts.SyntaxKind.ClassExpression -> "ClassExpression"
        | Ts.SyntaxKind.OmittedExpression -> "OmittedExpression"
        | Ts.SyntaxKind.ExpressionWithTypeArguments -> "ExpressionWithTypeArguments"
        | Ts.SyntaxKind.AsExpression -> "AsExpression"
        | Ts.SyntaxKind.NonNullExpression -> "NonNullExpression"
        | Ts.SyntaxKind.MetaProperty -> "MetaProperty"
        | Ts.SyntaxKind.SyntheticExpression -> "SyntheticExpression"
        | Ts.SyntaxKind.SatisfiesExpression -> "SatisfiesExpression"
        | Ts.SyntaxKind.TemplateSpan -> "TemplateSpan"
        | Ts.SyntaxKind.SemicolonClassElement -> "SemicolonClassElement"
        | Ts.SyntaxKind.Block -> "Block"
        | Ts.SyntaxKind.EmptyStatement -> "EmptyStatement"
        | Ts.SyntaxKind.VariableStatement -> "VariableStatement"
        | Ts.SyntaxKind.ExpressionStatement -> "ExpressionStatement"
        | Ts.SyntaxKind.IfStatement -> "IfStatement"
        | Ts.SyntaxKind.DoStatement -> "DoStatement"
        | Ts.SyntaxKind.WhileStatement -> "WhileStatement"
        | Ts.SyntaxKind.ForStatement -> "ForStatement"
        | Ts.SyntaxKind.ForInStatement -> "ForInStatement"
        | Ts.SyntaxKind.ForOfStatement -> "ForOfStatement"
        | Ts.SyntaxKind.ContinueStatement -> "ContinueStatement"
        | Ts.SyntaxKind.BreakStatement -> "BreakStatement"
        | Ts.SyntaxKind.ReturnStatement -> "ReturnStatement"
        | Ts.SyntaxKind.WithStatement -> "WithStatement"
        | Ts.SyntaxKind.SwitchStatement -> "SwitchStatement"
        | Ts.SyntaxKind.LabeledStatement -> "LabeledStatement"
        | Ts.SyntaxKind.ThrowStatement -> "ThrowStatement"
        | Ts.SyntaxKind.TryStatement -> "TryStatement"
        | Ts.SyntaxKind.DebuggerStatement -> "DebuggerStatement"
        | Ts.SyntaxKind.VariableDeclaration -> "VariableDeclaration"
        | Ts.SyntaxKind.VariableDeclarationList -> "VariableDeclarationList"
        | Ts.SyntaxKind.FunctionDeclaration -> "FunctionDeclaration"
        | Ts.SyntaxKind.ClassDeclaration -> "ClassDeclaration"
        | Ts.SyntaxKind.InterfaceDeclaration -> "InterfaceDeclaration"
        | Ts.SyntaxKind.TypeAliasDeclaration -> "TypeAliasDeclaration"
        | Ts.SyntaxKind.EnumDeclaration -> "EnumDeclaration"
        | Ts.SyntaxKind.ModuleDeclaration -> "ModuleDeclaration"
        | Ts.SyntaxKind.ModuleBlock -> "ModuleBlock"
        | Ts.SyntaxKind.CaseBlock -> "CaseBlock"
        | Ts.SyntaxKind.NamespaceExportDeclaration -> "NamespaceExportDeclaration"
        | Ts.SyntaxKind.ImportEqualsDeclaration -> "ImportEqualsDeclaration"
        | Ts.SyntaxKind.ImportDeclaration -> "ImportDeclaration"
        | Ts.SyntaxKind.ImportClause -> "ImportClause"
        | Ts.SyntaxKind.NamespaceImport -> "NamespaceImport"
        | Ts.SyntaxKind.NamedImports -> "NamedImports"
        | Ts.SyntaxKind.ImportSpecifier -> "ImportSpecifier"
        | Ts.SyntaxKind.ExportAssignment -> "ExportAssignment"
        | Ts.SyntaxKind.ExportDeclaration -> "ExportDeclaration"
        | Ts.SyntaxKind.NamedExports -> "NamedExports"
        | Ts.SyntaxKind.NamespaceExport -> "NamespaceExport"
        | Ts.SyntaxKind.ExportSpecifier -> "ExportSpecifier"
        | Ts.SyntaxKind.MissingDeclaration -> "MissingDeclaration"
        | Ts.SyntaxKind.ExternalModuleReference -> "ExternalModuleReference"
        | Ts.SyntaxKind.JsxElement -> "JsxElement"
        | Ts.SyntaxKind.JsxSelfClosingElement -> "JsxSelfClosingElement"
        | Ts.SyntaxKind.JsxOpeningElement -> "JsxOpeningElement"
        | Ts.SyntaxKind.JsxClosingElement -> "JsxClosingElement"
        | Ts.SyntaxKind.JsxFragment -> "JsxFragment"
        | Ts.SyntaxKind.JsxOpeningFragment -> "JsxOpeningFragment"
        | Ts.SyntaxKind.JsxClosingFragment -> "JsxClosingFragment"
        | Ts.SyntaxKind.JsxAttribute -> "JsxAttribute"
        | Ts.SyntaxKind.JsxAttributes -> "JsxAttributes"
        | Ts.SyntaxKind.JsxSpreadAttribute -> "JsxSpreadAttribute"
        | Ts.SyntaxKind.JsxExpression -> "JsxExpression"
        | Ts.SyntaxKind.JsxNamespacedName -> "JsxNamespacedName"
        | Ts.SyntaxKind.CaseClause -> "CaseClause"
        | Ts.SyntaxKind.DefaultClause -> "DefaultClause"
        | Ts.SyntaxKind.HeritageClause -> "HeritageClause"
        | Ts.SyntaxKind.CatchClause -> "CatchClause"
        | Ts.SyntaxKind.AssertClause -> "AssertClause"
        | Ts.SyntaxKind.AssertEntry -> "AssertEntry"
        | Ts.SyntaxKind.ImportTypeAssertionContainer -> "ImportTypeAssertionContainer"
        | Ts.SyntaxKind.PropertyAssignment -> "PropertyAssignment"
        | Ts.SyntaxKind.ShorthandPropertyAssignment -> "ShorthandPropertyAssignment"
        | Ts.SyntaxKind.SpreadAssignment -> "SpreadAssignment"
        | Ts.SyntaxKind.EnumMember -> "EnumMember"
        | Ts.SyntaxKind.SourceFile -> "SourceFile"
        | Ts.SyntaxKind.Bundle -> "Bundle"
        | Ts.SyntaxKind.JSDocTypeExpression -> "JSDocTypeExpression"
        | Ts.SyntaxKind.JSDocNameReference -> "JSDocNameReference"
        | Ts.SyntaxKind.JSDocMemberName -> "JSDocMemberName"
        | Ts.SyntaxKind.JSDocAllType -> "JSDocAllType"
        | Ts.SyntaxKind.JSDocUnknownType -> "JSDocUnknownType"
        | Ts.SyntaxKind.JSDocNullableType -> "JSDocNullableType"
        | Ts.SyntaxKind.JSDocNonNullableType -> "JSDocNonNullableType"
        | Ts.SyntaxKind.JSDocOptionalType -> "JSDocOptionalType"
        | Ts.SyntaxKind.JSDocFunctionType -> "JSDocFunctionType"
        | Ts.SyntaxKind.JSDocVariadicType -> "JSDocVariadicType"
        | Ts.SyntaxKind.JSDocNamepathType -> "JSDocNamepathType"
        | Ts.SyntaxKind.JSDoc -> "JSDoc"
        | Ts.SyntaxKind.JSDocText -> "JSDocText"
        | Ts.SyntaxKind.JSDocTypeLiteral -> "JSDocTypeLiteral"
        | Ts.SyntaxKind.JSDocSignature -> "JSDocSignature"
        | Ts.SyntaxKind.JSDocLink -> "JSDocLink"
        | Ts.SyntaxKind.JSDocLinkCode -> "JSDocLinkCode"
        | Ts.SyntaxKind.JSDocLinkPlain -> "JSDocLinkPlain"
        | Ts.SyntaxKind.JSDocTag -> "JSDocTag"
        | Ts.SyntaxKind.JSDocAugmentsTag -> "JSDocAugmentsTag"
        | Ts.SyntaxKind.JSDocImplementsTag -> "JSDocImplementsTag"
        | Ts.SyntaxKind.JSDocAuthorTag -> "JSDocAuthorTag"
        | Ts.SyntaxKind.JSDocDeprecatedTag -> "JSDocDeprecatedTag"
        | Ts.SyntaxKind.JSDocClassTag -> "JSDocClassTag"
        | Ts.SyntaxKind.JSDocPublicTag -> "JSDocPublicTag"
        | Ts.SyntaxKind.JSDocPrivateTag -> "JSDocPrivateTag"
        | Ts.SyntaxKind.JSDocProtectedTag -> "JSDocProtectedTag"
        | Ts.SyntaxKind.JSDocReadonlyTag -> "JSDocReadonlyTag"
        | Ts.SyntaxKind.JSDocOverrideTag -> "JSDocOverrideTag"
        | Ts.SyntaxKind.JSDocCallbackTag -> "JSDocCallbackTag"
        | Ts.SyntaxKind.JSDocOverloadTag -> "JSDocOverloadTag"
        | Ts.SyntaxKind.JSDocEnumTag -> "JSDocEnumTag"
        | Ts.SyntaxKind.JSDocParameterTag -> "JSDocParameterTag"
        | Ts.SyntaxKind.JSDocReturnTag -> "JSDocReturnTag"
        | Ts.SyntaxKind.JSDocThisTag -> "JSDocThisTag"
        | Ts.SyntaxKind.JSDocTypeTag -> "JSDocTypeTag"
        | Ts.SyntaxKind.JSDocTemplateTag -> "JSDocTemplateTag"
        | Ts.SyntaxKind.JSDocTypedefTag -> "JSDocTypedefTag"
        | Ts.SyntaxKind.JSDocSeeTag -> "JSDocSeeTag"
        | Ts.SyntaxKind.JSDocPropertyTag -> "JSDocPropertyTag"
        | Ts.SyntaxKind.JSDocThrowsTag -> "JSDocThrowsTag"
        | Ts.SyntaxKind.JSDocSatisfiesTag -> "JSDocSatisfiesTag"
        | Ts.SyntaxKind.SyntaxList -> "SyntaxList"
        | Ts.SyntaxKind.NotEmittedStatement -> "NotEmittedStatement"
        | Ts.SyntaxKind.PartiallyEmittedExpression -> "PartiallyEmittedExpression"
        | Ts.SyntaxKind.CommaListExpression -> "CommaListExpression"
        | Ts.SyntaxKind.SyntheticReferenceExpression -> "SyntheticReferenceExpression"
        | Ts.SyntaxKind.Count -> "Count"
        | Ts.SyntaxKind.DeferKeyword -> "DeferKeyword"
        | Ts.SyntaxKind.JSDocImportTag -> "JSDocImportTag"
        | Ts.SyntaxKind.NotEmittedTypeElement -> "NotEmittedTypeElement"
        | unknown -> $"Unknown {unknown}"

type Ts.TypeFlags with
    member this.ToStringArray() =
        [|
            Ts.TypeFlags.Any, "Any"
            Ts.TypeFlags.Unknown, "Unknown"
            Ts.TypeFlags.String, "String"
            Ts.TypeFlags.Number, "Number"
            Ts.TypeFlags.Boolean, "Boolean"
            Ts.TypeFlags.Enum, "Enum"
            Ts.TypeFlags.BigInt, "BigInt"
            Ts.TypeFlags.StringLiteral, "StringLiteral"
            Ts.TypeFlags.NumberLiteral, "NumberLiteral"
            Ts.TypeFlags.BooleanLiteral, "BooleanLiteral"
            Ts.TypeFlags.EnumLiteral, "EnumLiteral"
            Ts.TypeFlags.BigIntLiteral, "BigIntLiteral"
            Ts.TypeFlags.ESSymbol, "ESSymbol"
            Ts.TypeFlags.UniqueESSymbol, "UniqueESSymbol"
            Ts.TypeFlags.Void, "Void"
            Ts.TypeFlags.Undefined, "Undefined"
            Ts.TypeFlags.Null, "Null"
            Ts.TypeFlags.Never, "Never"
            Ts.TypeFlags.TypeParameter, "TypeParameter"
            Ts.TypeFlags.Object, "Object"
            Ts.TypeFlags.Union, "Union"
            Ts.TypeFlags.Intersection, "Intersection"
            Ts.TypeFlags.Index, "Index"
            Ts.TypeFlags.IndexedAccess, "IndexedAccess"
            Ts.TypeFlags.Conditional, "Conditional"
            Ts.TypeFlags.Substitution, "Substitution"
            Ts.TypeFlags.NonPrimitive, "NonPrimitive"
            Ts.TypeFlags.TemplateLiteral, "TemplateLiteral"
            Ts.TypeFlags.StringMapping, "StringMapping"
            Ts.TypeFlags.Literal, "Literal"
            Ts.TypeFlags.Unit, "Unit"
            Ts.TypeFlags.Freshable, "Freshable"
            Ts.TypeFlags.StringOrNumberLiteral, "StringOrNumberLiteral"
            Ts.TypeFlags.PossiblyFalsy, "PossiblyFalsy"
            Ts.TypeFlags.StringLike, "StringLike"
            Ts.TypeFlags.NumberLike, "NumberLike"
            Ts.TypeFlags.BigIntLike, "BigIntLike"
            Ts.TypeFlags.BooleanLike, "BooleanLike"
            Ts.TypeFlags.EnumLike, "EnumLike"
            Ts.TypeFlags.ESSymbolLike, "ESSymbolLike"
            Ts.TypeFlags.VoidLike, "VoidLike"
            Ts.TypeFlags.UnionOrIntersection, "UnionOrIntersection"
            Ts.TypeFlags.StructuredType, "StructuredType"
            Ts.TypeFlags.TypeVariable, "TypeVariable"
            Ts.TypeFlags.InstantiableNonPrimitive, "InstantiableNonPrimitive"
            Ts.TypeFlags.InstantiablePrimitive, "InstantiablePrimitive"
            Ts.TypeFlags.Instantiable, "Instantiable"
            Ts.TypeFlags.StructuredOrInstantiable, "StructuredOrInstantiable"
        |]
        |> Array.choose (fun (flag, name) ->
            if this.HasFlag flag then
                Some name
            else None
        )
    /// Prints type flags for the type node to console
    /// 
    member this.Debug() =
        [
            Ts.TypeFlags.Any, "Any"
            Ts.TypeFlags.Unknown, "Unknown"
            Ts.TypeFlags.String, "String"
            Ts.TypeFlags.Number, "Number"
            Ts.TypeFlags.Boolean, "Boolean"
            Ts.TypeFlags.Enum, "Enum"
            Ts.TypeFlags.BigInt, "BigInt"
            Ts.TypeFlags.StringLiteral, "StringLiteral"
            Ts.TypeFlags.NumberLiteral, "NumberLiteral"
            Ts.TypeFlags.BooleanLiteral, "BooleanLiteral"
            Ts.TypeFlags.EnumLiteral, "EnumLiteral"
            Ts.TypeFlags.BigIntLiteral, "BigIntLiteral"
            Ts.TypeFlags.ESSymbol, "ESSymbol"
            Ts.TypeFlags.UniqueESSymbol, "UniqueESSymbol"
            Ts.TypeFlags.Void, "Void"
            Ts.TypeFlags.Undefined, "Undefined"
            Ts.TypeFlags.Null, "Null"
            Ts.TypeFlags.Never, "Never"
            Ts.TypeFlags.TypeParameter, "TypeParameter"
            Ts.TypeFlags.Object, "Object"
            Ts.TypeFlags.Union, "Union"
            Ts.TypeFlags.Intersection, "Intersection"
            Ts.TypeFlags.Index, "Index"
            Ts.TypeFlags.IndexedAccess, "IndexedAccess"
            Ts.TypeFlags.Conditional, "Conditional"
            Ts.TypeFlags.Substitution, "Substitution"
            Ts.TypeFlags.NonPrimitive, "NonPrimitive"
            Ts.TypeFlags.TemplateLiteral, "TemplateLiteral"
            Ts.TypeFlags.StringMapping, "StringMapping"
            Ts.TypeFlags.Literal, "Literal"
            Ts.TypeFlags.Unit, "Unit"
            Ts.TypeFlags.Freshable, "Freshable"
            Ts.TypeFlags.StringOrNumberLiteral, "StringOrNumberLiteral"
            Ts.TypeFlags.PossiblyFalsy, "PossiblyFalsy"
            Ts.TypeFlags.StringLike, "StringLike"
            Ts.TypeFlags.NumberLike, "NumberLike"
            Ts.TypeFlags.BigIntLike, "BigIntLike"
            Ts.TypeFlags.BooleanLike, "BooleanLike"
            Ts.TypeFlags.EnumLike, "EnumLike"
            Ts.TypeFlags.ESSymbolLike, "ESSymbolLike"
            Ts.TypeFlags.VoidLike, "VoidLike"
            Ts.TypeFlags.UnionOrIntersection, "UnionOrIntersection"
            Ts.TypeFlags.StructuredType, "StructuredType"
            Ts.TypeFlags.TypeVariable, "TypeVariable"
            Ts.TypeFlags.InstantiableNonPrimitive, "InstantiableNonPrimitive"
            Ts.TypeFlags.InstantiablePrimitive, "InstantiablePrimitive"
            Ts.TypeFlags.Instantiable, "Instantiable"
            Ts.TypeFlags.StructuredOrInstantiable, "StructuredOrInstantiable"
        ]
        |> List.iter (fun (flag, name) ->
            if this.HasFlag flag then
                printfn "%s" name
        )
type Ts.SymbolFlags with
    member this.ToStringArray() =
        [|
            Ts.SymbolFlags.None, "None"
            Ts.SymbolFlags.FunctionScopedVariable, "FunctionScopedVariable"
            Ts.SymbolFlags.BlockScopedVariable, "BlockScopedVariable"
            Ts.SymbolFlags.Property, "Property"
            Ts.SymbolFlags.EnumMember, "EnumMember"
            Ts.SymbolFlags.Function, "Function"
            Ts.SymbolFlags.Class, "Class"
            Ts.SymbolFlags.Interface, "Interface"
            Ts.SymbolFlags.ConstEnum, "ConstEnum"
            Ts.SymbolFlags.RegularEnum, "RegularEnum"
            Ts.SymbolFlags.ValueModule, "ValueModule"
            Ts.SymbolFlags.NamespaceModule, "NamespaceModule"
            Ts.SymbolFlags.TypeLiteral, "TypeLiteral"
            Ts.SymbolFlags.ObjectLiteral, "ObjectLiteral"
            Ts.SymbolFlags.Method, "Method"
            Ts.SymbolFlags.Constructor, "Constructor"
            Ts.SymbolFlags.GetAccessor, "GetAccessor"
            Ts.SymbolFlags.SetAccessor, "SetAccessor"
            Ts.SymbolFlags.Signature, "Signature"
            Ts.SymbolFlags.TypeParameter, "TypeParameter"
            Ts.SymbolFlags.TypeAlias, "TypeAlias"
            Ts.SymbolFlags.ExportValue, "ExportValue"
            Ts.SymbolFlags.Alias, "Alias"
            Ts.SymbolFlags.Prototype, "Prototype"
            Ts.SymbolFlags.ExportStar, "ExportStar"
            Ts.SymbolFlags.Optional, "Optional"
            Ts.SymbolFlags.Transient, "Transient"
            Ts.SymbolFlags.Assignment, "Assignment"
            Ts.SymbolFlags.ModuleExports, "ModuleExports"
            Ts.SymbolFlags.All, "All"
            Ts.SymbolFlags.Enum, "Enum"
            Ts.SymbolFlags.Variable, "Variable"
            Ts.SymbolFlags.Value, "Value"
            Ts.SymbolFlags.Type, "Type"
            Ts.SymbolFlags.Namespace, "Namespace"
            Ts.SymbolFlags.Module, "Module"
            Ts.SymbolFlags.Accessor, "Accessor"
            Ts.SymbolFlags.FunctionScopedVariableExcludes, "FunctionScopedVariableExcludes"
            Ts.SymbolFlags.BlockScopedVariableExcludes, "BlockScopedVariableExcludes"
            Ts.SymbolFlags.ParameterExcludes, "ParameterExcludes"
            Ts.SymbolFlags.PropertyExcludes, "PropertyExcludes"
            Ts.SymbolFlags.EnumMemberExcludes, "EnumMemberExcludes"
            Ts.SymbolFlags.FunctionExcludes, "FunctionExcludes"
            Ts.SymbolFlags.ClassExcludes, "ClassExcludes"
            Ts.SymbolFlags.InterfaceExcludes, "InterfaceExcludes"
            Ts.SymbolFlags.RegularEnumExcludes, "RegularEnumExcludes"
            Ts.SymbolFlags.ConstEnumExcludes, "ConstEnumExcludes"
            Ts.SymbolFlags.ValueModuleExcludes, "ValueModuleExcludes"
            Ts.SymbolFlags.NamespaceModuleExcludes, "NamespaceModuleExcludes"
            Ts.SymbolFlags.MethodExcludes, "MethodExcludes"
            Ts.SymbolFlags.GetAccessorExcludes, "GetAccessorExcludes"
            Ts.SymbolFlags.SetAccessorExcludes, "SetAccessorExcludes"
            Ts.SymbolFlags.AccessorExcludes, "AccessorExcludes"
            Ts.SymbolFlags.TypeParameterExcludes, "TypeParameterExcludes"
            Ts.SymbolFlags.TypeAliasExcludes, "TypeAliasExcludes"
            Ts.SymbolFlags.AliasExcludes, "AliasExcludes"
            Ts.SymbolFlags.ModuleMember, "ModuleMember"
            Ts.SymbolFlags.ExportHasLocal, "ExportHasLocal"
            Ts.SymbolFlags.BlockScoped, "BlockScoped"
            Ts.SymbolFlags.PropertyOrAccessor, "PropertyOrAccessor"
            Ts.SymbolFlags.ClassMember, "ClassMember"
        |]
        |> Array.choose (fun (flag, name) ->
            if this.HasFlag flag then
                Some name
            else None
        )
    /// Prints the SymbolFlags for the symbol to console
    member this.Debug() =
        [
            Ts.SymbolFlags.None, "None"
            Ts.SymbolFlags.FunctionScopedVariable, "FunctionScopedVariable"
            Ts.SymbolFlags.BlockScopedVariable, "BlockScopedVariable"
            Ts.SymbolFlags.Property, "Property"
            Ts.SymbolFlags.EnumMember, "EnumMember"
            Ts.SymbolFlags.Function, "Function"
            Ts.SymbolFlags.Class, "Class"
            Ts.SymbolFlags.Interface, "Interface"
            Ts.SymbolFlags.ConstEnum, "ConstEnum"
            Ts.SymbolFlags.RegularEnum, "RegularEnum"
            Ts.SymbolFlags.ValueModule, "ValueModule"
            Ts.SymbolFlags.NamespaceModule, "NamespaceModule"
            Ts.SymbolFlags.TypeLiteral, "TypeLiteral"
            Ts.SymbolFlags.ObjectLiteral, "ObjectLiteral"
            Ts.SymbolFlags.Method, "Method"
            Ts.SymbolFlags.Constructor, "Constructor"
            Ts.SymbolFlags.GetAccessor, "GetAccessor"
            Ts.SymbolFlags.SetAccessor, "SetAccessor"
            Ts.SymbolFlags.Signature, "Signature"
            Ts.SymbolFlags.TypeParameter, "TypeParameter"
            Ts.SymbolFlags.TypeAlias, "TypeAlias"
            Ts.SymbolFlags.ExportValue, "ExportValue"
            Ts.SymbolFlags.Alias, "Alias"
            Ts.SymbolFlags.Prototype, "Prototype"
            Ts.SymbolFlags.ExportStar, "ExportStar"
            Ts.SymbolFlags.Optional, "Optional"
            Ts.SymbolFlags.Transient, "Transient"
            Ts.SymbolFlags.Assignment, "Assignment"
            Ts.SymbolFlags.ModuleExports, "ModuleExports"
            Ts.SymbolFlags.All, "All"
            Ts.SymbolFlags.Enum, "Enum"
            Ts.SymbolFlags.Variable, "Variable"
            Ts.SymbolFlags.Value, "Value"
            Ts.SymbolFlags.Type, "Type"
            Ts.SymbolFlags.Namespace, "Namespace"
            Ts.SymbolFlags.Module, "Module"
            Ts.SymbolFlags.Accessor, "Accessor"
            Ts.SymbolFlags.FunctionScopedVariableExcludes, "FunctionScopedVariableExcludes"
            Ts.SymbolFlags.BlockScopedVariableExcludes, "BlockScopedVariableExcludes"
            Ts.SymbolFlags.ParameterExcludes, "ParameterExcludes"
            Ts.SymbolFlags.PropertyExcludes, "PropertyExcludes"
            Ts.SymbolFlags.EnumMemberExcludes, "EnumMemberExcludes"
            Ts.SymbolFlags.FunctionExcludes, "FunctionExcludes"
            Ts.SymbolFlags.ClassExcludes, "ClassExcludes"
            Ts.SymbolFlags.InterfaceExcludes, "InterfaceExcludes"
            Ts.SymbolFlags.RegularEnumExcludes, "RegularEnumExcludes"
            Ts.SymbolFlags.ConstEnumExcludes, "ConstEnumExcludes"
            Ts.SymbolFlags.ValueModuleExcludes, "ValueModuleExcludes"
            Ts.SymbolFlags.NamespaceModuleExcludes, "NamespaceModuleExcludes"
            Ts.SymbolFlags.MethodExcludes, "MethodExcludes"
            Ts.SymbolFlags.GetAccessorExcludes, "GetAccessorExcludes"
            Ts.SymbolFlags.SetAccessorExcludes, "SetAccessorExcludes"
            Ts.SymbolFlags.AccessorExcludes, "AccessorExcludes"
            Ts.SymbolFlags.TypeParameterExcludes, "TypeParameterExcludes"
            Ts.SymbolFlags.TypeAliasExcludes, "TypeAliasExcludes"
            Ts.SymbolFlags.AliasExcludes, "AliasExcludes"
            Ts.SymbolFlags.ModuleMember, "ModuleMember"
            Ts.SymbolFlags.ExportHasLocal, "ExportHasLocal"
            Ts.SymbolFlags.BlockScoped, "BlockScoped"
            Ts.SymbolFlags.PropertyOrAccessor, "PropertyOrAccessor"
            Ts.SymbolFlags.ClassMember, "ClassMember"
        ]
        |> List.iter (fun (flag, name) ->
            if this.HasFlag flag then
                printfn "%s" name
        )
type Ts.ObjectFlags with
    member this.ToStringArray() =
        [|
            Ts.ObjectFlags.None, "None"
            Ts.ObjectFlags.Class, "Class"
            Ts.ObjectFlags.Interface, "Interface"
            Ts.ObjectFlags.Reference, "Reference"
            Ts.ObjectFlags.Tuple, "Tuple"
            Ts.ObjectFlags.Anonymous, "Anonymous"
            Ts.ObjectFlags.Mapped, "Mapped"
            Ts.ObjectFlags.Instantiated, "Instantiated"
            Ts.ObjectFlags.ObjectLiteral, "ObjectLiteral"
            Ts.ObjectFlags.EvolvingArray, "EvolvingArray"
            Ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties, "ObjectLiteralPatternWithComputedProperties"
            Ts.ObjectFlags.ReverseMapped, "ReverseMapped"
            Ts.ObjectFlags.JsxAttributes, "JsxAttributes"
            Ts.ObjectFlags.JSLiteral, "JSLiteral"
            Ts.ObjectFlags.FreshLiteral, "FreshLiteral"
            Ts.ObjectFlags.ArrayLiteral, "ArrayLiteral"
            Ts.ObjectFlags.SingleSignatureType, "SingleSignatureType"
            Ts.ObjectFlags.ClassOrInterface, "ClassOrInterface"
            Ts.ObjectFlags.ContainsSpread, "ContainsSpread"
            Ts.ObjectFlags.ObjectRestType, "ObjectRestType"
            Ts.ObjectFlags.InstantiationExpressionType, "InstantiationExpressionType"
        |] |> Array.choose(fun (flag,name) ->
            if this.HasFlag flag then
                Some name
            else None
            )
    member this.Debug() =
        this.ToStringArray()
        |> Array.iter System.Console.WriteLine

type Ts.NodeFlags with
    member this.ToStringArray() =
        [|
            Ts.NodeFlags.None, "None"
            Ts.NodeFlags.Let, "Let"
            Ts.NodeFlags.Const, "Const"
            Ts.NodeFlags.Using, "Using"
            Ts.NodeFlags.AwaitUsing, "AwaitUsing"
            Ts.NodeFlags.NestedNamespace, "NestedNamespace"
            Ts.NodeFlags.Synthesized, "Synthesized"
            Ts.NodeFlags.Namespace, "Namespace"
            Ts.NodeFlags.OptionalChain, "OptionalChain"
            Ts.NodeFlags.ExportContext, "ExportContext"
            Ts.NodeFlags.ContainsThis, "ContainsThis"
            Ts.NodeFlags.HasImplicitReturn, "HasImplicitReturn"
            Ts.NodeFlags.HasExplicitReturn, "HasExplicitReturn"
            Ts.NodeFlags.GlobalAugmentation, "GlobalAugmentation"
            Ts.NodeFlags.HasAsyncFunctions, "HasAsyncFunctions"
            Ts.NodeFlags.DisallowInContext, "DisallowInContext"
            Ts.NodeFlags.YieldContext, "YieldContext"
            Ts.NodeFlags.DecoratorContext, "DecoratorContext"
            Ts.NodeFlags.AwaitContext, "AwaitContext"
            Ts.NodeFlags.DisallowConditionalTypesContext, "DisallowConditionalTypesContext"
            Ts.NodeFlags.ThisNodeHasError, "ThisNodeHasError"
            Ts.NodeFlags.JavaScriptFile, "JavaScriptFile"
            Ts.NodeFlags.ThisNodeOrAnySubNodesHasError, "ThisNodeOrAnySubNodesHasError"
            Ts.NodeFlags.HasAggregatedChildData, "HasAggregatedChildData"
            Ts.NodeFlags.JSDoc, "JSDoc"
            Ts.NodeFlags.JsonFile, "JsonFile"
            Ts.NodeFlags.BlockScoped, "BlockScoped"
            Ts.NodeFlags.Constant, "Constant"
            Ts.NodeFlags.ReachabilityCheckFlags, "ReachabilityCheckFlags"
            Ts.NodeFlags.ReachabilityAndEmitFlags, "ReachabilityAndEmitFlags"
            Ts.NodeFlags.ContextFlags, "ContextFlags"
            Ts.NodeFlags.TypeExcludesFlags, "TypeExcludesFlags"
        |] |> Array.choose(fun (flag,name) ->
                if this.HasFlag flag then
                    Some name
                else None
            )
    member this.Debug() =
        this.ToStringArray()
        |> Array.iter System.Console.WriteLine
        
/// Pattern matching helpers
module Patterns =
    /// <summary>
    /// Pattern matching helpers for derivatives of <c>Ts.Node</c>
    /// </summary>
    module Node =
        /// <summary>
        /// If the type we're casting to isn't compatible with <c>Ts.Node</c>, then we
        /// forcefully unbox the node to the target type if the predicate is true.
        /// </summary>
        let inline private forcePredToOption<'NodeType> (predicate: Ts.Node -> bool) (node: Ts.Node) =
            if predicate node then
                unbox<'NodeType> node
                |> Some
            else None
        /// <summary>
        /// Casts the <c>node</c> to the target type if the <c>predicate</c> is true.
        /// </summary>
        /// <param name="predicate"><c>Ts.Node -> bool</c></param>
        /// <param name="node"><c>#Ts.Node</c></param>
        let inline private predToOption<'NodeType when 'NodeType :> Ts.Node> (predicate: Ts.Node -> bool) (node: Ts.Node) =
            if predicate node then
                node :?> 'NodeType
                |> Some
            else None
        let inline (|NumericLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.NumericLiteral> ts.isNumericLiteral value
        let inline (|BigIntLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.BigIntLiteral> ts.isBigIntLiteral value
        let inline (|StringLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.StringLiteral> ts.isStringLiteral value
        let inline (|JsxText|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxText> ts.isJsxText value
        let inline (|RegularExpressionLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.RegularExpressionLiteral> ts.isRegularExpressionLiteral value
        let inline (|NoSubstitutionTemplateLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.NoSubstitutionTemplateLiteral> ts.isNoSubstitutionTemplateLiteral value
        let inline (|TemplateHead|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateHead> ts.isTemplateHead value
        let inline (|TemplateMiddle|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateMiddle> ts.isTemplateMiddle value
        let inline (|TemplateTail|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateTail> ts.isTemplateTail value
        let inline (|DotDotDotToken|_|) (value: Ts.Node) : _ = predToOption<Ts.DotDotDotToken> ts.isDotDotDotToken value
        let inline (|PlusToken|_|) (value: Ts.Node) : _ = predToOption<Ts.PlusToken> ts.isPlusToken value
        let inline (|MinusToken|_|) (value: Ts.Node) : _ = predToOption<Ts.MinusToken> ts.isMinusToken value
        let inline (|AsteriskToken|_|) (value: Ts.Node) : _ = predToOption<Ts.AsteriskToken> ts.isAsteriskToken value
        let inline (|ExclamationToken|_|) (value: Ts.Node) : _ = predToOption<Ts.ExclamationToken> ts.isExclamationToken value
        let inline (|QuestionToken|_|) (value: Ts.Node) : _ = predToOption<Ts.QuestionToken> ts.isQuestionToken value
        let inline (|ColonToken|_|) (value: Ts.Node) : _ = predToOption<Ts.ColonToken> ts.isColonToken value
        let inline (|QuestionDotToken|_|) (value: Ts.Node) : _ = predToOption<Ts.QuestionDotToken> ts.isQuestionDotToken value
        let inline (|EqualsGreaterThanToken|_|) (value: Ts.Node) : _ = predToOption<Ts.EqualsGreaterThanToken> ts.isEqualsGreaterThanToken value
        let inline (|Identifier|_|) (value: Ts.Node) : _ = predToOption<Ts.Identifier> ts.isIdentifier value
        let inline (|PrivateIdentifier|_|) (value: Ts.Node) : _ = predToOption<Ts.PrivateIdentifier> ts.isPrivateIdentifier value
        let inline (|AssertsKeyword|_|) (value: Ts.Node) : _ = predToOption<Ts.AssertsKeyword> ts.isAssertsKeyword value
        let inline (|AwaitKeyword|_|) (value: Ts.Node) : _ = predToOption<Ts.AwaitKeyword> ts.isAwaitKeyword value
        let inline (|QualifiedName|_|) (value: Ts.Node) : _ = predToOption<Ts.QualifiedName> ts.isQualifiedName value
        let inline (|ComputedPropertyName|_|) (value: Ts.Node) : _ = predToOption<Ts.ComputedPropertyName> ts.isComputedPropertyName value
        let inline (|TypeParameterDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeParameterDeclaration> ts.isTypeParameterDeclaration value
        let inline (|Parameter|_|) (value: Ts.Node) : _ = predToOption<Ts.ParameterDeclaration> ts.isParameter value
        let inline (|Decorator|_|) (value: Ts.Node) : _ = predToOption<Ts.Decorator> ts.isDecorator value
        let inline (|PropertySignature|_|) (value: Ts.Node) : _ = predToOption<Ts.PropertySignature> ts.isPropertySignature value
        let inline (|PropertyDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.PropertyDeclaration> ts.isPropertyDeclaration value
        let inline (|MethodSignature|_|) (value: Ts.Node) : _ = predToOption<Ts.MethodSignature> ts.isMethodSignature value
        let inline (|MethodDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.MethodDeclaration> ts.isMethodDeclaration value
        let inline (|ClassStaticBlockDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ClassStaticBlockDeclaration> ts.isClassStaticBlockDeclaration value
        let inline (|ConstructorDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ConstructorDeclaration> ts.isConstructorDeclaration value
        let inline (|GetAccessorDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.GetAccessorDeclaration> ts.isGetAccessorDeclaration value
        let inline (|SetAccessorDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.SetAccessorDeclaration> ts.isSetAccessorDeclaration value
        let inline (|CallSignatureDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.CallSignatureDeclaration> ts.isCallSignatureDeclaration value
        let inline (|ConstructSignatureDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ConstructSignatureDeclaration> ts.isConstructSignatureDeclaration value
        let inline (|IndexSignatureDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.IndexSignatureDeclaration> ts.isIndexSignatureDeclaration value
        let inline (|TypePredicateNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TypePredicateNode> ts.isTypePredicateNode value
        let inline (|TypeReferenceNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeReferenceNode> ts.isTypeReferenceNode value
        let inline (|FunctionTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.FunctionTypeNode> ts.isFunctionTypeNode value
        let inline (|ConstructorTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ConstructorTypeNode> ts.isConstructorTypeNode value
        let inline (|TypeQueryNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeQueryNode> ts.isTypeQueryNode value
        let inline (|TypeLiteralNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeLiteralNode> ts.isTypeLiteralNode value
        let inline (|ArrayTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ArrayTypeNode> ts.isArrayTypeNode value
        let inline (|TupleTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TupleTypeNode> ts.isTupleTypeNode value
        let inline (|NamedTupleMember|_|) (value: Ts.Node) : _ = predToOption<Ts.NamedTupleMember> ts.isNamedTupleMember value
        let inline (|OptionalTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.OptionalTypeNode> ts.isOptionalTypeNode value
        let inline (|RestTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.RestTypeNode> ts.isRestTypeNode value
        let inline (|UnionTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.UnionTypeNode> ts.isUnionTypeNode value
        let inline (|IntersectionTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.IntersectionTypeNode> ts.isIntersectionTypeNode value
        let inline (|ConditionalTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ConditionalTypeNode> ts.isConditionalTypeNode value
        let inline (|InferTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.InferTypeNode> ts.isInferTypeNode value
        let inline (|ParenthesizedTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ParenthesizedTypeNode> ts.isParenthesizedTypeNode value
        let inline (|ThisTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ThisTypeNode> ts.isThisTypeNode value
        let inline (|TypeOperatorNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeOperatorNode> ts.isTypeOperatorNode value
        let inline (|IndexedAccessTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.IndexedAccessTypeNode> ts.isIndexedAccessTypeNode value
        let inline (|MappedTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.MappedTypeNode> ts.isMappedTypeNode value
        let inline (|LiteralTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.LiteralTypeNode> ts.isLiteralTypeNode value
        let inline (|ImportTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportTypeNode> ts.isImportTypeNode value
        let inline (|TemplateLiteralTypeSpan|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateLiteralTypeSpan> ts.isTemplateLiteralTypeSpan value
        let inline (|TemplateLiteralTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateLiteralTypeNode> ts.isTemplateLiteralTypeNode value
        let inline (|ObjectBindingPattern|_|) (value: Ts.Node) : _ = predToOption<Ts.ObjectBindingPattern> ts.isObjectBindingPattern value
        let inline (|ArrayBindingPattern|_|) (value: Ts.Node) : _ = predToOption<Ts.ArrayBindingPattern> ts.isArrayBindingPattern value
        let inline (|BindingElement|_|) (value: Ts.Node) : _ = predToOption<Ts.BindingElement> ts.isBindingElement value
        let inline (|ArrayLiteralExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ArrayLiteralExpression> ts.isArrayLiteralExpression value
        let inline (|ObjectLiteralExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ObjectLiteralExpression> ts.isObjectLiteralExpression value
        let inline (|PropertyAccessExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.PropertyAccessExpression> ts.isPropertyAccessExpression value
        let inline (|ElementAccessExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ElementAccessExpression> ts.isElementAccessExpression value
        let inline (|CallExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.CallExpression> ts.isCallExpression value
        let inline (|NewExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.NewExpression> ts.isNewExpression value
        let inline (|TaggedTemplateExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.TaggedTemplateExpression> ts.isTaggedTemplateExpression value
        let inline (|TypeAssertionExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeAssertion> ts.isTypeAssertionExpression value
        let inline (|ParenthesizedExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ParenthesizedExpression> ts.isParenthesizedExpression value
        let inline (|FunctionExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.FunctionExpression> ts.isFunctionExpression value
        let inline (|ArrowFunction|_|) (value: Ts.Node) : _ = predToOption<Ts.ArrowFunction> ts.isArrowFunction value
        let inline (|DeleteExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.DeleteExpression> ts.isDeleteExpression value
        let inline (|TypeOfExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeOfExpression> ts.isTypeOfExpression value
        let inline (|VoidExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.VoidExpression> ts.isVoidExpression value
        let inline (|AwaitExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.AwaitExpression> ts.isAwaitExpression value
        let inline (|PrefixUnaryExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.PrefixUnaryExpression> ts.isPrefixUnaryExpression value
        let inline (|PostfixUnaryExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.PostfixUnaryExpression> ts.isPostfixUnaryExpression value
        let inline (|BinaryExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.BinaryExpression> ts.isBinaryExpression value
        let inline (|ConditionalExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ConditionalExpression> ts.isConditionalExpression value
        let inline (|TemplateExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateExpression> ts.isTemplateExpression value
        let inline (|YieldExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.YieldExpression> ts.isYieldExpression value
        let inline (|SpreadElement|_|) (value: Ts.Node) : _ = predToOption<Ts.SpreadElement> ts.isSpreadElement value
        let inline (|ClassExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.ClassExpression> ts.isClassExpression value
        let inline (|OmittedExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.OmittedExpression> ts.isOmittedExpression value
        let inline (|ExpressionWithTypeArguments|_|) (value: Ts.Node) : _ = predToOption<Ts.ExpressionWithTypeArguments> ts.isExpressionWithTypeArguments value
        let inline (|AsExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.AsExpression> ts.isAsExpression value
        let inline (|SatisfiesExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.SatisfiesExpression> ts.isSatisfiesExpression value
        let inline (|NonNullExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.NonNullExpression> ts.isNonNullExpression value
        let inline (|MetaProperty|_|) (value: Ts.Node) : _ = predToOption<Ts.MetaProperty> ts.isMetaProperty value
        let inline (|SyntheticExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.SyntheticExpression> ts.isSyntheticExpression value
        let inline (|PartiallyEmittedExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.PartiallyEmittedExpression> ts.isPartiallyEmittedExpression value
        let inline (|CommaListExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.CommaListExpression> ts.isCommaListExpression value
        let inline (|TemplateSpan|_|) (value: Ts.Node) : _ = predToOption<Ts.TemplateSpan> ts.isTemplateSpan value
        let inline (|SemicolonClassElement|_|) (value: Ts.Node) : _ = predToOption<Ts.SemicolonClassElement> ts.isSemicolonClassElement value
        let inline (|Block|_|) (value: Ts.Node) : _ = predToOption<Ts.Block> ts.isBlock value
        let inline (|VariableStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.VariableStatement> ts.isVariableStatement value
        let inline (|EmptyStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.EmptyStatement> ts.isEmptyStatement value
        let inline (|ExpressionStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ExpressionStatement> ts.isExpressionStatement value
        let inline (|IfStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.IfStatement> ts.isIfStatement value
        let inline (|DoStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.DoStatement> ts.isDoStatement value
        let inline (|WhileStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.WhileStatement> ts.isWhileStatement value
        let inline (|ForStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ForStatement> ts.isForStatement value
        let inline (|ForInStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ForInStatement> ts.isForInStatement value
        let inline (|ForOfStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ForOfStatement> ts.isForOfStatement value
        let inline (|ContinueStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ContinueStatement> ts.isContinueStatement value
        let inline (|BreakStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.BreakStatement> ts.isBreakStatement value
        let inline (|ReturnStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ReturnStatement> ts.isReturnStatement value
        let inline (|WithStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.WithStatement> ts.isWithStatement value
        let inline (|SwitchStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.SwitchStatement> ts.isSwitchStatement value
        let inline (|LabeledStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.LabeledStatement> ts.isLabeledStatement value
        let inline (|ThrowStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.ThrowStatement> ts.isThrowStatement value
        let inline (|TryStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.TryStatement> ts.isTryStatement value
        let inline (|DebuggerStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.DebuggerStatement> ts.isDebuggerStatement value
        let inline (|VariableDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.VariableDeclaration> ts.isVariableDeclaration value
        let inline (|VariableDeclarationList|_|) (value: Ts.Node) : _ = predToOption<Ts.VariableDeclarationList> ts.isVariableDeclarationList value
        let inline (|FunctionDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.FunctionDeclaration> ts.isFunctionDeclaration value
        let inline (|ClassDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ClassDeclaration> ts.isClassDeclaration value
        let inline (|InterfaceDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.InterfaceDeclaration> ts.isInterfaceDeclaration value
        let inline (|TypeAliasDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.TypeAliasDeclaration> ts.isTypeAliasDeclaration value
        let inline (|EnumDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.EnumDeclaration> ts.isEnumDeclaration value
        let inline (|ModuleDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ModuleDeclaration> ts.isModuleDeclaration value
        let inline (|ModuleBlock|_|) (value: Ts.Node) : _ = predToOption<Ts.ModuleBlock> ts.isModuleBlock value
        let inline (|CaseBlock|_|) (value: Ts.Node) : _ = predToOption<Ts.CaseBlock> ts.isCaseBlock value
        let inline (|NamespaceExportDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.NamespaceExportDeclaration> ts.isNamespaceExportDeclaration value
        let inline (|ImportEqualsDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportEqualsDeclaration> ts.isImportEqualsDeclaration value
        let inline (|ImportDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportDeclaration> ts.isImportDeclaration value
        let inline (|ImportClause|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportClause> ts.isImportClause value
        let inline (|ImportTypeAssertionContainer|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportTypeAssertionContainer> ts.isImportTypeAssertionContainer value
        let inline (|ImportAttributes|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportAttributes> ts.isImportAttributes value
        let inline (|ImportAttribute|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportAttribute> ts.isImportAttribute value
        let inline (|NamespaceImport|_|) (value: Ts.Node) : _ = predToOption<Ts.NamespaceImport> ts.isNamespaceImport value
        let inline (|NamespaceExport|_|) (value: Ts.Node) : _ = predToOption<Ts.NamespaceExport> ts.isNamespaceExport value
        let inline (|NamedImports|_|) (value: Ts.Node) : _ = predToOption<Ts.NamedImports> ts.isNamedImports value
        let inline (|ImportSpecifier|_|) (value: Ts.Node) : _ = predToOption<Ts.ImportSpecifier> ts.isImportSpecifier value
        let inline (|ExportAssignment|_|) (value: Ts.Node) : _ = predToOption<Ts.ExportAssignment> ts.isExportAssignment value
        let inline (|ExportDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.ExportDeclaration> ts.isExportDeclaration value
        let inline (|NamedExports|_|) (value: Ts.Node) : _ = predToOption<Ts.NamedExports> ts.isNamedExports value
        let inline (|ExportSpecifier|_|) (value: Ts.Node) : _ = predToOption<Ts.ExportSpecifier> ts.isExportSpecifier value
        let inline (|ModuleExportName|_|) (value: Ts.Node) : _ = forcePredToOption<Ts.ModuleExportName> ts.isModuleExportName value
        let inline (|MissingDeclaration|_|) (value: Ts.Node) : _ = predToOption<Ts.MissingDeclaration> ts.isMissingDeclaration value
        let inline (|NotEmittedStatement|_|) (value: Ts.Node) : _ = predToOption<Ts.NotEmittedStatement> ts.isNotEmittedStatement value
        let inline (|ExternalModuleReference|_|) (value: Ts.Node) : _ = predToOption<Ts.ExternalModuleReference> ts.isExternalModuleReference value
        let inline (|JsxElement|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxElement> ts.isJsxElement value
        let inline (|JsxSelfClosingElement|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxSelfClosingElement> ts.isJsxSelfClosingElement value
        let inline (|JsxOpeningElement|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxOpeningElement> ts.isJsxOpeningElement value
        let inline (|JsxClosingElement|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxClosingElement> ts.isJsxClosingElement value
        let inline (|JsxFragment|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxFragment> ts.isJsxFragment value
        let inline (|JsxOpeningFragment|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxOpeningFragment> ts.isJsxOpeningFragment value
        let inline (|JsxClosingFragment|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxClosingFragment> ts.isJsxClosingFragment value
        let inline (|JsxAttribute|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxAttribute> ts.isJsxAttribute value
        let inline (|JsxAttributes|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxAttributes> ts.isJsxAttributes value
        let inline (|JsxSpreadAttribute|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxSpreadAttribute> ts.isJsxSpreadAttribute value
        let inline (|JsxExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxExpression> ts.isJsxExpression value
        let inline (|JsxNamespacedName|_|) (value: Ts.Node) : _ = predToOption<Ts.JsxNamespacedName> ts.isJsxNamespacedName value
        let inline (|CaseClause|_|) (value: Ts.Node) : _ = predToOption<Ts.CaseClause> ts.isCaseClause value
        let inline (|DefaultClause|_|) (value: Ts.Node) : _ = predToOption<Ts.DefaultClause> ts.isDefaultClause value
        let inline (|HeritageClause|_|) (value: Ts.Node) : _ = predToOption<Ts.HeritageClause> ts.isHeritageClause value
        let inline (|CatchClause|_|) (value: Ts.Node) : _ = predToOption<Ts.CatchClause> ts.isCatchClause value
        let inline (|PropertyAssignment|_|) (value: Ts.Node) : _ = predToOption<Ts.PropertyAssignment> ts.isPropertyAssignment value
        let inline (|ShorthandPropertyAssignment|_|) (value: Ts.Node) : _ = predToOption<Ts.ShorthandPropertyAssignment> ts.isShorthandPropertyAssignment value
        let inline (|SpreadAssignment|_|) (value: Ts.Node) : _ = predToOption<Ts.SpreadAssignment> ts.isSpreadAssignment value
        let inline (|EnumMember|_|) (value: Ts.Node) : _ = predToOption<Ts.EnumMember> ts.isEnumMember value
        let inline (|SourceFile|_|) (value: Ts.Node) : _ = predToOption<Ts.SourceFile> ts.isSourceFile value
        let inline (|Bundle|_|) (value: Ts.Node) : _ = predToOption<Ts.Bundle> ts.isBundle value
        let inline (|JSDocTypeExpression|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocTypeExpression> ts.isJSDocTypeExpression value
        let inline (|JSDocNameReference|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocNameReference> ts.isJSDocNameReference value
        let inline (|JSDocMemberName|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocMemberName> ts.isJSDocMemberName value
        let inline (|JSDocLink|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocLink> ts.isJSDocLink value
        let inline (|JSDocLinkCode|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocLinkCode> ts.isJSDocLinkCode value
        let inline (|JSDocLinkPlain|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocLinkPlain> ts.isJSDocLinkPlain value
        let inline (|JSDocAllType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocAllType> ts.isJSDocAllType value
        let inline (|JSDocUnknownType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocUnknownType> ts.isJSDocUnknownType value
        let inline (|JSDocNullableType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocNullableType> ts.isJSDocNullableType value
        let inline (|JSDocNonNullableType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocNonNullableType> ts.isJSDocNonNullableType value
        let inline (|JSDocOptionalType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocOptionalType> ts.isJSDocOptionalType value
        let inline (|JSDocFunctionType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocFunctionType> ts.isJSDocFunctionType value
        let inline (|JSDocVariadicType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocVariadicType> ts.isJSDocVariadicType value
        let inline (|JSDocNamepathType|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocNamepathType> ts.isJSDocNamepathType value
        let inline (|JSDoc|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDoc> ts.isJSDoc value
        let inline (|JSDocTypeLiteral|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocTypeLiteral> ts.isJSDocTypeLiteral value
        let inline (|JSDocSignature|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocSignature> ts.isJSDocSignature value
        let inline (|JSDocAugmentsTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocAugmentsTag> ts.isJSDocAugmentsTag value
        let inline (|JSDocAuthorTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocAuthorTag> ts.isJSDocAuthorTag value
        let inline (|JSDocClassTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocClassTag> ts.isJSDocClassTag value
        let inline (|JSDocCallbackTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocCallbackTag> ts.isJSDocCallbackTag value
        let inline (|JSDocPublicTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocPublicTag> ts.isJSDocPublicTag value
        let inline (|JSDocPrivateTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocPrivateTag> ts.isJSDocPrivateTag value
        let inline (|JSDocProtectedTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocProtectedTag> ts.isJSDocProtectedTag value
        let inline (|JSDocReadonlyTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocReadonlyTag> ts.isJSDocReadonlyTag value
        let inline (|JSDocOverrideTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocOverrideTag> ts.isJSDocOverrideTag value
        let inline (|JSDocOverloadTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocOverloadTag> ts.isJSDocOverloadTag value
        let inline (|JSDocDeprecatedTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocDeprecatedTag> ts.isJSDocDeprecatedTag value
        let inline (|JSDocSeeTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocSeeTag> ts.isJSDocSeeTag value
        let inline (|JSDocEnumTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocEnumTag> ts.isJSDocEnumTag value
        let inline (|JSDocParameterTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocParameterTag> ts.isJSDocParameterTag value
        let inline (|JSDocReturnTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocReturnTag> ts.isJSDocReturnTag value
        let inline (|JSDocThisTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocThisTag> ts.isJSDocThisTag value
        let inline (|JSDocTypeTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocTypeTag> ts.isJSDocTypeTag value
        let inline (|JSDocTemplateTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocTemplateTag> ts.isJSDocTemplateTag value
        let inline (|JSDocTypedefTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocTypedefTag> ts.isJSDocTypedefTag value
        let inline (|JSDocUnknownTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocUnknownTag> ts.isJSDocUnknownTag value
        let inline (|JSDocPropertyTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocPropertyTag> ts.isJSDocPropertyTag value
        let inline (|JSDocImplementsTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocImplementsTag> ts.isJSDocImplementsTag value
        let inline (|JSDocSatisfiesTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocSatisfiesTag> ts.isJSDocSatisfiesTag value
        let inline (|JSDocThrowsTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocThrowsTag> ts.isJSDocThrowsTag value
        let inline (|JSDocImportTag|_|) (value: Ts.Node) : _ = predToOption<Ts.JSDocImportTag> ts.isJSDocImportTag value
        let inline (|QuestionOrExclamationToken|_|) (value: Ts.Node) : _ = predToOption<Ts.Node> ts.isQuestionOrExclamationToken value
        let inline (|IdentifierOrThisTypeNode|_|) (value: Ts.Node) : _ = predToOption<Ts.Node> ts.isIdentifierOrThisTypeNode value
        let inline (|ReadonlyKeywordOrPlusOrMinusToken|_|) (value: Ts.Node) : _ = predToOption<Ts.Node> ts.isReadonlyKeywordOrPlusOrMinusToken value
        let inline (|QuestionOrPlusOrMinusToken|_|) (value: Ts.Node) : _ = predToOption<Ts.Node> ts.isQuestionOrPlusOrMinusToken value
        let inline (|ModuleName|_|) (value: Ts.Node) : _ = forcePredToOption<Ts.ModuleName> ts.isModuleName value
        let inline (|BinaryOperatorToken|_|) (value: Ts.Node) : _ = predToOption<Ts.BinaryOperatorToken> ts.isBinaryOperatorToken value
        let inline (|PropertyName|_|) (value: Ts.Node) : _ = forcePredToOption<Ts.PropertyName> ts.isPropertyName value
        #nowarn 25
        module EntityNamePatterns =
            let (|Identifier|QualifiedName|): Ts.EntityName -> _ = unbox<Ts.Node> >>  function
                | Identifier node -> Identifier node
                | QualifiedName node -> QualifiedName node
        module MemberNamePatterns =
            let (|Identifier|PrivateIdentifier|): Ts.MemberName -> _ = unbox >> function
                | Identifier node -> Identifier node
                | PrivateIdentifier node -> PrivateIdentifier node
        module PropertyNamePatterns =
            let (|Identifier|StringLiteral|NoSubstitutionTemplateLiteral|NumericLiteral|ComputedPropertyName|PrivateIdentifier|BigIntLiteral|): Ts.PropertyName -> _ = unbox >> function
                | Identifier node -> Identifier node
                | StringLiteral node -> StringLiteral node
                | NoSubstitutionTemplateLiteral node -> NoSubstitutionTemplateLiteral node
                | NumericLiteral node -> NumericLiteral node
                | ComputedPropertyName node -> ComputedPropertyName node
                | PrivateIdentifier node -> PrivateIdentifier node
                | BigIntLiteral node -> BigIntLiteral node
        module JsxAttributeNamePatterns =
            let (|Identifier|JsxNamespacedName|): Ts.JsxAttributeName -> _ = unbox >> function
                | Identifier node -> Identifier node
                | JsxNamespacedName node -> JsxNamespacedName node
        module StringLiteralLikePatterns =
            let (|StringLiteral|NoSubstitutionTemplateLiteral|): Ts.StringLiteralLike -> _ = unbox >> function
                | StringLiteral node -> StringLiteral node
                | NoSubstitutionTemplateLiteral node -> NoSubstitutionTemplateLiteral node
        module BindingPatternPatterns =
            let (|ObjectBindingPattern|ArrayBindingPattern|): Ts.BindingPattern -> _ = unbox >> function
                | ObjectBindingPattern node -> ObjectBindingPattern node
                | ArrayBindingPattern node -> ArrayBindingPattern node
        module EntityNameExpressionPatterns =
            let (|Identifier|PropertyAccessExpression|): Ts.EntityNameExpression -> _ = unbox >> function
                | Identifier node -> Identifier node
                | PropertyAccessExpression node -> PropertyAccessExpression node
        module DeclarationNamePatterns =
            let (|PropertyName|JsxNamespacedName|ElementAccessExpression|BindingPattern|EntityNameExpression|): Ts.DeclarationName -> _ = unbox >> function
                | PropertyName node -> PropertyName node
                | JsxNamespacedName node -> JsxNamespacedName node
                | ElementAccessExpression node -> ElementAccessExpression node
                | ObjectBindingPattern node -> BindingPattern (unbox<Ts.BindingPattern> node)
                | ArrayBindingPattern node -> BindingPattern (unbox<Ts.BindingPattern> node)
                | PropertyAccessExpression node -> EntityNameExpression node
        module BindingNamePatterns =
            let (|Identifier|ObjectBindingPattern|ArrayBindingPattern|): Ts.BindingName -> _ = unbox >> function
                | Identifier node -> Identifier node
                | ObjectBindingPattern node -> ObjectBindingPattern node
                | ArrayBindingPattern node -> ArrayBindingPattern node
        module ModuleNamePatterns =
            let (|Identifier|StringLiteral|): Ts.ModuleName -> _ = unbox >> function
                | Identifier node -> Identifier node
                | StringLiteral node -> StringLiteral node
        let (|HasTypeArguments|_|): Ts.Node -> Ts.HasTypeArguments option = function
            | CallExpression node -> !^ node |> Some
            | NewExpression node -> !^ node |> Some
            | TaggedTemplateExpression node -> !^ node |> Some
            | JsxOpeningElement node -> !^ node |> Some
            | JsxSelfClosingElement node -> !^ node |> Some
            | _ -> None
        module NamedExportBindingsPatterns =
            let (|NamedExports|NamespaceExport|): Ts.NamedExportBindings -> _ = unbox >> function
                | NamedExports node -> NamedExports node
                | NamespaceExport node -> NamespaceExport node
        module NamedImportBindingsPatterns =
            let (|NamedImports|NamespaceImport|): Ts.NamedImportBindings -> _ = unbox >> function
                | NamedImports node -> NamedImports node
                | NamespaceImport node -> NamespaceImport node
        #warnon 25
    /// <summary>
    /// Patterns for determining the presence of a TypeFlag on a <c>Ts.Type</c> derivative, and
    /// casting to that <c>Ts.Type</c> derivative if one exists (else just acts as a boolean pattern matcher).
    /// </summary>
    module TypeFlags =
        type TF = Ts.TypeFlags
        /// <summary>
        /// Inline func that enforces the input to have a member <c>_.flags</c> of type <c>Ts.TypeFlags</c>
        /// </summary>
        let inline getFlags (token: ^T when ^T:(member flags: Ts.TypeFlags)) = token.flags
        /// <summary>
        /// Inline func that checks whether the input has the given flag
        /// </summary>
        let inline hasFlag (flag: TF) token = (getFlags token).HasFlag(flag)
        /// <summary>
        /// Inline func that checks whether the input has the given flag, and then casts it to an option if true.
        /// </summary>
        let inline hasFlagThenUnbox (flag: TF) token = if (getFlags token).HasFlag(flag) then unbox token |> Some else None
        let inline (|BigInt|_|) token = token |> hasFlag TF.BigInt
        let inline (|Boolean|_|) token = token |> hasFlag TF.Boolean
        let inline (|BooleanLiteral|_|) (token: ^T when ^T:(member flags: Ts.TypeFlags) and ^T :> Ts.Type)= token |> hasFlag TF.BooleanLiteral
        let inline (|String|_|) token = token |> hasFlag TF.String
        let inline (|StringLiteral|_|) token: Ts.StringLiteralType option = token |> hasFlagThenUnbox TF.StringLiteral
        let inline (|Number|_|) token = token |> hasFlag TF.Number
        let inline (|NumberLiteral|_|) token: Ts.NumberLiteralType option = token |> hasFlagThenUnbox TF.NumberLiteral
        let inline (|Any|_|) token = token |> hasFlag TF.Any
        let inline (|Null|_|) token = token |> hasFlag TF.Null
        let inline (|Undefined|_|) token = token |> hasFlag TF.Undefined
        let inline (|Enum|_|) token: Ts.EnumType option = token |> hasFlagThenUnbox TF.Enum
        let inline (|EnumLiteral|_|) token: Ts.LiteralType option  = token |> hasFlagThenUnbox TF.EnumLiteral
        let inline (|Freshable|_|) token: Ts.FreshableType option = token |> hasFlagThenUnbox TF.Freshable
        let inline (|Literal|_|) token: Ts.LiteralType option = token |> hasFlagThenUnbox TF.Literal
        let inline (|Object|_|) token: Ts.ObjectType option = token |> hasFlagThenUnbox TF.Object
        let inline (|Void|_|) token = token |> hasFlag TF.Void
        let inline (|UnionOrIntersection|_|) token: Ts.UnionOrIntersectionType option = token |> hasFlagThenUnbox TF.UnionOrIntersection
        let inline (|Union|_|) token: Ts.UnionType option = token |> hasFlagThenUnbox TF.Union
        let inline (|Intersection|_|) token: Ts.IntersectionType option = token |> hasFlagThenUnbox TF.Intersection
        let inline (|StructuredType|_|) token: Ts.StructuredType option = token |> hasFlagThenUnbox TF.StructuredType
        let inline (|Symbol|_|) token: Ts.UniqueESSymbolType option = token |> hasFlagThenUnbox TF.ESSymbol
        let inline (|Never|_|) token = token |> hasFlag TF.Never
        let inline (|Index|_|) token: Ts.IndexType option = token |> hasFlagThenUnbox TF.Index
        let inline (|IndexedAccess|_|) token: Ts.IndexedAccessType option = token |> hasFlagThenUnbox TF.IndexedAccess
        let inline (|Conditional|_|) token: Ts.ConditionalType option = token |> hasFlagThenUnbox TF.Conditional
        let inline (|TypeParameter|_|) token: Ts.TypeParameter option = token |> hasFlagThenUnbox TF.TypeParameter
        let inline (|TypeVariable|_|) token: Ts.TypeVariable option = token |> hasFlagThenUnbox TF.TypeVariable
        let inline (|BigIntLiteral|_|) token: Ts.BigIntLiteralType option = token |> hasFlagThenUnbox TF.BigIntLiteral
        let inline (|UniqueESSymbol|_|) token: Ts.UniqueESSymbolType option = token |> hasFlagThenUnbox TF.UniqueESSymbol
        let inline (|ESSymbol|_|) token = token |> hasFlag TF.ESSymbol
        let inline (|Substitution|_|) token: Ts.SubstitutionType option = token |> hasFlagThenUnbox TF.Substitution
        let inline (|Unknown|_|) token = token |> hasFlag TF.Unknown
        let inline (|NonPrimitive|_|) token = token |> hasFlag TF.NonPrimitive
        /// <summary>
        /// Pattern matches for disambiguating <c>Ts.TypeVariable</c> erased unions
        /// </summary>
        module TypeVariablePatterns =
            let inline (|TypeParameter|IndexedAccess|) (token: Ts.TypeVariable) =
                match unbox<Ts.Type> token with
                | TypeParameter typ -> TypeParameter typ
                | IndexedAccess typ -> IndexedAccess typ
                | _ -> failwith "Unreachable"
        let inline (|TemplateLiteral|_|) token: Ts.TemplateLiteralType option = token |> hasFlagThenUnbox TF.TemplateLiteral
        /// <summary>
        /// Pattern matches for disambiguating <c>Ts.StructuredType</c> erased unions
        /// </summary>
        module StructuredTypePatterns =
            let inline (|Union|Intersection|Object|) (token: Ts.StructuredType) =
                match unbox<Ts.Type> token with
                | Union typ -> Union typ
                | Intersection typ -> Intersection typ
                | Object typ -> Object typ
                | _ -> failwith "(Unreachable)"
        let inline (|StructuredOrInstantiable|_|) token: U4<Ts.InstantiableType, Ts.ObjectType, Ts.UnionType, Ts.IntersectionType> option =
            token |> hasFlagThenUnbox TF.StructuredOrInstantiable
        /// <summary>
        /// Pattern matches for disambiguating <c>Ts.StructuredTypeOrInstantiable</c> erased unions
        /// </summary>
        module StructuredOrInstantiablePatterns =
            let inline (|Union|Intersection|Object|Instantiable|) (token: U4<Ts.InstantiableType, Ts.ObjectType, Ts.UnionType, Ts.IntersectionType>) =
                match unbox<Ts.Type> token with
                | Union typ -> Union typ
                | Intersection typ -> Intersection typ
                | Object typ -> Object typ
                | _ -> Instantiable (unbox<Ts.InstantiableType> token)
    /// <summary>
    /// Pattern match helpers to type cast <c>Ts.ObjectType</c> based on the <c>Ts.ObjectFlags</c>
    /// </summary>
    module ObjectFlags =
        type OF = Ts.ObjectFlags
        let inline getFlags (token: ^T when ^T :> Ts.ObjectType) = token.objectFlags
        let inline hasFlag (flag: OF) token = (getFlags token).HasFlag(flag)
        let inline hasFlagThenUnbox (flag: OF) token = if (getFlags token).HasFlag(flag) then unbox token |> Some else None
        let inline (|Class|_|) token: Ts.InterfaceType option = token |> hasFlagThenUnbox OF.Class
        let inline (|Interface|_|) token : Ts.InterfaceType option = token |> hasFlagThenUnbox OF.Interface
        let inline (|Reference|_|) token: Ts.TypeReference option = token |> hasFlagThenUnbox OF.Reference
        let inline (|Tuple|_|) token: Ts.TupleType option = token |> hasFlagThenUnbox OF.Tuple
        let inline (|Anonymous|_|) token = token |> hasFlag OF.Anonymous
        let inline (|Mapped|_|) token = token |> hasFlag OF.Mapped
        let inline (|ObjectLiteral|_|) token = token |> hasFlag OF.ObjectLiteral
        let inline (|Instantiated|_|) token = token |> hasFlag OF.Instantiated
        let inline (|EvolvingArray|_|) token = token |> hasFlag OF.EvolvingArray
        let inline (|ArrayLiteral|_|) token = token |> hasFlag OF.ArrayLiteral
        // let (|InterfaceDeclaredMembers|_|) token: Ts.InterfaceTypeWithDeclaredMembers option = token |> hasFlagThenUnbox
    /// <summary>
    /// Pattern match helpers to categorise subset SyntaxKind derivatives
    /// </summary>
    module SyntaxKind =
        type SK = Ts.SyntaxKind
        let inline getKind (token: ^T when ^T:(member kind: Ts.SyntaxKind)) = token.kind
        let inline (|CategoryTrivia|_|) token =
            match getKind token with
            | Ts.SyntaxKind.SingleLineCommentTrivia
            | Ts.SyntaxKind.MultiLineCommentTrivia
            | Ts.SyntaxKind.NewLineTrivia
            | Ts.SyntaxKind.WhitespaceTrivia
            | Ts.SyntaxKind.ShebangTrivia
            | Ts.SyntaxKind.ConflictMarkerTrivia -> true
            | _ -> false
        let inline (|CategoryLiteral|_|) token =
            match getKind token with
            | SK.NumericLiteral
            | SK.BigIntLiteral
            | SK.StringLiteral
            | SK.JsxText
            | SK.JsxTextAllWhiteSpaces
            | SK.RegularExpressionLiteral
            | SK.NoSubstitutionTemplateLiteral -> true
            | _ -> false

        let inline (|CategoryPseudoLiteral|_|) token =
            match getKind token with
            | SK.TemplateHead | SK.TemplateMiddle | SK.TemplateTail -> true
            | _ -> false
        let inline (|CategoryPunctuation|_|) token =
            match getKind token with
            | SK.OpenBraceToken
            | SK.CloseBraceToken
            | SK.OpenParenToken
            | SK.CloseParenToken
            | SK.OpenBracketToken
            | SK.CloseBracketToken
            | SK.DotToken
            | SK.DotDotDotToken
            | SK.SemicolonToken
            | SK.CommaToken
            | SK.QuestionDotToken
            | SK.LessThanToken
            | SK.LessThanSlashToken
            | SK.GreaterThanToken
            | SK.LessThanEqualsToken
            | SK.GreaterThanEqualsToken
            | SK.EqualsEqualsToken
            | SK.ExclamationEqualsToken
            | SK.EqualsEqualsEqualsToken
            | SK.ExclamationEqualsEqualsToken
            | SK.EqualsGreaterThanToken
            | SK.PlusToken
            | SK.MinusToken
            | SK.AsteriskToken
            | SK.AsteriskAsteriskToken
            | SK.SlashToken
            | SK.PercentToken
            | SK.PlusPlusToken
            | SK.MinusMinusToken
            | SK.LessThanLessThanToken
            | SK.GreaterThanGreaterThanToken
            | SK.GreaterThanGreaterThanGreaterThanToken
            | SK.AmpersandToken
            | SK.BarToken
            | SK.CaretToken
            | SK.ExclamationToken
            | SK.TildeToken
            | SK.AmpersandAmpersandToken
            | SK.AmpersandAmpersandEqualsToken
            | SK.BarBarToken
            | SK.BarBarEqualsToken
            | SK.QuestionQuestionToken
            | SK.QuestionQuestionEqualsToken
            | SK.QuestionToken
            | SK.ColonToken
            | SK.AtToken
            | SK.BacktickToken
            | SK.HashToken
            | SK.EqualsToken
            | SK.PlusEqualsToken
            | SK.MinusEqualsToken
            | SK.AsteriskEqualsToken
            | SK.AsteriskAsteriskEqualsToken
            | SK.SlashEqualsToken
            | SK.PercentEqualsToken
            | SK.LessThanLessThanEqualsToken
            | SK.GreaterThanGreaterThanEqualsToken
            | SK.GreaterThanGreaterThanGreaterThanEqualsToken
            | SK.AmpersandEqualsToken
            | SK.BarEqualsToken
            | SK.CaretEqualsToken -> true
            | _ -> false

        let inline (|CategoryKeyword|_|) token =
            match getKind token with
            | SK.AbstractKeyword
            | SK.AccessorKeyword
            | SK.AnyKeyword
            | SK.AsKeyword
            | SK.AssertsKeyword
            | SK.AssertKeyword
            | SK.AsyncKeyword
            | SK.AwaitKeyword
            | SK.BigIntKeyword
            | SK.BooleanKeyword
            | SK.BreakKeyword
            | SK.CaseKeyword
            | SK.CatchKeyword
            | SK.ClassKeyword
            | SK.ConstKeyword
            | SK.ConstructorKeyword
            | SK.ContinueKeyword
            | SK.DebuggerKeyword
            | SK.DeclareKeyword
            | SK.DefaultKeyword
            | SK.DeferKeyword
            | SK.DeleteKeyword
            | SK.DoKeyword
            | SK.ElseKeyword
            | SK.EnumKeyword
            | SK.ExportKeyword
            | SK.ExtendsKeyword
            | SK.FalseKeyword
            | SK.FinallyKeyword
            | SK.ForKeyword
            | SK.FromKeyword
            | SK.FunctionKeyword
            | SK.GetKeyword
            | SK.GlobalKeyword
            | SK.IfKeyword
            | SK.ImplementsKeyword
            | SK.ImportKeyword
            | SK.InferKeyword
            | SK.InKeyword
            | SK.InstanceOfKeyword
            | SK.InterfaceKeyword
            | SK.IntrinsicKeyword
            | SK.IsKeyword
            | SK.KeyOfKeyword
            | SK.LetKeyword
            | SK.ModuleKeyword
            | SK.NamespaceKeyword
            | SK.NeverKeyword
            | SK.NewKeyword
            | SK.NullKeyword
            | SK.NumberKeyword
            | SK.ObjectKeyword
            | SK.OfKeyword
            | SK.PackageKeyword
            | SK.PrivateKeyword
            | SK.ProtectedKeyword
            | SK.PublicKeyword
            | SK.ReadonlyKeyword
            | SK.OutKeyword
            | SK.OverrideKeyword
            | SK.RequireKeyword
            | SK.ReturnKeyword
            | SK.SatisfiesKeyword
            | SK.SetKeyword
            | SK.StaticKeyword
            | SK.StringKeyword
            | SK.SuperKeyword
            | SK.SwitchKeyword
            | SK.SymbolKeyword
            | SK.ThisKeyword
            | SK.ThrowKeyword
            | SK.TrueKeyword
            | SK.TryKeyword
            | SK.TypeKeyword
            | SK.TypeOfKeyword
            | SK.UndefinedKeyword
            | SK.UniqueKeyword
            | SK.UnknownKeyword
            | SK.UsingKeyword
            | SK.VarKeyword
            | SK.VoidKeyword
            | SK.WhileKeyword
            | SK.WithKeyword
            | SK.YieldKeyword -> true
            | _ -> false
        let inline (|CategoryModifier|_|) token =
            match getKind token with
            | SK.AbstractKeyword
            | SK.AccessorKeyword
            | SK.AsyncKeyword
            | SK.ConstKeyword
            | SK.DeclareKeyword
            | SK.DefaultKeyword
            | SK.ExportKeyword
            | SK.InKeyword
            | SK.PrivateKeyword
            | SK.ProtectedKeyword
            | SK.PublicKeyword
            | SK.ReadonlyKeyword
            | SK.OutKeyword
            | SK.OverrideKeyword
            | SK.StaticKeyword -> true
            | _ -> false

        let inline (|CategoryKeywordType|_|) token =
            match getKind token with
            | SK.AnyKeyword
            | SK.BigIntKeyword
            | SK.BooleanKeyword
            | SK.IntrinsicKeyword
            | SK.NeverKeyword
            | SK.NumberKeyword
            | SK.ObjectKeyword
            | SK.StringKeyword
            | SK.SymbolKeyword
            | SK.UndefinedKeyword
            | SK.UnknownKeyword
            | SK.VoidKeyword -> true
            | _ -> false

        let inline (|CategoryToken|_|) token =
            match token with
            | CategoryTrivia | CategoryLiteral | CategoryPseudoLiteral | CategoryPunctuation | CategoryKeyword -> true
            | _ ->
                match getKind token with
                | SK.Unknown
                | SK.EndOfFileToken
                | SK.Identifier -> true
                | _ -> false

        let inline (|CategoryJsxToken|_|) token =
            match getKind token with
            | SK.LessThanSlashToken
            | SK.EndOfFileToken
            | SK.ConflictMarkerTrivia
            | SK.JsxText
            | SK.JsxTextAllWhiteSpaces
            | SK.OpenBraceToken
            | SK.LessThanToken -> true
            | _ -> false

        let inline (|CategoryJSDoc|_|) token =
            match getKind token with
            | SK.EndOfFileToken
            | SK.WhitespaceTrivia
            | SK.AtToken
            | SK.NewLineTrivia
            | SK.AsteriskToken
            | SK.OpenBraceToken
            | SK.CloseBraceToken
            | SK.LessThanToken
            | SK.GreaterThanToken
            | SK.OpenBracketToken
            | SK.CloseBracketToken
            | SK.OpenParenToken
            | SK.CloseParenToken
            | SK.EqualsToken
            | SK.CommaToken
            | SK.DotToken
            | SK.Identifier
            | SK.BacktickToken
            | SK.HashToken
            | SK.Unknown -> true
            | _ ->
                match token with
                | CategoryKeyword -> true
                | _ -> false
        /// <summary>
        /// Pattern match helpers that cast the input to the corresponding node based on the flag/kind.
        /// </summary>
        let inline hasFlag (flag: SK) token = (getKind token) = flag
        let inline hasFlagTo (flag: SK) token = if (getKind token) = flag then unbox token |> Some else None
        let inline (|EndOfFileToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.EndOfFileToken token
        let inline (|CommaToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.CommaToken token
        let inline (|DotToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.DotToken token
        let inline (|DotDotDotToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.DotDotDotToken token
        let inline (|SemicolonToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.SemicolonToken token
        let inline (|ColonToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.ColonToken token
        let inline (|QuestionToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.QuestionToken token
        let inline (|ExclamationToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.ExclamationToken token
        let inline (|EqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.EqualsToken token
        let inline (|PlusToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.PlusToken token
        let inline (|MinusToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.MinusToken token
        let inline (|AsteriskToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AsteriskToken token
        let inline (|SlashToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.SlashToken token
        let inline (|PercentToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.PercentToken token
        let inline (|LessThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.LessThanToken token
        let inline (|GreaterThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanToken token
        let inline (|LessThanEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.LessThanEqualsToken token
        let inline (|GreaterThanEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanEqualsToken token
        let inline (|EqualsEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.EqualsEqualsToken token
        let inline (|ExclamationEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.ExclamationEqualsToken token
        let inline (|EqualsEqualsEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.EqualsEqualsEqualsToken token
        let inline (|ExclamationEqualsEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.ExclamationEqualsEqualsToken token
        let inline (|AmpersandToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AmpersandToken token
        let inline (|BarToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.BarToken token
        let inline (|CaretToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.CaretToken token
        let inline (|AmpersandAmpersandToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AmpersandAmpersandToken token
        let inline (|BarBarToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.BarBarToken token
        let inline (|QuestionQuestionToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.QuestionQuestionToken token
        let inline (|PlusPlusToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.PlusPlusToken token
        let inline (|MinusMinusToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.MinusMinusToken token
        let inline (|LessThanLessThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.LessThanLessThanToken token
        let inline (|GreaterThanGreaterThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanGreaterThanToken token
        let inline (|GreaterThanGreaterThanGreaterThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanGreaterThanGreaterThanToken token
        let inline (|PlusEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.PlusEqualsToken token
        let inline (|MinusEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.MinusEqualsToken token
        let inline (|AsteriskEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AsteriskEqualsToken token
        let inline (|SlashEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.SlashEqualsToken token
        let inline (|PercentEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.PercentEqualsToken token
        let inline (|LessThanLessThanEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.LessThanLessThanEqualsToken token
        let inline (|GreaterThanGreaterThanEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanGreaterThanEqualsToken token
        let inline (|GreaterThanGreaterThanGreaterThanEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.GreaterThanGreaterThanGreaterThanEqualsToken token
        let inline (|AmpersandEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AmpersandEqualsToken token
        let inline (|BarEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.BarEqualsToken token
        let inline (|CaretEqualsToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.CaretEqualsToken token
        let inline (|EqualsGreaterThanToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.EqualsGreaterThanToken token
        let inline (|AtToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.AtToken token
        let inline (|HashToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.HashToken token
        let inline (|QuestionDotToken|_|) token: Ts.Token<SK> option = hasFlagTo SK.QuestionDotToken token

        // Keywords (represent as tokens)
        let inline (|ThisKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ThisKeyword token
        let inline (|SuperKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.SuperKeyword token
        let inline (|NullKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.NullKeyword token
        let inline (|TrueKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.TrueKeyword token
        let inline (|FalseKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.FalseKeyword token
        let inline (|AnyKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.AnyKeyword token
        let inline (|NumberKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.NumberKeyword token
        let inline (|BigIntKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.BigIntKeyword token
        let inline (|StringKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.StringKeyword token
        let inline (|SymbolKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.SymbolKeyword token
        let inline (|BooleanKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.BooleanKeyword token
        let inline (|VoidKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.VoidKeyword token
        let inline (|UndefinedKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.UndefinedKeyword token
        let inline (|NeverKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.NeverKeyword token
        let inline (|ObjectKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ObjectKeyword token
        let inline (|InKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.InKeyword token
        let inline (|InstanceOfKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.InstanceOfKeyword token
        let inline (|TypeOfKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.TypeOfKeyword token
        let inline (|NewKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.NewKeyword token
        let inline (|DeleteKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.DeleteKeyword token
        let inline (|ReturnKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ReturnKeyword token
        let inline (|YieldKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.YieldKeyword token
        let inline (|AwaitKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.AwaitKeyword token
        let inline (|BreakKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.BreakKeyword token
        let inline (|ContinueKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ContinueKeyword token
        let inline (|IfKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.IfKeyword token
        let inline (|ElseKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ElseKeyword token
        let inline (|ForKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ForKeyword token
        let inline (|WhileKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.WhileKeyword token
        let inline (|DoKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.DoKeyword token
        let inline (|SwitchKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.SwitchKeyword token
        let inline (|CaseKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.CaseKeyword token
        let inline (|DefaultKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.DefaultKeyword token
        let inline (|TryKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.TryKeyword token
        let inline (|CatchKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.CatchKeyword token
        let inline (|FinallyKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.FinallyKeyword token
        let inline (|ThrowKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ThrowKeyword token
        let inline (|ClassKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ClassKeyword token
        let inline (|ExtendsKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ExtendsKeyword token
        let inline (|ImplementsKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ImplementsKeyword token
        let inline (|InterfaceKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.InterfaceKeyword token
        let inline (|EnumKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.EnumKeyword token
        let inline (|ConstKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ConstKeyword token
        let inline (|LetKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.LetKeyword token
        let inline (|VarKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.VarKeyword token
        let inline (|FunctionKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.FunctionKeyword token
        let inline (|GetKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.GetKeyword token
        let inline (|SetKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.SetKeyword token
        let inline (|AsyncKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.AsyncKeyword token
        let inline (|StaticKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.StaticKeyword token
        let inline (|PublicKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.PublicKeyword token
        let inline (|PrivateKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.PrivateKeyword token
        let inline (|ProtectedKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ProtectedKeyword token
        let inline (|ReadOnlyKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ReadonlyKeyword token
        let inline (|AbstractKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.AbstractKeyword token
        let inline (|OverrideKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.OverrideKeyword token
        let inline (|DeclareKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.DeclareKeyword token
        let inline (|ExportKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ExportKeyword token
        let inline (|ModuleKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.ModuleKeyword token
        let inline (|NamespaceKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.NamespaceKeyword token
        let inline (|TypeKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.TypeKeyword token
        let inline (|AsKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.AsKeyword token
        let inline (|IsKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.IsKeyword token
        let inline (|InferKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.InferKeyword token
        let inline (|IntrinsicKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.IntrinsicKeyword token
        let inline (|GlobalKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.GlobalKeyword token
        let inline (|WithKeyword|_|) token: Ts.Token<SK> option = hasFlagTo SK.WithKeyword token

        // -----------------------------
        // Literals & Template
        // -----------------------------
        let inline (|Unknown|_|) token: Ts.Node option = hasFlagTo SK.Unknown token |> Option.orElse (hasFlagTo SK.UnknownKeyword token)
        let inline (|NumericLiteral|_|) token: Ts.NumericLiteral option = hasFlagTo SK.NumericLiteral token
        let inline (|BigIntLiteral|_|) token: Ts.BigIntLiteral option = hasFlagTo SK.BigIntLiteral token
        let inline (|StringLiteral|_|) token: Ts.StringLiteral option = hasFlagTo SK.StringLiteral token
        let inline (|RegularExpressionLiteral|_|) token: Ts.RegularExpressionLiteral option = hasFlagTo SK.RegularExpressionLiteral token
        let inline (|NoSubstitutionTemplateLiteral|_|) token: Ts.NoSubstitutionTemplateLiteral option = hasFlagTo SK.NoSubstitutionTemplateLiteral token
        let inline (|TemplateHead|_|) token: Ts.TemplateHead option = hasFlagTo SK.TemplateHead token
        let inline (|TemplateMiddle|_|) token: Ts.TemplateMiddle option = hasFlagTo SK.TemplateMiddle token
        let inline (|TemplateTail|_|) token: Ts.TemplateTail option = hasFlagTo SK.TemplateTail token
        let inline (|TemplateExpression|_|) token: Ts.TemplateExpression option = hasFlagTo SK.TemplateExpression token
        let inline (|TaggedTemplateExpression|_|) token: Ts.TaggedTemplateExpression option = hasFlagTo SK.TaggedTemplateExpression token
        let inline (|TemplateSpan|_|) token: Ts.TemplateSpan option = hasFlagTo SK.TemplateSpan token
        let inline (|JsxText|_|) token: Ts.JsxText option = hasFlagTo SK.JsxText token
        let inline (|JsxTextAllWhiteSpaces|_|) token: Ts.JsxText option = hasFlagTo SK.JsxTextAllWhiteSpaces token

        // -----------------------------
        // Names, Binding & Patterns
        // -----------------------------
        let inline (|Identifier|_|) token: Ts.Identifier option = hasFlagTo SK.Identifier token
        let inline (|PrivateIdentifier|_|) token: Ts.PrivateIdentifier option = hasFlagTo SK.PrivateIdentifier token
        let inline (|QualifiedName|_|) token: Ts.QualifiedName option = hasFlagTo SK.QualifiedName token
        let inline (|ComputedPropertyName|_|) token: Ts.ComputedPropertyName option = hasFlagTo SK.ComputedPropertyName token
        let inline (|ObjectBindingPattern|_|) token: Ts.ObjectBindingPattern option = hasFlagTo SK.ObjectBindingPattern token
        let inline (|ArrayBindingPattern|_|) token: Ts.ArrayBindingPattern option = hasFlagTo SK.ArrayBindingPattern token
        let inline (|BindingElement|_|) token: Ts.BindingElement option = hasFlagTo SK.BindingElement token
        let inline (|OmittedExpression|_|) token: Ts.OmittedExpression option = hasFlagTo SK.OmittedExpression token

        // -----------------------------
        // Expressions
        // -----------------------------
        let inline (|ParenthesizedExpression|_|) token: Ts.ParenthesizedExpression option = hasFlagTo SK.ParenthesizedExpression token
        let inline (|ArrayLiteralExpression|_|) token: Ts.ArrayLiteralExpression option = hasFlagTo SK.ArrayLiteralExpression token
        let inline (|ObjectLiteralExpression|_|) token: Ts.ObjectLiteralExpression option = hasFlagTo SK.ObjectLiteralExpression token
        let inline (|PropertyAccessExpression|_|) token: Ts.PropertyAccessExpression option = hasFlagTo SK.PropertyAccessExpression token
        let inline (|ElementAccessExpression|_|) token: Ts.ElementAccessExpression option = hasFlagTo SK.ElementAccessExpression token
        let inline (|CallExpression|_|) token: Ts.CallExpression option = hasFlagTo SK.CallExpression token
        let inline (|NewExpression|_|) token: Ts.NewExpression option = hasFlagTo SK.NewExpression token
        let inline (|TypeAssertionExpression|_|) token: Ts.TypeAssertion option = hasFlagTo SK.TypeAssertionExpression token
        let inline (|AsExpression|_|) token: Ts.AsExpression option = hasFlagTo SK.AsExpression token
        let inline (|NonNullExpression|_|) token: Ts.NonNullExpression option = hasFlagTo SK.NonNullExpression token
        let inline (|PrefixUnaryExpression|_|) token: Ts.PrefixUnaryExpression option = hasFlagTo SK.PrefixUnaryExpression token
        let inline (|PostfixUnaryExpression|_|) token: Ts.PostfixUnaryExpression option = hasFlagTo SK.PostfixUnaryExpression token
        let inline (|BinaryExpression|_|) token: Ts.BinaryExpression option = hasFlagTo SK.BinaryExpression token
        let inline (|ConditionalExpression|_|) token: Ts.ConditionalExpression option = hasFlagTo SK.ConditionalExpression token
        let inline (|YieldExpression|_|) token: Ts.YieldExpression option = hasFlagTo SK.YieldExpression token
        let inline (|ArrowFunction|_|) token: Ts.ArrowFunction option = hasFlagTo SK.ArrowFunction token
        let inline (|FunctionExpression|_|) token: Ts.FunctionExpression option = hasFlagTo SK.FunctionExpression token
        let inline (|DeleteExpression|_|) token: Ts.DeleteExpression option = hasFlagTo SK.DeleteExpression token
        let inline (|TypeOfExpression|_|) token: Ts.TypeOfExpression option = hasFlagTo SK.TypeOfExpression token
        let inline (|VoidExpression|_|) token: Ts.VoidExpression option = hasFlagTo SK.VoidExpression token
        let inline (|AwaitExpression|_|) token: Ts.AwaitExpression option = hasFlagTo SK.AwaitExpression token
        let inline (|SpreadElement|_|) token: Ts.SpreadElement option = hasFlagTo SK.SpreadElement token
        let inline (|ClassExpression|_|) token: Ts.ClassExpression option = hasFlagTo SK.ClassExpression token
        let inline (|ExpressionWithTypeArguments|_|) token: Ts.ExpressionWithTypeArguments option = hasFlagTo SK.ExpressionWithTypeArguments token

        // -----------------------------
        // Statements
        // -----------------------------
        let inline (|Block|_|) token: Ts.Block option = hasFlagTo SK.Block token
        let inline (|EmptyStatement|_|) token: Ts.EmptyStatement option = hasFlagTo SK.EmptyStatement token
        let inline (|VariableStatement|_|) token: Ts.VariableStatement option = hasFlagTo SK.VariableStatement token
        let inline (|ExpressionStatement|_|) token: Ts.ExpressionStatement option = hasFlagTo SK.ExpressionStatement token
        let inline (|IfStatement|_|) token: Ts.IfStatement option = hasFlagTo SK.IfStatement token
        let inline (|DoStatement|_|) token: Ts.DoStatement option = hasFlagTo SK.DoStatement token
        let inline (|WhileStatement|_|) token: Ts.WhileStatement option = hasFlagTo SK.WhileStatement token
        let inline (|ForStatement|_|) token: Ts.ForStatement option = hasFlagTo SK.ForStatement token
        let inline (|ForInStatement|_|) token: Ts.ForInStatement option = hasFlagTo SK.ForInStatement token
        let inline (|ForOfStatement|_|) token: Ts.ForOfStatement option = hasFlagTo SK.ForOfStatement token
        let inline (|ContinueStatement|_|) token: Ts.ContinueStatement option = hasFlagTo SK.ContinueStatement token
        let inline (|BreakStatement|_|) token: Ts.BreakStatement option = hasFlagTo SK.BreakStatement token
        let inline (|ReturnStatement|_|) token: Ts.ReturnStatement option = hasFlagTo SK.ReturnStatement token
        let inline (|WithStatement|_|) token: Ts.WithStatement option = hasFlagTo SK.WithStatement token
        let inline (|SwitchStatement|_|) token: Ts.SwitchStatement option = hasFlagTo SK.SwitchStatement token
        let inline (|CaseBlock|_|) token: Ts.CaseBlock option = hasFlagTo SK.CaseBlock token
        let inline (|CaseClause|_|) token: Ts.CaseClause option = hasFlagTo SK.CaseClause token
        let inline (|DefaultClause|_|) token: Ts.DefaultClause option = hasFlagTo SK.DefaultClause token
        let inline (|LabeledStatement|_|) token: Ts.LabeledStatement option = hasFlagTo SK.LabeledStatement token
        let inline (|ThrowStatement|_|) token: Ts.ThrowStatement option = hasFlagTo SK.ThrowStatement token
        let inline (|TryStatement|_|) token: Ts.TryStatement option = hasFlagTo SK.TryStatement token
        let inline (|CatchClause|_|) token: Ts.CatchClause option = hasFlagTo SK.CatchClause token
        let inline (|DebuggerStatement|_|) token: Ts.DebuggerStatement option = hasFlagTo SK.DebuggerStatement token

        // -----------------------------
        // Declarations
        // -----------------------------
        let inline (|VariableDeclaration|_|) token: Ts.VariableDeclaration option = hasFlagTo SK.VariableDeclaration token
        let inline (|VariableDeclarationList|_|) token: Ts.VariableDeclarationList option = hasFlagTo SK.VariableDeclarationList token
        let inline (|FunctionDeclaration|_|) token: Ts.FunctionDeclaration option = hasFlagTo SK.FunctionDeclaration token
        let inline (|ClassDeclaration|_|) token: Ts.ClassDeclaration option = hasFlagTo SK.ClassDeclaration token
        let inline (|InterfaceDeclaration|_|) token: Ts.InterfaceDeclaration option = hasFlagTo SK.InterfaceDeclaration token
        let inline (|TypeAliasDeclaration|_|) token: Ts.TypeAliasDeclaration option = hasFlagTo SK.TypeAliasDeclaration token
        let inline (|EnumDeclaration|_|) token: Ts.EnumDeclaration option = hasFlagTo SK.EnumDeclaration token
        let inline (|ModuleDeclaration|_|) token: Ts.ModuleDeclaration option = hasFlagTo SK.ModuleDeclaration token
        let inline (|ImportDeclaration|_|) token: Ts.ImportDeclaration option = hasFlagTo SK.ImportDeclaration token
        let inline (|ExportDeclaration|_|) token: Ts.ExportDeclaration option = hasFlagTo SK.ExportDeclaration token
        let inline (|NamespaceExportDeclaration|_|) token: Ts.NamespaceExportDeclaration option = hasFlagTo SK.NamespaceExportDeclaration token
        let inline (|ImportEqualsDeclaration|_|) token: Ts.ImportEqualsDeclaration option = hasFlagTo SK.ImportEqualsDeclaration token
        let inline (|ExportAssignment|_|) token: Ts.ExportAssignment option = hasFlagTo SK.ExportAssignment token
        let inline (|ModuleBlock|_|) token: Ts.ModuleBlock option = hasFlagTo SK.ModuleBlock token
        let inline (|ClassStaticBlockDeclaration|_|) token: Ts.ClassStaticBlockDeclaration option = hasFlagTo SK.ClassStaticBlockDeclaration token

        // -----------------------------
        // Clauses, Imports/Exports, and Modifiers
        // -----------------------------
        let inline (|HeritageClause|_|) token: Ts.HeritageClause option = hasFlagTo SK.HeritageClause token
        let inline (|ImportClause|_|) token: Ts.ImportClause option = hasFlagTo SK.ImportClause token
        let inline (|NamedImports|_|) token: Ts.NamedImports option = hasFlagTo SK.NamedImports token
        let inline (|ImportSpecifier|_|) token: Ts.ImportSpecifier option = hasFlagTo SK.ImportSpecifier token
        let inline (|NamedExports|_|) token: Ts.NamedExports option = hasFlagTo SK.NamedExports token
        let inline (|ExportSpecifier|_|) token: Ts.ExportSpecifier option = hasFlagTo SK.ExportSpecifier token
        // let inline (|AssertClause|_|) token: Ts.AssertClause option = hasFlagTo SK.AssertClause token
        // let inline (|AssertEntry|_|) token: Ts.AssertEntry option = hasFlagTo SK.AssertEntry token
        let inline (|Modifier|_|) token: Ts.Modifier option = match token with CategoryModifier -> unbox token |> Some | _ -> None

        // -----------------------------
        // Signatures & Parameters
        // -----------------------------
        let inline (|CallSignature|_|) token: Ts.CallSignatureDeclaration option = hasFlagTo SK.CallSignature token
        let inline (|ConstructSignature|_|) token: Ts.ConstructSignatureDeclaration option = hasFlagTo SK.ConstructSignature token
        let inline (|MethodSignature|_|) token: Ts.MethodSignature option = hasFlagTo SK.MethodSignature token
        let inline (|IndexSignature|_|) token: Ts.IndexSignatureDeclaration option = hasFlagTo SK.IndexSignature token
        let inline (|PropertySignature|_|) token: Ts.PropertySignature option = hasFlagTo SK.PropertySignature token
        let inline (|MethodDeclaration|_|) token: Ts.MethodDeclaration option = hasFlagTo SK.MethodDeclaration token
        let inline (|Constructor|_|) token: Ts.ConstructorDeclaration option = hasFlagTo SK.Constructor token
        let inline (|Parameter|_|) token: Ts.ParameterDeclaration option = hasFlagTo SK.Parameter token
        let inline (|TypeParameter|_|) token: Ts.TypeParameterDeclaration option = hasFlagTo SK.TypeParameter token

        // -----------------------------
        // Type Nodes
        // -----------------------------
        let inline (|TypePredicate|_|) token: Ts.TypePredicateNode option = hasFlagTo SK.TypePredicate token
        let inline (|TypeReference|_|) token: Ts.TypeReferenceNode option = hasFlagTo SK.TypeReference token
        let inline (|FunctionType|_|) token: Ts.FunctionTypeNode option = hasFlagTo SK.FunctionType token
        let inline (|ConstructorType|_|) token: Ts.ConstructorTypeNode option = hasFlagTo SK.ConstructorType token
        let inline (|TypeQuery|_|) token: Ts.TypeQueryNode option = hasFlagTo SK.TypeQuery token
        let inline (|TypeLiteral|_|) token: Ts.TypeLiteralNode option = hasFlagTo SK.TypeLiteral token
        let inline (|ArrayType|_|) token: Ts.ArrayTypeNode option = hasFlagTo SK.ArrayType token
        let inline (|TupleType|_|) token: Ts.TupleTypeNode option = hasFlagTo SK.TupleType token
        let inline (|OptionalType|_|) token: Ts.OptionalTypeNode option = hasFlagTo SK.OptionalType token
        let inline (|RestType|_|) token: Ts.RestTypeNode option = hasFlagTo SK.RestType token
        let inline (|UnionType|_|) token: Ts.UnionTypeNode option = hasFlagTo SK.UnionType token
        let inline (|IntersectionType|_|) token: Ts.IntersectionTypeNode option = hasFlagTo SK.IntersectionType token
        let inline (|LiteralType|_|) token: Ts.LiteralTypeNode option = hasFlagTo SK.LiteralType token
        let inline (|ParenthesizedType|_|) token: Ts.ParenthesizedTypeNode option = hasFlagTo SK.ParenthesizedType token
        let inline (|ThisType|_|) token: Ts.ThisTypeNode option = hasFlagTo SK.ThisType token
        let inline (|TypeOperator|_|) token: Ts.TypeOperatorNode option = hasFlagTo SK.TypeOperator token
        let inline (|IndexedAccessType|_|) token: Ts.IndexedAccessTypeNode option = hasFlagTo SK.IndexedAccessType token
        let inline (|MappedType|_|) token: Ts.MappedTypeNode option = hasFlagTo SK.MappedType token
        let inline (|ConditionalType|_|) token: Ts.ConditionalTypeNode option = hasFlagTo SK.ConditionalType token
        let inline (|InferType|_|) token: Ts.InferTypeNode option = hasFlagTo SK.InferType token
        let inline (|TemplateLiteralType|_|) token: Ts.TemplateLiteralTypeNode option = hasFlagTo SK.TemplateLiteralType token
        let inline (|TemplateLiteralTypeSpan|_|) token: Ts.TemplateLiteralTypeSpan option = hasFlagTo SK.TemplateLiteralTypeSpan token

        // -----------------------------
        // JSX
        // -----------------------------
        let inline (|JsxElement|_|) token: Ts.JsxElement option = hasFlagTo SK.JsxElement token
        let inline (|JsxSelfClosingElement|_|) token: Ts.JsxSelfClosingElement option = hasFlagTo SK.JsxSelfClosingElement token
        let inline (|JsxOpeningElement|_|) token: Ts.JsxOpeningElement option = hasFlagTo SK.JsxOpeningElement token
        let inline (|JsxClosingElement|_|) token: Ts.JsxClosingElement option = hasFlagTo SK.JsxClosingElement token
        let inline (|JsxFragment|_|) token: Ts.JsxFragment option = hasFlagTo SK.JsxFragment token
        let inline (|JsxOpeningFragment|_|) token: Ts.JsxOpeningFragment option = hasFlagTo SK.JsxOpeningFragment token
        let inline (|JsxClosingFragment|_|) token: Ts.JsxClosingFragment option = hasFlagTo SK.JsxClosingFragment token
        let inline (|JsxAttribute|_|) token: Ts.JsxAttribute option = hasFlagTo SK.JsxAttribute token
        let inline (|JsxAttributes|_|) token: Ts.JsxAttributes option = hasFlagTo SK.JsxAttributes token
        let inline (|JsxSpreadAttribute|_|) token: Ts.JsxSpreadAttribute option = hasFlagTo SK.JsxSpreadAttribute token
        let inline (|JsxExpression|_|) token: Ts.JsxExpression option = hasFlagTo SK.JsxExpression token

        // -----------------------------
        // JSDoc
        // -----------------------------
        let inline (|JSDoc|_|) token: Ts.JSDoc option = hasFlagTo SK.JSDocComment token
        let inline (|JSDocText|_|) token: Ts.JSDocText option = hasFlagTo SK.JSDocText token
        let inline (|JSDocTypeExpression|_|) token: Ts.JSDocTypeExpression option = hasFlagTo SK.JSDocTypeExpression token
        let inline (|JSDocAllType|_|) token: Ts.JSDocAllType option = hasFlagTo SK.JSDocAllType token
        let inline (|JSDocUnknownType|_|) token: Ts.JSDocUnknownType option = hasFlagTo SK.JSDocUnknownType token
        let inline (|JSDocNullableType|_|) token: Ts.JSDocNullableType option = hasFlagTo SK.JSDocNullableType token
        let inline (|JSDocNonNullableType|_|) token: Ts.JSDocNonNullableType option = hasFlagTo SK.JSDocNonNullableType token
        let inline (|JSDocOptionalType|_|) token: Ts.JSDocOptionalType option = hasFlagTo SK.JSDocOptionalType token
        let inline (|JSDocFunctionType|_|) token: Ts.JSDocFunctionType option = hasFlagTo SK.JSDocFunctionType token
        let inline (|JSDocVariadicType|_|) token: Ts.JSDocVariadicType option = hasFlagTo SK.JSDocVariadicType token
        let inline (|JSDocNamepathType|_|) token: Ts.JSDocNamepathType option = hasFlagTo SK.JSDocNamepathType token
        let inline (|JSDocTag|_|) token: Ts.JSDocTag option = hasFlagTo SK.JSDocTag token
        let inline (|JSDocAugmentsTag|_|) token: Ts.JSDocAugmentsTag option = hasFlagTo SK.JSDocAugmentsTag token
        let inline (|JSDocClassTag|_|) token: Ts.JSDocClassTag option = hasFlagTo SK.JSDocClassTag token
        let inline (|JSDocThisTag|_|) token: Ts.JSDocThisTag option = hasFlagTo SK.JSDocThisTag token
        let inline (|JSDocEnumTag|_|) token: Ts.JSDocEnumTag option = hasFlagTo SK.JSDocEnumTag token
        let inline (|JSDocReturnTag|_|) token: Ts.JSDocReturnTag option = hasFlagTo SK.JSDocReturnTag token
        let inline (|JSDocTypeTag|_|) token: Ts.JSDocTypeTag option = hasFlagTo SK.JSDocTypeTag token
        let inline (|JSDocTemplateTag|_|) token: Ts.JSDocTemplateTag option = hasFlagTo SK.JSDocTemplateTag token
        let inline (|JSDocParamTag|_|) token: Ts.JSDocParameterTag option = hasFlagTo SK.JSDocParameterTag token
        let inline (|JSDocPropertyTag|_|) token: Ts.JSDocPropertyTag option = hasFlagTo SK.JSDocPropertyTag token

        // -----------------------------
        // Misc / Synthetic / Emitted
        // -----------------------------
        let inline (|NotEmittedStatement|_|) token: Ts.NotEmittedStatement option = hasFlagTo SK.NotEmittedStatement token
        let inline (|PartiallyEmittedExpression|_|) token: Ts.PartiallyEmittedExpression option = hasFlagTo SK.PartiallyEmittedExpression token
        let inline (|CommaListExpression|_|) token: Ts.CommaListExpression option = hasFlagTo SK.CommaListExpression token
        let inline (|SyntheticExpression|_|) token: Ts.SyntheticExpression option = hasFlagTo SK.SyntheticExpression token
        let inline (|MissingDeclaration|_|) token: Ts.MissingDeclaration option = hasFlagTo SK.MissingDeclaration token
        let inline (|SyntheticReferenceExpression|_|) token: Ts.Node option = hasFlagTo SK.SyntheticReferenceExpression token

        // -----------------------------
        // SourceFile / Bundle / Unparsed
        // -----------------------------
        let inline (|SourceFile|_|) token: Ts.SourceFile option = hasFlagTo SK.SourceFile token
        let inline (|Bundle|_|) token: Ts.Bundle option = hasFlagTo SK.Bundle token
    let (|HasTypeArguments|_|) = function
        | Node.CallExpression node -> HasTypeArguments.CallExpression node |> Some
        | Node.NewExpression node -> HasTypeArguments.NewExpression node |> Some
        | Node.TaggedTemplateExpression node -> HasTypeArguments.TaggedTemplateExpression node |> Some
        | Node.JsxOpeningElement node -> HasTypeArguments.JsxOpeningElement node |> Some
        | Node.JsxSelfClosingElement node -> HasTypeArguments.JsxSelfClosingElement node |> Some
        | _ -> None
    let (|HasExpressionInitializer|_|) = function
        | Node.VariableDeclaration node -> HasExpressionInitializer.VariableDeclaration node |> Some
        | Node.Parameter node -> HasExpressionInitializer.ParameterDeclaration node |> Some
        | Node.BindingElement node -> HasExpressionInitializer.BindingElement node |> Some
        | Node.PropertyDeclaration node -> HasExpressionInitializer.PropertyDeclaration node |> Some
        | Node.PropertyAssignment node -> HasExpressionInitializer.PropertyAssignment node |> Some
        | Node.EnumMember node -> HasExpressionInitializer.EnumMember node |> Some
        | _ -> None
    let (|HasInitializer|_|) = function
        | HasExpressionInitializer node -> HasInitializer.HasExpressionInitializer node |> Some
        | Node.ForStatement node -> HasInitializer.ForStatement node |> Some
        | Node.ForInStatement node -> HasInitializer.ForInStatement node |> Some
        | Node.ForOfStatement node -> HasInitializer.ForOfStatement node |> Some
        | Node.JsxAttribute node -> HasInitializer.JsxAttribute node |> Some
        | _ -> None
    let (|HasDecorators|_|) = function
        | Node.Parameter node -> HasDecorators.ParameterDeclaration node |> Some
        | Node.PropertyDeclaration node -> HasDecorators.PropertyDeclaration node |> Some
        | Node.MethodDeclaration node -> HasDecorators.MethodDeclaration node |> Some
        | Node.GetAccessorDeclaration node -> HasDecorators.GetAccessorDeclaration node |> Some
        | Node.SetAccessorDeclaration node -> HasDecorators.SetAccessorDeclaration node |> Some
        | Node.ClassExpression node -> HasDecorators.ClassExpression node |> Some
        | Node.ClassDeclaration node -> HasDecorators.ClassDeclaration node |> Some
        | _ -> None
    let (|AssertionExpression|_|) = function
        | Node.TypeAssertionExpression node -> AssertionExpression.TypeAssertion node |> Some
        | Node.AsExpression node -> AssertionExpression.AsExpression node |> Some
        | _ -> None
    let (|HasType|_|) = function
        // | Node.Signature node -> HasType.SignatureDeclaration node |> Some
        | Node.VariableDeclaration node -> HasType.VariableDeclaration node |> Some
        | Node.Parameter node -> HasType.ParameterDeclaration node |> Some
        | Node.PropertySignature node -> HasType.PropertySignature node |> Some
        | Node.PropertyDeclaration node -> HasType.PropertyDeclaration node |> Some
        | Node.TypePredicateNode node -> HasType.TypePredicateNode node |> Some
        | Node.ParenthesizedTypeNode node -> HasType.ParenthesizedTypeNode node |> Some
        | Node.TypeOperatorNode node -> HasType.TypeOperatorNode node |> Some
        | Node.MappedTypeNode node -> HasType.MappedTypeNode node |> Some
        | AssertionExpression node -> HasType.AssertionExpression node |> Some
        | Node.TypeAliasDeclaration node -> HasType.TypeAliasDeclaration node |> Some
        | Node.JSDocTypeExpression node -> HasType.JSDocTypeExpression node |> Some
        | Node.JSDocNonNullableType node -> HasType.JSDocNonNullableType node |> Some
        | Node.JSDocNullableType node -> HasType.JSDocNullableType node |> Some
        | Node.JSDocOptionalType node -> HasType.JSDocOptionalType node |> Some
        | Node.JSDocVariadicType node -> HasType.JSDocVariadicType node |> Some
        | _ -> None
    let inline (|AccessorDeclaration|_|) value =
        match value with
        | Node.GetAccessorDeclaration node -> AccessorDeclaration.GetAccessorDeclaration node |> Some
        | Node.SetAccessorDeclaration node -> AccessorDeclaration.SetAccessorDeclaration node |> Some
        | _ -> None
    let inline (|ClassLikeDeclaration|_|) value =
        match value with
        | Node.ClassDeclaration node -> ClassLikeDeclaration.ClassDeclaration node |> Some
        | Node.ClassExpression node -> ClassLikeDeclaration.ClassExpression node |> Some
        | _ -> None
    let inline (|ModifierLike|_|) value =
        match value with
        | Node.Decorator node -> ModifierLike.Decorator node |> Some
        | SyntaxKind.Modifier node -> ModifierLike.Modifier node |> Some
        | _ -> None
    let inline (|HasJSDoc|_|) value =
        match value with
        | AccessorDeclaration node -> HasJSDoc.AccessorDeclaration node |> Some
        | Node.ArrowFunction node -> HasJSDoc.ArrowFunction node |> Some
        | Node.BinaryExpression node -> HasJSDoc.BinaryExpression node |> Some
        | Node.Block node -> HasJSDoc.Block node |> Some
        | Node.BreakStatement node -> HasJSDoc.BreakStatement node |> Some
        | Node.CallSignatureDeclaration node -> HasJSDoc.CallSignatureDeclaration node |> Some
        | Node.CaseClause node -> HasJSDoc.CaseClause node |> Some
        | ClassLikeDeclaration node -> HasJSDoc.ClassLikeDeclaration node |> Some
        | Node.ClassStaticBlockDeclaration node -> HasJSDoc.ClassStaticBlockDeclaration node |> Some
        | Node.ConstructorDeclaration node -> HasJSDoc.ConstructorDeclaration node |> Some
        | Node.ConstructorTypeNode node -> HasJSDoc.ConstructorTypeNode node |> Some
        | Node.ConstructSignatureDeclaration node -> HasJSDoc.ConstructSignatureDeclaration node |> Some
        | Node.ContinueStatement node -> HasJSDoc.ContinueStatement node |> Some
        | Node.DebuggerStatement node -> HasJSDoc.DebuggerStatement node |> Some
        | Node.DoStatement node -> HasJSDoc.DoStatement node |> Some
        | Node.ElementAccessExpression node -> HasJSDoc.ElementAccessExpression node |> Some
        | Node.EmptyStatement node -> HasJSDoc.EmptyStatement node |> Some
        | SyntaxKind.EndOfFileToken node -> HasJSDoc.EndOfFileToken (unbox node) |> Some
        | Node.EnumDeclaration node -> HasJSDoc.EnumDeclaration node |> Some
        | Node.EnumMember node -> HasJSDoc.EnumMember node |> Some
        | Node.ExportAssignment node -> HasJSDoc.ExportAssignment node |> Some
        | Node.ExportDeclaration node -> HasJSDoc.ExportDeclaration node |> Some
        | Node.ExportSpecifier node -> HasJSDoc.ExportSpecifier node |> Some
        | Node.ExpressionStatement node -> HasJSDoc.ExpressionStatement node |> Some
        | Node.ForInStatement node -> HasJSDoc.ForInStatement node |> Some
        | Node.ForOfStatement node -> HasJSDoc.ForOfStatement node |> Some
        | Node.ForStatement node -> HasJSDoc.ForStatement node |> Some
        | Node.FunctionDeclaration node -> HasJSDoc.FunctionDeclaration node |> Some
        | Node.FunctionExpression node -> HasJSDoc.FunctionExpression node |> Some
        | Node.FunctionTypeNode node -> HasJSDoc.FunctionTypeNode node |> Some
        | Node.Identifier node -> HasJSDoc.Identifier node |> Some
        | Node.IfStatement node -> HasJSDoc.IfStatement node |> Some
        | Node.ImportDeclaration node -> HasJSDoc.ImportDeclaration node |> Some
        | Node.ImportEqualsDeclaration node -> HasJSDoc.ImportEqualsDeclaration node |> Some
        | Node.IndexSignatureDeclaration node -> HasJSDoc.IndexSignatureDeclaration node |> Some
        | Node.InterfaceDeclaration node -> HasJSDoc.InterfaceDeclaration node |> Some
        | Node.JSDocFunctionType node -> HasJSDoc.JSDocFunctionType node |> Some
        | Node.JSDocSignature node -> HasJSDoc.JSDocSignature node |> Some
        | Node.LabeledStatement node -> HasJSDoc.LabeledStatement node |> Some
        | Node.MethodDeclaration node -> HasJSDoc.MethodDeclaration node |> Some
        | Node.MethodSignature node -> HasJSDoc.MethodSignature node |> Some
        | Node.ModuleDeclaration node -> HasJSDoc.ModuleDeclaration node |> Some
        | Node.NamedTupleMember node -> HasJSDoc.NamedTupleMember node |> Some
        | Node.NamespaceExportDeclaration node -> HasJSDoc.NamespaceExportDeclaration node |> Some
        | Node.ObjectLiteralExpression node -> HasJSDoc.ObjectLiteralExpression node |> Some
        | Node.Parameter node -> HasJSDoc.ParameterDeclaration node |> Some
        | Node.ParenthesizedExpression node -> HasJSDoc.ParenthesizedExpression node |> Some
        | Node.PropertyAccessExpression node -> HasJSDoc.PropertyAccessExpression node |> Some
        | Node.PropertyAssignment node -> HasJSDoc.PropertyAssignment node |> Some
        | Node.PropertyDeclaration node -> HasJSDoc.PropertyDeclaration node |> Some
        | Node.PropertySignature node -> HasJSDoc.PropertySignature node |> Some
        | Node.ReturnStatement node -> HasJSDoc.ReturnStatement node |> Some
        | Node.SemicolonClassElement node -> HasJSDoc.SemicolonClassElement node |> Some
        | Node.ShorthandPropertyAssignment node -> HasJSDoc.ShorthandPropertyAssignment node |> Some
        | Node.SpreadAssignment node -> HasJSDoc.SpreadAssignment node |> Some
        | Node.SwitchStatement node -> HasJSDoc.SwitchStatement node |> Some
        | Node.ThrowStatement node -> HasJSDoc.ThrowStatement node |> Some
        | Node.TryStatement node -> HasJSDoc.TryStatement node |> Some
        | Node.TypeAliasDeclaration node -> HasJSDoc.TypeAliasDeclaration node |> Some
        | Node.TypeParameterDeclaration node -> HasJSDoc.TypeParameterDeclaration node |> Some
        | Node.VariableDeclaration node -> HasJSDoc.VariableDeclaration node |> Some
        | Node.VariableStatement node -> HasJSDoc.VariableStatement node |> Some
        | Node.WhileStatement node -> HasJSDoc.WhileStatement node |> Some
        | Node.WithStatement node -> HasJSDoc.WithStatement node |> Some
        | _ -> None
    let (|EntityName|_|) = function
        | Node.Identifier node -> EntityName.Identifier node |> Some
        | Node.QualifiedName node -> EntityName.QualifiedName node |> Some
        | _ -> None
    let (|PropertyName|_|) = function
        | Node.Identifier node -> PropertyName.Identifier node |> Some
        | Node.StringLiteral node -> PropertyName.StringLiteral node |> Some
        | Node.NoSubstitutionTemplateLiteral node -> PropertyName.NoSubtitutionTemplateLiteral node |> Some
        | Node.NumericLiteral node -> PropertyName.NumericLiteral node |> Some
        | Node.ComputedPropertyName node -> PropertyName.ComputedPropertyName node |> Some
        | Node.PrivateIdentifier node -> PropertyName.PrivateIdentifier node |> Some
        | Node.BigIntLiteral node -> PropertyName.BigIntLiteral node |> Some
        | _ -> None
    let (|PropertyNameLiteral|_|) = function
        | Node.Identifier node -> PropertyNameLiteral.Identifier node |> Some
        | Node.StringLiteral node -> PropertyNameLiteral.StringLiteral node |> Some
        | Node.NumericLiteral node -> PropertyNameLiteral.NumericLiteral node |> Some
        | Node.JsxNamespacedName node -> PropertyNameLiteral.JsxNamespacedName node |> Some
        | Node.BigIntLiteral node -> PropertyNameLiteral.BigIntLiteral node |> Some
        | _ -> None
    let inline (|TypeVariable|_|) node =
        match node with
        | TypeFlags.TypeParameter node -> TypeVariable.TypeParameter node |> Some
        | TypeFlags.IndexedAccess node -> TypeVariable.IndexedAccessType node |> Some
        | _ -> None
    let inline (|BaseType|_|) node =
        match node with
        | TypeFlags.Object node -> BaseType.ObjectType node |> Some
        | TypeFlags.Intersection node -> BaseType.IntersectionType node |> Some
        | TypeVariable node -> BaseType.TypeVariable node |> Some
        | _ -> None
    let inline (|StructuredType|_|) node =
        match node with
        | TypeFlags.Object node -> StructuredType.ObjectType node |> Some
        | TypeFlags.Union node -> StructuredType.UnionType node |> Some
        | TypeFlags.Intersection node -> StructuredType.IntersectionType node |> Some
        | _ -> None
    let (|ObjectTypeDeclaration|_|) = function
        | ClassLikeDeclaration node -> ObjectTypeDeclaration.ClassLikeDeclaration node |> Some
        | Node.InterfaceDeclaration node -> ObjectTypeDeclaration.InterfaceDeclaration node |> Some
        | Node.TypeLiteralNode node -> ObjectTypeDeclaration.TypeLiteralNode node |> Some
        | _ -> None
    let (|ModuleName|_|) = function
        | Node.Identifier node -> TsModuleName.Identifier node |> Some
        | Node.StringLiteral node -> TsModuleName.StringLiteral node |> Some
        | _ -> None
    let (|JSDocComment|_|) = function
        | SyntaxKind.JSDocText node -> JSDocComment.JSDocText node |> Some
        | Node.JSDocLink node -> JSDocComment.JSDocLink node |> Some
        | Node.JSDocLinkCode node -> JSDocComment.JSDocLinkCode node |> Some
        | Node.JSDocLinkPlain node -> JSDocComment.JSDocLinkPlain node |> Some
        | _ -> None
    let (|MemberName|_|) = function
        | Node.Identifier node -> MemberName.Identifier node |> Some
        | Node.PrivateIdentifier node -> MemberName.PrivateIdentifier node |> Some
        | _ -> None
    let (|JsxAttributeName|_|) = function
        | Node.Identifier node -> JsxAttributeName.Identifier node |> Some
        | Node.JsxNamespacedName node -> JsxAttributeName.JsxNamespacedName node |> Some
        | _ -> None
    let (|StringLiteralLike|_|) = function
        | Node.StringLiteral node -> StringLiteralLike.StringLiteral  node |> Some
        | Node.NoSubstitutionTemplateLiteral node -> StringLiteralLike.NoSubstitutionTemplateLiteral  node |> Some
        | _ -> None
    let (|BindingPattern|_|) = function
        | Node.ObjectBindingPattern node -> BindingPattern.ObjectBindingPattern node |> Some
        | Node.ArrayBindingPattern node -> BindingPattern.ArrayBindingPattern node |> Some
        | _ -> None
    let (|BindingName|_|) = function
        | Node.Identifier node -> BindingName.Identifier node |> Some
        | BindingPattern node -> BindingName.BindingPattern node |> Some
        | _ -> None
    let (|ObjectLiteralElementLike|_|) = function
        | Node.PropertyAssignment node -> ObjectLiteralElementLike.PropertyAssignment node |> Some
        | Node.ShorthandPropertyAssignment node -> ObjectLiteralElementLike.ShorthandPropertyAssignment node |> Some
        | Node.SpreadAssignment node -> ObjectLiteralElementLike.SpreadAssignment node |> Some
        | Node.MethodDeclaration node -> ObjectLiteralElementLike.MethodDeclaration node |> Some
        | AccessorDeclaration node -> ObjectLiteralElementLike.AccessorDeclaration node |> Some
        | _ -> None
    let (|FunctionLikeDeclaration|_|) = function
        | Node.FunctionDeclaration node -> FunctionLikeDeclaration.FunctionDeclaration node |> Some
        | Node.MethodDeclaration node -> FunctionLikeDeclaration.MethodDeclaration node |> Some
        | AccessorDeclaration node -> FunctionLikeDeclaration.AccessorDeclaration node |> Some
        | Node.ConstructorDeclaration node -> FunctionLikeDeclaration.ConstructorDeclaration node |> Some
        | Node.FunctionExpression node -> FunctionLikeDeclaration.FunctionExpression node |> Some
        | Node.ArrowFunction node -> FunctionLikeDeclaration.ArrowFunction node |> Some
        | _ -> None
    let (|FunctionOrConstructorTypeNode|_|) = function
        | Node.FunctionTypeNode node -> FunctionOrConstructorTypeNode.FunctionTypeNode node |> Some
        | Node.ConstructorTypeNode node -> FunctionOrConstructorTypeNode.ConstructorTypeNode node |> Some
        | _ -> None
    let (|TypeReferenceType|_|) = function
        | Node.TypeReferenceNode node -> TypeReferenceType.TypeReferenceNode node |> Some
        | Node.ExpressionWithTypeArguments node -> TypeReferenceType.ExpressionWithTypeArguments node |> Some
        | _ -> None
    let (|UnionOrIntersectionTypeNode|_|) = function
        | Node.UnionTypeNode node -> UnionOrIntersectionTypeNode.UnionTypeNode node |> Some
        | Node.IntersectionTypeNode node -> UnionOrIntersectionTypeNode.IntersectionTypeNode node |> Some
        | _ -> None
    // let (|EntityNameExpression|_|) = function
        // | Node.Identifier node -> EntityNameExpression.Identifier node |> Some
        // | Node.PropertyAccessEntityNameExpression node -> EntityNameExpression.PropertyAccessEntityNameExpression node |> Some
        // | _ -> None
    let (|DeclarationName|_|) = function
        | PropertyName node -> DeclarationName.PropertyName node |> Some
        | JsxAttributeName node -> DeclarationName.JsxAttributeName node |> Some
        | StringLiteralLike node -> DeclarationName.StringLiteralLike node |> Some
        | Node.ElementAccessExpression node -> DeclarationName.ElementAccessExpression node |> Some
        | BindingPattern node -> DeclarationName.BindingPattern node |> Some
        // | EntityNameExpression node -> DeclarationName.EntityNameExpression node |> Some
        | _ -> None
    #nowarn 25
    module Wrap =
        let (|EntityNameExpression|): Ts.EntityNameExpression -> _ = unbox >> function
            | Node.Identifier node -> EntityNameExpression.Identifier node 
            | node -> EntityNameExpression.PropertyAccessEntityNameExpression (unbox node) 
        let (|DeclaratinName|): Ts.DeclarationName -> _ = unbox >> function
            | DeclarationName node -> node
            | node ->
                match unbox node with
                | EntityNameExpression node -> DeclarationName.EntityNameExpression node
        let (|AssertionExpression|): Ts.AssertionExpression -> _ = unbox >> function
            | AssertionExpression node -> node
        let (|AccessorDeclaration|): Ts.AccessorDeclaration -> _ = unbox >> function
            | AccessorDeclaration node -> node
        let (|ClassLikeDeclaration|): Ts.ClassLikeDeclaration -> _ = unbox >> function
            | ClassLikeDeclaration node -> node
        let (|ModifierLike|):Ts.ModifierLike -> _ = unbox >> function
            | ModifierLike node -> node
        let (|EntityName|): Ts.EntityName -> _ = unbox >> function
            | EntityName node -> node
        let (|PropertyName|): Ts.PropertyName -> _ = unbox >> function
            | PropertyName node -> node
        let (|PropertyNameLiteral|): Ts.PropertyNameLiteral -> _ = unbox >> function
            | PropertyNameLiteral node -> node
        let (|TypeVariable|): Ts.TypeVariable -> _ = unbox<Ts.Type> >> function
            | TypeVariable node -> node
        let (|BaseType|): Ts.BaseType -> _ = unbox<Ts.Type> >> function
            | BaseType node -> node
        let (|StructuredType|): Ts.StructuredType -> _ = unbox<Ts.Type> >> function
            | StructuredType  node -> node
        let (|ObjectTypeDeclaration|): Ts.ObjectTypeDeclaration -> _ = unbox >> function
            | ObjectTypeDeclaration node -> node
        let (|ModuleName|): Ts.ModuleName -> _ = unbox >> function ModuleName node -> node
        let (|JSDocComment|): Ts.JSDocComment -> _ = unbox >> function JSDocComment node -> node
        let (|MemberName|): Ts.MemberName -> _ = unbox >> function MemberName node -> node
        let (|StringLiteralLike|): Ts.StringLiteralLike -> _ = unbox >> function StringLiteralLike node -> node
        let (|BindingPattern|): Ts.BindingPattern -> _ = unbox >> function BindingPattern node -> node
        let (|BindingName|): Ts.BindingName -> _ = unbox >> function BindingName node -> node
        let (|ObjectLiteralElementLike|): Ts.ObjectLiteralElementLike -> _ = unbox >> function ObjectLiteralElementLike node -> node
        let (|FunctionLikeDeclaration|): Ts.FunctionLikeDeclaration -> _ = unbox >> function FunctionLikeDeclaration node -> node
        let (|FunctionOrConstructorTypeNode|): Ts.FunctionOrConstructorTypeNode -> _ = unbox >> function FunctionOrConstructorTypeNode node -> node
        let (|TypeReferenceType|): Ts.TypeReferenceType -> _ = unbox >> function TypeReferenceType node -> node
        let (|UnionOrIntersectionTypeNode|): Ts.UnionOrIntersectionTypeNode -> _ = unbox >> function UnionOrIntersectionTypeNode node -> node
        let (|DeclarationName|): Ts.DeclarationName -> _ = unbox >> function DeclarationName node -> node
    module Symbol =
        type SF = Ts.SymbolFlags
        let inline hasFlag flag: Ts.Symbol -> bool = fun sym -> sym.flags.HasFlag flag
        let (|None|_|): Ts.Symbol -> bool = _.flags.Equals(SF.None)
        let (|FunctionScopedVariable|_|): Ts.Symbol -> _ = hasFlag SF.FunctionScopedVariable
        let (|BlockScopedVariable|_|): Ts.Symbol -> _ = hasFlag SF.BlockScopedVariable
        let (|Property|_|): Ts.Symbol -> bool = hasFlag SF.Property 
        let (|EnumMember|_|): Ts.Symbol -> bool = hasFlag SF.EnumMember 
        let (|Function|_|): Ts.Symbol -> bool = hasFlag SF.Function 
        let (|Class|_|): Ts.Symbol -> bool = hasFlag SF.Class 
        let (|Interface|_|): Ts.Symbol -> bool = hasFlag SF.Interface 
        let (|ConstEnum|_|): Ts.Symbol -> bool = hasFlag SF.ConstEnum 
        let (|RegularEnum|_|): Ts.Symbol -> bool = hasFlag SF.RegularEnum 
        let (|ValueModule|_|): Ts.Symbol -> bool = hasFlag SF.ValueModule 
        let (|NamespaceModule|_|): Ts.Symbol -> bool = hasFlag SF.NamespaceModule 
        let (|TypeLiteral|_|): Ts.Symbol -> bool = hasFlag SF.TypeLiteral 
        let (|ObjectLiteral|_|): Ts.Symbol -> bool = hasFlag SF.ObjectLiteral 
        let (|Method|_|): Ts.Symbol -> bool = hasFlag SF.Method 
        let (|Constructor|_|): Ts.Symbol -> bool = hasFlag SF.Constructor 
        let (|GetAccessor|_|): Ts.Symbol -> bool = hasFlag SF.GetAccessor 
        let (|SetAccessor|_|): Ts.Symbol -> bool = hasFlag SF.SetAccessor 
        let (|Signature|_|): Ts.Symbol -> bool = hasFlag SF.Signature 
        let (|TypeParameter|_|): Ts.Symbol -> bool = hasFlag SF.TypeParameter 
        let (|TypeAlias|_|): Ts.Symbol -> bool = hasFlag SF.TypeAlias 
        let (|ExportValue|_|): Ts.Symbol -> bool = hasFlag SF.ExportValue 
        let (|Alias|_|): Ts.Symbol -> bool = hasFlag SF.Alias 
        let (|Prototype|_|): Ts.Symbol -> bool = hasFlag SF.Prototype 
        let (|ExportStar|_|): Ts.Symbol -> bool = hasFlag SF.ExportStar 
        let (|Optional|_|): Ts.Symbol -> bool = hasFlag SF.Optional 
        let (|Transient|_|): Ts.Symbol -> bool = hasFlag SF.Transient 
        let (|Assignment|_|): Ts.Symbol -> bool = hasFlag SF.Assignment 
        let (|ModuleExports|_|): Ts.Symbol -> bool = hasFlag SF.ModuleExports 
        let (|All|_|): Ts.Symbol -> bool = hasFlag SF.All 
        let (|Enum|_|): Ts.Symbol -> bool = hasFlag SF.Enum 
        let (|Variable|_|): Ts.Symbol -> bool = hasFlag SF.Variable 
        let (|Value|_|): Ts.Symbol -> bool = hasFlag SF.Value 
        let (|Type|_|): Ts.Symbol -> bool = hasFlag SF.Type 
        let (|Namespace|_|): Ts.Symbol -> bool = hasFlag SF.Namespace 
        let (|Module|_|): Ts.Symbol -> bool = hasFlag SF.Module 
        let (|Accessor|_|): Ts.Symbol -> bool = hasFlag SF.Accessor 
        let (|FunctionScopedVariableExcludes|_|): Ts.Symbol -> bool = hasFlag SF.FunctionScopedVariableExcludes 
        let (|BlockScopedVariableExcludes|_|): Ts.Symbol -> bool = hasFlag SF.BlockScopedVariableExcludes 
        let (|ParameterExcludes|_|): Ts.Symbol -> bool = hasFlag SF.ParameterExcludes 
        let (|PropertyExcludes|_|): Ts.Symbol -> bool = hasFlag SF.PropertyExcludes 
        let (|EnumMemberExcludes|_|): Ts.Symbol -> bool = hasFlag SF.EnumMemberExcludes 
        let (|FunctionExcludes|_|): Ts.Symbol -> bool = hasFlag SF.FunctionExcludes 
        let (|ClassExcludes|_|): Ts.Symbol -> bool = hasFlag SF.ClassExcludes 
        let (|InterfaceExcludes|_|): Ts.Symbol -> bool = hasFlag SF.InterfaceExcludes 
        let (|RegularEnumExcludes|_|): Ts.Symbol -> bool = hasFlag SF.RegularEnumExcludes 
        let (|ConstEnumExcludes|_|): Ts.Symbol -> bool = hasFlag SF.ConstEnumExcludes 
        let (|ValueModuleExcludes|_|): Ts.Symbol -> bool = hasFlag SF.ValueModuleExcludes 
        let (|NamespaceModuleExcludes|_|): Ts.Symbol -> bool = hasFlag SF.NamespaceModuleExcludes 
        let (|MethodExcludes|_|): Ts.Symbol -> bool = hasFlag SF.MethodExcludes 
        let (|GetAccessorExcludes|_|): Ts.Symbol -> bool = hasFlag SF.GetAccessorExcludes 
        let (|SetAccessorExcludes|_|): Ts.Symbol -> bool = hasFlag SF.SetAccessorExcludes 
        let (|AccessorExcludes|_|): Ts.Symbol -> bool = hasFlag SF.AccessorExcludes 
        let (|TypeParameterExcludes|_|): Ts.Symbol -> bool = hasFlag SF.TypeParameterExcludes 
        let (|TypeAliasExcludes|_|): Ts.Symbol -> bool = hasFlag SF.TypeAliasExcludes 
        let (|AliasExcludes|_|): Ts.Symbol -> bool = hasFlag SF.AliasExcludes 
        let (|ModuleMember|_|): Ts.Symbol -> bool = hasFlag SF.ModuleMember 
        let (|ExportHasLocal|_|): Ts.Symbol -> bool = hasFlag SF.ExportHasLocal 
        let (|BlockScoped|_|): Ts.Symbol -> bool = hasFlag SF.BlockScoped 
        let (|PropertyOrAccessor|_|): Ts.Symbol -> bool = hasFlag SF.PropertyOrAccessor 
        let (|ClassMember|_|): Ts.Symbol -> bool = hasFlag SF.ClassMember 
        
    #warnon 25
        

type NameHelpers =
    static member getName(n: Ts.EntityName) =
        match n with
        | Patterns.Node.EntityNamePatterns.Identifier node -> node.text
        | Patterns.Node.EntityNamePatterns.QualifiedName node -> node.getText()

    static member getName(n: Ts.MemberName) =
        match n with
        | Patterns.Node.MemberNamePatterns.Identifier node -> node.text
        | Patterns.Node.MemberNamePatterns.PrivateIdentifier node -> node.text
    static member getName(n: Ts.PropertyName) =
        match n with
        | Patterns.Node.PropertyNamePatterns.Identifier node -> node.text
        | Patterns.Node.PropertyNamePatterns.StringLiteral node -> node.text
        | Patterns.Node.PropertyNamePatterns.NoSubstitutionTemplateLiteral node -> node.text
        | Patterns.Node.PropertyNamePatterns.NumericLiteral node -> node.text
        | Patterns.Node.PropertyNamePatterns.ComputedPropertyName node -> node.getText()
        | Patterns.Node.PropertyNamePatterns.PrivateIdentifier node -> node.text
        | Patterns.Node.PropertyNamePatterns.BigIntLiteral node -> node.text
    static member getName(n: Ts.JsxAttributeName) =
        match n with
        | Patterns.Node.JsxAttributeNamePatterns.Identifier node -> node.text
        | Patterns.Node.JsxAttributeNamePatterns.JsxNamespacedName node -> node.name.text
    static member getName(n: Ts.StringLiteralLike) =
        match n with
        | Patterns.Node.StringLiteralLikePatterns.StringLiteral node -> node.text
        | Patterns.Node.StringLiteralLikePatterns.NoSubstitutionTemplateLiteral node -> node.text
    static member getName(n: Ts.BindingPattern) =
        match n with
        | Patterns.Node.BindingPatternPatterns.ObjectBindingPattern node -> node.getText()
        | Patterns.Node.BindingPatternPatterns.ArrayBindingPattern node -> node.getText()
    static member getName(n: Ts.EntityNameExpression) =
        match n with
        | Patterns.Node.EntityNameExpressionPatterns.Identifier node -> node.text
        | Patterns.Node.EntityNameExpressionPatterns.PropertyAccessExpression node -> node.getText()

    static member getName(n: Ts.DeclarationName) =
        match n with
        | Patterns.Node.DeclarationNamePatterns.JsxNamespacedName node -> node.name.text
        | Patterns.Node.DeclarationNamePatterns.ElementAccessExpression node -> node.getText()
        | Patterns.Node.DeclarationNamePatterns.PropertyName node -> NameHelpers.getName node
        | Patterns.Node.DeclarationNamePatterns.BindingPattern node -> NameHelpers.getName node
        | Patterns.Node.DeclarationNamePatterns.EntityNameExpression node -> node.getText()
    static member getName(n: Ts.ComputedPropertyName) = n.getText()
    static member getName(n: Ts.Identifier) = n.text
    static member getName(n: Ts.BindingName) =
        match n with
        | Patterns.Node.BindingNamePatterns.Identifier node -> node.text
        | Patterns.Node.BindingNamePatterns.ObjectBindingPattern node -> node.getText()
        | Patterns.Node.BindingNamePatterns.ArrayBindingPattern node -> node.getText()
    static member getName(n: Ts.Identifier option) =
        match n with
        | Some v -> NameHelpers.getName v
        | None -> "$ANONYMOUS$"
    static member getName(n: Ts.ModuleName) =
        match n with
        | Patterns.Node.ModuleNamePatterns.Identifier value ->
            value.text
        | Patterns.Node.ModuleNamePatterns.StringLiteral value ->
            value.text
            

// Original impl Credit @MangelMaxine
let isFromEs5Lib (symbolOpt: Ts.Symbol option) =
    match symbolOpt with
    | None -> false
    | Some symbol ->
        match symbol.declarations with
        | Some declarations when declarations.Count > 0 && !!declarations[0].parent ->
            match declarations[0].parent with
            | Patterns.Node.SourceFile sourceFile ->
                // All TypeScript built-in lib files follow the pattern */lib/lib.*.d.ts
                let fn = sourceFile.fileName
                fn.Contains("/lib/lib.") && fn.EndsWith(".d.ts")
            | _ -> false
        | _ ->
            // For some reason, I can't seem to resolve the actual symbol for some Es5 types
            // So, we make a naive fallback checking the name of the symbol
            [ "Iterable"; "IterableIterator" ] |> List.contains symbol.name
            
module TypeCrawler =
    type InterfaceTypeAliasDeclarations =
        | Interface of Ts.InterfaceDeclaration
        | TypeAlias of Ts.TypeAliasDeclaration
        | PropertySignature of Ts.PropertySignature
        | MethodSignature of Ts.MethodSignature
        | IndexSignature of Ts.IndexSignatureDeclaration
        | CallSignature of Ts.CallSignatureDeclaration
        | ConstructSignature of Ts.ConstructSignatureDeclaration
        | GetAccessor of Ts.GetAccessorDeclaration
        | SetAccessor of Ts.SetAccessorDeclaration
        | HeritageClause of Ts.HeritageClause
    type ClassDeclarations =
        | ClassDeclaration of Ts.ClassDeclaration
        | Property of Ts.PropertyDeclaration
        | Method of Ts.MethodDeclaration
        | Constructor of Ts.ConstructorDeclaration
        | GetAccessor of Ts.GetAccessorDeclaration
        | SetAccessor of Ts.SetAccessorDeclaration
        | HeritageClause of Ts.HeritageClause
        | ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments
    type EnumDeclarations =
        | EnumMember of Ts.EnumMember
        | EnumDeclaration of Ts.EnumDeclaration
    
    type VariableDeclarations =
        | VariableStatement of Ts.VariableStatement
        | VariableDeclaration of Ts.VariableDeclaration
    type FunctionDeclarations =
        | FunctionDeclaration of Ts.FunctionDeclaration
    type ParameterDeclaration =
        | Parameter of Ts.ParameterDeclaration
    
    type TypeDeclaration =
        | Interface of Ts.InterfaceDeclaration
        | TypeAlias of Ts.TypeAliasDeclaration
        | PropertySignature of Ts.PropertySignature
        | MethodSignature of Ts.MethodSignature
        | IndexSignature of Ts.IndexSignatureDeclaration
        | CallSignature of Ts.CallSignatureDeclaration
        | ConstructSignature of Ts.ConstructSignatureDeclaration
        | Class of Ts.ClassDeclaration
        | Property of Ts.PropertyDeclaration
        | Method of Ts.MethodDeclaration
        | Constructor of Ts.ConstructorDeclaration
        | GetAccessor of Ts.GetAccessorDeclaration
        | SetAccessor of Ts.SetAccessorDeclaration
        | HeritageClause of Ts.HeritageClause
        | ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments
        | EnumDeclaration of Ts.EnumDeclaration
        | EnumMember of Ts.EnumMember
        | VariableStatement of Ts.VariableStatement
        | VariableDeclaration of Ts.VariableDeclaration
        | FunctionDeclaration of Ts.FunctionDeclaration
        | Parameter of Ts.ParameterDeclaration
    
    type TypeNodes =
        | StringKeyword of Ts.KeywordTypeNode
        | NumberKeyword of Ts.KeywordTypeNode
        | BooleanKeyword of Ts.KeywordTypeNode
        | NullKeyword of Ts.KeywordTypeNode
        | UndefinedKeyword of Ts.KeywordTypeNode
        | VoidKeyword of Ts.KeywordTypeNode
        | NeverKeyword of Ts.KeywordTypeNode
        | AnyKeyword of Ts.KeywordTypeNode
        | UnknownKeyword of Ts.KeywordTypeNode
        | ObjectKeyword of Ts.KeywordTypeNode
        | SymbolKeyword of Ts.KeywordTypeNode
        | BigIntKeyword of Ts.KeywordTypeNode
        | UnionType of Ts.UnionTypeNode
        | IntersectionType of Ts.IntersectionTypeNode
        | ArrayType of Ts.ArrayTypeNode
        | TupleType of Ts.TupleTypeNode
        | NamedTupleMember of Ts.NamedTupleMember
        | RestType of Ts.RestTypeNode
        | OptionalType of Ts.OptionalTypeNode
        | ParenthesizedType of Ts.ParenthesizedTypeNode
        | TypeReference of Ts.TypeReferenceNode
        | TypeParameter of Ts.TypeParameter
        | TypePredicate of Ts.TypePredicateNode
        | TypeQuery of Ts.TypeQueryNode
        | TypeOperator of Ts.TypeOperatorNode
        | IndexedAccessType of Ts.IndexedAccessTypeNode
        | MappedType of Ts.MappedTypeNode
        | ConditionalType of Ts.ConditionalTypeNode
        | InferType of Ts.InferTypeNode
        | TemplateLiteralType of Ts.TemplateLiteralTypeNode
        | TemplateLiteralTypeSpan of Ts.TemplateLiteralTypeSpan
        | FunctionType of Ts.FunctionTypeNode
        | ConstructorType of Ts.ConstructorTypeNode
        | TypeLiteral of Ts.TypeLiteralNode
        | LiteralType of Ts.LiteralTypeNode
        | ThisType of Ts.ThisTypeNode