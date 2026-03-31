[<AutoOpen>]
module rec Xantham.Fable.AutoOpenXanTagKind

open System.Collections.Generic
open System.ComponentModel
open Fable.Core
open Fable.Core.JsInterop
open TypeScript

module private Internal =
    let inline private (>=>) a b = KeyValuePair(a, unbox >> b)
    let inline private (>->) a b = a, (unbox >> b)
    let memberDeclarationKindSetMap: Dictionary<Ts.SyntaxKind, obj -> MemberDeclaration> =
        Dictionary [
            Ts.SyntaxKind.PropertySignature >=> MemberDeclaration.PropertySignature
            Ts.SyntaxKind.MethodSignature >=> MemberDeclaration.MethodSignature
            Ts.SyntaxKind.IndexSignature >=> MemberDeclaration.IndexSignature
            Ts.SyntaxKind.CallSignature >=> MemberDeclaration.CallSignature
            Ts.SyntaxKind.ConstructSignature >=> MemberDeclaration.ConstructSignature
            Ts.SyntaxKind.PropertyDeclaration >=> MemberDeclaration.Property
            Ts.SyntaxKind.MethodDeclaration >=> MemberDeclaration.Method
            Ts.SyntaxKind.Constructor >=> MemberDeclaration.Constructor
            Ts.SyntaxKind.GetAccessor >=> MemberDeclaration.GetAccessor
            Ts.SyntaxKind.SetAccessor >=> MemberDeclaration.SetAccessor
            Ts.SyntaxKind.Parameter >=> MemberDeclaration.Parameter
        ]
    let typeDeclarationKindSetMap: Dictionary<Ts.SyntaxKind, obj -> TypeDeclaration> =
        Dictionary [
            Ts.SyntaxKind.TypeParameter >=> TypeDeclaration.TypeParameter
            Ts.SyntaxKind.InterfaceDeclaration >=> TypeDeclaration.Interface
            Ts.SyntaxKind.TypeAliasDeclaration >=> TypeDeclaration.TypeAlias
            Ts.SyntaxKind.ClassDeclaration >=> TypeDeclaration.Class
            Ts.SyntaxKind.HeritageClause >=> TypeDeclaration.HeritageClause
            Ts.SyntaxKind.ExpressionWithTypeArguments >=> TypeDeclaration.ExpressionWithTypeArguments
            Ts.SyntaxKind.EnumDeclaration >=> TypeDeclaration.Enum
            Ts.SyntaxKind.EnumMember >=> TypeDeclaration.EnumMember
            Ts.SyntaxKind.VariableStatement >=> TypeDeclaration.VariableStatement
            Ts.SyntaxKind.VariableDeclaration >=> TypeDeclaration.VariableDeclaration
            Ts.SyntaxKind.FunctionDeclaration >=> TypeDeclaration.FunctionDeclaration
            Ts.SyntaxKind.ModuleDeclaration >=> TypeDeclaration.Module
            Ts.SyntaxKind.ModuleBlock >=> TypeDeclaration.ModuleBlock
        ]
    let typeNodeKindSetMap: Dictionary<Ts.SyntaxKind,obj -> TypeNode> =
        Dictionary [
            Ts.SyntaxKind.StringKeyword >=> TypeNode.StringKeyword
            Ts.SyntaxKind.NumberKeyword  >=> TypeNode.NumberKeyword
            Ts.SyntaxKind.BooleanKeyword  >=> TypeNode.BooleanKeyword
            Ts.SyntaxKind.NullKeyword  >=> TypeNode.NullKeyword
            Ts.SyntaxKind.UndefinedKeyword  >=> TypeNode.UndefinedKeyword
            Ts.SyntaxKind.VoidKeyword  >=> TypeNode.VoidKeyword
            Ts.SyntaxKind.NeverKeyword  >=> TypeNode.NeverKeyword
            Ts.SyntaxKind.AnyKeyword  >=> TypeNode.AnyKeyword
            Ts.SyntaxKind.UnknownKeyword  >=> TypeNode.UnknownKeyword
            Ts.SyntaxKind.ObjectKeyword  >=> TypeNode.ObjectKeyword
            Ts.SyntaxKind.SymbolKeyword  >=> TypeNode.SymbolKeyword
            Ts.SyntaxKind.BigIntKeyword  >=> TypeNode.BigIntKeyword
            Ts.SyntaxKind.IntrinsicKeyword  >=> TypeNode.IntrinsicKeyword
            Ts.SyntaxKind.UnionType  >=> TypeNode.UnionType
            Ts.SyntaxKind.IntersectionType  >=> TypeNode.IntersectionType
            Ts.SyntaxKind.ArrayType  >=> TypeNode.ArrayType
            Ts.SyntaxKind.TupleType  >=> TypeNode.TupleType
            Ts.SyntaxKind.NamedTupleMember  >=> TypeNode.NamedTupleMember
            Ts.SyntaxKind.RestType  >=> TypeNode.RestType
            Ts.SyntaxKind.OptionalType  >=> TypeNode.OptionalType
            Ts.SyntaxKind.ParenthesizedType  >=> TypeNode.ParenthesizedType
            Ts.SyntaxKind.TypeReference  >=> TypeNode.TypeReference
            Ts.SyntaxKind.TypeParameter >=> TypeNode.TypeParameterDeclaration
            Ts.SyntaxKind.InferType  >=> TypeNode.InferType
            Ts.SyntaxKind.TypePredicate  >=> TypeNode.TypePredicate
            Ts.SyntaxKind.TypeQuery  >=> TypeNode.TypeQuery
            Ts.SyntaxKind.TypeOperator  >=> TypeNode.TypeOperator
            Ts.SyntaxKind.IndexedAccessType  >=> TypeNode.IndexedAccessType
            Ts.SyntaxKind.MappedType  >=> TypeNode.MappedType
            Ts.SyntaxKind.ConditionalType  >=> TypeNode.ConditionalType
            Ts.SyntaxKind.TemplateLiteralType  >=> TypeNode.TemplateLiteralType
            Ts.SyntaxKind.TemplateLiteralTypeSpan  >=> TypeNode.TemplateLiteralTypeSpan
            Ts.SyntaxKind.ImportType  >=> TypeNode.ImportType
            Ts.SyntaxKind.FunctionType  >=> TypeNode.FunctionType
            Ts.SyntaxKind.ConstructorType  >=> TypeNode.ConstructorType
            Ts.SyntaxKind.TypeLiteral  >=> TypeNode.TypeLiteral
            Ts.SyntaxKind.LiteralType  >=> TypeNode.LiteralType
            Ts.SyntaxKind.ThisType  >=> TypeNode.ThisType
            Ts.SyntaxKind.SatisfiesExpression  >=> TypeNode.SatisfiesExpression
            Ts.SyntaxKind.AsExpression  >=> TypeNode.AsExpression
            Ts.SyntaxKind.TypeAssertionExpression >=> TypeNode.TypeAssertion
        ]
    let jsDocKindSetMap: Dictionary<Ts.SyntaxKind, obj -> JSDocTags> =
        Dictionary [
            Ts.SyntaxKind.JSDocParameterTag >=> JSDocTags.ParameterTag
            Ts.SyntaxKind.JSDocReturnTag >=> JSDocTags.ReturnTag
            Ts.SyntaxKind.JSDocDeprecatedTag >=> JSDocTags.DeprecatedTag
            Ts.SyntaxKind.JSDocTag >=> JSDocTags.DocTag
            Ts.SyntaxKind.JSDocThrowsTag >=> JSDocTags.ThrowsTag
            Ts.SyntaxKind.JSDocPropertyTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocTypeExpression >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocAllType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocUnknownType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocCallbackTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocTypeTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocTemplateTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocTypedefTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocAugmentsTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocAuthorTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocClassTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocEnumTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocPublicTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocPrivateTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocProtectedTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocReadonlyTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocOverrideTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocComment >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocText >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocNameReference >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocMemberName >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocNullableType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocNonNullableType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocOptionalType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocFunctionType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocVariadicType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocNamepathType >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocTypeLiteral >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocSignature >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocLink >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocLinkCode >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocLinkPlain >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocImplementsTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocOverloadTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocThisTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocSeeTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocSatisfiesTag >=> JSDocTags.UnknownTag
            Ts.SyntaxKind.JSDocImportTag >=> JSDocTags.UnknownTag
        ]
    let moduleExportSetMap: Dictionary<Ts.SyntaxKind, obj -> ModulesAndExports> =
        Dictionary [
            Ts.SyntaxKind.ImportDeclaration >=> ModulesAndExports.ImportDeclaration
            Ts.SyntaxKind.ImportClause >=> ModulesAndExports.ImportClause
            Ts.SyntaxKind.NamespaceImport >=> ModulesAndExports.NamespaceImport
            Ts.SyntaxKind.NamedImports >=> ModulesAndExports.NamedImports
            Ts.SyntaxKind.ImportSpecifier >=> ModulesAndExports.ImportSpecifier
            Ts.SyntaxKind.ImportEqualsDeclaration >=> ModulesAndExports.ImportEqualsDeclaration
            Ts.SyntaxKind.AssertClause >=> ModulesAndExports.AssertClause
            Ts.SyntaxKind.ExportAssignment >=> ModulesAndExports.ExportAssignment
            Ts.SyntaxKind.ExportDeclaration >=> ModulesAndExports.ExportDeclaration
            Ts.SyntaxKind.ExportSpecifier >=> ModulesAndExports.ExportSpecifier
            Ts.SyntaxKind.NamedExports >=> ModulesAndExports.NamedExports
            Ts.SyntaxKind.NamespaceExportDeclaration >=> ModulesAndExports.NamespaceExport
        ]
    let modifierSetMap: Dictionary<Ts.SyntaxKind, obj -> Modifiers> =
        Dictionary [
            Ts.SyntaxKind.ExportKeyword >=> Modifiers.Export
            Ts.SyntaxKind.DeclareKeyword >=> Modifiers.Declare
            Ts.SyntaxKind.DefaultKeyword >=> Modifiers.Default
            Ts.SyntaxKind.AbstractKeyword >=> Modifiers.Abstract
            Ts.SyntaxKind.ReadonlyKeyword >=> Modifiers.ReadOnly
            Ts.SyntaxKind.StaticKeyword >=> Modifiers.Static
            Ts.SyntaxKind.OverrideKeyword >=> Modifiers.Override
            Ts.SyntaxKind.AccessorKeyword >=> Modifiers.Accessor
            Ts.SyntaxKind.PublicKeyword >=> Modifiers.Public
            Ts.SyntaxKind.ProtectedKeyword >=> Modifiers.Protected
            Ts.SyntaxKind.PrivateKeyword >=> Modifiers.Private
        ]
    let literalTokenNodeKindSet: Dictionary<Ts.SyntaxKind, obj -> LiteralTokenNodes> =
        Dictionary [
            Ts.SyntaxKind.StringLiteral >=> LiteralTokenNodes.StringLiteral 
            Ts.SyntaxKind.NumericLiteral >=> LiteralTokenNodes.NumericLiteral 
            Ts.SyntaxKind.BigIntLiteral >=> LiteralTokenNodes.BigIntLiteral 
            Ts.SyntaxKind.TrueKeyword >=> LiteralTokenNodes.TrueLiteral 
            Ts.SyntaxKind.FalseKeyword >=> LiteralTokenNodes.FalseLiteral 
            Ts.SyntaxKind.NullKeyword >=> LiteralTokenNodes.NullLiteral 
            Ts.SyntaxKind.PrefixUnaryExpression >=> LiteralTokenNodes.PrefixUnaryExpression 
            Ts.SyntaxKind.NoSubstitutionTemplateLiteral >=> LiteralTokenNodes.NoSubstitutionTemplateLiteral
        ]

    let typeFlagLiteralKindSet: (Ts.TypeFlags * (obj -> TypeFlagLiteral))[] =
        [|
            Ts.TypeFlags.UniqueESSymbol >-> TypeFlagLiteral.UniqueESSymbol 
            Ts.TypeFlags.EnumLiteral >-> TypeFlagLiteral.EnumLiteral
            Ts.TypeFlags.BooleanLiteral >-> TypeFlagLiteral.Boolean
            Ts.TypeFlags.BigIntLiteral >-> TypeFlagLiteral.BigInt
            Ts.TypeFlags.NumberLiteral >-> TypeFlagLiteral.Number
            Ts.TypeFlags.StringLiteral >-> TypeFlagLiteral.String
        |]
    let typeFlagObjectKindSet: (Ts.ObjectFlags * (obj -> TypeFlagObject))[] =
        [|
            Ts.ObjectFlags.Class >-> TypeFlagObject.Class
            Ts.ObjectFlags.Interface >-> TypeFlagObject.Interface
            Ts.ObjectFlags.Tuple >-> TypeFlagObject.Tuple
            Ts.ObjectFlags.Reference >-> TypeFlagObject.Reference
            Ts.ObjectFlags.Anonymous >-> TypeFlagObject.Anonymous
            Ts.ObjectFlags.Mapped >-> TypeFlagObject.Mapped
            Ts.ObjectFlags.Instantiated >-> TypeFlagObject.Instantiated
            Ts.ObjectFlags.EvolvingArray >-> TypeFlagObject.EvolvingArray
        |]
    #nowarn 40
    let typeFlagPrimaryKindSet: (Ts.TypeFlags * (obj -> TypeFlagPrimary))[] =
        let literalKindSet o =
            typeFlagLiteralKindSet
            |> Array.find (fst >> (unbox<Ts.Type> o).flags.HasFlag)
            |> snd |> funApply o |> TypeFlagPrimary.Literal
        let objectKindSet o =
            typeFlagObjectKindSet
            |> Array.find (fst >> (unbox<Ts.ObjectType> o).objectFlags.HasFlag)
            |> snd |> funApply o |> TypeFlagPrimary.Object
        [|
            Ts.TypeFlags.Any >-> TypeFlagPrimary.Any 
            Ts.TypeFlags.Unknown >-> TypeFlagPrimary.Unknown 
            Ts.TypeFlags.UniqueESSymbol >-> TypeFlagPrimary.UniqueESSymbol 
            Ts.TypeFlags.EnumLiteral >-> literalKindSet
            Ts.TypeFlags.BooleanLiteral >-> literalKindSet
            Ts.TypeFlags.BigIntLiteral >-> literalKindSet
            Ts.TypeFlags.NumberLiteral >-> literalKindSet
            Ts.TypeFlags.StringLiteral >-> literalKindSet
            Ts.TypeFlags.Enum >-> literalKindSet
            Ts.TypeFlags.ESSymbol >-> TypeFlagPrimary.ESSymbol
            Ts.TypeFlags.String >-> TypeFlagPrimary.String
            Ts.TypeFlags.Number >-> TypeFlagPrimary.Number
            Ts.TypeFlags.Boolean >-> TypeFlagPrimary.Boolean
            Ts.TypeFlags.BigInt >-> TypeFlagPrimary.BigInt
            Ts.TypeFlags.Void >-> TypeFlagPrimary.Void
            Ts.TypeFlags.Undefined >-> TypeFlagPrimary.Undefined
            Ts.TypeFlags.Null >-> TypeFlagPrimary.Null
            Ts.TypeFlags.Never >-> TypeFlagPrimary.Never
            Ts.TypeFlags.Object >-> objectKindSet
            Ts.TypeFlags.TypeParameter >-> TypeFlagPrimary.TypeParameter
            Ts.TypeFlags.Union >-> TypeFlagPrimary.Union
            Ts.TypeFlags.Intersection >-> TypeFlagPrimary.Intersection
            Ts.TypeFlags.Index >-> TypeFlagPrimary.Index
            Ts.TypeFlags.IndexedAccess >-> TypeFlagPrimary.IndexedAccess
            Ts.TypeFlags.Conditional >-> TypeFlagPrimary.Conditional
            Ts.TypeFlags.Substitution >-> TypeFlagPrimary.Substitution
            Ts.TypeFlags.NonPrimitive >-> TypeFlagPrimary.NonPrimitive
            Ts.TypeFlags.TemplateLiteral >-> TypeFlagPrimary.TemplateLiteral
            Ts.TypeFlags.StringMapping >-> TypeFlagPrimary.StringMapping
            Ts.TypeFlags.Unit >-> TypeFlagPrimary.NonPrimitive
        |]
    #warnon 40
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isTypeDeclaration key = typeDeclarationKindSetMap.Keys |> Seq.contains key
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isTypeNode key = typeNodeKindSetMap.Keys |> Seq.contains key
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isJsDoc key = jsDocKindSetMap.Keys |> Seq.contains key
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isModuleExport key = moduleExportSetMap.Keys |> Seq.contains key
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isModifier key = modifierSetMap.Keys |> Seq.contains key
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    let isLiteralNode key = literalTokenNodeKindSet.Keys |> Seq.contains key

[<RequireQualifiedAccess>]
type MemberDeclaration =
    /// <summary>
    /// A parameter declaration in a function, method, or constructor signature.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the parameter binding; <c>type</c> (optional) — declared type; <c>questionToken</c> (optional) — marks optional parameter; <c>dotDotDotToken</c> (optional) — marks rest parameter; <c>initializer</c> — always absent in <c>.d.ts</c>.</para>
    /// <para><b>Safety:</b> <c>type</c> may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>...args: string[]</c> or <c>x?: number</c></para>
    /// </remarks>
    | Parameter of Ts.ParameterDeclaration
    /// <summary>
    /// A setter accessor in a class or object type: <c>set name(value: Type)</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the accessor identifier; <c>parameters</c> — exactly one parameter. The setter's own <c>type</c> field is typically absent.</para>
    /// <para><b>Safety:</b> Do not rely on the setter's <c>type</c> field; use <c>checker.getTypeAtLocation</c> on the parameter instead.</para>
    /// <para><b>Example:</b> <c>set value(v: number)</c></para>
    /// </remarks>
    | SetAccessor of Ts.SetAccessorDeclaration
    /// <summary>
    /// A getter accessor in a class or object type: <c>get name(): Type</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the accessor identifier; <c>type</c> (optional) — return type. The parameters list is always empty for a getter; <c>modifiers</c> may include <c>static</c>.</para>
    /// <para><b>Safety:</b> <c>type</c> (return type) may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>get value(): number</c></para>
    /// </remarks>
    | GetAccessor of Ts.GetAccessorDeclaration
    /// <summary>
    /// A constructor declaration inside a class body: <c>constructor(params) { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>parameters</c> — parameter list; <c>type</c> (optional) — return type, rarely present in <c>.d.ts</c>. There is no <c>name</c> field.</para>
    /// <para><b>Safety:</b> <c>body</c> is absent in <c>.d.ts</c> files; <c>type</c> is rarely present and should not be assumed.</para>
    /// <para><b>Example:</b> <c>constructor(x: string)</c></para>
    /// </remarks>
    | Constructor of Ts.ConstructorDeclaration
    /// <summary>
    /// A method declaration inside a class body: <c>name(params): ReturnType { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the method identifier; <c>typeParameters</c> (optional) — generic parameters; <c>parameters</c> — parameter list; <c>type</c> (optional) — return type; <c>body</c> (optional) — implementation; <c>modifiers</c> — includes <c>static</c>, <c>abstract</c>, <c>override</c>.</para>
    /// <para><b>Safety:</b> In <c>.d.ts</c> files <c>body</c> is always absent; <c>type</c> (return type) may also be absent.</para>
    /// <para><b>Example:</b> <c>static foo&lt;T&gt;(x: T): void</c></para>
    /// </remarks>
    | Method of Ts.MethodDeclaration
    /// <summary>
    /// A property declaration inside a class body: <c>name: Type</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the property identifier (may be <c>ComputedPropertyName</c> or <c>PrivateIdentifier</c>); <c>questionToken</c> (optional); <c>exclamationToken</c> (optional) — definite assignment assertion; <c>type</c> (optional) — declared type; <c>modifiers</c> — e.g. <c>static</c>, <c>readonly</c>.</para>
    /// <para><b>Safety:</b> <c>type</c> may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>readonly foo: string</c></para>
    /// </remarks>
    | Property of Ts.PropertyDeclaration
    /// <summary>
    /// A construct signature in an interface or object type: <c>new(params): InstanceType</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>typeParameters</c> (optional) — generic parameters; <c>parameters</c> — parameter list; <c>type</c> (optional) — the constructed instance type.</para>
    /// <para><b>Safety:</b> <c>type</c> may be absent; fall back to <c>checker.getTypeAtLocation</c> for the constructed type when needed.</para>
    /// <para><b>Example:</b> <c>new(x: string): Foo</c></para>
    /// </remarks>
    | ConstructSignature of Ts.ConstructSignatureDeclaration
    /// <summary>
    /// A call signature in an interface or object type: <c>(params): ReturnType</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>typeParameters</c> (optional) — generic parameters; <c>parameters</c> — parameter list; <c>type</c> (optional) — return type. There is no <c>name</c> field.</para>
    /// <para><b>Safety:</b> <c>type</c> (return type) may be absent; fall back to <c>checker.getTypeAtLocation</c> when needed.</para>
    /// <para><b>Example:</b> <c>(x: string): number</c></para>
    /// </remarks>
    | CallSignature of Ts.CallSignatureDeclaration
    /// <summary>
    /// An index signature: <c>[key: KeyType]: ValueType</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>parameters[0]</c> — the key parameter (its <c>type</c> field is the key type); <c>type</c> (optional) — the value type on the declaration itself.</para>
    /// <para><b>Safety:</b> <c>type</c> (value type) may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>[key: string]: number</c></para>
    /// </remarks>
    | IndexSignature of Ts.IndexSignatureDeclaration
    /// <summary>
    /// A method signature in an interface or object type: <c>name(params): ReturnType</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the method identifier; <c>typeParameters</c> (optional) — generic parameters; <c>parameters</c> — parameter list; <c>type</c> (optional) — return type; <c>questionToken</c> (optional) — marks optional methods.</para>
    /// <para><b>Safety:</b> <c>type</c> (return type) may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>foo&lt;T&gt;(x: T): void</c></para>
    /// </remarks>
    | MethodSignature of Ts.MethodSignature
    /// <summary>
    /// A property signature in an interface or object type: <c>name?: Type</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the property identifier (may be <c>ComputedPropertyName</c>); <c>questionToken</c> (optional) — present when the property is optional; <c>type</c> (optional) — the declared type.</para>
    /// <para><b>Safety:</b> <c>type</c> may be absent; fall back to <c>checker.getTypeAtLocation</c> when it is.</para>
    /// <para><b>Example:</b> <c>foo?: string</c></para>
    /// </remarks>
    | PropertySignature of Ts.PropertySignature

[<RequireQualifiedAccess>]
type TypeDeclaration =
    /// <summary>
    /// A generic type parameter declaration such as <c>T</c> in <c>interface Foo&lt;T&gt;</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the <c>Identifier</c> for the parameter name; <c>constraint</c> (optional) — the upper-bound type; <c>default</c> (optional) — the default type.</para>
    /// <para><b>Safety:</b> Neither <c>constraint</c> nor <c>default</c> is guaranteed to be present; always check before accessing.</para>
    /// <para><b>Example:</b> <c>T extends string = "hello"</c></para>
    /// </remarks>
    | TypeParameter of Ts.TypeParameterDeclaration
    /// <summary>
    /// An interface declaration: <c>interface Foo { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the interface identifier; <c>typeParameters</c> (optional) — generic parameters; <c>heritageClauses</c> (optional) — <c>extends</c> clauses; <c>members</c> — body member declarations.</para>
    /// <para><b>Safety:</b> <c>typeParameters</c> and <c>heritageClauses</c> may be absent; always guard before iteration.</para>
    /// <para><b>Example:</b> <c>interface Foo&lt;T&gt; extends Bar { prop: T }</c></para>
    /// </remarks>
    | Interface of Ts.InterfaceDeclaration
    /// <summary>
    /// A type alias declaration: <c>type Foo = ...</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the alias identifier; <c>typeParameters</c> (optional) — generic parameters; <c>type</c> — the aliased type node.</para>
    /// <para><b>Safety:</b> <c>typeParameters</c> may be absent; <c>type</c> is technically non-optional in the AST but fall back to <c>checker.getTypeAtLocation</c> for inferred cases.</para>
    /// <para><b>Example:</b> <c>type Result&lt;T&gt; = T | null</c></para>
    /// </remarks>
    | TypeAlias of Ts.TypeAliasDeclaration
    /// <summary>
    /// A class declaration: <c>class Foo { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> (optional) — the class identifier (absent for anonymous class expressions); <c>typeParameters</c> (optional) — generic parameters; <c>heritageClauses</c> (optional) — <c>extends</c> and <c>implements</c> clauses; <c>members</c> — class body members.</para>
    /// <para><b>Safety:</b> <c>name</c> may be absent for anonymous classes; <c>typeParameters</c> and <c>heritageClauses</c> may also be absent.</para>
    /// <para><b>Example:</b> <c>class Foo&lt;T&gt; extends Bar implements IBaz { }</c></para>
    /// </remarks>
    | Class of Ts.ClassDeclaration
    /// <summary>
    /// A heritage clause in a class or interface: the <c>extends</c> or <c>implements</c> keyword
    /// together with its list of base types.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>token</c> — the keyword token; <c>types</c> — array of <c>ExpressionWithTypeArguments</c> nodes representing the base types.</para>
    /// <para><b>Safety:</b> Check <c>token.kind</c> to distinguish <c>ExtendsKeyword</c> from <c>ImplementsKeyword</c> before processing.</para>
    /// <para><b>Example:</b> <c>extends Foo&lt;string&gt;, Bar</c></para>
    /// </remarks>
    | HeritageClause of Ts.HeritageClause
    /// <summary>
    /// A single base-type entry inside a heritage clause: <c>Foo&lt;T&gt;</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>expression</c> — the base type name (may be a <c>PropertyAccessExpression</c> for dotted names such as <c>Foo.Bar</c>); <c>typeArguments</c> (optional) — type argument array.</para>
    /// <para><b>Safety:</b> <c>typeArguments</c> may be absent; always check before iterating.</para>
    /// <para><b>Example:</b> <c>Foo.Bar&lt;string&gt;</c></para>
    /// </remarks>
    | ExpressionWithTypeArguments of Ts.ExpressionWithTypeArguments
    /// <summary>
    /// An enum declaration: <c>enum Color { Red, Green, Blue }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the enum identifier; <c>members</c> — array of <c>EnumMember</c> nodes; <c>modifiers</c> — check for <c>const</c> keyword.</para>
    /// <para><b>Safety:</b> No special concerns.</para>
    /// <para><b>Example:</b> <c>const enum Direction { Up = 1, Down }</c></para>
    /// </remarks>
    | Enum of Ts.EnumDeclaration
    /// <summary>
    /// A single member of an enum: <c>Name = Value</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the member identifier; <c>initializer</c> (optional) — the value expression.</para>
    /// <para><b>Safety:</b> <c>initializer</c> may be absent; when it is, the member auto-increments from the previous numeric value.</para>
    /// <para><b>Example:</b> <c>Red = "red"</c> or <c>Green</c> (no initializer)</para>
    /// </remarks>
    | EnumMember of Ts.EnumMember
    /// <summary>
    /// A variable statement that may declare one or more variables: <c>declare const x: number</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>declarationList.declarations</c> — array of <c>VariableDeclaration</c> nodes; <c>declarationList</c> flags — distinguish <c>const</c>/<c>let</c>/<c>var</c>.</para>
    /// <para><b>Safety:</b> Individual declarations are nested inside <c>declarationList</c>; do not assume a single binding per statement.</para>
    /// <para><b>Example:</b> <c>declare const x: number, y: string</c></para>
    /// </remarks>
    | VariableStatement of Ts.VariableStatement
    /// <summary>
    /// A single variable binding inside a <c>VariableStatement</c>: <c>name: Type</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the binding (may be a binding pattern in non-declaration files); <c>type</c> (optional) — declared type; <c>initializer</c> — always absent in <c>.d.ts</c>.</para>
    /// <para><b>Safety:</b> <c>type</c> may be absent; prefer <c>checker.getTypeAtLocation</c> in that case.</para>
    /// <para><b>Example:</b> <c>x: number</c></para>
    /// </remarks>
    | VariableDeclaration of Ts.VariableDeclaration
    /// <summary>
    /// A top-level function declaration: <c>function foo(params): ReturnType</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> (optional) — the function identifier (absent for anonymous overloads); <c>typeParameters</c> (optional) — generic parameters; <c>parameters</c> — parameter list; <c>type</c> (optional) — return type.</para>
    /// <para><b>Safety:</b> <c>body</c> is absent in <c>.d.ts</c> files; <c>name</c> may be absent for overload stubs.</para>
    /// <para><b>Example:</b> <c>function foo&lt;T&gt;(x: T): T</c></para>
    /// </remarks>
    | FunctionDeclaration of Ts.FunctionDeclaration
    /// <summary>
    /// A module or namespace declaration: <c>declare module "foo" { ... }</c> or
    /// <c>namespace Foo { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — either an <c>Identifier</c> or a <c>StringLiteral</c>; <c>body</c> (optional) — may be a <c>ModuleBlock</c>, a nested <c>NamespaceDeclaration</c>, or absent.</para>
    /// <para><b>Safety:</b> Always check the kind of <c>body</c> before unwrapping; it may be absent for ambient modules with no body.</para>
    /// <para><b>Example:</b> <c>declare module "foo" { }</c></para>
    /// </remarks>
    | Module of Ts.ModuleDeclaration
    // /// <summary>
    // /// A <c>export as namespace Foo</c> declaration that exposes the module as a UMD global.
    // /// </summary>
    // /// <remarks>
    // /// <para><b>Fields:</b> <c>name</c> — the global namespace identifier. This node has no type parameters or body.</para>
    // /// <para><b>Safety:</b> No special concerns.</para>
    // /// <para><b>Example:</b> <c>export as namespace myLib</c></para>
    // /// </remarks>
    // | NamespaceExportDeclaration of Ts.NamespaceExportDeclaration
    /// <summary>
    /// A namespace declaration that is the body of a dotted module path: the inner part of
    /// <c>namespace Foo.Bar { ... }</c>.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>name</c> — the right-hand segment identifier; <c>body</c> — may be another <c>NamespaceDeclaration</c> for deeper nesting or a <c>ModuleBlock</c>.</para>
    /// <para><b>Safety:</b> Always check the kind of <c>body</c> before unwrapping; deeper dotted paths produce nested <c>NamespaceDeclaration</c> nodes.</para>
    /// <para><b>Example:</b> <c>Bar</c> in <c>namespace Foo.Bar { }</c></para>
    /// </remarks>
    | Namespace of Ts.NamespaceDeclaration
    /// <summary>
    /// The block of statements that forms the body of a module or namespace declaration.
    /// </summary>
    /// <remarks>
    /// <para><b>Fields:</b> <c>statements</c> — array of declarations inside the block. This node is the direct child of a <c>ModuleDeclaration</c> whose body is a block.</para>
    /// <para><b>Safety:</b> No special concerns.</para>
    /// <para><b>Example:</b> the <c>{ ... }</c> in <c>declare module "foo" { export const x: number }</c></para>
    /// </remarks>
    | ModuleBlock of Ts.ModuleBlock


[<RequireQualifiedAccess>]
type TypeNode =
    /// <summary>
    /// The <c>string</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract; the node itself represents the primitive type.
    /// Example: <c>x: string</c>.
    /// </remarks>
    | StringKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>number</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract; the node itself represents the primitive type.
    /// Example: <c>x: number</c>.
    /// </remarks>
    | NumberKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>boolean</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract; the node itself represents the primitive type.
    /// Example: <c>x: boolean</c>.
    /// </remarks>
    | BooleanKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>null</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Typically appears in union types such as <c>string | null</c>.
    /// Also routed here when it appears as a <c>LiteralType</c> child — check the call site.
    /// Example: <c>null</c>.
    /// </remarks>
    | NullKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>undefined</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Typically appears in union types or optional return positions.
    /// Example: <c>x: undefined</c>.
    /// </remarks>
    | UndefinedKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>void</c> keyword used as a return-type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Indicates a function returns no usable value.
    /// Example: <c>(): void</c>.
    /// </remarks>
    | VoidKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>never</c> keyword used as a type annotation for values that never occur.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Appears as a bottom type in exhaustive checks or always-throwing
    /// functions. Example: <c>(): never</c>.
    /// </remarks>
    | NeverKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>any</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Represents an unchecked escape hatch from the type system.
    /// Example: <c>x: any</c>.
    /// </remarks>
    | AnyKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>unknown</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. The type-safe counterpart of <c>any</c>; requires narrowing before use.
    /// Example: <c>x: unknown</c>.
    /// </remarks>
    | UnknownKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>object</c> keyword used as a type annotation (non-primitive object type).
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Distinct from <c>Object</c> (the interface) and object-literal types.
    /// Example: <c>x: object</c>.
    /// </remarks>
    | ObjectKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>symbol</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Represents the general <c>symbol</c> primitive type as opposed to a
    /// unique symbol. Example: <c>x: symbol</c>.
    /// </remarks>
    | SymbolKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>bigint</c> keyword used as a type annotation.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Example: <c>x: bigint</c>.
    /// </remarks>
    | BigIntKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// The <c>intrinsic</c> keyword used internally by TypeScript for built-in string manipulation types.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. This keyword is not part of the public TypeScript syntax and should
    /// be treated as opaque. Example: <c>type Uppercase&lt;S extends string&gt; = intrinsic</c>.
    /// </remarks>
    | IntrinsicKeyword of Ts.KeywordTypeNode
    /// <summary>
    /// A union type node: <c>A | B | ...</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>types</c> (array of constituent <c>TypeNode</c>s). Be aware that TypeScript normalises
    /// <c>true | false</c> into the single <c>boolean</c> type at the checker level; at the AST level
    /// both members are still present. Example: <c>string | number | null</c>.
    /// </remarks>
    | UnionType of Ts.UnionTypeNode
    /// <summary>
    /// An intersection type node: <c>A &amp; B &amp; ...</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>types</c> (array of constituent <c>TypeNode</c>s).
    /// Example: <c>Foo &amp; Bar</c>.
    /// </remarks>
    | IntersectionType of Ts.IntersectionTypeNode
    /// <summary>
    /// An array type shorthand: <c>T[]</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>elementType</c> for the element type node. This is syntactic sugar for
    /// <c>Array&lt;T&gt;</c>. Example: <c>string[]</c>.
    /// </remarks>
    | ArrayType of Ts.ArrayTypeNode
    /// <summary>
    /// A tuple type: <c>[A, B, ...]</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>elements</c> (array of type nodes or <c>NamedTupleMember</c>/<c>RestType</c>/
    /// <c>OptionalType</c> wrappers). Check each element's kind before unwrapping.
    /// Example: <c>[string, number?]</c>.
    /// </remarks>
    | TupleType of Ts.TupleTypeNode
    /// <summary>
    /// A named tuple element: <c>name: Type</c> or <c>name?: Type</c> inside a tuple.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the element label, <c>type</c> for the element type, and
    /// <c>questionToken</c> (optional). Example: <c>value: string</c> in <c>[value: string, id: number]</c>.
    /// </remarks>
    | NamedTupleMember of Ts.NamedTupleMember
    /// <summary>
    /// A rest element in a tuple type: <c>...T</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>type</c> for the spread type (typically an array or tuple type).
    /// Example: <c>...string[]</c> in <c>[number, ...string[]]</c>.
    /// </remarks>
    | RestType of Ts.RestTypeNode
    /// <summary>
    /// An optional element in a tuple type: <c>T?</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>type</c> for the underlying type. This wrapper appears only inside tuple types, not
    /// in plain optional properties (which use <c>questionToken</c> on the property node instead).
    /// Example: <c>number?</c> in <c>[string, number?]</c>.
    /// </remarks>
    | OptionalType of Ts.OptionalTypeNode
    /// <summary>
    /// A parenthesised type used for grouping: <c>(T)</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>type</c> for the inner type node. The parentheses carry no semantic meaning; they exist
    /// only to clarify precedence in source. Example: <c>(string | number)[]</c>.
    /// </remarks>
    | ParenthesizedType of Ts.ParenthesizedTypeNode
    /// <summary>
    /// A type reference that names a previously declared type, possibly with type arguments: <c>Foo&lt;T&gt;</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>typeName</c> (may be an <c>Identifier</c> or a <c>QualifiedName</c> for dotted references
    /// such as <c>Foo.Bar</c>) and <c>typeArguments</c> (optional array). Resolve the actual type via
    /// <c>checker.getTypeAtLocation</c> if needed. Example: <c>Map&lt;string, number&gt;</c>.
    /// </remarks>
    | TypeReference of Ts.TypeReferenceNode
    /// <summary>
    /// A type parameter node appearing in a syntactic position (e.g. inside a mapped type or infer).
    /// </summary>
    /// <remarks>
    /// Same fields as <c>TypeDeclaration.TypeParameter</c>: <c>name</c>, <c>constraint</c> (optional),
    /// and <c>default</c> (optional). This case covers occurrences outside a declaration header.
    /// Example: <c>T extends string</c> inside <c>{ [K in keyof T]: ... }</c>.
    /// </remarks>
    | TypeParameterDeclaration of Ts.TypeParameterDeclaration
    /// <summary>
    /// An infer type used in the true branch of a conditional type: <c>infer R</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>typeParameter</c> for the inferred binding. The binding is scoped to the
    /// <c>trueType</c> branch of the enclosing <c>ConditionalType</c>. The <c>typeParameter</c> has no
    /// <c>constraint</c> in most cases. Example: <c>T extends Array&lt;infer R&gt; ? R : never</c>.
    /// </remarks>
    | InferType of Ts.InferTypeNode
    /// <summary>
    /// A type predicate used as a function return type: <c>param is Type</c> or <c>asserts param</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>parameterName</c> (may be an <c>Identifier</c> or a <c>ThisTypeNode</c> — always check
    /// the kind), <c>type</c> (optional — absent for <c>asserts</c> predicates without a type),
    /// and <c>assertsModifier</c> (optional). Example: <c>x is string</c> or <c>asserts value</c>.
    /// </remarks>
    | TypePredicate of Ts.TypePredicateNode
    /// <summary>
    /// A <c>typeof</c> query used as a type: <c>typeof expr</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>exprName</c> for the expression being queried. The expression is an entity name
    /// (identifier or qualified name), not an arbitrary expression.
    /// Example: <c>typeof window</c>.
    /// </remarks>
    | TypeQuery of Ts.TypeQueryNode
    /// <summary>
    /// A type operator applied to another type: <c>keyof T</c>, <c>unique symbol</c>, or <c>readonly T</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>operator</c> (a <c>SyntaxKind</c> integer: keyof=148, unique=152, readonly=146 in TS 5.x —
    /// do not rely on exact values; compare against <c>Ts.SyntaxKind</c> constants) and <c>type</c> for the
    /// operand type node. Example: <c>keyof Foo</c>.
    /// </remarks>
    | TypeOperator of Ts.TypeOperatorNode
    /// <summary>
    /// An indexed access type: <c>T[K]</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>objectType</c> for the base type and <c>indexType</c> for the index type.
    /// Example: <c>Foo["bar"]</c> or <c>T[number]</c>.
    /// </remarks>
    | IndexedAccessType of Ts.IndexedAccessTypeNode
    /// <summary>
    /// A mapped type: <c>{ [K in keyof T]?: T[K] }</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>typeParameter</c> for the iteration binding, <c>nameType</c> (optional — remaps keys
    /// via <c>as</c>), <c>questionToken</c> and <c>readonlyToken</c> for optional/readonly modifiers, and
    /// <c>type</c> (optional) for the value type. Mapped types live at the AST layer and are also
    /// exposed via <c>TypeFlagObject.Mapped</c> at the type-checker layer.
    /// Example: <c>{ [K in keyof T]: T[K] }</c>.
    /// </remarks>
    | MappedType of Ts.MappedTypeNode
    /// <summary>
    /// A conditional type: <c>T extends U ? TrueType : FalseType</c>.
    /// </summary>
    /// <remarks>
    /// All four branches (<c>checkType</c>, <c>extendsType</c>, <c>trueType</c>, <c>falseType</c>) are
    /// present in the AST and non-optional. The inferred branch at runtime can be queried via
    /// <c>checker.getResolvedSignature()</c> for call-site contexts.
    /// Example: <c>T extends string ? "yes" : "no"</c>.
    /// </remarks>
    | ConditionalType of Ts.ConditionalTypeNode
    /// <summary>
    /// A template literal type: <c>`prefix${T}suffix`</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>head</c> (the leading <c>TemplateHead</c> token with its text) and <c>templateSpans</c>
    /// (array of <c>TemplateLiteralTypeSpan</c>, each with a <c>type</c> and a trailing <c>literal</c>).
    /// Example: <c>`on${string}`</c>.
    /// </remarks>
    | TemplateLiteralType of Ts.TemplateLiteralTypeNode
    /// <summary>
    /// A single interpolated span inside a template literal type: <c>${T}suffix</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>type</c> for the interpolated type and <c>literal</c> for the trailing literal text
    /// (a <c>TemplateMiddle</c> or <c>TemplateTail</c> token). Example: the <c>${string}world</c> portion
    /// of <c>`hello${string}world`</c>.
    /// </remarks>
    | TemplateLiteralTypeSpan of Ts.TemplateLiteralTypeSpan
    /// <summary>
    /// A dynamic <c>import()</c> used as a type: <c>import("module").ExportName</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>argument</c> for the module specifier literal and <c>qualifier</c> (optional) for the
    /// dotted export path after the import. <c>typeArguments</c> is also optional.
    /// Example: <c>import("fs").PathLike</c>.
    /// </remarks>
    | ImportType of Ts.ImportTypeNode
    /// <summary>
    /// A function type node (not a declaration): <c>(params) =&gt; ReturnType</c>.
    /// </summary>
    /// <remarks>
    /// This is a <c>TypeNode</c>, not a declaration — there is no <c>name</c>. Extract <c>typeParameters</c>
    /// (optional), <c>parameters</c>, and <c>type</c> (return type, required here). Cannot go through
    /// <c>RefMember.fromDeclaration</c>; must be built inline.
    /// Example: <c>(x: string) =&gt; number</c>.
    /// </remarks>
    | FunctionType of Ts.FunctionTypeNode
    /// <summary>
    /// A constructor type node (not a declaration): <c>new(params) =&gt; InstanceType</c>.
    /// </summary>
    /// <remarks>
    /// This is a <c>TypeNode</c>, not a declaration — there is no <c>name</c>. Extract <c>typeParameters</c>
    /// (optional), <c>parameters</c>, and <c>type</c> (constructed instance type, required). Cannot go
    /// through <c>RefMember.fromDeclaration</c>; must be built inline.
    /// Example: <c>new(x: string) =&gt; Foo</c>.
    /// </remarks>
    | ConstructorType of Ts.ConstructorTypeNode
    /// <summary>
    /// An inline object type literal: <c>{ prop: Type; ... }</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>members</c> for the array of member declarations (property signatures, method signatures,
    /// index signatures, call signatures, etc.). Example: <c>{ x: number; y: number }</c>.
    /// </remarks>
    | TypeLiteral of Ts.TypeLiteralNode
    /// <summary>
    /// A literal type wrapping a concrete value token: <c>"hello"</c>, <c>42</c>, <c>true</c>, etc.
    /// </summary>
    /// <remarks>
    /// Extract <c>literal</c> for the child token node and route it through <c>LiteralTokenNodes</c>.
    /// The child may be a <c>StringLiteral</c>, <c>NumericLiteral</c>, <c>BigIntLiteral</c>,
    /// <c>TrueKeyword</c>, <c>FalseKeyword</c>, <c>NullKeyword</c>, <c>PrefixUnaryExpression</c> (negative
    /// numbers), or <c>NoSubstitutionTemplateLiteral</c>. Do not assume the child kind without checking.
    /// Example: <c>"hello"</c> or <c>-1</c>.
    /// </remarks>
    | LiteralType of Ts.LiteralTypeNode
    /// <summary>
    /// The <c>this</c> keyword used as a type: <c>this</c>.
    /// </summary>
    /// <remarks>
    /// No child fields to extract. Represents the polymorphic <c>this</c> type inside a class or interface.
    /// Example: <c>clone(): this</c>.
    /// </remarks>
    | ThisType of Ts.ThisTypeNode
    /// <summary>
    /// A <c>satisfies</c> expression used in a type position: <c>expr satisfies Type</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>expression</c> for the left-hand operand and <c>type</c> for the constraint type.
    /// This node appears in source files; it is rare in pure <c>.d.ts</c> files.
    /// Example: <c>x satisfies Record&lt;string, unknown&gt;</c>.
    /// </remarks>
    | SatisfiesExpression of Ts.SatisfiesExpression
    /// <summary>
    /// An <c>as</c> type assertion expression: <c>expr as Type</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>expression</c> for the left-hand operand and <c>type</c> for the asserted type.
    /// Rare in <c>.d.ts</c> files; more common in implementation files.
    /// Example: <c>value as string</c>.
    /// </remarks>
    | AsExpression of Ts.AsExpression
    /// <summary>
    /// An angle-bracket type assertion expression: <c>&lt;Type&gt;expr</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>expression</c> for the operand and <c>type</c> for the asserted type. This form is not
    /// valid in TSX files (use <c>as</c> instead). Rare in <c>.d.ts</c> files.
    /// Example: <c>&lt;string&gt;value</c>.
    /// </remarks>
    | TypeAssertion of Ts.TypeAssertion

[<RequireQualifiedAccess>]
type JSDocTags =
    /// <summary>
    /// A JSDoc <c>@param</c> tag describing a function parameter.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the parameter name (an <c>Identifier</c> or <c>QualifiedName</c>),
    /// <c>typeExpression</c> (optional) for the declared type, and <c>comment</c> (optional) for the
    /// description text. Example: <c>@param x - the input value</c>.
    /// </remarks>
    | ParameterTag of Ts.JSDocParameterTag
    /// <summary>
    /// A JSDoc <c>@returns</c> tag describing the return value of a function.
    /// </summary>
    /// <remarks>
    /// Extract <c>typeExpression</c> (optional) for the declared return type and <c>comment</c>
    /// (optional) for the description. Example: <c>@returns the computed result</c>.
    /// </remarks>
    | ReturnTag of Ts.JSDocReturnTag
    /// <summary>
    /// A JSDoc <c>@deprecated</c> tag marking the declaration as deprecated.
    /// </summary>
    /// <remarks>
    /// Extract <c>comment</c> (optional) for an optional deprecation message. The presence of this tag
    /// alone is sufficient to mark the declaration deprecated.
    /// Example: <c>@deprecated use newFoo instead</c>.
    /// </remarks>
    | DeprecatedTag of Ts.JSDocDeprecatedTag
    /// <summary>
    /// A JSDoc <c>@throws</c> tag describing an exception thrown by a function.
    /// </summary>
    /// <remarks>
    /// Extract <c>typeExpression</c> (optional) for the thrown type and <c>comment</c> (optional) for
    /// the description. Example: <c>@throws {RangeError} when the value is out of range</c>.
    /// </remarks>
    | ThrowsTag of Ts.JSDocThrowsTag
    /// <summary>
    /// A generic JSDoc tag not covered by the more specific cases above.
    /// </summary>
    /// <remarks>
    /// Extract <c>tagName</c> (an <c>Identifier</c>) for the tag keyword and <c>comment</c> (optional)
    /// for the tag body text. Use this for custom or unrecognised tags.
    /// Example: <c>@since 2.0.0</c>.
    /// </remarks>
    | DocTag of Ts.JSDocTag
    /// <summary>
    /// A JSDoc node whose specific tag kind is not yet handled by this extractor.
    /// </summary>
    /// <remarks>
    /// This is a catch-all for all JSDoc <c>SyntaxKind</c> values that the <c>Create</c> dispatch routes
    /// to <c>UnknownTag</c> (e.g. <c>@typedef</c>, <c>@template</c>, <c>@callback</c>, etc.). The raw
    /// <c>Ts.Node</c> is preserved so callers can inspect <c>kind</c> and decide whether to handle it.
    /// Do not rely on the shape of the contained node without checking its <c>kind</c>.
    /// </remarks>
    | UnknownTag of Ts.Node

[<RequireQualifiedAccess>]
type ModulesAndExports =
    /// <summary>
    /// A top-level import declaration: <c>import ... from "module"</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>importClause</c> (optional — absent for side-effect-only imports), <c>moduleSpecifier</c>
    /// for the module path literal, and <c>assertClause</c> (optional) for import assertions.
    /// Example: <c>import { Foo } from "./foo"</c>.
    /// </remarks>
    | ImportDeclaration of Ts.ImportDeclaration
    /// <summary>
    /// The clause binding portion of an import: the <c>DefaultName</c> or <c>* as Ns</c> or
    /// <c>{ ... }</c> part.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> (optional default import identifier) and <c>namedBindings</c> (optional —
    /// either a <c>NamespaceImport</c> or <c>NamedImports</c>).
    /// Example: <c>Foo, { Bar }</c> in <c>import Foo, { Bar } from "m"</c>.
    /// </remarks>
    | ImportClause of Ts.ImportClause
    /// <summary>
    /// A namespace import binding: <c>* as Ns</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the namespace alias identifier.
    /// Example: <c>* as ns</c> in <c>import * as ns from "m"</c>.
    /// </remarks>
    | NamespaceImport of Ts.NamespaceImport
    /// <summary>
    /// A named-imports binding list: <c>{ A, B as C }</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>elements</c> (array of <c>ImportSpecifier</c> nodes).
    /// Example: <c>{ Foo, Bar as Baz }</c>.
    /// </remarks>
    | NamedImports of Ts.NamedImports
    /// <summary>
    /// A single named import specifier: <c>A</c> or <c>A as B</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the local binding identifier and <c>propertyName</c> (optional) for the
    /// original export name when an alias is used.
    /// Example: <c>Bar as Baz</c>.
    /// </remarks>
    | ImportSpecifier of Ts.ImportSpecifier
    /// <summary>
    /// A CommonJS-style or namespace import alias: <c>import Foo = require("m")</c> or
    /// <c>import Foo = Namespace.Foo</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the alias identifier and <c>moduleReference</c> for the target (either an
    /// <c>ExternalModuleReference</c> for <c>require()</c> or an <c>EntityName</c> for namespace aliases).
    /// Example: <c>import fs = require("fs")</c>.
    /// </remarks>
    | ImportEqualsDeclaration of Ts.ImportEqualsDeclaration
    /// <summary>
    /// An import assertion clause: <c>assert { type: "json" }</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>elements</c> (array of <c>AssertEntry</c> nodes), each with a <c>name</c> and a
    /// <c>value</c>. This feature is stage-3 and may not appear in all declaration files.
    /// Example: <c>assert { type: "json" }</c>.
    /// </remarks>
    | AssertClause of Ts.AssertClause
    /// <summary>
    /// An <c>export = value</c> or <c>export default expr</c> assignment.
    /// </summary>
    /// <remarks>
    /// Extract <c>expression</c> for the exported value and check <c>isExportEquals</c> to distinguish
    /// <c>export = x</c> (CommonJS default) from <c>export default x</c>.
    /// Example: <c>export = MyClass</c>.
    /// </remarks>
    | ExportAssignment of Ts.ExportAssignment
    /// <summary>
    /// A named or namespace re-export declaration: <c>export { A } from "m"</c> or <c>export * from "m"</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>exportClause</c> (optional — absent for <c>export *</c>), <c>moduleSpecifier</c>
    /// (optional — absent for local re-exports), and <c>assertClause</c> (optional).
    /// Example: <c>export { Foo as Bar } from "./foo"</c>.
    /// </remarks>
    | ExportDeclaration of Ts.ExportDeclaration
    /// <summary>
    /// A single named export specifier: <c>A</c> or <c>A as B</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the exported name and <c>propertyName</c> (optional) for the original
    /// local name when an alias is used. Example: <c>Foo as Bar</c>.
    /// </remarks>
    | ExportSpecifier of Ts.ExportSpecifier
    /// <summary>
    /// A named-exports binding list: <c>{ A, B as C }</c> on the export side.
    /// </summary>
    /// <remarks>
    /// Extract <c>elements</c> (array of <c>ExportSpecifier</c> nodes).
    /// Example: <c>{ Foo, Bar as Baz }</c>.
    /// </remarks>
    | NamedExports of Ts.NamedExports
    /// <summary>
    /// An <c>export as namespace Foo</c> declaration that exposes the module as a UMD global namespace.
    /// </summary>
    /// <remarks>
    /// Extract <c>name</c> for the global namespace identifier. This declaration has no body; it is
    /// purely a marker. Example: <c>export as namespace myLib</c>.
    /// </remarks>
    | NamespaceExport of Ts.NamespaceExportDeclaration
[<RequireQualifiedAccess>]
type LiteralTokenNodes =
    /// <summary>
    /// A string literal token: <c>"hello"</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>text</c> for the raw string value (without surrounding quotes). The <c>text</c> field
    /// is always present. Example: <c>"hello"</c> in the literal type <c>"hello"</c>.
    /// </remarks>
    | StringLiteral of Ts.StringLiteral
    /// <summary>
    /// A numeric literal token: <c>42</c> or <c>3.14</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>text</c> for the raw numeric string as it appears in source. Parse with
    /// <c>float</c>/<c>int</c> as appropriate. For negative literals see <c>PrefixUnaryExpression</c>.
    /// Example: <c>42</c>.
    /// </remarks>
    | NumericLiteral of Ts.NumericLiteral
    /// <summary>
    /// A BigInt literal token: <c>9007199254740991n</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>text</c> for the raw value string (includes the trailing <c>n</c> suffix).
    /// Example: <c>100n</c>.
    /// </remarks>
    | BigIntLiteral of Ts.BigIntLiteral
    /// <summary>
    /// The <c>true</c> boolean literal token.
    /// </summary>
    /// <remarks>
    /// No extractable fields beyond the node itself. Appears as a child of <c>TypeNode.LiteralType</c>
    /// for the literal type <c>true</c>. Example: <c>true</c>.
    /// </remarks>
    | TrueLiteral of Ts.TrueLiteral
    /// <summary>
    /// The <c>false</c> boolean literal token.
    /// </summary>
    /// <remarks>
    /// No extractable fields beyond the node itself. Appears as a child of <c>TypeNode.LiteralType</c>
    /// for the literal type <c>false</c>. Example: <c>false</c>.
    /// </remarks>
    | FalseLiteral of Ts.FalseLiteral
    /// <summary>
    /// The <c>null</c> literal token appearing inside a <c>LiteralType</c>.
    /// </summary>
    /// <remarks>
    /// No extractable fields beyond the node itself. Distinct from <c>TypeNode.NullKeyword</c>, which
    /// represents <c>null</c> as a standalone keyword type node. Example: <c>null</c> inside a literal
    /// type context.
    /// </remarks>
    | NullLiteral of Ts.NullLiteral
    /// <summary>
    /// A prefix unary expression used to represent a negative numeric or BigInt literal: <c>-42</c>.
    /// </summary>
    /// <remarks>
    /// In <c>.d.ts</c> files only <c>MinusToken</c> is valid as the operator. Extract <c>operator</c>
    /// (will be <c>Ts.SyntaxKind.MinusToken</c>) and <c>operand</c> (a <c>NumericLiteral</c> or
    /// <c>BigIntLiteral</c>). Do not attempt to use this in expression contexts other than literal types.
    /// Example: <c>-1</c> or <c>-100n</c>.
    /// </remarks>
    | PrefixUnaryExpression of Ts.PrefixUnaryExpression
    /// <summary>
    /// A no-substitution template literal used as a literal type: <c>`hello`</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>text</c> for the raw string content (without backticks). This token appears when a
    /// template literal with no interpolations is used as a literal type.
    /// Example: <c>`hello`</c> in <c>type T = `hello`</c>.
    /// </remarks>
    | NoSubstitutionTemplateLiteral of Ts.NoSubstitutionTemplateLiteral
[<RequireQualifiedAccess>]
type TypeFlagLiteral =
    /// <summary>
    /// A string literal type at the checker layer, e.g. the type of <c>"hello"</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>value</c> for the string value. This is a <c>Ts.StringLiteralType</c> which extends
    /// <c>Ts.LiteralType</c>. Example: the type <c>"hello"</c> resolved from a string literal type node.
    /// </remarks>
    | String of Ts.StringLiteralType
    /// <summary>
    /// A numeric literal type at the checker layer, e.g. the type of <c>42</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>value</c> for the numeric value. This is a <c>Ts.NumberLiteralType</c> which extends
    /// <c>Ts.LiteralType</c>. Example: the type <c>42</c> resolved from a numeric literal type node.
    /// </remarks>
    | Number of Ts.NumberLiteralType
    /// <summary>
    /// A BigInt literal type at the checker layer, e.g. the type of <c>100n</c>.
    /// </summary>
    /// <remarks>
    /// Extract <c>value</c> for the BigInt value descriptor. This is a <c>Ts.BigIntLiteralType</c> which
    /// extends <c>Ts.LiteralType</c>. Example: the type <c>100n</c>.
    /// </remarks>
    | BigInt of Ts.BigIntLiteralType
    /// <summary>
    /// A boolean literal type at the checker layer: the type <c>true</c> or <c>false</c>.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.LiteralType</c> has <c>TypeFlags.BooleanLiteral</c>. To determine whether it
    /// is <c>true</c> or <c>false</c>, call <c>checker.typeToString</c> or inspect the type's
    /// <c>intrinsicName</c> property (internal, not in the public API). Example: the type <c>true</c>.
    /// </remarks>
    | Boolean of Ts.LiteralType
    /// <summary>
    /// An enum member literal type at the checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.LiteralType</c> has <c>TypeFlags.EnumLiteral</c> set in addition to
    /// <c>StringLiteral</c> or <c>NumberLiteral</c>. Use <c>checker.getSymbolAtLocation</c> on the
    /// originating node to retrieve the enum member symbol.
    /// Example: <c>Direction.Up</c> resolved to its literal type.
    /// </remarks>
    | EnumLiteral of Ts.LiteralType
    /// <summary>
    /// A <c>unique symbol</c> type at the checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.LiteralType</c> has <c>TypeFlags.UniqueESSymbol</c>. Access <c>symbol</c> on
    /// the type to retrieve the associated symbol declaration. This case is checked first in
    /// <c>TypeFlagLiteral.Create</c> because it is the most specific literal kind.
    /// Example: <c>declare const foo: unique symbol</c>.
    /// </remarks>
    | UniqueESSymbol of Ts.LiteralType
    
[<RequireQualifiedAccess>]
type TypeFlagObject =
    /// <summary>
    /// An object type originating from a class declaration, at the checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.InterfaceType</c> has <c>ObjectFlags.Class</c>. Use
    /// <c>checker.getPropertiesOfType</c> to enumerate members and <c>checker.getBaseTypes</c> for
    /// the class hierarchy. Example: the type of a <c>class Foo { }</c> declaration.
    /// </remarks>
    | Class of Ts.InterfaceType
    /// <summary>
    /// An object type originating from an interface declaration, at the checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.InterfaceType</c> has <c>ObjectFlags.Interface</c>. Use
    /// <c>checker.getPropertiesOfType</c> and <c>checker.getBaseTypes</c> to explore the shape.
    /// Example: the type of an <c>interface Foo { }</c> declaration.
    /// </remarks>
    | Interface of Ts.InterfaceType
    /// <summary>
    /// An anonymous object type, e.g. from an object-literal or an inline type literal.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.ObjectType</c> has <c>ObjectFlags.Anonymous</c>. Call
    /// <c>checker.getPropertiesOfType</c> and <c>checker.getSignaturesOfType</c> to enumerate members.
    /// Example: the type <c>{ x: number; y: number }</c>.
    /// </remarks>
    | Anonymous of Ts.ObjectType
    /// <summary>
    /// A mapped type at the checker layer: <c>{ [K in keyof T]: T[K] }</c>.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.ObjectType</c> has <c>ObjectFlags.Mapped</c>. This is the checker-layer
    /// counterpart of <c>TypeNode.MappedType</c>. Use <c>checker.getPropertiesOfType</c> on resolved
    /// instantiations. Inspect <c>mappedType.declaration</c> to navigate back to the AST node.
    /// Example: the type produced by <c>{ [K in keyof T]?: T[K] }</c>.
    /// </remarks>
    | Mapped of Ts.ObjectType
    /// <summary>
    /// An instantiated object type — a generic type applied to concrete type arguments.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.ObjectType</c> has <c>ObjectFlags.Instantiated</c>. Typically produced when
    /// a generic interface or class is specialised. The target type and type arguments can be accessed
    /// via the <c>Ts.TypeReference</c> cast on the object. Example: <c>Foo&lt;string&gt;</c> resolved.
    /// </remarks>
    | Instantiated of Ts.ObjectType
    /// <summary>
    /// A type reference object type — a generic type applied with explicit type arguments, at the
    /// checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.ObjectType</c> has <c>ObjectFlags.Reference</c>. Cast to <c>Ts.TypeReference</c>
    /// to access <c>target</c> (the generic type) and <c>typeArguments</c>. This is the resolved form of
    /// a <c>TypeNode.TypeReference</c>. Example: <c>Array&lt;string&gt;</c> or <c>Map&lt;K, V&gt;</c>.
    /// </remarks>
    | Reference of Ts.TypeReference
    /// <summary>
    /// A tuple type at the checker layer.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.TupleType</c> has <c>ObjectFlags.Tuple</c>. Access <c>typeArguments</c> (via
    /// the reference cast) for the element types and <c>elementFlags</c> for optional/rest information
    /// on each element. Example: the type <c>[string, number?]</c>.
    /// </remarks>
    | Tuple of Ts.TupleType
    /// <summary>
    /// An evolving array type used internally by the checker during control-flow analysis.
    /// </summary>
    /// <remarks>
    /// The contained <c>Ts.EvolvingArrayType</c> has <c>ObjectFlags.EvolvingArray</c>. This type is an
    /// internal TypeScript concept and rarely appears in declaration files. Treat it as an array of
    /// its <c>elementType</c>. Example: an array whose element type is inferred incrementally by push calls.
    /// </remarks>
    | EvolvingArray of Ts.EvolvingArrayType

[<RequireQualifiedAccess>]
type TypeFlagPrimary =
    /// <summary>
    /// A literal type at the checker layer — a concrete value type such as <c>"hello"</c>, <c>42</c>,
    /// <c>true</c>, an enum member, or a <c>unique symbol</c>.
    /// </summary>
    /// <remarks>
    /// The nested <c>TypeFlagLiteral</c> sub-DU further classifies the literal kind. Use
    /// <c>TypeFlagLiteral.Create</c> to construct or pattern-match on the inner case.
    /// Example: the type of the literal <c>"hello"</c> or an enum member <c>Color.Red</c>.
    /// </remarks>
    | Literal of TypeFlagLiteral
    /// <summary>
    /// The <c>any</c> type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract from the <c>Ts.Type</c> value. Checked first in priority order.
    /// Example: a parameter typed as <c>any</c>.
    /// </remarks>
    | Any of Ts.Type
    /// <summary>
    /// The <c>unknown</c> type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. The type-safe equivalent of <c>any</c>; requires narrowing before
    /// member access. Example: a parameter typed as <c>unknown</c>.
    /// </remarks>
    | Unknown of Ts.Type
    /// <summary>
    /// The <c>string</c> primitive type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Distinct from <c>Literal(TypeFlagLiteral.String ...)</c> which
    /// represents a specific string literal type. Example: the widened type of a <c>string</c> annotation.
    /// </remarks>
    | String of Ts.Type
    /// <summary>
    /// The <c>number</c> primitive type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Distinct from <c>Literal(TypeFlagLiteral.Number ...)</c> which
    /// represents a specific numeric literal type. Example: the widened type of a <c>number</c> annotation.
    /// </remarks>
    | Number of Ts.Type
    /// <summary>
    /// The <c>boolean</c> primitive type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. TypeScript normalises the union <c>true | false</c> into this single
    /// <c>boolean</c> type; individual literal boolean types are <c>Literal(TypeFlagLiteral.Boolean ...)</c>.
    /// Example: a parameter typed as <c>boolean</c>.
    /// </remarks>
    | Boolean of Ts.Type
    /// <summary>
    /// The <c>bigint</c> primitive type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Distinct from <c>Literal(TypeFlagLiteral.BigInt ...)</c> which is a
    /// specific BigInt literal type. Example: a parameter typed as <c>bigint</c>.
    /// </remarks>
    | BigInt of Ts.Type
    /// <summary>
    /// The general <c>symbol</c> primitive type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Distinct from <c>UniqueESSymbol</c> which is a specific
    /// <c>unique symbol</c> type tied to a particular declaration.
    /// Example: a parameter typed as <c>symbol</c>.
    /// </remarks>
    | ESSymbol of Ts.Type
    /// <summary>
    /// A <c>unique symbol</c> type tied to a specific symbol declaration, at the checker layer.
    /// </summary>
    /// <remarks>
    /// Access <c>symbol</c> on the <c>Ts.UniqueESSymbolType</c> for the associated symbol. Checked
    /// before <c>ESSymbol</c> in priority order. Example: <c>declare const foo: unique symbol</c>.
    /// </remarks>
    | UniqueESSymbol of Ts.UniqueESSymbolType
    /// <summary>
    /// The <c>void</c> type at the checker layer, indicating a function returns no usable value.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Distinct from <c>Undefined</c>; functions returning <c>void</c> may
    /// actually return <c>undefined</c> at runtime but the types are treated differently.
    /// Example: a function typed as returning <c>void</c>.
    /// </remarks>
    | Void of Ts.Type
    /// <summary>
    /// The <c>undefined</c> type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Represents the explicit <c>undefined</c> type, as opposed to the
    /// implicit absence of a value represented by optional members.
    /// Example: a union member typed as <c>undefined</c>.
    /// </remarks>
    | Undefined of Ts.Type
    /// <summary>
    /// The <c>null</c> type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Appears in union types when strict null checks are enabled.
    /// Example: a union member typed as <c>null</c>.
    /// </remarks>
    | Null of Ts.Type
    /// <summary>
    /// The <c>never</c> bottom type at the checker layer.
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract. Appears as the return type of functions that always throw or never
    /// return, and as the empty union type.
    /// Example: the return type of a function that only throws.
    /// </remarks>
    | Never of Ts.Type
    /// <summary>
    /// An object type at the checker layer, further classified by its <c>ObjectFlags</c>.
    /// </summary>
    /// <remarks>
    /// The nested <c>TypeFlagObject</c> sub-DU further classifies the object kind (class, interface,
    /// anonymous, mapped, reference, tuple, etc.). Type-parameter types are also routed here via a
    /// cast to <c>Ts.ObjectType</c> — only instantiated type-parameter types carry matching
    /// <c>ObjectFlags</c>; uninstantiated ones will raise in <c>TypeFlagObject.Create</c>.
    /// Example: the resolved type of <c>interface Foo { }</c> or <c>Map&lt;string, number&gt;</c>.
    /// </remarks>
    | Object of TypeFlagObject
    /// <summary>
    /// A union type at the checker layer: the resolved form of <c>A | B | ...</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>types</c> on the <c>Ts.UnionType</c> for the constituent types (already deduped and
    /// normalised by the checker). Note that <c>true | false</c> is normalised to <c>Boolean</c> by the
    /// checker. Example: the resolved type of <c>string | number</c>.
    /// </remarks>
    | Union of Ts.UnionType
    /// <summary>
    /// An intersection type at the checker layer: the resolved form of <c>A &amp; B &amp; ...</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>types</c> on the <c>Ts.IntersectionType</c> for the constituent types.
    /// Example: the resolved type of <c>Foo &amp; Bar</c>.
    /// </remarks>
    | Intersection of Ts.IntersectionType
    /// <summary>
    /// A <c>keyof T</c> index type at the checker layer.
    /// </summary>
    /// <remarks>
    /// Access <c>type</c> on the <c>Ts.IndexType</c> for the base type being indexed into.
    /// Example: the type produced by <c>keyof Foo</c>.
    /// </remarks>
    | Index of Ts.IndexType
    /// <summary>
    /// An indexed access type at the checker layer: the resolved form of <c>T[K]</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>objectType</c> and <c>indexType</c> on the <c>Ts.IndexedAccessType</c>.
    /// Example: the type produced by <c>Foo["bar"]</c>.
    /// </remarks>
    | IndexedAccess of Ts.IndexedAccessType
    /// <summary>
    /// A conditional type at the checker layer: the resolved or deferred form of
    /// <c>T extends U ? TrueType : FalseType</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>checkType</c>, <c>extendsType</c>, <c>trueType</c>, and <c>falseType</c> on the
    /// <c>Ts.ConditionalType</c>. The checker may have resolved the type to one of its branches;
    /// inspect <c>resolvedTrueType</c>/<c>resolvedFalseType</c> (internal fields) with caution.
    /// Example: the type produced by <c>T extends string ? "yes" : "no"</c>.
    /// </remarks>
    | Conditional of Ts.ConditionalType
    /// <summary>
    /// A substitution type used internally during conditional type resolution.
    /// </summary>
    /// <remarks>
    /// Access <c>baseType</c> and <c>constraint</c> on the <c>Ts.SubstitutionType</c>. This type is an
    /// internal TypeScript concept that rarely needs to be handled explicitly in declaration file
    /// processing; treat it as its <c>baseType</c> for most purposes.
    /// </remarks>
    | Substitution of Ts.SubstitutionType
    /// <summary>
    /// The <c>object</c> non-primitive type at the checker layer (the intrinsic <c>object</c> keyword).
    /// </summary>
    /// <remarks>
    /// No sub-structure to extract beyond the <c>Ts.Type</c> itself. Represents the non-primitive
    /// object type and also catches <c>TypeFlags.Unit</c> fallback cases in <c>TypeFlagPrimary.Create</c>.
    /// Example: a parameter typed as <c>object</c>.
    /// </remarks>
    | NonPrimitive of Ts.Type
    /// <summary>
    /// A template literal type at the checker layer: the resolved form of <c>`prefix${T}`</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>texts</c> (array of literal string segments) and <c>types</c> (array of interpolated
    /// types) on the <c>Ts.TemplateLiteralType</c>. The two arrays interleave: texts[i] precedes types[i].
    /// Example: the type produced by <c>`on${string}`</c>.
    /// </remarks>
    | TemplateLiteral of Ts.TemplateLiteralType
    /// <summary>
    /// A string-mapping type at the checker layer, produced by intrinsic string manipulation utilities
    /// such as <c>Uppercase&lt;T&gt;</c>.
    /// </summary>
    /// <remarks>
    /// Access <c>symbol</c> on the <c>Ts.StringMappingType</c> for the intrinsic symbol and <c>type</c>
    /// for the base type argument. Example: the type produced by <c>Uppercase&lt;string&gt;</c>.
    /// </remarks>
    | StringMapping of Ts.StringMappingType
    | TypeParameter of Ts.TypeParameter
[<RequireQualifiedAccess>]
type Modifiers =
    | Export of Ts.ExportKeyword
    | Declare of Ts.DeclareKeyword
    | Default of Ts.DefaultKeyword
    | Abstract of Ts.AbstractKeyword
    | ReadOnly of Ts.ReadonlyKeyword
    | Static of Ts.StaticKeyword
    | Override of Ts.OverrideKeyword
    | Accessor of Ts.AccessorKeyword
    | Public of Ts.PublicKeyword
    | Protected of Ts.ProtectedKeyword
    | Private of Ts.PrivateKeyword
[<RequireQualifiedAccess>]
type IgnoredNode = Modifier of Modifiers
type XanTagKind =
    /// <summary>
    /// The tag wraps a resolved TypeScript <c>Ts.Type</c> object, classified into a <c>TypeFlagPrimary</c>
    /// sub-DU by the checker-level type flags.
    /// </summary>
    /// <remarks>
    /// Use this case when processing a type that was obtained from the type checker (e.g. via
    /// <c>checker.getTypeAtLocation</c> or <c>checker.getTypeOfSymbol</c>). The nested
    /// <c>TypeFlagPrimary</c> further classifies the type. Access the raw <c>Ts.Type</c> via
    /// <c>.Value</c>. Example: the resolved type of any expression or declaration.
    /// </remarks>
    | Type of TypeFlagPrimary
    /// <summary>
    /// The tag wraps a <c>Ts.Node</c> that is a named declaration or structural element in the AST,
    /// classified into a <c>TypeDeclaration</c> sub-DU.
    /// </summary>
    /// <remarks>
    /// Covers interfaces, classes, type aliases, properties, methods, enums, modules, variables,
    /// function declarations, and all other declaration-level constructs. Access the raw <c>Ts.Node</c>
    /// via <c>.Value</c>. Example: an <c>InterfaceDeclaration</c>, <c>EnumDeclaration</c>, or
    /// <c>FunctionDeclaration</c> node.
    /// </remarks>
    | TypeDeclaration of TypeDeclaration
    | MemberDeclaration of MemberDeclaration
    /// <summary>
    /// The tag wraps a <c>Ts.Node</c> that appears syntactically in a type-annotation position,
    /// classified into a <c>TypeNode</c> sub-DU.
    /// </summary>
    /// <remarks>
    /// Covers keyword types, union/intersection types, references, tuple types, conditional types,
    /// mapped types, template literal types, function/constructor type nodes, and all other AST
    /// type-position constructs. Access the raw node via <c>.Value</c>. Example: the <c>string[]</c>
    /// in a parameter annotation or the <c>T extends U ? A : B</c> of a type alias.
    /// </remarks>
    | TypeNode of TypeNode
    /// <summary>
    /// The tag wraps a JSDoc annotation node, classified into a <c>JSDocTags</c> sub-DU.
    /// </summary>
    /// <remarks>
    /// Covers <c>@param</c>, <c>@returns</c>, <c>@deprecated</c>, <c>@throws</c>, generic doc tags, and
    /// unhandled JSDoc kinds routed to <c>UnknownTag</c>. Access the raw node via <c>.Value</c>.
    /// Example: a <c>@param x - the input</c> annotation on a function.
    /// </remarks>
    | JSDocTag of JSDocTags
    /// <summary>
    /// The tag wraps a literal token node (a concrete value token), classified into a
    /// <c>LiteralTokenNodes</c> sub-DU.
    /// </summary>
    /// <remarks>
    /// Covers string, numeric, BigInt, boolean, null, prefix-unary (negative), and no-substitution
    /// template literals. These appear as the <c>literal</c> child of a <c>LiteralType</c> node.
    /// Access the raw node via <c>.Value</c>. Example: the <c>"hello"</c> token inside a
    /// <c>LiteralType</c> node.
    /// </remarks>
    | LiteralTokenNode of LiteralTokenNodes
    
    | ModulesAndExports of ModulesAndExports
    
    | Ignore of IgnoredNode

type MemberDeclaration with
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    static member Create(decl: Ts.Node) = Internal.memberDeclarationKindSetMap[decl.kind] decl
    static member IsMemberDeclarationKind(decl: Ts.Node) = Internal.memberDeclarationKindSetMap.ContainsKey decl.kind

type TypeDeclaration with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    static member Create(decl: Ts.Node) = Internal.typeDeclarationKindSetMap[decl.kind] decl
    static member IsTypeDeclarationKind(decl: Ts.Node) = Internal.typeDeclarationKindSetMap.ContainsKey decl.kind
type TypeNode with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: U2<Ts.TypeNode, Ts.Expression> = emitJsExpr this "$0.fields[0]"
    static member IsTypeNodeKind(node: Ts.Node) = Internal.typeNodeKindSetMap.ContainsKey node.kind
    static member Create(node: Ts.Node) = Internal.typeNodeKindSetMap[node.kind] node
type JSDocTags with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: U2<Ts.JSDocTag, Ts.Node> = emitJsExpr this "$0.fields[0]"
    static member IsJSDocTagKind(tag: Ts.Node) = Internal.jsDocKindSetMap.ContainsKey(tag.kind)
    static member Create(tag: Ts.Node) = Internal.jsDocKindSetMap[tag.kind] tag
type ModulesAndExports with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    static member IsModulesAndExportsKind(node: Ts.Node) = Internal.moduleExportSetMap.ContainsKey(node.kind)
    static member Create(node: Ts.Node) = Internal.moduleExportSetMap[node.kind] node
type Modifiers with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.Modifier = emitJsExpr this "$0.fields[0]"
    static member IsModifierKind(node: Ts.Node) = Internal.modifierSetMap.ContainsKey(node.kind)
    static member Create(node: Ts.Node) = Internal.modifierSetMap[node.kind] node
type LiteralTokenNodes with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.Node = emitJsExpr this "$0.fields[0]"
    member inline this.IsBooleanLiteral =
        this.IsFalseLiteral || this.IsTrueLiteral
    static member IsLiteralTokenNodeKind(node: Ts.Node) = Internal.literalTokenNodeKindSet.ContainsKey(node.kind) 
    static member Create(node: Ts.Node) = Internal.literalTokenNodeKindSet[node.kind] node

type TypeFlagLiteral with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.LiteralType = emitJsExpr this "$0.fields[0]"
    static member inline IsTypeFlagLiteralKind(node: Ts.Type) =
        node.flags &&& Ts.TypeFlags.Literal <> enum 0
    static member Create(node: Ts.Type) =
        Internal.typeFlagLiteralKindSet
        |> Array.find (fst >> node.flags.HasFlag)
        |> snd
        |> funApply node
type TypeFlagObject with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping pattern matching and accessing the union field directly in JS.
    /// </summary>
    /// <remarks>
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value: Ts.ObjectType = emitJsExpr this "$0.fields[0]"
    static member inline IsTypeFlagObjectKind(node: Ts.Type) =
        node.flags &&& Ts.TypeFlags.Object <> enum 0
    static member Create(node: Ts.ObjectType) =
        Internal.typeFlagObjectKindSet
        |> Array.find (fst >> node.objectFlags.HasFlag)
        |> snd |> funApply node
type TypeFlagPrimary with
    /// <summary>
    /// Optimised for performance, emits immediate access to the underlying value
    /// by skipping unnecessary pattern matching and accessing the union field
    /// directly in JS when the field value is not a DU itself.
    /// </summary>
    /// <remarks>
    /// The <c>Literal</c> and <c>Object</c> cases delegate to their nested DU's
    /// own <c>.Value</c> for an upcast to <c>Ts.Type</c>. All other cases use
    /// <c>emitJsExpr "$0.fields[0]"</c> directly.
    /// Relies on Fable 5's internal DU representation: single-payload union cases
    /// store their field at <c>fields[0]</c> on the emitted JS object. Verified
    /// against Fable 5.0.0-beta.3. A layout change in a future Fable version would
    /// cause this to silently return <c>undefined</c> rather than throw.
    /// </remarks>
    member inline this.Value =
        match this with
        | Literal l -> l.Value :> Ts.Type
        | Object o -> o.Value :> Ts.Type
        | _ -> emitJsExpr this "$0.fields[0]"
    /// <summary>
    /// Creates a <see cref="T:Xantham.Fable.AutoOpenXTag.TypeFlagPrimary"/> from a <see cref="T:TypeScript.Ts.Type"/>.
    /// </summary>
    /// <remarks>
    /// <b>Flag matching:</b> uses <c>HasFlag</c> checks in priority order so that types carrying
    /// multiple flag bits (e.g. an enum-member string type has <c>StringLiteral ||| EnumLiteral</c>)
    /// are routed to the most specific case. Literal kinds are checked before their primitive
    /// counterparts; <c>UniqueESSymbol</c> before <c>ESSymbol</c>; <c>EnumLiteral</c> before
    /// <c>StringLiteral</c>/<c>NumberLiteral</c>.<br/>
    /// <b>TypeParameter routing:</b> the <c>TypeParameter</c> flag is routed to
    /// <c>TypeFlagObject.Create</c> via a cast to <c>Ts.ObjectType</c>. This is intentional for
    /// instantiated type-parameter types but will raise for uninstantiated ones whose
    /// <c>objectFlags</c> do not match any <c>TypeFlagObject</c> case.
    /// </remarks>
    static member Create(node: Ts.Type) =
        Internal.typeFlagPrimaryKindSet
        |> Array.find (fst >> node.flags.HasFlag)
        |> snd |> funApply node

type IgnoredNode with
    static member IsIgnoredNode (node: Ts.Node) =
        Modifiers.IsModifierKind node || ModulesAndExports.IsModulesAndExportsKind node
    static member Create(node: Ts.Node) =
        if Modifiers.IsModifierKind node then IgnoredNode.Modifier(Modifiers.Create node)
        else failwithf "Unknown IgnoredNode %A" node
    member this.Value =
        match this with
        | Modifier m -> m.Value :> Ts.Node

type XanTagKind with
    member this.Value: U2<Ts.Type, Ts.Node> =
        match this with
        | Type t -> t.Value |> (!^)
        | TypeDeclaration typeDeclaration -> unbox<Ts.Node> typeDeclaration.Value |> (!^)
        | TypeNode typeNode -> unbox<Ts.Node> typeNode.Value |> (!^)
        | JSDocTag jsDocTags -> unbox<Ts.Node> jsDocTags.Value |> (!^)
        | LiteralTokenNode literalTokenNodes -> literalTokenNodes.Value |> (!^)
        | ModulesAndExports modulesAndExports -> modulesAndExports.Value |> (!^)
        | Ignore node -> node.Value |> (!^)
        | MemberDeclaration memberDeclaration -> memberDeclaration.Value |> (!^)
    member inline this.IsParameterDeclaration =
        match this with
        | MemberDeclaration (MemberDeclaration.Parameter _) -> true
        | _ -> false
    member this.UnderlyingValue =
        match this with
        | Type t -> Choice1Of2 t.Value
        | _ -> Choice2Of2 (unbox<Ts.Node> this.Value)
    static member Create(node: Ts.Node) =
        // Use optimised checks to determine which pattern match is most appropriate
        // for the given node.
        if TypeDeclaration.IsTypeDeclarationKind node
        then TypeDeclaration.Create node
            |> TypeDeclaration
        elif MemberDeclaration.IsMemberDeclarationKind node
        then MemberDeclaration.Create node
            |> MemberDeclaration
        elif TypeNode.IsTypeNodeKind node
        then TypeNode.Create node
            |> TypeNode
        elif JSDocTags.IsJSDocTagKind node
        then JSDocTags.Create node
            |> JSDocTag
        elif LiteralTokenNodes.IsLiteralTokenNodeKind node
        then LiteralTokenNodes.Create node
            |> LiteralTokenNode
        elif ModulesAndExports.IsModulesAndExportsKind node
        then ModulesAndExports.Create node
            |> ModulesAndExports
        elif IgnoredNode.IsIgnoredNode node
        then IgnoredNode.Create node // Intentionally ignored — see CLAUDE.md "Removed XanTagKind cases"
            |> Ignore
        else
            Log.warn $"Unrecognized node kind: {node.kind.Name}"
            Ignore JS.undefined
    static member Create(node: Ts.Type) =
        TypeFlagPrimary.Create node
        |> Type
        
