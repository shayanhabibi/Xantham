module Xantham.Fable.Types.Reaver

open Fable.Core
open Fable.Core.JsInterop
open TypeScript
open Xantham.Fable
open Xantham.TypeScript

type KeyOfTypeNode =
    | Bounded of string list
    | Unbounded of string list
    | Generic
    | ConstrainedGeneric
    // static member Create (typeChecker: Ts.TypeChecker) (node: Ts.TypeNode) =
    //     let targetType = typeChecker.getTypeFromTypeNode node
    //     let props =
    //         typeChecker.getPropertiesOfType targetType
    //         |> _.AsArray
    //         |> Array.map _.symbolName
    //         |> Array.choose (function SymbolName.String value -> Some value | _ -> None)
    //     let isIndexer = typeChecker.getIndexInfosOfType targetType |> _.AsArray |> Array.isEmpty |> not

type TypeOperatorNode =
    | KeyOf of Ts.TypeNode
    | Readonly of Ts.TypeNode
    | Unique of Ts.TypeNode
    member this.TypeChecker = SymbolTypeKey.unsafeAccess SymbolTypeKeys.typeCheckerSigil this
    member inline this.Value = emitJsExpr this "$0.fields[0]"
    static member Create typeChecker (typeOperatorNode: Ts.TypeOperatorNode) =
        SymbolTypeKey.accessOrInit SymbolTypeKeys.typeCheckerSigil (fun () -> typeChecker) typeOperatorNode
        |> ignore
        match typeOperatorNode.operator with
        | Ts.SyntaxKind.KeyOfKeyword -> KeyOf typeOperatorNode.``type``
        | Ts.SyntaxKind.ReadonlyKeyword -> Readonly typeOperatorNode.``type``
        | Ts.SyntaxKind.UniqueKeyword -> Unique typeOperatorNode.``type``
        | _ -> failwithf "Unknown TypeOperatorNode operator %A" typeOperatorNode.operator
        

type TypeReferenceNode =
    {
        Node: Ts.TypeReferenceNode
        TargetSymbol: Ts.Symbol
    }
    static member Create (typeChecker: Ts.TypeChecker) (typeReferenceNode: Ts.TypeReferenceNode) =
        {
            Node = typeReferenceNode
            TargetSymbol =
                typeChecker.getSymbolAtLocation !!typeReferenceNode.typeName
                |> Option.defaultWith(fun () -> failwith "Could not find symbol for type reference node")
        }
        

[<Interface>]
type IMethod = interface end
type IMethod with
    [<Emit "$0">]
    member inline this.Value<'T, 'U when 'T:(member questionToken: Ts.QuestionToken option) and 'U:(member questionToken: Ts.QuestionToken option)>(): 'T = unbox this
    member this.TypeChecker = SymbolTypeKey.unsafeAccess SymbolTypeKeys.typeCheckerSigil this
    member inline this.IsOptional = this.Value<Ts.MethodSignature, Ts.MethodDeclaration>().questionToken.IsSome
    member this.Type =
        this
        |> SymbolTypeKey.accessOrInit Symbols.methodSignatureTypeSigil (fun () ->
            let typ = this.TypeChecker.getTypeAtLocation (this.Value<Ts.MethodSignature, Ts.MethodDeclaration>())
            if this.IsOptional then
                typ :?> Ts.UnionType
                |> _.types.AsArray
                |> Array.find (_.flags.HasFlag(Ts.TypeFlags.Undefined) >> not)
            else typ 
            :?> Ts.ObjectType)
    static member Create<'T, 'U>(checker: Ts.TypeChecker) (node: 'T): 'U =
        node |> SymbolTypeKey.set SymbolTypeKeys.typeCheckerSigil checker
        unbox<'U> node
type MethodSignature = inherit IMethod
type MethodSignature with
    static member Create = IMethod.Create<Ts.MethodSignature, MethodSignature>
    [<Emit "$0">] member inline this.Value = unbox<Ts.MethodSignature> this
type MethodDeclaration = inherit IMethod
type MethodDeclaration with
    static member Create = IMethod.Create<Ts.MethodDeclaration, MethodDeclaration>
    member inline this.Value = (this :> IMethod).Value<Ts.MethodDeclaration, Ts.MethodSignature>()


type EnumMember = {
    SymbolKey: int
    NodeKey: int
    AliasKeys: int array
    TypeKey: int
    Symbol: Ts.Symbol
    Node: Ts.EnumMember
    Aliases: Ts.EnumMember array
    Type: Ts.LiteralType
    Value: Choice<string, int, float>
} with
    static member inline private getValue(typ: Ts.Type) =
        if typ.flags.HasFlag Ts.TypeFlags.NumberLiteral then
            let typ = typ :?> Ts.NumberLiteralType
            if JS.Constructors.Number.isSafeInteger typ.value then
                Choice2Of3 (int typ.value)
            else
                Choice3Of3 typ.value
        else
            let typ = typ :?> Ts.StringLiteralType
            Choice1Of3 typ.value
    static member Create(typ: Ts.Type) =
        let value = EnumMember.getValue typ
        let symbol = typ.unsafeGetCanonicalSymbol()
        let canonicalDeclaration = symbol.valueDeclaration.Value :?> Ts.EnumMember
        let decls =
            symbol.declarations.Value.AsArray
            |> Array.filter (ts.getNodeId >> (<>) (ts.getNodeId canonicalDeclaration))
        {
            Symbol = symbol
            Node = canonicalDeclaration
            Type = typ :?> Ts.LiteralType
            Aliases = unbox<Ts.EnumMember array> decls
            Value = value
            SymbolKey = ts.getSymbolId symbol
            NodeKey = ts.getNodeId canonicalDeclaration
            TypeKey = typ.id
            AliasKeys = decls |> Array.map ts.getNodeId
        }
    static member TryCreate(typ: Ts.Type) =
        match typ.flags with
        | flags when
            flags.HasFlag Ts.TypeFlags.EnumLiteral
            && not(flags.HasFlag Ts.TypeFlags.Union || flags.HasFlag Ts.TypeFlags.Enum) ->
            EnumMember.Create typ
            |> Some
        | _ -> None
    static member Create(node: Ts.EnumMember, checker: Ts.TypeChecker) =
        let typ = checker.getTypeAtLocation node
        let value = EnumMember.getValue typ
        let symbol = typ.unsafeGetCanonicalSymbol()
        let canonicalDeclaration = symbol.valueDeclaration.Value
        let decls =
            symbol.declarations.Value.AsArray
            |> Array.filter (ts.getNodeId >> (<>) (ts.getNodeId canonicalDeclaration))
        {
            Symbol = symbol
            Node = canonicalDeclaration :?> Ts.EnumMember
            Type = typ :?> Ts.LiteralType
            Aliases = unbox<Ts.EnumMember array> decls
            Value = value
            SymbolKey = ts.getSymbolId symbol
            NodeKey = ts.getNodeId canonicalDeclaration
            TypeKey = typ.id
            AliasKeys = decls |> Array.map ts.getNodeId
        }
    static member TryCreate(node: Ts.Node, checker: Ts.TypeChecker) =
        match node with
        | Patterns.Node.EnumMember node -> EnumMember.Create(node,checker) |> Some
        | _ -> None
    static member Create(symbol: Ts.Symbol, checker: Ts.TypeChecker) =
        let canonicalDeclaration = symbol.valueDeclaration.Value
        let decls =
            symbol.declarations.Value.AsArray
            |> Array.filter (ts.getNodeId >> (<>) (ts.getNodeId canonicalDeclaration))
        let typ = checker.getTypeOfSymbolAtLocation(symbol, canonicalDeclaration)
        let value = EnumMember.getValue typ
        {
            Symbol = symbol
            Node = canonicalDeclaration :?> Ts.EnumMember
            Type = typ :?> Ts.LiteralType
            Aliases = unbox<Ts.EnumMember array> decls
            Value = value
            SymbolKey = ts.getSymbolId symbol
            NodeKey = ts.getNodeId canonicalDeclaration
            TypeKey = typ.id
            AliasKeys = decls |> Array.map ts.getNodeId
        }
    static member TryCreate(symbol: Ts.Symbol, checker: Ts.TypeChecker) =
        let canonicalSymbol =
            if symbol.flags.HasFlag Ts.SymbolFlags.Alias then
                checker.getAliasedSymbol symbol
            else checker.getMergedSymbol symbol
        if canonicalSymbol.flags.HasFlag(Ts.SymbolFlags.EnumMember) then
            EnumMember.Create(canonicalSymbol, checker) |> Some
        else None
        
type EnumDeclaration = {
    SymbolKey: int
    NodeKey: int
    TypeKey: int
    Name: string
    Symbol: Ts.Symbol
    Node: Ts.EnumDeclaration
    Type: Ts.EnumType
    Members: EnumMember array
} with
    static member inline private getMembers (checker: Ts.TypeChecker) (enumDecl: Ts.EnumDeclaration) =
        enumDecl.members.AsArray
        |> Array.map (fun node -> EnumMember.Create(node, checker))
    static member inline private getCanonicalSymbol (checker: Ts.TypeChecker) (symbol: Ts.Symbol) =
        if symbol.flags.HasFlag(Ts.SymbolFlags.Alias) then
            checker.getAliasedSymbol symbol
        else checker.getMergedSymbol symbol
    static member Create(symbol: Ts.Symbol, checker: Ts.TypeChecker) =
        let symbol = EnumDeclaration.getCanonicalSymbol checker symbol
        let node = symbol.valueDeclaration.Value :?> Ts.EnumDeclaration
        let typ = checker.getTypeAtLocation node
        {
            SymbolKey = ts.getSymbolId symbol
            NodeKey = ts.getNodeId node
            TypeKey = typ.id
            Name = symbol.name
            Symbol = symbol
            Node = node
            Type = typ :?> Ts.EnumType
            Members = EnumDeclaration.getMembers checker node
        }
    static member Create(node: Ts.EnumDeclaration, checker: Ts.TypeChecker) =
        node.name
        |> checker.getSymbolAtLocation
        |> Option.get
        |> fun symbol -> EnumDeclaration.Create(symbol, checker)
    static member Create(typ: Ts.EnumType) = EnumDeclaration.Create(typ.symbol, typ.checker)
    static member TryCreate(node: Ts.Node, checker: Ts.TypeChecker) =
        match node with
        | Patterns.Node.EnumDeclaration enumDecl ->
            EnumDeclaration.Create(enumDecl, checker)
            |> Some
        | _ -> None
    static member TryCreate(typ: Ts.Type) =
        if typ.flags.HasFlag(Ts.TypeFlags.Enum) then
            EnumDeclaration.Create(typ.symbol, typ.checker)
            |> Some
        else None

and [<RequireQualifiedAccess>] PrimitiveSingleton =
    | Any
    | Unknown
    | String
    | Number
    | Boolean
    | BigInt
    | ESSymbol
    | Void
    | Undefined
    | Null
    | Never
    | NonPrimitive
and BooleanLiteral = bool
and [<RequireQualifiedAccess>] NumberLiteral =
    | Float of float
    | Int of int
and [<RequireQualifiedAccess>] EnumMemberType =
    | String of Ts.StringLiteral
    | BigInt of Ts.BigIntLiteral
    | Boolean of BooleanLiteral
    | Number of NumberLiteral
and [<RequireQualifiedAccess>] EnumType = EnumType of Ts.EnumDeclaration
and [<RequireQualifiedAccess>] LiteralType =
    | Number of NumberLiteral
    | BigInt of Ts.BigIntLiteralType
    | Boolean of BooleanLiteral
    | String of Ts.StringLiteral
    | UniqueESSymbol of Ts.UniqueESSymbolType
    | Enum of EnumType
    | EnumMember of EnumMemberType
    static member inline TryCreate(typ: Ts.Type) =
        match typ.flags with
        | flags when flags.HasFlag Ts.TypeFlags.UniqueESSymbol ->
            typ :?> Ts.UniqueESSymbolType
            |> LiteralType.UniqueESSymbol
        | flags when flags.HasFlag Ts.TypeFlags.EnumLiteral && (flags.HasFlag Ts.TypeFlags.Enum || flags.HasFlag Ts.TypeFlags.Union) ->
            // resolves to a enumdeclaration
            typ.symbol.valueDeclaration.Value :?> Ts.EnumDeclaration
            |> EnumType.EnumType
            |> LiteralType.Enum
        | flags when flags.HasFlag Ts.TypeFlags.EnumLiteral && not(flags.HasFlag Ts.TypeFlags.Enum || flags.HasFlag Ts.TypeFlags.Union) ->
            typ.symbol.valueDeclaration.Value :?> Ts.EnumMember
            |> failwith ""
        | flags when flags.HasFlag Ts.TypeFlags.Enum ->
            let value =
                typ :?> Ts.NumberLiteralType
                |> _.value
            match JS.Constructors.Number.isSafeInteger value with
            | true -> int value |> NumberLiteral.Int |> LiteralType.Number
            | false -> value |> NumberLiteral.Float |> LiteralType.Number
        // | flags when flags.HasFlag Ts.TypeFlags.EnumLiteral 
and [<RequireQualifiedAccess>] InstantiablePrimitiveType =
    | Index
    | StringMapping
    | TemplateLiteral
and [<RequireQualifiedAccess>] StructuralType =
    | Object
    | Union
    | Intersection
and [<RequireQualifiedAccess>] TypeVariable =
    | TypeParameter
    | IndexedAccess
and [<RequireQualifiedAccess>] InstantiableNonPrimitive =
    | TypeVariable of TypeVariable
    | Conditional
    | Substitution

and [<RequireQualifiedAccess>] ClassType =
    // reference type flag
    | Generic
    // We'll discount 'thisType' as being reference
    | Concrete
and [<RequireQualifiedAccess>] InterfaceType =
    // reference type flag
    | Generic
    // We'll discount 'thisType' as being reference
    | Concrete
