module Xantham.Fable.Reading.TypeDeclaration

open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

let inline private setAstSignal (tag: XanthamTag) (astValue: STsAstNodeBuilder) =
    tag
    |> GuardedData.AstNodeBuilder.getOrSetDefault
    |> Signal.fill astValue

// ---------------------------------------------------------------------------
// Module-level helpers (shared across all declaration sub-readers)
// ---------------------------------------------------------------------------

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    match ctx.CreateXanthamTag node with
    | TagState.Unvisited tag, _ -> pushToStack ctx tag; ctx.typeSignal tag
    | TagState.Visited tag, _ -> ctx.typeSignal tag

/// Returns Void TypeSignal when the type annotation is absent.
let inline private getReturnTypeSignal (ctx: TypeScriptReader) (typeNode: Ts.TypeNode option) =
    typeNode
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Void.TypeKey)

/// Resolves an optional TypeNode to a TypeSignal, falling back to checker.getTypeAtLocation when None.
let private getSignalFromTypeNodeOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) =
    typ
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.orElseWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key |> Some
        | TagState.Unvisited typeTag, _ ->
            pushToStack ctx typeTag
            ctx.typeSignal typeTag |> Some
        | TagState.Visited typeTag, _ ->
            ctx.typeSignal typeTag |> Some
        )
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Any.TypeKey)

/// Returns TypeSignal voption for constraint/default slots on type parameters.
let inline getTypeSignalFromOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) =
    typ
    |> Option.map (ctx.CreateXanthamTag >> function
        | TagState.Unvisited tag, _ ->
            pushToStack ctx tag
            ctx.typeSignal tag
        | TagState.Visited tag, _ -> ctx.typeSignal tag
        )
    |> Option.toValueOption

let inline getFullyQualifiedName (ctx: TypeScriptReader) (tag: XanthamTag) =
    match tag.IdentityKey with
    | IdentityKey.AliasSymbol symbol | IdentityKey.Symbol symbol ->
        let name = ctx.checker.getFullyQualifiedName symbol
        if name.StartsWith("{") then [||]
        else name.Split('.')
    | _ -> [||]

/// Maps parameter declarations to reactive builder slots.
let private getParameterSlots (ctx: TypeScriptReader) (parameters: ResizeArray<Ts.ParameterDeclaration>) =
    parameters.AsArray
    |> Array.map (Member.resolveToParameterBuilder ctx)

/// Maps type parameter declarations to reactive builder slots.
let private getTypeParamSlots (ctx: TypeScriptReader) (typeParams: ResizeArray<Ts.TypeParameterDeclaration> option) =
    typeParams
    |> Option.map _.AsArray
    |> Option.defaultValue [||]
    |> Array.map (
        ctx.CreateXanthamTag
        >> fst >> function
        | TagState.Unvisited tag ->
            pushToStack ctx tag
            GuardedData.TypeSignal.getOrSetDefault tag,
            GuardedData.AstNodeBuilder.getOrSetDefault tag
        | TagState.Visited tag ->
            GuardedData.TypeSignal.getOrSetDefault tag,
            GuardedData.AstNodeBuilder.getOrSetDefault tag
        >> fun signals -> signals ||> Signal.map2 (fun typeKey -> function
            | ValueSome (STsAstNodeBuilder.TypeParameter tp) ->
                ValueSome {
                    Type = typeKey
                    TypeParameter = tp
                }
            | _ -> ValueNone
            )
        )

// ---------------------------------------------------------------------------
// Per-declaration sub-readers
// ---------------------------------------------------------------------------

module TypeParameter =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.TypeParameterDeclaration) =
        {
            STypeParameterBuilder.Name = NameHelpers.getName node.name
            Constraint = getTypeSignalFromOption ctx node.``constraint``
            Default = getTypeSignalFromOption ctx node.``default``
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.TypeParameter
        |> setAstSignal xanTag
        // TypeStore.Key is a generated unique key (see Prelude.usesGeneratedKey). Use that key
        // for the TypeSignal — getTypeAtLocation on a TypeParameterDeclaration returns the same
        // type object as TypeFlagPrimary.TypeParameter (type-level), which would cause the
        // node-level and type-level entries to collide when their constraint representations differ.
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag
        // Ensure the type-level TypeParameter (TypeFlagPrimary.TypeParameter) is also dispatched.
        // Its natural TypeKey (e.g. 832) is what other types reference in their constraint/type
        // signals via getTypeSignal/routeViaChecker. If this declaration is never encountered via
        // those paths, the natural TypeKey is absent from the output and the generator fails.
        let paramType = ctx.checker.getTypeAtLocation node
        match ctx.CreateXanthamTag paramType |> fst with
        | TagState.Unvisited typeTag -> pushToStack ctx typeTag
        | TagState.Visited _ -> ()

module Interface =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.InterfaceDeclaration) (source: ModuleName) =
        {
            Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Enumerable = false
            SInterfaceBuilder.Name = NameHelpers.getName node.name
            Members =
                node.members.AsArray
                |> Array.map (Member.resolveToMemberBuilder ctx)
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            Heritage =
                let clauses =
                    node.heritageClauses
                    |> Option.map (_.AsArray >> Array.map (ctx.CreateXanthamTag >> fst >> function
                        | TagState.Unvisited tag ->
                            pushToStack ctx tag
                            ctx.typeSignal tag, GuardedData.AstNodeBuilder.getOrSetDefault tag
                        | TagState.Visited tag -> ctx.typeSignal tag, GuardedData.AstNodeBuilder.getOrSetDefault tag
                        ))
                    |> Option.defaultValue [||]
                clauses
                |> Array.collect (fun (typeSignal, builderSignal) -> [| typeSignal.Invalidated; builderSignal.Invalidated |])
                |> Array.toList
                |> Signal.computed (fun () ->
                    ValueSome {
                        SInterfaceHeritageBuilder.Extends =
                            clauses
                            |> Array.map (fun (typeSignal, builderSignal) ->
                                match builderSignal.Value with
                                | ValueSome (STsAstNodeBuilder.TypeReference typeReferenceBuilder) ->
                                    typeReferenceBuilder
                                | _ ->
                                    {
                                        STypeReferenceBuilder.Type = typeSignal
                                        TypeArguments = [||]
                                        ResolvedType = ValueNone
                                    }
                                |> ValueSome
                                |> Signal.source
                                )
                    }
                    )
        }
        |> STsAstNodeBuilder.Interface
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

module TypeAlias =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.TypeAliasDeclaration) (source: ModuleName) =
        {
            SAliasBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Type = getTypeSignalFromNode ctx node.``type``
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.Alias
        |> setAstSignal xanTag
        // TypeStore.Key is a generated TypeKey (see Prelude.usesGeneratedKey). Use that same key
        // for the TypeSignal so TypeStore.Key == TypeSignal.Value and downstream refs are consistent.
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag


module Enum =
    let private resolveEnumMemberSlot (ctx: TypeScriptReader) (node: Ts.EnumMember) =
        let state = ctx.CreateXanthamTag (unbox<Ts.Node> node) |> fst
        let tag = state.Value
        if state.IsUnvisited then pushToStack ctx tag
        tag
        |> GuardedData.AstNodeBuilder.getOrSetDefault
        |> Signal.map (function
            | ValueSome (STsAstNodeBuilder.EnumCase c) -> ValueSome c
            | _ -> ValueNone
            )

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.EnumDeclaration) (source: ModuleName) =
        {
            SEnumTypeBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Members = node.members.AsArray |> Array.map (resolveEnumMemberSlot ctx)
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.Enum
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

    let readMember (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.EnumMember) =
        let value =
            ctx.checker.getConstantValue(!^node)
            |> Option.map (function
                | U2.Case1 s -> TsLiteral.String s
                | U2.Case2 v when JS.Constructors.Number.isSafeInteger v -> TsLiteral.Int (int v)
                | U2.Case2 v -> TsLiteral.Float v
                )
            |> Option.defaultValue (TsLiteral.String (NameHelpers.getName node.name))
        {
            SEnumCaseBuilder.Source = trySetSourceForTag xanTag (ctx.moduleMap.Item(node.parent.getSourceFile()))
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Value = value
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.EnumCase
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

module Variable =
    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.VariableDeclaration) (source: ModuleName) =
        {
            SVariableBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Type =
                getSignalFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node)
                |> ctx.routeTypeTo xanTag
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.Variable
        |> setAstSignal xanTag
        // TypeStore.Key is a generated TypeKey (see Prelude.usesGeneratedKey).
        ctx.signalCache[xanTag.IdentityKey].Key
        |> setTypeKeyForTag xanTag

module FunctionDecl =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.FunctionDeclaration) (source: ModuleName) =
        {
            SFunctionBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            IsDeclared = node.body.IsNone
            Type = getReturnTypeSignal ctx node.``type``
            Parameters = getParameterSlots ctx node.parameters
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> STsAstNodeBuilder.FunctionDeclaration
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag
module Class =
    let private exprWithTypeArgsToRefSlot (ctx: TypeScriptReader) (expr: Ts.ExpressionWithTypeArguments) =
        let typeSignal =
            let typ = ctx.checker.getTypeAtLocation expr
            match ctx.CreateXanthamTag typ |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; ctx.typeSignal tag
            | TagState.Visited tag -> ctx.typeSignal tag
        // Type arguments from the expression (syntactic) since checker.getTypeArguments needs a TypeReference
        let typeArgs =
            expr.typeArguments
            |> Option.map (_.AsArray >> Array.map (getTypeSignalFromNode ctx))
            |> Option.defaultValue [||]
        {
            STypeReferenceBuilder.Type = typeSignal
            TypeArguments = typeArgs
            ResolvedType = ValueNone
        }
        |> ValueSome
        |> Signal.source

    let private resolveHeritage (ctx: TypeScriptReader) (clauses: ResizeArray<Ts.HeritageClause> option) =
        let clauseArr = clauses |> Option.map _.AsArray |> Option.defaultValue [||]
        let extendsRefs =
            clauseArr
            |> Array.tryFind (_.token >> (=) Ts.SyntaxKind.ExtendsKeyword)
            |> Option.map (_.types.AsArray >> Array.map (exprWithTypeArgsToRefSlot ctx))
            |> Option.defaultValue [||]
        // TsClassHeritage.Implements only holds one type; take the first if multiple.
        let implementsSlot =
            clauseArr
            |> Array.tryFind (_.token >> (=) Ts.SyntaxKind.ImplementsKeyword)
            |> Option.bind (_.types.AsArray >> Array.tryHead)
            |> Option.map (exprWithTypeArgsToRefSlot ctx)
            |> ValueOption.ofOption
        Signal.source (ValueSome {
            SClassHeritageBuilder.Extends = extendsRefs
            Implements = implementsSlot
        })

    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ClassDeclaration) (source: ModuleName) =
        let ctors, members =
            node.members.AsArray
            |> Array.partition (fun elem -> (unbox<Ts.Node> elem).kind = Ts.SyntaxKind.Constructor)
        {
            SClassBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Enumerable = false
            Name = NameHelpers.getName node.name
            Constructors = ctors |> Array.map (unbox<Ts.ConstructorDeclaration> >> Member.resolveToConstructorBuilder ctx)
            Members = members |> Array.map (Member.resolveClassMemberBuilder ctx)
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Heritage = resolveHeritage ctx node.heritageClauses
        }
        |> STsAstNodeBuilder.Class
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag
        

module Module =
    let private collectModuleTypes (ctx: TypeScriptReader) (body: Ts.ModuleBody option) =
        match body with
        | None -> [||]
        | Some body ->
            match unbox body with
            | Patterns.Node.ModuleBlock block ->
                block.statements.AsArray
                |> Array.map (fun stmt ->
                    let state = ctx.CreateXanthamTag (unbox<Ts.Node> stmt) |> fst
                    let tag = state.Value
                    if state.IsUnvisited then pushToStack ctx tag
                    ctx.typeSignal tag
                    )
            | Patterns.Node.ModuleDeclaration nested ->
                let state = ctx.CreateXanthamTag (unbox<Ts.Node> nested) |> fst
                let tag = state.Value
                if state.IsUnvisited then pushToStack ctx tag
                [| ctx.typeSignal tag |]
            | _ -> [||]

    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ModuleDeclaration) (source: ModuleName) =
        {
            SModuleBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            IsNamespace = node.flags.HasFlag(Ts.NodeFlags.Namespace)
            IsRecursive = false
            Types = collectModuleTypes ctx !!node.body
        }
        |> STsAstNodeBuilder.Module
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: TypeDeclaration) =
    let source = ctx.moduleMap[node]
    match node with
    | TypeDeclaration.TypeParameter typeParameterDeclaration ->
        TypeParameter.read ctx xanTag typeParameterDeclaration
    | TypeDeclaration.Interface interfaceDeclaration ->
        Interface.read ctx xanTag interfaceDeclaration source
    | TypeDeclaration.TypeAlias typeAliasDeclaration ->
        TypeAlias.read ctx xanTag typeAliasDeclaration source
    | TypeDeclaration.Class classDeclaration ->
        Class.read ctx xanTag classDeclaration source
    | TypeDeclaration.HeritageClause heritageClause ->
        // Wire this tag to the first type in the clause so Interface.read's Heritage
        // computed signal can read a TypeReference builder from it.
        match heritageClause.types.AsArray |> Array.tryHead with
        | None -> ()
        | Some exprWithTypeArgs ->
            let resolvedType = ctx.checker.getTypeAtLocation exprWithTypeArgs
            let innerTag =
                match ctx.CreateXanthamTag resolvedType with
                | TagState.Unvisited tag, _ ->
                    pushToStack ctx tag; tag
                | TagState.Visited tag, _ -> tag
            ctx.typeSignal xanTag
            |> Signal.fulfillWith (fun () -> (ctx.typeSignal innerTag).Value)
            GuardedData.AstNodeBuilder.getOrSetDefault xanTag
            |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault innerTag).Value)
    | TypeDeclaration.ExpressionWithTypeArguments exprWithTypeArgs ->
        // Route via checker; consumers look up TypeSignal/Builder on this tag.
        let resolvedType = ctx.checker.getTypeAtLocation exprWithTypeArgs
        let innerTag =
            match ctx.CreateXanthamTag resolvedType |> fst with
            | TagState.Unvisited tag -> pushToStack ctx tag; tag
            | TagState.Visited tag -> tag
        ctx.typeSignal xanTag
        |> Signal.fulfillWith (fun () -> (ctx.typeSignal innerTag).Value)
        GuardedData.AstNodeBuilder.getOrSetDefault xanTag
        |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault innerTag).Value)
    | TypeDeclaration.Enum enumDeclaration ->
        Enum.readDeclaration ctx xanTag enumDeclaration source
    | TypeDeclaration.EnumMember enumMember ->
        Enum.readMember ctx xanTag enumMember
    | TypeDeclaration.VariableStatement variableStatement ->
        // Push each inner VariableDeclaration and wire this tag's signals to the first one.
        let tags =
            variableStatement.declarationList.declarations.AsArray
            |> Array.map (fun decl ->
                let state = ctx.CreateXanthamTag (unbox<Ts.Node> decl) |> fst
                if state.IsUnvisited then pushToStack ctx state.Value
                state.Value
                )
        match tags with
        | [||] -> ()
        | tags ->
            let firstTag = tags[0]
            GuardedData.AstNodeBuilder.getOrSetDefault xanTag
            |> Signal.fulfillWith (fun () -> (GuardedData.AstNodeBuilder.getOrSetDefault firstTag).Value)
            ctx.typeSignal xanTag
            |> Signal.fulfillWith (fun () -> (ctx.typeSignal firstTag).Value)
    | TypeDeclaration.VariableDeclaration variableDeclaration ->
        Variable.readDeclaration ctx xanTag variableDeclaration source
    | TypeDeclaration.FunctionDeclaration functionDeclaration ->
        FunctionDecl.read ctx xanTag functionDeclaration source
    | TypeDeclaration.Module moduleDeclaration ->
        Module.read ctx xanTag moduleDeclaration source
    | TypeDeclaration.Namespace namespaceDeclaration ->
        Module.read ctx xanTag namespaceDeclaration source
    | TypeDeclaration.ModuleBlock _ ->
        () // Processed inline during Module/Namespace dispatch; no standalone signal needed
