module Xantham.Fable.Reading.TypeDeclaration

open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.Signal
open Xantham.Fable.Types.Tracer

let inline private setAstSignal (tag: XanthamTag) (astValue: SType) = tag.Builder |> Signal.fill astValue

// ---------------------------------------------------------------------------
// Module-level helpers (shared across all declaration sub-readers)
// ---------------------------------------------------------------------------

let inline private getTypeSignalFromNode (ctx: TypeScriptReader) (node: Ts.TypeNode) =
    ctx.CreateXanthamTag node
    |> fst
    |> stackPushAndThen ctx _.TypeSignal

/// Returns Void TypeSignal when the type annotation is absent.
let inline private getReturnTypeSignal (ctx: TypeScriptReader) (typeNode: Ts.TypeNode option) =
    typeNode
    |> Option.map (getTypeSignalFromNode ctx)
    |> Option.defaultValue (TypeSignal.ofKey TypeKindPrimitive.Void.TypeKey)

/// Resolves an optional TypeNode to a TypeSignal, falling back to checker.getTypeAtLocation when None.
let private getSignalsFromTypeNodeOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) =
    typ
    |> Option.map (
        ctx.CreateXanthamTag
        >> fst
        >> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
        )
    |> Option.defaultWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key, ctx.signalCache[guard.Value].Builder
        | tagState, _ -> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder) tagState)

/// Returns TypeSignal voption for constraint/default slots on type parameters.
let inline getTypeSignalFromOption (ctx: TypeScriptReader) (typ: Ts.TypeNode option) =
    typ
    |> Option.map (ctx.CreateXanthamTag >> fst >> stackPushAndThen ctx _.TypeSignal)
    |> Option.toValueOption

let inline getFullyQualifiedName (ctx: TypeScriptReader) (tag: XanthamTag) =
    let name =
        match tag.IdentityKey with
        | IdentityKey.DeclarationPosition _ ->
            match tag.ToUnderlyingValue() with
            | Choice1Of2 typ ->
                typ.aliasSymbol
                |> Option.orElse (typ.getSymbol())
                |> Option.map ctx.checker.getFullyQualifiedName
            | Choice2Of2 decl ->
                ctx.checker.getSymbolAtLocation decl
                |> Option.orElseWith (fun () ->
                    let typ = ctx.checker.getTypeAtLocation decl
                    typ.aliasSymbol
                    |> Option.orElse (typ.getSymbol())
                    )
                |> Option.map ctx.checker.getFullyQualifiedName
        | IdentityKey.AliasSymbol symbol | IdentityKey.Symbol symbol ->
            ctx.checker.getFullyQualifiedName symbol
            |> Some
        | _ -> None
    match name with
    | Some name ->
        if name.StartsWith("{") then [||]
        else name.Split('.')
    | None -> [||]

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
        >> fst >> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
        >> fun signals -> signals ||> Signal.map2 (fun typeKey -> function
            | ValueSome (SType.TypeParameter tp) ->
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
        |> SType.TypeParameter
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
        pushToStackIfTypeUnseen ctx paramType

module Interface =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.InterfaceDeclaration) (source: ModuleName) =
        let builder = {
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
                    |> Option.map (
                        _.AsArray
                        >> Array.map (
                            ctx.CreateXanthamTag
                            >> fst
                            >> stackPushAndThen ctx (fun tag ->
                                tag.TypeSignal, tag.Builder)))
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
                                | ValueSome (SType.TypeReference typeReferenceBuilder) ->
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
        xanTag.ExportBuilder <- STsExportDeclaration.Interface builder
        xanTag.Builder <- SType.Interface builder
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

module TypeAlias =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.TypeAliasDeclaration) (source: ModuleName) =
        let innerTypeSignal, innerBuilderSignal =
            ctx.CreateXanthamTag node.``type``
            |> fst
            |> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
        let builder = {
            SAliasBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Type = innerTypeSignal |> ctx.routeTypeTo xanTag
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerBuilderSignal.Value)
        xanTag.ExportBuilder <- STsExportDeclaration.TypeAlias builder
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
            | ValueSome (SType.EnumCase c) -> ValueSome c
            | _ -> ValueNone
            )

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.EnumDeclaration) (source: ModuleName) =
        let builder = {
            SEnumTypeBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Members = node.members.AsArray |> Array.map (resolveEnumMemberSlot ctx)
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.Builder <- SType.Enum builder
        xanTag.ExportBuilder <- STsExportDeclaration.Enum builder
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
            Parent = ctx.CreateXanthamTag node.parent |> fst |> stackPushAndThen ctx _.TypeSignal
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Value = value
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        |> SType.EnumCase
        |> setAstSignal xanTag
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag

module Variable =
    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.VariableDeclaration) (source: ModuleName) =
        let innerTypeSignal, innerBuilderSignal =
            getSignalsFromTypeNodeOption ctx node.``type`` (unbox<Ts.Node> node)
        let variableBuilder = {
            SVariableBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Type =
                innerTypeSignal
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.ExportBuilder <- STsExportDeclaration.Variable variableBuilder
        xanTag.Builder
        |> Signal.fulfillWith(fun () -> innerBuilderSignal.Value)

module FunctionDecl =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.FunctionDeclaration) (source: ModuleName) =
        // the underlying type will have the 'reference' type representation of the function
        let signature = ctx.checker.getTypeAtLocation node |> ctx.CreateXanthamTag |> fst // get underlying type
        let typeSignal, builderSignal = stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder) signature
        xanTag.TypeSignal // fulfill this nodes typesignal with the representation of the function
        |> Signal.fulfillWith (fun () -> typeSignal.Value)
        xanTag.Builder // fulfill this nodes builder with the representation of the function
        |> Signal.fulfillWith (fun () -> builderSignal.Value)
        // fulfill this nodes builder with the representation of the function
        let fnBuilder = { // create the export builder
            SFunctionBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            IsDeclared = node.body.IsNone
            Type = getReturnTypeSignal ctx node.``type``
            Parameters = getParameterSlots ctx node.parameters
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
            SignatureKey = typeSignal
        }
        xanTag.ExportBuilder <- STsExportDeclaration.Function fnBuilder // fill the export builder
module Class =
    let private exprWithTypeArgsToRefSlot (ctx: TypeScriptReader) (expr: Ts.ExpressionWithTypeArguments) =
        let typeSignal =
            expr
            |> ctx.checker.getTypeAtLocation
            |> ctx.CreateXanthamTag
            |> fst
            |> stackPushAndThen ctx _.TypeSignal
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
        let builder = {
            SClassBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Enumerable = false
            Name = NameHelpers.getName node.name
            Constructors = ctors |> Array.map (unbox<Ts.ConstructorDeclaration> >> Member.resolveToConstructorBuilder ctx)
            Members = members |> Array.map (Member.resolveClassMemberBuilder ctx)
            TypeParameters = getTypeParamSlots ctx node.typeParameters
            Heritage = resolveHeritage ctx node.heritageClauses
        }
        xanTag.Builder <- SType.Class builder
        xanTag.ExportBuilder <- STsExportDeclaration.Class builder
        ctx.checker.getTypeAtLocation node |> _.TypeKey
        |> setTypeKeyForTag xanTag
        

module Module =
    let private collectModuleTypes (ctx: TypeScriptReader) (body: Ts.ModuleBody option) (source: ModuleName) =
        match body with
        | None -> [||]
        | Some body ->
            match unbox body with
            | Patterns.Node.ModuleBlock block ->
                block.statements.AsArray
                |> Array.map (
                    unbox<Ts.Node>
                    >> ctx.CreateXanthamTag
                    >> fst
                    >> stackPushAndThen ctx (fun tag ->
                        trySetSourceForTag tag source
                        |> ignore
                        tag.ExportBuilder)
                    )
            | Patterns.Node.ModuleDeclaration nested ->
                unbox<Ts.Node> nested
                |> ctx.CreateXanthamTag
                |> fst
                |> stackPushAndThen ctx (fun tag -> ignore(trySetSourceForTag tag source); tag.ExportBuilder)
                |> Array.singleton
            | _ -> [||]

    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ModuleDeclaration) (source: ModuleName) =
        let moduleBuilder = {
            SModuleBuilder.Source = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            IsNamespace = node.flags.HasFlag(Ts.NodeFlags.Namespace)
            IsRecursive = false
            Exports = collectModuleTypes ctx !!node.body source
        }
        xanTag.ExportBuilder <- STsExportDeclaration.Module moduleBuilder
        let typeSignal, builderSignal =
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
            |> fst
            |> stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder)
        xanTag.TypeSignal |> Signal.fulfillWith (fun () -> typeSignal.Value)
        xanTag.Builder |> Signal.fulfillWith (fun () -> builderSignal.Value)

// ---------------------------------------------------------------------------
// Dispatch
// ---------------------------------------------------------------------------

let dispatch (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: TypeDeclaration) =
    let source =
        if xanTag |> GuardedData.Source.Keyed.has then
            xanTag
            |> GuardedData.Source.Keyed.get
            |> _.Value
        else
        ctx.moduleMap[node]
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
            xanTag.TypeSignal
            |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
            xanTag.Builder
            |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeDeclaration.ExpressionWithTypeArguments exprWithTypeArgs ->
        // Route via checker; consumers look up TypeSignal/Builder on this tag.
        let resolvedType = ctx.checker.getTypeAtLocation exprWithTypeArgs
        let innerTag = ctx.CreateXanthamTag resolvedType |> fst |> stackPushAndThen ctx id
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeDeclaration.Enum enumDeclaration ->
        Enum.readDeclaration ctx xanTag enumDeclaration source
    | TypeDeclaration.EnumMember enumMember ->
        Enum.readMember ctx xanTag enumMember
    | TypeDeclaration.VariableStatement variableStatement ->
        // Push each inner VariableDeclaration and wire this tag's signals to the first one.
        let tags =
            variableStatement.declarationList.declarations.AsArray
            |> Array.map (
                unbox<Ts.Node>
                >> ctx.CreateXanthamTag >> fst
                >> stackPushAndThen ctx id
                )
        match tags with
        | [||] -> ()
        | tags ->
            let firstTag = tags[0]
            xanTag.Builder |> Signal.fulfillWith (fun () -> firstTag.Builder.Value)
            xanTag.TypeSignal |> Signal.fulfillWith (fun () -> firstTag.TypeSignal.Value)
            xanTag.ExportBuilder |> Signal.fulfillWith(fun () -> firstTag.ExportBuilder.Value)
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
