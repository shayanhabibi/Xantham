module Xantham.Fable.Reading.TypeDeclaration

open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open Xantham.Fable.Types.SourceTag
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
let private getSignalsFromTypeNodeOption callingTag (ctx: TypeScriptReader) (typ: Ts.TypeNode option) (node: Ts.Node) =
    typ
    |> Option.map (
        ctx.CreateXanthamTag
        >> fst
        >> stackPushAndThen ctx (
            _.chainDebug(callingTag)
            >> fun tag -> tag.TypeSignal, tag.Builder)
        )
    |> Option.defaultWith (fun () ->
        match
            ctx.checker.getTypeAtLocation node
            |> ctx.CreateXanthamTag
        with
        | _, TagState.Visited guard when ctx.signalCache.ContainsKey(guard.Value) ->
            TypeSignal.ofKey ctx.signalCache[guard.Value].Key, ctx.signalCache[guard.Value].Builder
        | tagState, _ -> stackPushAndThen ctx (_.chainDebug(callingTag) >> fun tag -> tag.TypeSignal, tag.Builder) tagState)

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

// let trySetSourceForTag (tag: XanthamTag) (source: ExportCollection voption) =
//     GuardedData.Source.Keyed.getOrSetWith (fun () -> Signal.source source) tag
let trySetSourceForTag (tag: XanthamTag) (metadata: Metadata) =
    GuardedData.Metadata.Keyed.getOrSetWith (fun () -> Signal.source metadata) tag

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
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.InterfaceDeclaration) source =
        let builder = {
            Metadata = trySetSourceForTag xanTag source
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
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.TypeAliasDeclaration) source =
        let innerTypeSignal, innerBuilderSignal =
            ctx.CreateXanthamTag node.``type``
            |> fst
            |> stackPushAndThen ctx (_.chainDebug(xanTag) >> fun tag -> tag.TypeSignal, tag.Builder)
        let builder = {
            SAliasBuilder.Metadata = trySetSourceForTag xanTag source
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

    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.EnumDeclaration) source =
        let builder = {
            SEnumTypeBuilder.Metadata = trySetSourceForTag xanTag source
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
            SEnumCaseBuilder.Parent = ctx.CreateXanthamTag node.parent |> fst |> stackPushAndThen ctx _.TypeSignal
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
    let readDeclaration (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.VariableDeclaration) source =
        let innerTypeSignal, innerBuilderSignal =
            getSignalsFromTypeNodeOption xanTag ctx node.``type`` (unbox<Ts.Node> node)
        let innerTag =
            node.``type``
            |> Option.map (ctx.CreateXanthamTag >> fst >> _.Value)
            |> Option.defaultWith (fun () ->
                ctx.checker.getTypeAtLocation node
                |> ctx.CreateXanthamTag
                |> fst |> _.Value
                )
        let variableBuilder = {
            SVariableBuilder.Metadata = trySetSourceForTag xanTag source
            FullyQualifiedName = getFullyQualifiedName ctx xanTag
            Name = NameHelpers.getName node.name
            Type =
                innerTypeSignal
            Documentation = JSDocTags.resolveDocsForTag ctx xanTag
        }
        xanTag.ExportBuilder <- STsExportDeclaration.Variable variableBuilder
        xanTag.Builder
        |> Signal.fulfillWith(fun () ->
            match innerBuilderSignal.Value with
            | ValueSome _ as v -> v
            | ValueNone when innerTag.TryExportBuilder.IsSome && innerTag.ExportBuilder.Value.IsSome && ctx.exportCache.ContainsKey(innerTag.IdentityKey) ->
                let refKey = TypeSignal.ofKey ctx.exportCache[innerTag.IdentityKey].RefKey
                let makeTypeReferenceBuilder innerType innerTypeArguments = ValueSome <| SType.TypeReference {
                    Type = innerType
                    TypeArguments = innerTypeArguments
                    ResolvedType = ValueNone
                }
                let makeTypeArgument (signal: Signal<InlinedSTypeParameterBuilder voption>) =
                    match signal.Value with
                    | ValueSome { TypeParameter = { Constraint = ValueSome typeSignal } }
                    | ValueSome { TypeParameter = { Default = ValueSome typeSignal } } -> typeSignal
                    | ValueSome { Type = typeKey } -> Signal.source typeKey
                    | ValueNone -> TypeSignal.pending()
                innerTag.ExportBuilder.Value
                |> ValueOption.bind (function
                    | STsExportDeclaration.TypeAlias { TypeParameters = typeParams; Type = aliasType } -> 
                        typeParams
                        |> Array.map makeTypeArgument
                        |> makeTypeReferenceBuilder aliasType
                    | STsExportDeclaration.Class { TypeParameters = typeParams } 
                    | STsExportDeclaration.Interface { TypeParameters = typeParams } ->
                        typeParams
                        |> Array.map makeTypeArgument
                        |> makeTypeReferenceBuilder refKey
                    | STsExportDeclaration.Enum _
                    | STsExportDeclaration.Variable _
                    | STsExportDeclaration.Function _
                    | STsExportDeclaration.Module _ -> makeTypeReferenceBuilder refKey [||]
                    )
                // ValueSome (SType.Primitive TypeKindPrimitive.NonPrimitive)
            | ValueNone ->
                ValueNone
            )

module FunctionDecl =
    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.FunctionDeclaration) source =
        // the underlying type will have the 'reference' type representation of the function
        let signature = ctx.checker.getTypeAtLocation node |> ctx.CreateXanthamTag |> fst // get underlying type
        let typeSignal, builderSignal = stackPushAndThen ctx (fun tag -> tag.TypeSignal, tag.Builder) signature
        xanTag.TypeSignal // fulfill this nodes typesignal with the representation of the function
        |> Signal.fulfillWith (fun () -> typeSignal.Value)
        xanTag.Builder // fulfill this nodes builder with the representation of the function
        |> Signal.fulfillWith (fun () -> builderSignal.Value)
        // fulfill this nodes builder with the representation of the function
        let fnBuilder = { // create the export builder
            SFunctionBuilder.Metadata = trySetSourceForTag xanTag source
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

    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ClassDeclaration) source =
        let ctors, members =
            node.members.AsArray
            |> Array.partition (fun elem -> (unbox<Ts.Node> elem).kind = Ts.SyntaxKind.Constructor)
        let builder = {
            SClassBuilder.Metadata = trySetSourceForTag xanTag source
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
    let private collectModuleTypes (ctx: TypeScriptReader) (body: Ts.ModuleBody option) source =
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

    let read (ctx: TypeScriptReader) (xanTag: XanthamTag) (node: Ts.ModuleDeclaration) source =
        let moduleBuilder = {
            SModuleBuilder.Metadata = trySetSourceForTag xanTag source
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
    let metadata =
        node.Symbol
        |> ValueOption.bind (fun symbol ->
            if symbol.flags.HasFlag(Ts.SymbolFlags.Alias) then
                ctx.checker.getAliasedSymbol symbol
            else ctx.checker.getMergedSymbol symbol
            |> ctx.program.GetExportCollection
            |> ValueOption.map Source.Package
            )
        |> ValueOption.defaultWith (fun () ->
            let sourceTag = ctx.CreateSourceTagValue(node)
            match sourceTag.Value with
            | SourceKind.LibEs ->
                sourceTag.Guard.Source.fileName
                |> Node.Api.path.basename
                |> Source.LibEs
            | SourceKind.Package _ when sourceTag.SubModuleId.IsSome ->
                sourceTag.SubModuleId.Value
                |> Source.PackageInternal
            | SourceKind.Ambient _ when sourceTag.SubModuleId.IsSome ->
                sourceTag.SubModuleId.Value
                |> Source.PackageInternal
            | _ ->
                #if !FABLE_TEST
                ctx.logger.logfd "Declaration not identified as a lib-es decl, had no export collection, and no submodule id."
                #endif
                let source =
                    sourceTag.Guard.Source.fileName
                    |> Node.Api.path.basename
                    |> Source.UnknownDeclared
                xanTag.trace (fun log tagId -> log.logft "[%i{tagId}] [%s{fileName}] Declaration not identified as a lib-es decl, had no export collection, and no submodule id." tagId (source.ToString()))
                source
            )
        |> fun source -> { Source = source }
    let makeDebugMessage = sprintf "Dispatched type declaration -> %s" >> xanTag.doDebugMessage
    match node with
    | TypeDeclaration.TypeParameter typeParameterDeclaration ->
        "Type Parameter" |> makeDebugMessage 
        TypeParameter.read ctx xanTag typeParameterDeclaration
    | TypeDeclaration.Interface interfaceDeclaration ->
        "Interface" |> makeDebugMessage 
        Interface.read ctx xanTag interfaceDeclaration metadata
    | TypeDeclaration.TypeAlias typeAliasDeclaration ->
        "TypeAlias" |> makeDebugMessage 
        TypeAlias.read ctx xanTag typeAliasDeclaration metadata
    | TypeDeclaration.Class classDeclaration ->
        "Class" |> makeDebugMessage 
        Class.read ctx xanTag classDeclaration metadata
    | TypeDeclaration.HeritageClause heritageClause ->
        makeDebugMessage "Heritage Clause"
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
        "Expression With Type Arguments" |> makeDebugMessage 
        // Route via checker; consumers look up TypeSignal/Builder on this tag.
        let resolvedType = ctx.checker.getTypeAtLocation exprWithTypeArgs
        let innerTag = ctx.CreateXanthamTag resolvedType |> fst |> stackPushAndThen ctx id
        xanTag.TypeSignal
        |> Signal.fulfillWith (fun () -> innerTag.TypeSignal.Value)
        xanTag.Builder
        |> Signal.fulfillWith (fun () -> innerTag.Builder.Value)
    | TypeDeclaration.Enum enumDeclaration ->
        "Enum" |> makeDebugMessage
        Enum.readDeclaration ctx xanTag enumDeclaration metadata
    | TypeDeclaration.EnumMember enumMember ->
        "Enum Member" |> makeDebugMessage
        Enum.readMember ctx xanTag enumMember
    | TypeDeclaration.VariableStatement variableStatement ->
        "Variable Statement" |> makeDebugMessage
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
        "Variable Declaration" |> makeDebugMessage
        Variable.readDeclaration ctx xanTag variableDeclaration metadata
    | TypeDeclaration.FunctionDeclaration functionDeclaration ->
        "Function Declaration" |> makeDebugMessage
        FunctionDecl.read ctx xanTag functionDeclaration metadata
    | TypeDeclaration.Module moduleDeclaration ->
        "Module" |> makeDebugMessage
        Module.read ctx xanTag moduleDeclaration metadata
    | TypeDeclaration.Namespace namespaceDeclaration ->
        "Namespace" |> makeDebugMessage
        Module.read ctx xanTag namespaceDeclaration metadata
    | TypeDeclaration.ModuleBlock _ ->
        // Processed inline during Module/Namespace dispatch; no standalone signal needed
        "Module Block" |> makeDebugMessage
