// ReSharper disable FSharpInterpolatedString

[<AutoOpen>]
module rec Xantham.TypeScript.Core

open System.Collections.Generic
open System.ComponentModel
open TypeScript
open Fable.Core
open Fable.Core.JsInterop
open Xantham.Annotations
open Xantham.Fable
open Xantham.TypeScript.Collections

type SK = Ts.SyntaxKind
type SF = Ts.SymbolFlags
type TF = Ts.TypeFlags
type OF = Ts.ObjectFlags

[<RequireQualifiedAccess>]
module INode =
    let inline nodeKey (node: INode) = NodeKey.fromINode node
    let inline program (node: INode) = SymbolTypeKey.Program.unsafeGet node
    let inline checker (node: INode) = SymbolTypeKey.TypeChecker.unsafeGet node
    let inline toNode (node: INode) = unbox<Ts.Node> node
    let inline hasFlag (flag: Ts.NodeFlags) = toNode >> _.flags.HasFlag(flag)
    let inline create (program: Ts.Program) (node: Ts.Node) =
        unbox<INode> node
        |> SymbolTypeKey.Program.addIfAbsentWith program (SymbolTypeKey.TypeChecker.addIfAbsent (program.getTypeChecker()))
    let inline kind (node: INode) = (toNode node).kind.Name
    let inline trySymbolAtLocation (node: INode) =
        (checker node).getSymbolAtLocation(toNode node)
        |> Option.map (ISymbol.create (program node))
    let inline trySymbolKindAtLocation (node: INode) =
        trySymbolAtLocation node |> Option.map Symbol.Kind.create
    let inline typeAtLocation (node: INode) =
        if ts.isTypeNode (toNode node)
        then (checker node).getTypeFromTypeNode(toNode node |> unbox<Ts.TypeNode>)
        else (checker node).getTypeAtLocation(toNode node)
        |> IType.create (program node)
    let inline parent (node: INode) =
        (toNode node).parent
        |> create (program node)
    let inline source (node: INode) =
        (toNode node).getSourceFile()
        |> Node.SourceKind.create (program node)

[<RequireQualifiedAccess>]
module IType =
    let inline typeKey (type': IType) = TypeKey.fromIType type'
    let inline program (type': IType) = SymbolTypeKey.Program.unsafeGet type'
    let inline checker (type': IType) = SymbolTypeKey.TypeChecker.unsafeGet type'
    let inline toType (type': IType) = unbox<Ts.Type> type'
    let inline hasFlag (flag: Ts.TypeFlags) = toType >> _.flags.HasFlag(flag)
    let inline create (program: Ts.Program) (type': Ts.Type) =
        unbox<IType> type'
        |> SymbolTypeKey.Program.addIfAbsentWith program (
            SymbolTypeKey.TypeChecker.addIfAbsent (program.getTypeChecker())
            >> CompositeCollection.Type.register program.CompositeCollection
            )
    let inline wrapErasure (program: Ts.Program) (type': ^U): ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Type =
        create program type' |> unbox
        
    let inline canonicalSymbol (type': IType) =
        (toType type').getCanonicalSymbol()
        |> Option.map (ISymbol.create (program type'))
    let inline properties (type': IType) =
        (toType type').getProperties().AsArray
        |> Array.map (ISymbol.create (program type'))
        |> NonEmptyArray.create
    let inline apparentProperties (type': IType) =
        (toType type').getApparentProperties().AsArray
        |> Array.map (ISymbol.create (program type'))
        |> NonEmptyArray.create
    let inline nonAliasSymbol (type': IType) =
        (toType type').getNonAliasSymbol()
        |> Option.map (ISymbol.create (program type'))
        

[<RequireQualifiedAccess>]
module ISymbol =
    let inline symbolKey (symbol: ISymbol) = SymbolKey.fromISymbol symbol
    let inline program (symbol: ISymbol) = SymbolTypeKey.Program.unsafeGet symbol
    let inline checker (symbol: ISymbol) = SymbolTypeKey.TypeChecker.unsafeGet symbol
    let inline toSymbol (symbol: ISymbol) = unbox<Ts.Symbol> symbol 
    let inline hasFlag (flag: Ts.SymbolFlags) = toSymbol >> _.flags.HasFlag(flag)
    let inline name (symbol: ISymbol) = toSymbol symbol |> _.escapedName |> SymbolName.Create
    let create (program: Ts.Program) (symbol: Ts.Symbol) =
        let checker = program.getTypeChecker()
        if symbol.flags |> Enum.hasFlag SF.Alias
        then checker.getAliasedSymbol(symbol)
        else symbol
        |> checker.getMergedSymbol
        |> unbox<ISymbol>
        |> SymbolTypeKey.Program.addIfAbsentWith program (
            SymbolTypeKey.TypeChecker.addIfAbsent checker
            >> CompositeCollection.Symbol.register program.CompositeCollection
            )
    let declarations symbol =
        toSymbol symbol
        |> _.getDeclarations()
        |> Option.filter (_.AsArray >> Array.isEmpty >> not)
        |> Option.map _.AsArray
        |> Option.orElse (toSymbol symbol |> _.valueDeclaration |> Option.map Array.singleton)
        |> Option.orElse (
            checker symbol
            |> _.getRootSymbols(toSymbol symbol)
            |> _.AsArray
            |> Array.tryPick(
                _.getDeclarations()
                >> Option.bind (_.AsArray >> NonEmptyArray.create)
                >> Option.map _.Values
                )
            )
        |> Option.bind NonEmptyArray.create
    let tryPickDeclaration (fn: Ts.Declaration -> 'T option) symbol =
        declarations symbol
        |> Option.bind (NonEmptyArray.tryPick fn)
    let chooseDeclarations (fn: Ts.Declaration -> 'T option) symbol =
        declarations symbol
        |> Option.bind (NonEmptyArray.choose fn)
    let declaredType symbol =
        toSymbol symbol
        |> (checker symbol).getDeclaredTypeOfSymbol
        |> IType.create (program symbol)
    let typeAtLocation (node: INode) (symbol: ISymbol) =
        (checker symbol).getTypeOfSymbolAtLocation(toSymbol symbol, INode.toNode node)
        |> IType.create (program symbol)
    // let getTypes symbol =
    //     declarations symbol
    //     |> Option.map (
    //         NonEmptyArray.map (
    //             INode.create symbol
    //             >> typeAtLocation
    //             >> funApply symbol
    //             )
    //         )
    
module private Packages =
    module Version =
        let read (version: string) =
            version
            |> Option.ofObj
            |> Option.defaultValue "0.0.0"
            |> Measures.annotate<Packages.packageVersion>
    module PackageId =
        let inline asArray (packageId: Packages.PackageId) = unbox<string array> packageId
        let inline create (name: string) version =
            Packages.PackageId(Measures.annotate name, version)
        let inline name (Packages.PackageId(name, _)) = name
        let inline version (Packages.PackageId(_, version)) = version
    module SubModuleId =
        let symbolTypeKey = SymbolTypeKey.create<Packages.SubModuleId> "SubModuleId"
        let inline asArray (subModuleId: Packages.SubModuleId) = unbox<string array> subModuleId
        let inline create (packageId: Packages.PackageId) (subModuleName: string) =
            [| yield! PackageId.asArray packageId; subModuleName |]
            |> unbox<Packages.SubModuleId>
        let inline subName (Packages.SubModuleId(_, _, subModuleName)) = subModuleName
        let inline packageId (subModule: Packages.SubModuleId) = (asArray subModule)[ 0 .. 1 ] |> unbox<Packages.PackageId>
        let inline name (Packages.SubModuleId(name, _, _)) = name
        let inline version (Packages.SubModuleId(_, version, _)) = version
        module Flip =
            let inline create subModuleName packageId = SubModuleId.create packageId subModuleName
    
    module ConditionalExport =
        let create key =
            match key with
            | "types" | "Types" -> Packages.ConditionalExport.Types
            | "default" | "Default" -> Packages.ConditionalExport.Default
            | "browser" | "Browser" -> Packages.ConditionalExport.Browser
            | "development" | "Development" -> Packages.ConditionalExport.Development
            | "production" | "Production" -> Packages.ConditionalExport.Production
            | "node-addons" | "NodeAddons" -> Packages.ConditionalExport.NodeAddons
            | "node" | "Node" -> Packages.ConditionalExport.Node
            | "import" | "Import" -> Packages.ConditionalExport.Import
            | "require" | "Require" -> Packages.ConditionalExport.Require
            | "module" | "Module" -> Packages.ConditionalExport.Module
            | "module-sync" | "ModuleSync" -> Packages.ConditionalExport.ModuleSync
            | "esnext" | "ESNext" -> Packages.ConditionalExport.ESNext 
            | key -> fun value -> Packages.ConditionalExport.Unknown(key, value)
            
    module PackageExportKind =
        let rec read: obj -> _ = function
            | :? string as value -> Packages.ExportValue.String value
            | value ->
                readConditional value
                |> Packages.ExportValue.Conditional
        and private readConditional values =
            JS.Constructors.Object.entries values
            |> _.AsArray
            |> Array.map (fun (key, value) -> createConditionalValue key value)
            |> NonEmptyArray.create
            |> Option.defaultWith (fun () -> failwith "Conditional export value should have at least one key")
        and private createConditionalValue (key: string) value =
            read value
            |> ConditionalExport.create key
    
    module PackageExport =
        let private tryFromExports (jsonFields: PackageJsonPathFields): Packages.Export option =
            match jsonFields.exports with
            | Some (:? string as export) ->
                Map [ Measures.annotate ".", Packages.ExportValue.String export ]
                |> Some
            | Some exports ->
                JS.Constructors.Object.entries exports |> _.AsArray
                |> Array.map (fun (key, value) -> Measures.annotate key, PackageExportKind.read value)
                |> Map
                |> Some
            | None -> None
        let inline private makeDummyAbbrev (value: string) =
            Map [ Measures.annotate ".", Packages.ExportValue.String value ]
        let private tryFromTypes (jsonFields: PackageJsonPathFields) =
            Option.map makeDummyAbbrev jsonFields.types
        let private tryFromTypings (jsonFields: PackageJsonPathFields) =
            Option.map makeDummyAbbrev jsonFields.typings
        let private tryFromMain (jsonFields: PackageJsonPathFields) =
            Option.map makeDummyAbbrev jsonFields.main
            
        let tryFromFields (jsonFields: PackageJsonPathFields) =
            tryFromExports jsonFields
            |> Option.orElse (tryFromTypes jsonFields)
            |> Option.orElse (tryFromTypings jsonFields)
            |> Option.orElse (tryFromMain jsonFields)
    module Prelude =
        [<RequireQualifiedAccess>]
        type Error =
            | MissingSourceFileForPath of string
            | NoResolvedModule of PackedArgs
            | NoSourceFileForResolvedModule of Ts.ResolvedModuleFull
        type PackedArgs = {
            ResolvedModule: Ts.ResolvedModuleWithFailedLookupLocations
            ModuleName: string option
            SourceFile: Ts.SourceFile
        }
        type ProcessedArgs = {
            ResolvedModule: Ts.ResolvedModuleFull
            ResolvedModuleSourceFile: Node.SourceKind
            ModuleName: string option
            SourceFile: Node.SourceKind
        }
            
        let private argPacker (program: Ts.Program) resolvedModule (moduleName: string) _ sourceFilePath =
            program.getSourceFile sourceFilePath
            |> Option.map (fun sourceFile ->
                Ok {
                    ResolvedModule = resolvedModule
                    ModuleName = if System.String.IsNullOrWhiteSpace(moduleName) then None else Some moduleName
                    SourceFile = sourceFile
                }
                )
            |> Option.defaultValue (Error.MissingSourceFileForPath sourceFilePath |> Result.Error)
        
        let resolveModule (program: Ts.Program) (packedArgs: PackedArgs) =
            packedArgs.ResolvedModule.resolvedModule
            |> Option.map Ok
            |> Option.defaultValue (Error.NoResolvedModule packedArgs |> Result.Error)
            |> Result.bind (fun resolvedModule ->
                let resolvedFileName = resolvedModule.resolvedFileName
                program.getSourceFile resolvedFileName
                |> Option.orElseWith(fun () ->
                    let extensionless = Path.extensionless resolvedFileName
                    program.getSourceFiles().AsArray
                    |> Array.tryFind (_.fileName >> Path.extensionless >> (=) extensionless)
                    )
                |> Option.map Ok
                |> Option.defaultValue (Error.NoResolvedModule packedArgs |> Result.Error)
                |> Result.map (fun sourceFile ->
                    {
                        ResolvedModule = resolvedModule
                        ModuleName = packedArgs.ModuleName
                        SourceFile = Node.SourceKind.create program packedArgs.SourceFile
                        ResolvedModuleSourceFile = Node.SourceKind.create program sourceFile
                    }
                    )
                )
        let inline private createSubmoduleId (packageId: Ts.PackageId) =
            packageId.version
            |> Version.read
            |> PackageId.create packageId.name
            |> SubModuleId.create
            |> funApply packageId.subModuleName
            
        let resolveSubModulePackageId (program: Ts.Program) (processedArgs: ProcessedArgs) =
            processedArgs.ResolvedModule.packageId
            |> Option.map createSubmoduleId
            
        let init (program: Ts.Program) =
            let cb = fun a b c d ->
                argPacker program a b c d
                |> Result.bind (resolveModule program)
                |> Result.map (fun args ->
                    match args with
                    | { ModuleName = Some moduleName; ResolvedModuleSourceFile = sf } as args ->
                        resolveSubModulePackageId program args
                        |> Option.orElseWith (fun () ->
                            match Node.SourceKind.create program sf with
                            | Node.SourceKind.ExternalModule externalModule ->
                                Node.ExternalModule.getPackageId externalModule
                                |> SubModuleId.Flip.create moduleName
                                |> Some
                            | Node.SourceKind.Script script ->
                                Node.Script.getPackageId script
                                |> Option.map (SubModuleId.Flip.create moduleName)
                            )
                    | args -> resolveSubModulePackageId program args
                    |> Option.iter (fun subModuleId ->
                        args.ResolvedModuleSourceFile
                        PackageCollection.addPackageId
                        )
                    )
            program.forEachResolvedModule cb
            
    
[<RequireQualifiedAccess>]
module Node =
    module Internal =
        let inline getSymbol (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^T :> ICanHaveSymbol) = node.Checker.getSymbolAtLocation(node.Value)
        let inline unsafeGetSymbol (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^T :> IAlwaysSymbol) = node.Checker.getSymbolAtLocation(node.Value) |> Option.defaultWith (fun () -> failwith "Node should have symbol")
        let inline getEmbeddedSymbol (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^T :> IEmbedded<^E> and ^E:>ICanHaveSymbol) = node?symbol : Ts.Symbol option
        let inline unsafeGetEmbeddedSymbol (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^T :> IEmbedded<^E> and ^E :> IAlwaysSymbol) = node?symbol |> Option.defaultWith (fun () -> failwith "Node should have symbol") : Ts.Symbol
        let inline getNodeKey (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node) = node.Value.NodeKey
        let inline getUnionNodeKey (node: ^T when ^T :> IFastUnionUnwrappable<^U> and ^U :> Ts.Node) = node.Value.NodeKey
        let inline getMaybeModifiers (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^U:(member modifiers: ResizeArray<Ts.Modifier> option)) =
            node.Value.modifiers
            |> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.create node.Program)
                >> NonEmptyArray.create
                )
        let inline getMaybeTypeArguments (node: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node and ^U:(member typeArguments: ResizeArray<Ts.TypeNode> option)) =
            node.Value.typeArguments
            |> Option.bind (
                _.AsArray
                >> Array.map (Type.create node.Program)
                >> NonEmptyArray.create
                )
        let inline getNodeType (node: ^T when ^T :> IErasedWrapper<^U> and ^U:(member ``type``: Ts.TypeNode)) =
            node.Value.``type``
            |> Type.create node.Program
        
    let inline private (==*) a ([<InlineIfLambda>] b: Ts.Program -> 'NodeType -> 'IntermediateKind, [<InlineIfLambda>] c: 'IntermediateKind -> 'FinalKind): KeyValuePair<Ts.SyntaxKind, Ts.Program -> obj -> 'FinalKind> = KeyValuePair(a, (fun program node -> b program (unbox node) |> c |> InlinedProgram.inject program))
    let inline private (==>) a (b: Ts.Program -> 'NodeType -> 'WrappedType): KeyValuePair<Ts.SyntaxKind, Ts.Program -> obj -> 'FinalKind> = KeyValuePair(a, unbox b)
    let inline private (==>!) a (b: 'FinalKind): KeyValuePair<Ts.SyntaxKind, Ts.Program -> obj -> 'FinalKind> = KeyValuePair(a, fun _ _ -> b)
    let inline private fetchMap (kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> 'IntermediateKind>) (finaliser: Ts.Program -> 'IntermediateKind -> 'FinalKind) = seq {
        for KeyValue(k, v) in kindMap do
            k ==> fun program node -> v program node |> finaliser program
    }
    let inline private fetchMapAndInject
        (kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> ^IntermediateKind>)
        (finaliser: ^IntermediateKind -> ^FinalKind when ^FinalKind :> IInlinedProgram) = seq {
        for KeyValue(k, v) in kindMap do
            k ==> fun program node -> v program node |> finaliser |> InlinedProgram.inject program
    }
        
    let inline private dictToSet (dict: IDictionary<'Key, 'Value>) = dict.Keys |> Set.ofSeq
    let inline private setContainsNodeKind (kindSet: Set<Ts.SyntaxKind>) (node: Ts.Node) = kindSet.Contains node.kind
    let inline private tryCreateWithNodeCheck (dict: IDictionary<Ts.SyntaxKind, Ts.Program -> obj -> 'Result>) (check: Ts.Node -> bool) program (node: Ts.Node) =
        if not <| check node then None else
        dict[node.kind] program node
        |> Some

    module NumericLiteral =
        let inline create program node: Node.NumericLiteral = IErasedWrapper.create program node
        let value: Node.NumericLiteral -> _ = IErasedWrapper.map (_.text >> JS.Constructors.Number.parseFloat)
    module StringLiteral =
        let inline create program node: Node.StringLiteral = IErasedWrapper.create program node
        let value: Node.StringLiteral -> _ = IErasedWrapper.map _.text
    module BooleanLiteral =
        let inline create program (node: Ts.BooleanLiteral): Node.BooleanLiteral = IErasedWrapper.create program node
        let internal kindMap = Dictionary<_, _ -> _ -> Node.BooleanLiteral> [
            SK.TrueKeyword ==> create
            SK.FalseKeyword ==> create
        ]
        let kindSet = dictToSet kindMap
        let isBooleanLiteral node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isBooleanLiteral program node
        let value: Node.BooleanLiteral -> bool = _.Map(unbox<Ts.Node> >> _.kind.Equals(Ts.SyntaxKind.TrueKeyword))
        let (|True|False|): Node.BooleanLiteral -> _ = _.Map(unbox<Ts.Node> >> _.kind >> function
            | SK.TrueKeyword -> True
            | SK.FalseKeyword | _ -> False)
    module BigIntLiteral =
        let inline create program node: Node.BigIntLiteral = IErasedWrapper.create program node
        let text: Node.BigIntLiteral -> _ = IErasedWrapper.map _.text
    module NoSubstitutionTemplateLiteral =
        let inline create program node: Node.NoSubstitutionTemplateLiteral = IErasedWrapper.create program node
        let value: Node.NoSubstitutionTemplateLiteral -> _ = IErasedWrapper.map _.text
    module UnionType =
        open Xantham.TypeScript.Node
        let inline create program node: Node.UnionType = IErasedWrapper.create program node
        let types: Node.UnionType -> _ = _.MapWithProgram(fun program ->
            _.types
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "UnionType should have types")
            >> NonEmptyArray.map (Type.create program)
            )
        [<System.Obsolete("In practice this is never true")>]
        let isBoolean (node: Node.UnionType) = types node |> _.Values |> function
            | [| Type.Literal (Literal.Boolean _, _); Type.Literal (Literal.Boolean _, _) |] -> true
            | arr when arr.Length = 3 && arr |> Array.sumBy (function
                | Type.Literal (Literal.Boolean _, _) -> 1
                | Type.Keyword (TypeKeyword.Undefined, _) -> -3
                | _ -> 0) |> (=) -1 -> true
            | _ -> false
        let contains predicate = types >> NonEmptyArray.exists predicate
        let inline containsNullable union = contains (function Type.Keyword (keyword, _) -> TypeKeyword.isNullable keyword | _ -> false) union
        let collect mapping = types >> NonEmptyArray.collect mapping
        let collectArrays mapping = types >> NonEmptyArray.collectArrays mapping
    module IntersectionType =
        let inline create program node: Node.IntersectionType = IErasedWrapper.create program node
        let types: Node.IntersectionType -> _ = _.MapWithProgram(fun program ->
            _.types
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "IntersectionType should have types")
            >> NonEmptyArray.map (Type.create program)
            )
        let contains predicate = types >> NonEmptyArray.exists predicate
        let collect mapping = types >> NonEmptyArray.collect mapping
        let collectArrays mapping = types >> NonEmptyArray.collectArrays mapping
    module ArrayType =
        let inline create program node: Node.ArrayType = IErasedWrapper.create program node
        let target: Node.ArrayType -> _ = _.MapWithProgram(fun program -> _.elementType >> Type.create program)
    module TupleType =
        let inline create program node: Node.TupleType = IErasedWrapper.create program node
        let elements (tuple: Node.TupleType) =
            tuple
            |> IErasedWrapper.map (fun node ->
                node.elements.AsArray
                |> Array.map (unbox<Ts.TypeNode> >> Type.create tuple.Program)
                |> NonEmptyArray.create
                )
        let parent: Node.TupleType -> _ = _.MapWithProgram(fun program -> _.parent >> DeclarationKind.tryCreate program >> Option.defaultWith (fun () -> failwith "TupleType should have parent of declaration kind"))
    module NamedTupleMember =
        let inline create program node: Node.NamedTupleMember = IErasedWrapper.create program node
        let isOptional: Node.NamedTupleMember -> _ = IErasedWrapper.map _.questionToken.IsSome
        let isSpread: Node.NamedTupleMember -> _ = IErasedWrapper.map _.dotDotDotToken.IsSome
        let name: Node.NamedTupleMember -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let toString tupleMember = name tupleMember |> Identifier.toString
        let type': Node.NamedTupleMember -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let parent: Node.NamedTupleMember -> Node.TupleType = IErasedWrapper.wrappedMap (_.parent >> fun n ->
            if n.kind <> SK.TupleType then failwith "NamedTupleMember should have parent of TupleType" else
            n :?> Ts.TupleTypeNode)
    module RestType =
        let inline create program node: Node.RestType = IErasedWrapper.create program node
        let type': Node.RestType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let nodeKey: Node.RestType -> _ = Internal.getNodeKey
    module OptionalType =
        let inline create program node: Node.OptionalType = IErasedWrapper.create program node
        let type': Node.OptionalType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let nodeKey: Node.OptionalType -> _ = Internal.getNodeKey
    module ParenthesizedType =
        let inline create program node: Node.ParenthesizedType = IErasedWrapper.create program node
        let type': Node.ParenthesizedType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let nodeKey: Node.ParenthesizedType -> _ = Internal.getNodeKey
    module TypeParameterDeclaration =
        let inline create program node: Node.TypeParameterDeclaration = IErasedWrapper.create program node
        [<System.Obsolete("Use alternative method", true)>]
        let inline getForNode (node: ^T when ^T :> ICanHaveTypeParameters<Node.TypeParameterDeclaration>) = node :> ICanHaveTypeParameters<Node.TypeParameterDeclaration> |> _.TypeParameters
        let embeddedSymbol: Node.TypeParameterDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let name: Node.TypeParameterDeclaration -> Node.Identifier = _.WrappedMap(_.name)
        let toString typar = name typar |> Identifier.toString
        let ``constraint``: Node.TypeParameterDeclaration -> _ = _.MapWithProgram(fun program -> _.``constraint`` >> Option.map (Type.create program))
        let ``default``: Node.TypeParameterDeclaration -> _ = _.MapWithProgram(fun program -> _.``default`` >> Option.map (Type.create program))
        let modifiers: Node.TypeParameterDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.create program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.TypeParameterDeclaration -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> DeclarationKind.tryCreate program
            >> Option.defaultWith (fun () -> failwith "TypeParameterDeclaration should have parent")
            )
        let nodeKey: Node.TypeParameterDeclaration -> _ = Internal.getNodeKey
    module InferType =
        let inline create program node: Node.InferType = IErasedWrapper.create program node
        let typeParameter: Node.InferType -> Node.TypeParameterDeclaration = IErasedWrapper.wrappedMap _.typeParameter
        let nodeKey: Node.InferType -> _ = Internal.getNodeKey
    module TypePredicate =
        let inline create program node: Node.TypePredicate = IErasedWrapper.create program node
        let parameterName (typePredicate: Node.TypePredicate) =
            typePredicate
            |> IErasedWrapper.map (
                _.parameterName
                >> unbox<Ts.Node>
                >> function
                    | Patterns.Node.Identifier identNode -> Identifier.create typePredicate.Program identNode |> Choice1Of2 
                    | Patterns.Node.ThisTypeNode thisTypeNode -> ThisType.create typePredicate.Program thisTypeNode |> Choice2Of2 
                    | n -> failwithf "TypePredicate parameterName should be Identifier or ThisType, but was %A" n.kind.Name
                )
        let type': Node.TypePredicate -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let isAssertionTypePredicate: Node.TypePredicate -> _ = _.Map(_.assertsModifier.IsSome)
        let nodeKey: Node.TypePredicate -> _ = Internal.getNodeKey
    module TypeQuery =
        let inline create program node: Node.TypeQuery = IErasedWrapper.create program node
        let exprName: Node.TypeQuery -> _ = _.MapWithProgram(fun program -> _.exprName >> IdentifierKind.fromEntityName program)
        let typeArguments: Node.TypeQuery -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray
                >> Array.map (Type.create program)
                >> NonEmptyArray.create
                )
            )
        let nodeKey: Node.TypeQuery -> _ = Internal.getNodeKey
    module IndexedAccessType =
        let inline create program node: Node.IndexedAccessType = IErasedWrapper.create program node
        let indexType: Node.IndexedAccessType -> _ = _.MapWithProgram(fun program -> _.indexType >> Type.create program)
        let objectType: Node.IndexedAccessType -> _ = _.MapWithProgram(fun program -> _.objectType >> Type.create program)
        let nodeKey: Node.IndexedAccessType -> _ = Internal.getNodeKey
    module MappedType =
        let inline create program node: Node.MappedType = IErasedWrapper.create program node
        let embeddedSymbol: Node.MappedType -> _ = Internal.unsafeGetEmbeddedSymbol
        let hasQuestionToken: Node.MappedType -> _ = IErasedWrapper.map _.questionToken.IsSome
        let hasReadonlyToken: Node.MappedType -> _ = IErasedWrapper.map _.readonlyToken.IsSome
        let nameType: Node.MappedType -> _ = _.MapWithProgram(fun program -> _.nameType >> Option.map (Type.create program))
        /// <summary>
        /// In practice this always seems to provide a value
        /// </summary>
        let type': Node.MappedType -> _  = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let unsafeType: Node.MappedType -> _ = type' >> Option.defaultWith (fun () -> failwith "MappedType should have type in practice")
        let typeParameter: Node.MappedType -> Node.TypeParameterDeclaration = IErasedWrapper.wrappedMap _.typeParameter
        // [<System.Obsolete("In practice this never returns a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        // let members : Node.MappedType -> _ = _.MapWithProgram(fun program ->
        //     _.members
        //     >> Option.bind (
        //         _.AsArray
        //         >> Array.map (TypeMemberKind.create program)
        //         >> NonEmptyArray.create
        //         )
        //     )
        let nodeKey: Node.MappedType -> _ = Internal.getNodeKey
    module ConditionalType =
        let inline create program node: Node.ConditionalType = IErasedWrapper.create program node
        let checkType: Node.ConditionalType -> _ = _.MapWithProgram(fun program -> _.checkType >> Type.create program)
        let extendsType: Node.ConditionalType -> _ = _.MapWithProgram(fun program -> _.extendsType >> Type.create program)
        let trueType: Node.ConditionalType -> _ = _.MapWithProgram(fun program -> _.trueType >> Type.create program)
        let falseType: Node.ConditionalType -> _ = _.MapWithProgram(fun program -> _.falseType >> Type.create program)
        let nodeKey: Node.ConditionalType -> _ = Internal.getNodeKey
    
    module TemplateLiteralType =
        let inline create program node: Node.TemplateLiteralType = IErasedWrapper.create program node
        let head: Node.TemplateLiteralType -> _ = _.MapWithProgram(fun program -> _.head >> TemplatePart.unsafeCreate program)
        let spans: Node.TemplateLiteralType -> _ = _.MapWithProgram(fun program ->
            _.templateSpans.AsArray
            >> Array.map (TemplateLiteralTypeSpan.create program)
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "TemplateLiteralType should have spans")
            )
        let nodeKey: Node.TemplateLiteralType -> _ = Internal.getNodeKey
    module TemplateLiteralTypeSpan =
        let inline create program node: Node.TemplateLiteralTypeSpan = IErasedWrapper.create program node
        let type': Node.TemplateLiteralTypeSpan -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let literal: Node.TemplateLiteralTypeSpan -> _ = _.MapWithProgram(fun program ->
            _.literal
            >> unbox<Ts.Node>
            >> TemplatePart.unsafeCreate program
            )
        let parent: Node.TemplateLiteralTypeSpan -> Node.TemplateLiteralType = IErasedWrapper.wrappedMap _.parent
        let nodeKey: Node.TemplateLiteralTypeSpan -> _ = Internal.getNodeKey
    module ImportType =
        let inline create program node: Node.ImportType = IErasedWrapper.create program node
        let symbol: Node.ImportType -> _ = Internal.unsafeGetSymbol
        let argument: Node.ImportType -> _ = _.MapWithProgram(fun program -> _.argument >> Type.create program)
        let typeArguments: Node.ImportType -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray
                >> Array.map (Type.create program)
                >> NonEmptyArray.create
                )
            )
        let qualifier: Node.ImportType -> _ = _.MapWithProgram(fun program ->
            _.qualifier
            >> Option.map (IdentifierKind.fromEntityName program)
            )
        let nodeKey: Node.ImportType -> _ = Internal.getNodeKey
        let parent: Node.ImportType -> _ = _.MapWithProgram(fun program -> _.parent >> DeclarationKind.tryCreate program >> Option.defaultWith (fun () -> failwith "ImportType should have parent of declaration kind"))
    module FunctionType =
        let inline create program node: Node.FunctionType = IErasedWrapper.create program node
        let typeParameters: Node.FunctionType -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let type': Node.FunctionType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        [<System.Obsolete("never returns a value in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let name: Node.FunctionType -> _ = _.MapWithProgram(fun program ->
            _.name
            >> Option.map (
                unbox<Ts.Node>
                >> IdentifierKind.tryCreate program
                >> Option.defaultWith (fun () -> failwith "FunctionType should have name")
                )
            )
        let parameters: Node.FunctionType -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let embeddedSymbol: Node.FunctionType -> _ = _.MapWithProgram(fun program -> _.symbol >> Symbol.Kind.createFromSymbol program)
        let nodeKey: Node.FunctionType -> _ = _.Map(_.NodeKey)
    module ConstructorType =
        let inline create program node: Node.ConstructorType = IErasedWrapper.create program node
        let typeParameters: Node.ConstructorType -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.ConstructorType -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.ConstructorType -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.create program)
                >> NonEmptyArray.create
                )
            )
        [<System.Obsolete("never returns a value in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let name: Node.ConstructorType -> _ = _.MapWithProgram(fun program ->
            _.name
            >> Option.map (
                unbox<Ts.Node> >> IdentifierKind.tryCreate program
                >> Option.defaultWith (fun () -> failwith "ConstructorType should have name")
                )
            )
        let type': Node.ConstructorType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let parameters: Node.ConstructorType -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let nodeKey: Node.ConstructorType -> _ = Internal.getNodeKey
    module TypeLiteralType =
        let inline create program node: Node.TypeLiteralType = IErasedWrapper.create program node
        let embeddedSymbol: Node.TypeLiteralType -> _ = Internal.unsafeGetEmbeddedSymbol
        let members: Node.TypeLiteralType -> _ = _.MapWithProgram(fun program ->
            _.members.AsArray
            >> Array.map (TypeMemberKind.create program)
            >> NonEmptyArray.create
            )
        let nodeKey: Node.TypeLiteralType -> _ = Internal.getNodeKey
    module LiteralType =
        let inline create program node: Node.LiteralType = IErasedWrapper.create program node
        let literal (node: Node.LiteralType) = node.MapWithProgram(fun program -> _.literal >> unbox<Ts.Node> >> Literal.unsafeCreate program)
        let nodeKey: Node.LiteralType -> _ = Internal.getNodeKey
        let (|Literal|): Node.LiteralType -> Node.Literal = literal
    module ThisType =
        let inline create program node: Node.ThisType = IErasedWrapper.create program node
        let symbol: Node.ThisType -> _ = Internal.unsafeGetSymbol
        let nodeKey: Node.ThisType -> _ = Internal.getNodeKey
    module TypeReference =
        let inline create program node: Node.TypeReference = IErasedWrapper.create program node
        let targetSymbol: Node.TypeReference -> _ = _.MapWithChecker(fun checker ->
            _.typeName
            >> unbox<Ts.Node>
            >> checker.getSymbolAtLocation
            >> Option.defaultWith (fun () -> failwith "TypeReference should have symbol")
            )
        let typeName: Node.TypeReference -> _ = _.MapWithProgram(fun program -> _.typeName >> IdentifierKind.fromEntityName program)
        let typeArguments: Node.TypeReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray
                >> Array.map (Type.create program)
                >> NonEmptyArray.create
                )
            )
        let nodeKey: Node.TypeReference -> _ = Internal.getNodeKey
    module ExpressionWithTypeArguments =
        let inline create program node: Node.ExpressionWithTypeArguments = IErasedWrapper.create program node
        let getType: Node.ExpressionWithTypeArguments -> _ = _.MapWithProgram(fun program -> program.getTypeChecker().getTypeFromTypeNode >> Type.Kind.createFromType program)
        let typeArguments: Node.ExpressionWithTypeArguments -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray
                >> Array.map (Type.create program)
                >> NonEmptyArray.create
                )
            )
        let expression: Node.ExpressionWithTypeArguments -> _ = _.MapWithProgram(fun program -> _.expression >> IdentifierExpressionKind.unsafeCreate program)
        let nodeKey: Node.ExpressionWithTypeArguments -> _ = Internal.getNodeKey
    module TypeNode =
        let inline create program node: Node.TypeNode = IErasedWrapper.create program node
        let toNodeType: Node.TypeNode -> _ = _.MapWithProgram(Type.create)
    module ParameterDeclaration =
        let inline create program node: Node.ParameterDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.ParameterDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let isOptional: Node.ParameterDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        let isRest: Node.ParameterDeclaration -> _ = IErasedWrapper.map _.dotDotDotToken.IsSome
        let type': Node.ParameterDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "ParameterDeclaration should have type in type declaration files"))
        let name: Node.ParameterDeclaration -> _ = _.MapWithProgram(fun program ->
            _.name
            >> function
                | Patterns.Node.BindingNamePatterns.Identifier identNode ->
                    Identifier.create program identNode
                    |> Choice1Of2
                | Patterns.Node.BindingNamePatterns.ObjectBindingPattern objectBindingPattern ->
                    ObjectBindingPattern.create program objectBindingPattern
                    |> ObjectBindingPattern.toBindingPattern
                    |> Choice2Of2
                | Patterns.Node.BindingNamePatterns.ArrayBindingPattern arrayBindingPattern ->
                    ArrayBindingPattern.create program arrayBindingPattern
                    |> ArrayBindingPattern.toBindingPattern
                    |> Choice2Of2
            )
        [<System.Obsolete("Does not seem to provide a value in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let modifiers: Node.ParameterDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let nodeKey: Node.ParameterDeclaration -> _ = Internal.getNodeKey
        let isBindingLike: Node.ParameterDeclaration -> _ = name >> _.IsChoice2Of2
    module BindingElement =
        let inline create program node: Node.BindingElement = IErasedWrapper.create program node
        let embeddedSymbol: Node.BindingElement -> _ = Internal.getEmbeddedSymbol
        let name: Node.BindingElement -> _ = unbox<Node.ParameterDeclaration> >> ParameterDeclaration.name
        let propertyName: Node.BindingElement -> _ = _.MapWithProgram(fun program ->
            _.propertyName
            >> Option.map (
                unbox<Ts.Node>
                >> IdentifierKind.tryCreate program
                >> Option.defaultWith (fun () -> failwith "BindingElement should have propertyName parse correctly to identifier kind")
                )
            )
        let isSpread: Node.BindingElement -> _ = IErasedWrapper.map _.dotDotDotToken.IsSome
        let parent: Node.BindingElement -> _ = _.MapWithProgram(fun program -> _.parent >> BindingPattern.create program)
        let nodeKey: Node.BindingElement -> _ = Internal.getNodeKey

    module IndexSignature =
        let inline create program node: Node.IndexSignature = IErasedWrapper.create program node
        [<System.Obsolete("Does not seem to provide a value in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let typeParameters: Node.IndexSignature -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.IndexSignature -> _ = Internal.unsafeGetEmbeddedSymbol
        let type': Node.IndexSignature -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let modifiers: Node.IndexSignature -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parameters: Node.IndexSignature -> Node.ParameterDeclaration NonEmptyArray = IErasedWrapper.wrappedArrayMap _.parameters >> Option.defaultWith (fun () -> failwith "IndexSignature should have parameters")
        [<System.Obsolete("Never true in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.IndexSignature -> _ = IErasedWrapper.map _.questionToken.IsSome
        let nodeKey: Node.IndexSignature -> _ = Internal.getNodeKey
        let parent: Node.IndexSignature -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of2
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice2Of2
                // in practice this never occurs
                // | Patterns.Node.ClassDeclaration node -> ClassDeclaration.create program node |> Choice3Of3
                | _ -> failwith "IndexSignature should have parent be either interface declaration, class declaration or type literal"
            )

    module CallSignature =
        let inline create program node: Node.CallSignature = IErasedWrapper.create program node
        let typeParameters: Node.CallSignature -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.CallSignature -> _ = Internal.unsafeGetEmbeddedSymbol
        // always provides a value in declaration files
        let type': Node.CallSignature -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "CallSignature should have type in type declaration files"))
        [<System.Obsolete("Never true in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.CallSignature -> _ = IErasedWrapper.map _.questionToken.IsSome
        let parameters: Node.CallSignature -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let nodeKey: Node.CallSignature -> _ = Internal.getNodeKey
        let parent: Node.CallSignature -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of2
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice2Of2
                | _ -> failwith "CallSignature should have parent be either interface declaration or type literal"
            )

    module ConstructSignature =
        let inline create program node: Node.ConstructSignature = IErasedWrapper.create program node
        let typeParameters: Node.ConstructSignature -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.ConstructSignature -> _ = Internal.unsafeGetEmbeddedSymbol
        // always provides a value in declaration files
        let type': Node.ConstructSignature -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "ConstructSignature should have type in type declaration files"))
        [<System.Obsolete("Never true in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.ConstructSignature -> _ = IErasedWrapper.map _.questionToken.IsSome
        let parameters: Node.ConstructSignature -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let nodeKey: Node.ConstructSignature -> _ = Internal.getNodeKey
        let parent: Node.ConstructSignature -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of2
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice2Of2
                | _ -> failwith "ConstructSignature should have parent be either interface declaration or type literal"
            )

    module PropertyDeclaration =
        let inline create program node: Node.PropertyDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.PropertyDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let isOptional: Node.PropertyDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        [<System.Obsolete("Never true in declaration files"); EditorBrowsable(EditorBrowsableState.Never)>]
        let hasExclamationToken: Node.PropertyDeclaration -> _ = IErasedWrapper.map _.exclamationToken.IsSome
        /// Provides no value if private identifier
        let name: Node.PropertyDeclaration -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.PrivateIdentifier _ -> None
                | node -> IdentifierLiteralKind.unsafeCreate program node |> Some
            )
        let modifiers: Node.PropertyDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.PropertyDeclaration -> Node.ClassDeclaration = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.ClassDeclaration>)
        let type': Node.PropertyDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let toClassMemberKind (prop: Node.PropertyDeclaration) = ClassMemberKind.fromPropertyDeclaration prop
        let nodeKey: Node.PropertyDeclaration -> _ = Internal.getNodeKey

    module PropertySignature =
        let inline create program node: Node.PropertySignature = IErasedWrapper.create program node
        let embeddedSymbol: Node.PropertySignature -> _ = Internal.unsafeGetEmbeddedSymbol
        let isOptional: Node.PropertySignature -> _ = IErasedWrapper.map _.questionToken.IsSome
        let name: Node.PropertySignature -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.NumericLiteral node -> NumericLiteral.create program node |> Choice2Of2
                | node -> IdentifierLiteralKind.unsafeCreate program node |> Choice1Of2
            )
        let type': Node.PropertySignature -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "PropertySignature should have type in type declaration files"))
        let modifiers: Node.PropertySignature -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.create program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.PropertySignature -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of2
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice2Of2
                | _ -> failwith "PropertySignature should have parent be either interface declaration or type literal"
            )
        let toTypeMemberKind (prop: Node.PropertySignature) =
            Node.TypeMemberKind.Property prop
            |> InlinedProgram.inject prop.Program
        let nodeKey: Node.PropertySignature -> _ = Internal.getNodeKey
    module MethodDeclaration =
        let inline create program node: Node.MethodDeclaration = IErasedWrapper.create program node
        let typeParameters: Node.MethodDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.MethodDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.MethodDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let name: Node.MethodDeclaration -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> IdentifierLiteralKind.unsafeCreate program
            )
        let type': Node.MethodDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "MethodDeclaration should have type in type declaration files"))
        let parameters: Node.MethodDeclaration -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let isOptional: Node.MethodDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        let parent: Node.MethodDeclaration -> Node.ClassDeclaration = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.ClassDeclaration>)
        let nodeKey: Node.MethodDeclaration -> _ = Internal.getNodeKey

    module MethodSignature =
        let inline create program node: Node.MethodSignature = IErasedWrapper.create program node
        let typeParameters: Node.MethodSignature -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.MethodSignature -> _ = Internal.unsafeGetEmbeddedSymbol
        let parent: Node.MethodSignature -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of2
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice2Of2
                | _ -> failwith "MethodSignature should have parent be either interface declaration or type literal"
            )
        [<System.Obsolete("In practice doesn't seem to provide any value")>]
        let modifiers: Node.MethodSignature -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.create program)
                >> NonEmptyArray.create
                )
            )
        let name: Node.MethodSignature -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> IdentifierLiteralKind.unsafeCreate program
            )
        let isOptional: Node.MethodSignature -> _ = IErasedWrapper.map _.questionToken.IsSome
        let parameters: Node.MethodSignature -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let nodeKey: Node.MethodSignature -> _ = Internal.getNodeKey

    module GetAccessorDeclaration =
        let inline create program node: Node.GetAccessorDeclaration = IErasedWrapper.create program node
        [<System.Obsolete("In practice doesn't seem to provide any value")>]
        let typeParameters: Node.GetAccessorDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.GetAccessorDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.GetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.GetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of3
                | Patterns.Node.ClassDeclaration node -> ClassDeclaration.create program node |> Choice2Of3
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice3Of3
                | _ -> failwith "GetAccessorDeclaration should have parent be either interface declaration, class declaration or type literal"
            )
        let name: Node.GetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> IdentifierKind.tryCreate program
            >> Option.defaultWith (fun () -> failwith "GetAccessorDeclaration should have name parse correctly to identifier kind")
            )
        [<System.Obsolete("In practice doesn't seem to provide any value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let hasAsteriskToken: Node.GetAccessorDeclaration -> _ = IErasedWrapper.map _.asteriskToken.IsSome
        [<System.Obsolete("In practice doesn't seem to provide any value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let hasExclamationToken: Node.GetAccessorDeclaration -> _ = IErasedWrapper.map _.exclamationToken.IsSome
        [<System.Obsolete("In practice is never true"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.GetAccessorDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let parameters: Node.GetAccessorDeclaration -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        let type': Node.GetAccessorDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let nodeKey: Node.GetAccessorDeclaration -> _ = Internal.getNodeKey

    module SetAccessorDeclaration =
        let inline create program node: Node.SetAccessorDeclaration = IErasedWrapper.create program node
        [<System.Obsolete("In practice doesn't seem to provide any value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let typeParameters: Node.SetAccessorDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.SetAccessorDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.SetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.SetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.InterfaceDeclaration node -> InterfaceDeclaration.create program node |> Choice1Of3
                | Patterns.Node.ClassDeclaration node -> ClassDeclaration.create program node |> Choice2Of3
                | Patterns.Node.TypeLiteralNode node -> TypeLiteralType.create program node |> Choice3Of3
                | _ -> failwith "GetAccessorDeclaration should have parent be either interface declaration, class declaration or type literal"
            )
        let name: Node.SetAccessorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> IdentifierKind.tryCreate program
            >> Option.defaultWith (fun () -> failwith "GetAccessorDeclaration should have name parse correctly to identifier kind")
            )
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let hasAsteriskToken: Node.SetAccessorDeclaration -> _ = IErasedWrapper.map _.asteriskToken.IsSome
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let hasExclamationToken: Node.SetAccessorDeclaration -> _ = IErasedWrapper.map _.exclamationToken.IsSome
        [<System.Obsolete("Never seems to return true"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.SetAccessorDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        let parameters: Node.SetAccessorDeclaration -> Node.ParameterDeclaration NonEmptyArray = IErasedWrapper.wrappedArrayMap _.parameters >> Option.defaultWith (fun () -> failwith "SetAccessorDeclaration should have parameters")
        [<System.Obsolete("Never returns a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let type': Node.SetAccessorDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let nodeKey: Node.SetAccessorDeclaration -> _ = Internal.getNodeKey

    module ModuleDeclaration =
        let inline create program node: Node.ModuleDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.ModuleDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.ModuleDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let name: Node.ModuleDeclaration -> Node.IdentifierLiteralKind = _.MapWithProgram(fun program -> _.name >> unbox<Ts.Node> >> IdentifierLiteralKind.unsafeCreate program)
        let body: Node.ModuleDeclaration -> Node.ModuleBlock = IErasedWrapper.wrappedMap (_.body >> unbox<Ts.ModuleBlock>)
        let nodeKey: Node.ModuleDeclaration -> _ = Internal.getNodeKey

    module FunctionDeclaration =
        let inline create program node: Node.FunctionDeclaration = IErasedWrapper.create program node
        let typeParameters: Node.FunctionDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let embeddedSymbol: Node.FunctionDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.FunctionDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        // always provides a value in declaration files
        let name: Node.FunctionDeclaration -> Node.Identifier = IErasedWrapper.wrappedMapMaybe _.name >> Option.defaultWith (fun () -> failwith "FunctionDeclaration should have name in type declaration files")
        // always provides a value in declaration files
        let type': Node.FunctionDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program) >> Option.defaultWith (fun () -> failwith "FunctionDeclaration should have type in type declaration files"))
        let parameters: Node.FunctionDeclaration -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        [<System.Obsolete("Never seems to return true"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.FunctionDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        let nodeKey: Node.FunctionDeclaration -> _ = Internal.getNodeKey
    module Variable =
        let inline create program node: Node.Variable = IErasedWrapper.create program node
        let tryCreate program (node: Ts.Node) =
            if not <| ts.isVariableDeclaration node then None else
            node :?> Ts.VariableDeclaration
            |> create program
            |> Some
        let type': Node.Variable -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        let name: Node.Variable -> _ = _.MapWithProgram(fun program ->
            _.name
            >> function
                | Patterns.Node.BindingNamePatterns.Identifier identNode ->
                    Identifier.create program identNode
                    |> Choice1Of2
                | Patterns.Node.BindingNamePatterns.ObjectBindingPattern objectBindingPattern ->
                    ObjectBindingPattern.create program objectBindingPattern
                    |> ObjectBindingPattern.toBindingPattern
                    |> Choice2Of2
                | Patterns.Node.BindingNamePatterns.ArrayBindingPattern arrayBindingPattern ->
                    ArrayBindingPattern.create program arrayBindingPattern
                    |> ArrayBindingPattern.toBindingPattern
                    |> Choice2Of2
            )
        // never seems to return true
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let isBindingLike: Node.Variable -> _ = name >> _.IsChoice2Of2
        let embeddedSymbol: Node.Variable -> _ = Internal.unsafeGetEmbeddedSymbol
        let parent: Node.Variable -> Node.VariableDeclarationList = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.VariableDeclarationList>)
        let nodeKey: Node.Variable -> _ = Internal.getNodeKey
    module ClassDeclaration =
        let inline create program node: Node.ClassDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.ClassDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let typeParameters: Node.ClassDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let members: Node.ClassDeclaration -> _ = _.MapWithProgram(fun program ->
            _.members.AsArray
            >> Array.map (ClassMemberKind.create program)
            >> NonEmptyArray.create)
        let name: Node.ClassDeclaration -> _ = _.MapWithProgram(fun program -> _.name >> Option.map (Identifier.create program))
        let modifiers: Node.ClassDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let heritage: Node.ClassDeclaration -> _ = _.MapWithProgram(fun program ->
            _.heritageClauses
            >> Option.bind NonEmptyArray.create
            >> Option.map (NonEmptyArray.map (
                    HeritageClause.create program
                    >> HeritageClauseKind.fromHeritageClause
                    )
                )
            >> Option.bind ClassLikeHeritageClause.tryFromHeritageClauses
            )
        let nodeKey: Node.ClassDeclaration -> _ = Internal.getNodeKey
    module InterfaceDeclaration =
        let inline create program node: Node.InterfaceDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.InterfaceDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let typeParameters: Node.InterfaceDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let heritage: Node.InterfaceDeclaration -> _ = _.MapWithProgram(fun program ->
            _.heritageClauses
            >> Option.bind NonEmptyArray.create
            >> Option.map (NonEmptyArray.map (
                HeritageClause.create program
                >> HeritageClauseKind.fromHeritageClause
                ))
            >> Option.bind (_.Value >> TypeHeritageClause.tryFromHeritageClauseKind)
            )
        let members: Node.InterfaceDeclaration -> _ = _.MapWithProgram(fun program ->
            _.members.AsArray
            >> Array.map (TypeMemberKind.create program)
            >> NonEmptyArray.create
            )
        let name: Node.InterfaceDeclaration -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let modifiers: Node.InterfaceDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let nodeKey: Node.InterfaceDeclaration -> _ = Internal.getNodeKey
    module EnumDeclaration =
        let inline create program node: Node.EnumDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.EnumDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let name: Node.EnumDeclaration -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let modifiers: Node.EnumDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let members: Node.EnumDeclaration -> Node.EnumMember NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.members
        let nodeKey: Node.EnumDeclaration -> _ = Internal.getNodeKey
    module TypeAliasDeclaration =
        let inline create program node: Node.TypeAliasDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.TypeAliasDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let typeParameters: Node.TypeAliasDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let type': Node.TypeAliasDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Type.create program)
        let name: Node.TypeAliasDeclaration -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let modifiers: Node.TypeAliasDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let nodeKey: Node.TypeAliasDeclaration -> _ = Internal.getNodeKey
    module EnumMember =
        let inline create program node: Node.EnumMember = IErasedWrapper.create program node
        let tryCreate program (node: Ts.Node) = if ts.isEnumMember node then Some (create program (node :?> Ts.EnumMember)) else None
        let embeddedSymbol: Node.EnumMember -> _ = Internal.unsafeGetEmbeddedSymbol
        let getSymbol: Node.EnumMember -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> program.getTypeChecker().getSymbolAtLocation
            >> Option.defaultWith (fun () -> failwith "EnumMember should have symbol")
            )
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        let isCanonical enumMember =
            getSymbol enumMember
            |> _.valueDeclaration
            |> Option.exists (NodeKey.fromNode >> (=) (NodeKey.fromNode enumMember.Value))
        let getType: Node.EnumMember -> _ = _.MapWithChecker(_.getTypeAtLocation)
        let getValue: Node.EnumMember -> _ =
            getType >> fun typ ->
                if typ.flags.HasFlag TF.NumberLiteral then
                    let typ = typ :?> Ts.NumberLiteralType
                    if JS.Constructors.Number.isSafeInteger typ.value
                    then Choice2Of3 (int typ.value)
                    else Choice3Of3 (float typ.value)
                else
                    let typ = typ :?> Ts.StringLiteralType
                    Choice1Of3 typ.value
        let name: Node.EnumMember -> _ = _.MapWithProgram(fun program ->
            _.name
            >> unbox<Ts.Node>
            >> IdentifierLiteralKind.unsafeCreate program
            )
        let parent: Node.EnumMember -> Node.EnumDeclaration = IErasedWrapper.wrappedMap _.parent
        let nodeKey: Node.EnumMember -> _ = Internal.getNodeKey
    module ImportSpecifier =
        let inline create program node: Node.ImportSpecifier = IErasedWrapper.create program node
        let embeddedSymbol: Node.ImportSpecifier -> _ = Internal.unsafeGetEmbeddedSymbol
        let name: Node.ImportSpecifier -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let parent: Node.ImportSpecifier -> Node.NamedImports = IErasedWrapper.wrappedMap _.parent
        let propertyName: Node.ImportSpecifier -> Node.Identifier option = IErasedWrapper.wrappedMapMaybe (_.propertyName >> unbox<Ts.Identifier option>)
        let isTypeOnly: Node.ImportSpecifier -> _ = IErasedWrapper.map _.isTypeOnly
        let nodeKey: Node.ImportSpecifier -> _ = Internal.getNodeKey
    module ExportSpecifier =
        let inline create program node: Node.ExportSpecifier = IErasedWrapper.create program node
        let name: Node.ExportSpecifier -> Node.Identifier = IErasedWrapper.wrappedMap (_.name >> unbox<Ts.Identifier>)
        let parent: Node.ExportSpecifier -> Node.NamedExports = IErasedWrapper.wrappedMap _.parent
        let propertyName: Node.ExportSpecifier -> Node.Identifier option = IErasedWrapper.wrappedMapMaybe (_.propertyName >> unbox<Ts.Identifier option>)
        let isTypeOnly: Node.ExportSpecifier -> _ = IErasedWrapper.map _.isTypeOnly
        let embeddedSymbol: Node.ExportSpecifier -> _ = Internal.unsafeGetEmbeddedSymbol
        let nodeKey: Node.ExportSpecifier -> _ = Internal.getNodeKey
    module NamespaceImport =
        let inline create program node: Node.NamespaceImport = IErasedWrapper.create program node
        let name: Node.NamespaceImport -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let parent: Node.NamespaceImport -> Node.ImportClause = IErasedWrapper.wrappedMap _.parent
        let nodeKey: Node.NamespaceImport -> _ = Internal.getNodeKey
        let embeddedSymbol: Node.NamespaceImport -> _ = Internal.unsafeGetEmbeddedSymbol
    module ExportAssignment =
        let inline create program node: Node.ExportAssignment = IErasedWrapper.create program node
        let expression: Node.ExportAssignment -> _ = _.MapWithProgram(fun program -> _.expression >> IdentifierExpressionKind.unsafeCreate program)
        let isExportEquals: Node.ExportAssignment -> _ = IErasedWrapper.map _.isExportEquals.IsSome
        [<System.Obsolete("Never provides a value in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let modifiers: Node.ExportAssignment -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.ExportAssignment -> Node.ModuleKind = _.MapWithProgram(fun program -> _.parent >> ModuleKind.unsafeCreate program)
        [<System.Obsolete("Never provides a value in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let name: Node.ExportAssignment -> Node.Identifier option = IErasedWrapper.wrappedMapMaybe (_.name >> unbox<Ts.Identifier option>)
        let nodeKey: Node.ExportAssignment -> _ = Internal.getNodeKey
        let embeddedSymbol: Node.ExportAssignment -> _ = Internal.unsafeGetEmbeddedSymbol
    module NamespaceExport =
        let inline create program node: Node.NamespaceExport = IErasedWrapper.create program node
        let embeddedSymbol: Node.NamespaceExport -> _ = Internal.unsafeGetEmbeddedSymbol
        let name: Node.NamespaceExport -> Node.Identifier = IErasedWrapper.wrappedMap (_.name >> unbox<Ts.Identifier>)
        let parent: Node.NamespaceExport -> Node.ExportDeclaration = IErasedWrapper.wrappedMap _.parent
        let nodeKey: Node.NamespaceExport -> _ = Internal.getNodeKey
    module ConstructorDeclaration =
        let inline create program node: Node.ConstructorDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.ConstructorDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        let modifiers: Node.ConstructorDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let parent: Node.ConstructorDeclaration -> Node.ClassDeclaration = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.ClassDeclaration>)
        [<System.Obsolete("Never returns a value in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let type': Node.ConstructorDeclaration -> _ = _.MapWithProgram(fun program -> _.``type`` >> Option.map (Type.create program))
        [<System.Obsolete("Never returns a value in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let name: Node.ConstructorDeclaration -> Node.IdentifierKind option = _.MapWithProgram(fun program ->
            _.name
            >> Option.map (
                unbox<Ts.Node>
                >> IdentifierKind.tryCreate program
                >> Option.defaultWith (fun () -> failwith "ConstructorDeclaration should have name parse correctly to identifier kind")
                )
            )
        let parameters: Node.ConstructorDeclaration -> Node.ParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.parameters
        [<System.Obsolete("Never returns true in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isOptional: Node.ConstructorDeclaration -> _ = IErasedWrapper.map _.questionToken.IsSome
        [<System.Obsolete("Never returns a value in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let typeParameters: Node.ConstructorDeclaration -> Node.TypeParameterDeclaration NonEmptyArray option = IErasedWrapper.wrappedArrayMap (unbox >> ts.getEffectiveTypeParameterDeclarations)
        let nodeKey: Node.ConstructorDeclaration -> _ = Internal.getNodeKey
    module ImportClause =
        let inline create program node: Node.ImportClause = IErasedWrapper.create program node
        let name: Node.ImportClause -> Node.Identifier option = IErasedWrapper.wrappedMapMaybe _.name
        let parent: Node.ImportClause -> Node.ImportDeclaration = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.ImportDeclaration>)
        let namedBindings: Node.ImportClause -> _ = _.MapWithProgram(fun program ->
            _.namedBindings
            >> Option.map (
                unbox<Ts.Node>
                >> function
                    | Patterns.Node.NamedImports namedImports -> NamedImports.create program namedImports |> Choice1Of2
                    | Patterns.Node.NamespaceImport namespaceImport -> NamespaceImport.create program namespaceImport |> Choice2Of2
                    | _ -> failwith "ImportClause should have named bindings be either named imports or namespace import"
                )
            )
        let nodeKey: Node.ImportClause -> _ = Internal.getNodeKey
        /// <summary>
        /// Symbol present if it also has an identifier
        /// </summary>
        let embeddedSymbol: Node.ImportClause -> _ = Internal.getEmbeddedSymbol
        let embeddedSymbolKind (clause: Node.ImportClause) =
            embeddedSymbol clause
            |> Option.map (Symbol.Kind.createFromSymbol clause.Program)
    module ImportEqualsDeclaration =
        let inline create program node: Node.ImportEqualsDeclaration = IErasedWrapper.create program node
        let embeddedSymbol: Node.ImportEqualsDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
        [<System.Obsolete("Never returns true in practice"); EditorBrowsable(EditorBrowsableState.Never)>]
        let isTypeOnly: Node.ImportEqualsDeclaration -> _ = IErasedWrapper.map _.isTypeOnly
        let modifiers: Node.ImportEqualsDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let moduleReference: Node.ImportEqualsDeclaration -> _ = _.MapWithProgram(fun program ->
            _.moduleReference
            >> unbox<Ts.Node>
            >> function
                | Patterns.Node.Identifier _ | Patterns.Node.QualifiedName _ as identifierNode ->
                    unbox<Ts.EntityName> identifierNode
                    |> IdentifierKind.fromEntityName program
                    |> Choice1Of2
                | Patterns.Node.ExternalModuleReference externalModuleReference ->
                    ExternalModuleReference.create program externalModuleReference
                    |> Choice2Of2
                | _ -> failwith "ImportEqualsDeclaration should have module reference be either identifier or external module reference"
            )
        let name: Node.ImportEqualsDeclaration -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let parent: Node.ImportEqualsDeclaration -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> ModuleKind.unsafeCreate program
            // >> function
            //     | Patterns.Node.SourceFile node -> SourceKind.create program node |> Choice1Of2
            //     | Patterns.Node.ModuleBlock node -> ModuleBlock.create program node |> Choice2Of2
            //     | _ -> failwith "ImportEqualsDeclaration should have parent be either source file or module block"
            )
        let nodeKey: Node.ImportEqualsDeclaration -> _ = Internal.getNodeKey
    module NamespaceExportDeclaration =
        let inline create program node: Node.NamespaceExportDeclaration = IErasedWrapper.create program node
        let name: Node.NamespaceExportDeclaration -> Node.Identifier = IErasedWrapper.wrappedMap _.name
        let nodeKey: Node.NamespaceExportDeclaration -> _ = Internal.getNodeKey
        let parent: Node.NamespaceExportDeclaration -> _ = _.MapWithProgram(fun program -> _.parent >> ModuleKind.unsafeCreate program)
        let embeddedSymbol: Node.NamespaceExportDeclaration -> _ = Internal.unsafeGetEmbeddedSymbol
    module ExportDeclaration =
        let inline create program node: Node.ExportDeclaration = IErasedWrapper.create program node
        let isTypeOnly: Node.ExportDeclaration -> _ = IErasedWrapper.map _.isTypeOnly
        let parent: Node.ExportDeclaration -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> ModuleKind.unsafeCreate program
            // >> function
            //     | Patterns.Node.SourceFile node -> SourceKind.create program node |> Choice1Of2
            //     | Patterns.Node.ModuleBlock node -> ModuleBlock.create program node |> Choice2Of2
            //     | _ -> failwith "ExportDeclaration should have parent be either source file or module block"
            )
        let moduleSpecifier: Node.ExportDeclaration -> _ = _.MapWithProgram(fun program ->
            _.moduleSpecifier
            >> Option.map (unbox<Ts.Node> >> IdentifierLiteralKind.unsafeCreate program)
            )
        [<System.Obsolete("Corpus of tests have no example where provides a value")>]
        let modifiers: Node.ExportDeclaration -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let exportClause: Node.ExportDeclaration -> _ = _.MapWithProgram(fun program ->
            _.exportClause
            >> Option.map (
                unbox<Ts.Node>
                >> function
                    | Patterns.Node.NamedExports namedExports -> NamedExports.create program namedExports |> Choice1Of2
                    | Patterns.Node.NamespaceExport namespaceExport -> NamespaceExport.create program namespaceExport |> Choice2Of2
                    | _ -> failwith "ExportDeclaration should have export clause be either named exports or namespace export"
                )
            )
        [<System.Obsolete("Corpus of tests have no example where provides a value")>]
        let name: Node.ExportDeclaration -> Node.Identifier option = IErasedWrapper.wrappedMapMaybe (_.name >> unbox<Ts.Identifier option>)
        let nodeKey: Node.ExportDeclaration -> _ = Internal.getNodeKey
        /// <summary>
        /// Symbol not present if it has a string literal as a module specifier, or the module specifier is absent.
        /// </summary>
        let embeddedSymbol: Node.ExportDeclaration -> _ = Internal.getEmbeddedSymbol
        let (|EmbeddedSymbol|_|): Node.ExportDeclaration -> _ = embeddedSymbol
        let (|ModuleSpecifier|_|): Node.ExportDeclaration -> _ = moduleSpecifier
    module ObjectBindingPattern =
        let inline create program node: Node.ObjectBindingPattern = IErasedWrapper.create program node
        let inline toBindingPattern node = Node.BindingPattern.Object node
        let elements: Node.ObjectBindingPattern -> Node.BindingElement NonEmptyArray = IErasedWrapper.wrappedArrayMap _.elements >> Option.defaultWith(fun () -> failwith "Expected object binding pattern to have elements")
        let parent: Node.ObjectBindingPattern -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                // never true in practice
                // | Patterns.Node.VariableDeclaration variableDeclaration -> Variable.create program variableDeclaration |> Choice1Of3
                | Patterns.Node.Parameter node -> ParameterDeclaration.create program node |> Choice1Of2
                | Patterns.Node.BindingElement bindingElement -> BindingElement.create program bindingElement |> Choice2Of2
                | _ -> failwith "ObjectBindingPattern should have parent be either variable declaration, parameter or binding element"
            )
        let nodeKey: Node.ObjectBindingPattern -> _ = Internal.getNodeKey
    module ArrayBindingPattern =
        let inline create program node: Node.ArrayBindingPattern = IErasedWrapper.create program node
        let inline toBindingPattern node = Node.BindingPattern.Array node
        let elements: Node.ArrayBindingPattern -> Node.BindingElement NonEmptyArray = IErasedWrapper.wrappedArrayMap (_.elements >> unbox<Ts.BindingElement ResizeArray>) >> Option.defaultWith(fun () -> failwith "Expected array binding pattern to have elements")
        let parent: Node.ArrayBindingPattern -> _ = _.MapWithProgram(fun program ->
            _.parent
            >> unbox<Ts.Node>
            >> function
                // never true in practice
                // | Patterns.Node.VariableDeclaration variableDeclaration -> Variable.create program variableDeclaration |> Choice1Of3
                | Patterns.Node.Parameter node -> ParameterDeclaration.create program node 
                // | Patterns.Node.BindingElement bindingElement -> BindingElement.create program bindingElement |> Choice2Of2
                | _ -> failwith "ArrayBindingPattern should have parent be either variable declaration, parameter or binding element"
            )
        let nodeKey: Node.ArrayBindingPattern -> _ = Internal.getNodeKey
    module PrefixUnaryExpression =
        open Xantham.TypeScript.Node
        let inline create program node: Node.PrefixUnaryExpression = IErasedWrapper.create program node
        let isMinusOperator: Node.PrefixUnaryExpression -> _ = IErasedWrapper.map _.operator.Equals(SK.MinusToken)
        let asNodeLiteral: Node.PrefixUnaryExpression -> _ = _.MapWithProgram(fun program -> _.operand >> Literal.unsafeCreate program)
        let literal: Node.PrefixUnaryExpression -> _ = asNodeLiteral >> function
            | Literal.Numeric literal -> Choice1Of2 literal
            | Literal.BigInt literal -> Choice2Of2 literal
            | literal -> failwith $"Expected prefix unary expression literal to be numeric or bigint, got {literal}"
        let nodeKey: Node.PrefixUnaryExpression -> _ = Internal.getNodeKey
    module PropertyAccessExpression =
        let inline create program node: Node.PropertyAccessExpression = IErasedWrapper.create program node
        let name: Node.PropertyAccessExpression -> Node.Identifier = IErasedWrapper.wrappedMap (_.name >> unbox<Ts.Identifier>)
        let expression: Node.PropertyAccessExpression -> _ = _.MapWithProgram(fun program -> _.expression >> IdentifierKind.unsafeCreate program)
        let nodeKey: Node.PropertyAccessExpression -> _ = Internal.getNodeKey
    module TemplateHead =
        let inline create program node: Node.TemplateHead = IErasedWrapper.create program node
        let text: Node.TemplateHead -> _ = IErasedWrapper.map _.text
        let nodeKey: Node.TemplateHead -> _ = Internal.getNodeKey
        let parent: Node.TemplateHead -> Node.TemplateLiteralType = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.TemplateLiteralTypeNode>)
    module TemplateMiddle =
        let inline create program node: Node.TemplateMiddle = IErasedWrapper.create program node
        let parent: Node.TemplateMiddle -> Node.TemplateLiteralTypeSpan = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.TemplateLiteralTypeSpan>)
        let text: Node.TemplateMiddle -> _ = IErasedWrapper.map _.text
        let nodeKey: Node.TemplateMiddle -> _ = Internal.getNodeKey
    module TemplateTail =
        let inline create program node: Node.TemplateTail = IErasedWrapper.create program node
        let parent: Node.TemplateTail -> Node.TemplateLiteralTypeSpan = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.TemplateLiteralTypeSpan>)
        let text: Node.TemplateTail -> _ = IErasedWrapper.map _.text
        let nodeKey: Node.TemplateTail -> _ = Internal.getNodeKey
    module ComputedPropertyName =
        let inline create program node: Node.ComputedPropertyName = IErasedWrapper.create program node
        let expression: Node.ComputedPropertyName -> _  = _.MapWithProgram(fun program ->
            _.expression
            >> function
                | Patterns.Node.PropertyAccessExpression propNode ->
                    PropertyAccessExpression.create program propNode
                    |> Choice1Of2
                | Patterns.Node.Identifier identifierNode ->
                    Identifier.create program identifierNode
                    |> Choice2Of2
                | _ -> failwith "Computed propertyName should be either PropertyAccessExpression or Identifier"
            )
        let toString: Node.ComputedPropertyName -> string = IErasedWrapper.map _.getText()
        let parent: Node.ComputedPropertyName -> _ = _.MapWithProgram(fun program -> _.parent >> DeclarationKind.create program)
        let nodeKey: Node.ComputedPropertyName -> _ = Internal.getNodeKey
        let trySymbol (computedPropertyName: Node.ComputedPropertyName) =
            computedPropertyName |> INode.trySymbolKindAtLocation
            
    module Identifier =
        let inline create program node: Node.Identifier = IErasedWrapper.create program node
        let toString: Node.Identifier -> string = IErasedWrapper.map _.text
        let text = toString
        let trySymbol (identifier: Node.Identifier) = identifier |> INode.trySymbolKindAtLocation
        let nodeKey: Node.Identifier -> _ = Internal.getNodeKey
    module QualifiedName =
        let inline create program node: Node.QualifiedName = IErasedWrapper.create program node
        let toStringArray: Node.QualifiedName -> string array = fun node ->
            let qualification =
                Some node.Value.left
                |> Array.unfold (function
                    | Some (Patterns.Node.EntityNamePatterns.Identifier node) -> Some (node.text, None)
                    | Some (Patterns.Node.EntityNamePatterns.QualifiedName node) -> Some (node.right.text, Some node.left)
                    | None -> None
                    )
            qualification
            |> Array.insertAt 0 node.Value.right.text
            |> Array.rev
        let toString: Node.QualifiedName -> string = toStringArray >> String.concat "."
        let terminalString: Node.QualifiedName -> string = IErasedWrapper.map _.right.text
        let nodeKey: Node.QualifiedName -> _ = Internal.getNodeKey
        let trySymbol (qualifiedName: Node.QualifiedName): Symbol.Kind option = INode.trySymbolKindAtLocation qualifiedName 
    module JSDocMemberName =
        let inline create program node: Node.JSDocMemberName = IErasedWrapper.create program node
        let toStringArray: Node.JSDocMemberName -> string array = IErasedWrapper.map (fun node ->
            let qualification =
                Some node.left
                |> Array.unfold (unbox >> function
                    | Some (Patterns.Node.Identifier node) -> Some (node.text, None)
                    | Some (Patterns.Node.QualifiedName node) -> Some (node.right.text, U2.Case1 node.left |> Some)
                    | Some (Patterns.Node.JSDocMemberName node) -> Some (node.right.text, Some node.left)
                    | _ -> None
                    )
            qualification
            |> Array.insertAt 0 node.right.text
            |> Array.rev
            )
        let toString: Node.JSDocMemberName -> string = toStringArray >> String.concat "."
        let terminalString: Node.JSDocMemberName -> string = IErasedWrapper.map _.right.text
        let nodeKey: Node.JSDocMemberName -> _ = Internal.getNodeKey
    module VariableStatement =
        let inline create program node: Node.VariableStatement = IErasedWrapper.create program node
        let modifiers: Node.VariableStatement -> _ = _.MapWithProgram(fun program ->
            _.modifiers
            >> Option.bind (
                _.AsArray
                >> Array.map (ModifierKeyword.createFromModifierLike program)
                >> NonEmptyArray.create
                )
            )
        let declarationList: Node.VariableStatement -> Node.VariableDeclarationList = IErasedWrapper.wrappedMap _.declarationList
        let nodeKey: Node.VariableStatement -> _ = Internal.getNodeKey
    module VariableDeclarationList =
        let inline create program node: Node.VariableDeclarationList = IErasedWrapper.create program node
        let declarations: Node.VariableDeclarationList -> Node.Variable NonEmptyArray = IErasedWrapper.wrappedArrayMap _.declarations >> Option.defaultWith(fun () -> failwith "Expected variable declaration list to have declarations")
        let parent: Node.VariableDeclarationList -> Node.VariableStatement = IErasedWrapper.wrappedMap (_.parent >> unbox<Ts.VariableStatement>)
        let nodeKey: Node.VariableDeclarationList -> _ = Internal.getNodeKey
    module ModuleBlock =
        let inline create program node: Node.ModuleBlock = IErasedWrapper.create program node
        let parent: Node.ModuleBlock -> Node.ModuleDeclaration = IErasedWrapper.wrappedMap _.parent
        let statements: Node.ModuleBlock -> TopLevelStatements NonEmptyArray option = _.MapWithProgram(fun program ->
            _.statements
            >> _.AsArray
            >> Array.map (TopLevelStatements.Create >> InlinedProgram.inject program)
            >> NonEmptyArray.create
            )
        let nodeKey: Node.ModuleBlock -> _ = Internal.getNodeKey
    module HeritageClause =
        let inline create program node: Node.HeritageClause = IErasedWrapper.create program node
        let isExtendsClause: Node.HeritageClause -> bool = IErasedWrapper.map _.token.Equals(SK.ExtendsKeyword)
        let isImplementsClause: Node.HeritageClause -> bool = IErasedWrapper.map _.token.Equals(SK.ImplementsKeyword)
        let types: Node.HeritageClause -> _ =
            _.WrappedMap<Node.ExpressionWithTypeArguments,_>(_.types)
            >> Option.defaultWith (fun () -> failwith "HeritageClause should have types")
        let parent: Node.HeritageClause -> _ = _.MapWithProgram(fun program ->
            _.parent >> unbox<Ts.Node> >> function
                | Patterns.Node.ClassDeclaration node ->
                    ClassDeclaration.create program node
                    |> Choice1Of2
                | Patterns.Node.InterfaceDeclaration node ->
                    InterfaceDeclaration.create program node
                    |> Choice2Of2
                | _ -> failwith "HeritageClause parent should be either ClassDeclaration or InterfaceDeclaration"
            )
        let isParentInterface: Node.HeritageClause -> bool = parent >> _.IsChoice2Of2
        let isParentClass: Node.HeritageClause -> bool = parent >> _.IsChoice1Of2
        let (|Implements|Extends|) = function
            | clause when isExtendsClause clause -> Extends(types clause)
            | clause when isImplementsClause clause -> Implements(types clause)
            | _ -> failwith "HeritageClause should be either Implements or Extends"
        let nodeKey: Node.HeritageClause -> _ = Internal.getNodeKey
    module NamedExports =
        let inline create program node: Node.NamedExports = IErasedWrapper.create program node
        let parent: Node.NamedExports -> Node.ExportDeclaration = IErasedWrapper.wrappedMap _.parent
        let elements: Node.NamedExports -> Node.ExportSpecifier NonEmptyArray option = IErasedWrapper.wrappedArrayMap _.elements
        let nodeKey: Node.NamedExports -> _ = Internal.getNodeKey
    module ImportDeclaration =
        let inline create program node: Node.ImportDeclaration = IErasedWrapper.create program node
        let importClause: Node.ImportDeclaration -> Node.ImportClause option = IErasedWrapper.wrappedMapMaybe _.importClause
        let moduleSpecifier: Node.ImportDeclaration -> _ = _.MapWithProgram(fun program -> _.moduleSpecifier >> IdentifierLiteralKind.tryCreate program >> Option.defaultWith (fun () -> failwith "ImportDeclaration should have module specifier parse correctly to identifier kind"))
        let parent: Node.ImportDeclaration -> _ = _.MapWithProgram(fun program -> _.parent >> unbox<Ts.Node> >> ModuleKind.unsafeCreate program)
            // | Patterns.Node.SourceFile node -> SourceKind.create program node |> Choice1Of2
            // | Patterns.Node.ModuleBlock node -> ModuleBlock.create program node |> Choice2Of2
            // | _ -> failwith "ImportDeclaration should have parent be source file or module block")
        let nodeKey: Node.ImportDeclaration -> _ = Internal.getNodeKey
    module NamedImports =
        let inline create program node: Node.NamedImports = IErasedWrapper.create program node
        let elements: Node.NamedImports -> Node.ImportSpecifier NonEmptyArray = IErasedWrapper.wrappedArrayMap _.elements >> Option.defaultWith(fun () -> failwith "Expected named imports to have elements")
        let parent: Node.NamedImports -> Node.ImportClause = IErasedWrapper.wrappedMap _.parent
        let nodeKey: Node.NamedImports -> _ = Internal.getNodeKey
    module ExternalModuleReference =
        let inline create program node: Node.ExternalModuleReference = IErasedWrapper.create program node
        let parent: Node.ExternalModuleReference -> Node.ImportEqualsDeclaration = IErasedWrapper.wrappedMap _.parent
        let expression: Node.ExternalModuleReference -> _ = _.MapWithProgram(fun program ->
            _.expression
            >> IdentifierLiteralKind.unsafeCreate program
            )
        let nodeKey: Node.ExternalModuleReference -> _ = Internal.getNodeKey
    module SemanticToken =
        let internal kindMap = Dictionary [
            SK.QuestionToken ==>! Node.SemanticToken.Optional
            SK.AssertsKeyword ==>! Node.SemanticToken.Asserts
            SK.DotDotDotToken ==>! Node.SemanticToken.Spread
            SK.EndOfFileToken ==>! Node.SemanticToken.EoF
            SK.MinusToken ==>! Node.SemanticToken.Minus
            SK.PrivateIdentifier ==>! Node.SemanticToken.PrivateField
        ]
        let kindSet = dictToSet kindMap
        let isSemanticToken node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isSemanticToken program node
    module BindingPattern =
        let fromObjectBindingPattern = ObjectBindingPattern.toBindingPattern 
        let fromArrayBindingPattern = ArrayBindingPattern.toBindingPattern
        let internal kindMap: Dictionary<_, _ -> _ -> Node.BindingPattern> = Dictionary [
            SK.ObjectBindingPattern ==* (ObjectBindingPattern.create, Node.BindingPattern.Object)
            SK.ArrayBindingPattern ==* (ArrayBindingPattern.create, Node.BindingPattern.Array)
        ]
        let kindSet = dictToSet kindMap
        let isBindingPattern node = setContainsNodeKind kindSet node
        let tryCreate (program: Ts.Program) (node: Ts.Node) = tryCreateWithNodeCheck kindMap isBindingPattern program node
        let create program (node: Ts.BindingPattern) =
            tryCreate program !!node
            |> Option.defaultWith (fun () -> failwith "BindingPattern should be either ObjectBindingPattern or ArrayBindingPattern")
    module Expression =
        // let fromPrefixUnaryExpression = Node.Expression.PrefixUnary
        let fromPropertyAccessExpression = Node.Expression.PropertyAccess
        let internal kindMap: Dictionary<_, _ -> _ -> Node.Expression> = Dictionary [
            // SK.PrefixUnaryExpression ==* (PrefixUnaryExpression.create, Node.Expression.PrefixUnary)
            SK.PropertyAccessExpression ==* (PropertyAccessExpression.create, Node.Expression.PropertyAccess)
        ]
        let kindSet = dictToSet kindMap
        let isExpression node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isExpression program node
#nowarn 40
    module IdentifierExpressionKind =
        let internal kindMap: Dictionary<_, _ -> _ -> Node.IdentifierExpressionKind> = Dictionary [
            SK.PropertyAccessExpression ==* (PropertyAccessExpression.create, Node.IdentifierExpressionKind.Expression)
            yield! fetchMapAndInject IdentifierKind.kindMap Node.IdentifierExpressionKind.Identifier
        ]
        let kindSet = dictToSet kindMap
        let isIdentifierExpressionKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isIdentifierExpressionKind program node
        let unsafeCreate program (node: Ts.Node) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwithf "IdentifierExpressionKind should be either Identifier or PropertyAccessExpression. Got: %s" node.kind.Name)
#warnon 40
    module TemplatePart =
        let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Node.TemplatePart> = Dictionary [
            Ts.SyntaxKind.TemplateHead ==* (TemplateHead.create, Node.TemplatePart.Head)
            Ts.SyntaxKind.TemplateMiddle ==* (TemplateMiddle.create, Node.TemplatePart.Middle)
            Ts.SyntaxKind.TemplateTail ==* (TemplateTail.create, Node.TemplatePart.Tail) 
        ]
        let kindSet = dictToSet kindMap
        let isTemplatePart node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isTemplatePart program node
        let unsafeCreate program (node: Ts.Node) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwithf "TemplatePart.create failed for node: %A" node)
    module IdentifierKind =
        let internal kindMap = Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Node.IdentifierKind> [
            Ts.SyntaxKind.ComputedPropertyName ==* (ComputedPropertyName.create, Node.IdentifierKind.ComputedPropertyName)
            Ts.SyntaxKind.Identifier ==* (Identifier.create, Node.IdentifierKind.Identifier)
            Ts.SyntaxKind.QualifiedName ==* (QualifiedName.create, Node.IdentifierKind.QualifiedName)
        ]
        let kindSet = dictToSet kindMap
        let isIdentifierKind node = setContainsNodeKind kindSet node
        let tryCreate program (node: Ts.Node) = tryCreateWithNodeCheck kindMap isIdentifierKind program node
        let unsafeCreate program (node: Ts.Node) = tryCreate program node |> Option.defaultWith (fun () -> failwithf "IdentifierKind.create failed for node: %A" node.kind.Name)
        let fromEntityName program (node: Ts.EntityName) =
            tryCreate program (unbox<Ts.Node> node)
            |> Option.defaultWith (fun () -> failwith "EntityName should be either Identifier, QualifiedName or ComputedPropertyName")
        let getSymbolKind (node: Node.IdentifierKind) =
            match node with
            | Node.IdentifierKind.Identifier identifierNode -> Identifier.trySymbol identifierNode
            | Node.IdentifierKind.QualifiedName qualifiedNameNode -> QualifiedName.trySymbol qualifiedNameNode
            | Node.IdentifierKind.ComputedPropertyName computedPropertyName -> ComputedPropertyName.trySymbol computedPropertyName
        let toStringArray = function
            | Node.IdentifierKind.Identifier identifier -> Identifier.toString identifier |> Array.singleton
            | Node.IdentifierKind.QualifiedName qualifiedName -> QualifiedName.toStringArray qualifiedName
            | Node.IdentifierKind.ComputedPropertyName computedPropertyName -> ComputedPropertyName.toString computedPropertyName |> Array.singleton
        let toString = function
            | Node.IdentifierKind.Identifier identifier -> Identifier.toString identifier 
            | Node.IdentifierKind.QualifiedName qualifiedName -> QualifiedName.toString qualifiedName
            | Node.IdentifierKind.ComputedPropertyName computedPropertyName -> ComputedPropertyName.toString computedPropertyName
#nowarn 40
    module IdentifierLiteralKind =
        let internal kindMap: Dictionary<SK, Ts.Program -> obj -> Node.IdentifierLiteralKind> = Dictionary [
            SK.StringLiteral ==* (StringLiteral.create, Node.IdentifierLiteralKind.StringLiteral)
            yield! fetchMapAndInject IdentifierKind.kindMap Node.IdentifierLiteralKind.Identifier
        ]
        let kindSet = dictToSet kindMap
        let isIdentifierLiteralKind node = setContainsNodeKind kindSet node
        let tryCreate program (node: Ts.Node) = tryCreateWithNodeCheck kindMap isIdentifierLiteralKind program node
        let unsafeCreate program node = tryCreate program node |> Option.defaultWith (fun () -> failwithf "IdentifierLiteralKind.create failed for node: %A" node)
        let toUnderlyingValue = function
            | Node.IdentifierLiteralKind.StringLiteral stringLiteral -> stringLiteral.Value :> Ts.Node
            | Node.IdentifierLiteralKind.Identifier identifier -> identifier.Value
        let toString = function
            | Node.IdentifierLiteralKind.StringLiteral stringLiteral -> StringLiteral.value stringLiteral
            | Node.IdentifierLiteralKind.Identifier identifier -> IdentifierKind.toString identifier
        let toStringArray = function
            | Node.IdentifierLiteralKind.StringLiteral stringLiteral -> StringLiteral.value stringLiteral |> Array.singleton
            | Node.IdentifierLiteralKind.Identifier identifier -> IdentifierKind.toStringArray identifier
        let nodeKey = toUnderlyingValue >> NodeKey.fromNode
#warnon 40
    module Container =
        let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> Node.Container> = Dictionary [
            Ts.SyntaxKind.VariableStatement ==* (VariableStatement.create, Node.Container.VariableStatement)
            Ts.SyntaxKind.VariableDeclarationList ==* (VariableDeclarationList.create, Node.Container.VariableDeclarationList)
            Ts.SyntaxKind.ModuleBlock ==* (ModuleBlock.create, Node.Container.ModuleBlock)
        ]
        let kindSet = dictToSet kindMap
        let isContainer node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isContainer program node
    
    module HeritageClauseKind =
        let fromHeritageClause (clause: Node.HeritageClause) =
            match clause with
            | HeritageClause.Implements types ->
                Node.HeritageClauseKind.Implements(types, clause)
            | HeritageClause.Extends types ->
                Node.HeritageClauseKind.Extends(types, clause)
            |> InlinedProgram.inject clause.Program
        let tryCreate program (node: Ts.Node) =
            if node.kind.Equals(SK.HeritageClause) then
                HeritageClause.create program !!node
                |> fromHeritageClause
                |> Some
            else None
        let heritageClause = function
            | Node.HeritageClauseKind.Extends(_, clause) -> clause
            | Node.HeritageClauseKind.Implements(_, clause) -> clause
        let types = function
            | Node.HeritageClauseKind.Extends(types, _) -> types
            | Node.HeritageClauseKind.Implements(types, _) -> types
        let parent = heritageClause >> HeritageClause.parent
    
    module TypeHeritageClause =
        let tryFromHeritageClauseKind clauseKind =
            match clauseKind with
            | Node.HeritageClauseKind.Extends(types, clause) ->
                Some (Node.TypeHeritageClause.Extends(types, clause))
            | _ -> None
        let tryFromHeritageClause clause =
            if HeritageClause.isImplementsClause clause then None else
            Node.TypeHeritageClause.Extends(HeritageClause.types clause, clause)
            |> Some
        let heritageClause: Node.TypeHeritageClause -> _ = function
            | Node.TypeHeritageClause.Extends(_, clause) -> clause
        let types: Node.TypeHeritageClause -> _ = function
            | Node.TypeHeritageClause.Extends(types, _) -> types
        let parent = heritageClause >> HeritageClause.parent
    module ClassLikeHeritageClause =
        let private validate = function
            | Node.HeritageClauseKind.Extends(types,clause) when HeritageClause.isParentClass clause ->
                let head,tail = NonEmptyArray.popHead types
                tail
                |> Option.iter (fun _ -> failwith "ClassLikeHeritageClause should have only one type for extends")
                Choice1Of2(head, clause)
                |> Some
            | Node.HeritageClauseKind.Implements(types, clause) when HeritageClause.isParentClass clause ->
                Choice2Of2(types, clause)
                |> Some
            | _ -> None
        let tryFromHeritageClause heritageClause =
            validate heritageClause
            |> Option.map (function
                | Choice1Of2 payload ->
                    Node.ClassLikeHeritageClause.Extends payload
                    |> InlinedProgram.inject heritageClause.Program
                | Choice2Of2 payload ->
                    Node.ClassLikeHeritageClause.Implements payload
                    |> InlinedProgram.inject heritageClause.Program
                )
        let tryFromHeritageClauses heritageClauses =
            let head, tail = NonEmptyArray.popHead heritageClauses
            tail
            |> Option.bind (_.Value >> fun clause ->
                match validate head, validate clause with
                | Some _, None ->
                    tryFromHeritageClause head
                | None, Some _ ->
                    tryFromHeritageClause clause
                | None, None -> None
                | Some l, Some r ->
                    match l,r with
                    | Choice1Of2 extendsPayload, Choice2Of2 implementsPayload
                    | Choice2Of2 implementsPayload, Choice1Of2 extendsPayload ->
                        Node.ClassLikeHeritageClause.ImplementsAndExtends(
                            fst implementsPayload,
                            fst extendsPayload,
                            snd implementsPayload,
                            snd extendsPayload
                            )
                        |> InlinedProgram.inject head.Program
                        |> Some
                    | _, _ ->
                        failwith "ClassLikeHeritageClause should have only one type for extends and implements"
                )
            |> Option.orElseWith (fun () -> tryFromHeritageClause head)
        let inline tryFromCurriedHeritageClauses clause clause2 =
            NonEmptyArray.create [ clause; clause2 ]
            |> Option.bind tryFromHeritageClauses
        let implements = function
            | Node.ClassLikeHeritageClause.Implements(types, _)
            | Node.ClassLikeHeritageClause.ImplementsAndExtends(implements = types) -> Some types
            | _ -> None
        let extends = function
            | Node.ClassLikeHeritageClause.Extends(types, _)
            | Node.ClassLikeHeritageClause.ImplementsAndExtends(extends = types) -> Some types
            | _ -> None
        let heritageClause = function
            | Node.ClassLikeHeritageClause.Implements(_, clause)
            | Node.ClassLikeHeritageClause.Extends(_, clause)
            | Node.ClassLikeHeritageClause.ImplementsAndExtends(implementsClause = clause) -> clause
        let parent = heritageClause >> HeritageClause.parent >> function
            | Choice1Of2 node -> node
            | _ -> failwith "ClassLikeHeritageClause parent should be ClassDeclaration"
    module Literal =
        let internal kindMap = Dictionary [
            SK.StringLiteral ==* (StringLiteral.create, Node.Literal.String)
            SK.NumericLiteral ==* (NumericLiteral.create, Node.Literal.Numeric)
            SK.TrueKeyword ==* (BooleanLiteral.create, Node.Literal.Boolean)
            SK.FalseKeyword ==* (BooleanLiteral.create, Node.Literal.Boolean)
            SK.NullKeyword ==>! Node.Literal.Null
            SK.BigIntLiteral ==* (BigIntLiteral.create, Node.Literal.BigInt)
            SK.NoSubstitutionTemplateLiteral ==* (NoSubstitutionTemplateLiteral.create, Node.Literal.NoSubstitutionTemplateLiteral)
            SK.PrefixUnaryExpression ==* (PrefixUnaryExpression.create, Node.Literal.PrefixUnary)
        ]
        let kindSet = dictToSet kindMap
        let isLiteral node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isLiteral program node
        let unsafeCreate program node = tryCreate program node |> Option.defaultWith (fun () -> failwithf "Literal should be created, got: %A" node.kind.Name)
    module CommentLink =
        open System
        open Xantham.TypeScript.Node
        let private flattenLinkName (linkName: U2<Ts.EntityName, Ts.JSDocMemberName> option) =
            let rec flattenEntityName acc = function
                | Patterns.Node.EntityNamePatterns.Identifier identifier ->
                    identifier.text :: acc
                | Patterns.Node.EntityNamePatterns.QualifiedName qualifiedName ->
                    qualifiedName.right.text :: (flattenEntityName acc qualifiedName.left)
            and flattenQualifiedName acc (qualifiedName: Ts.QualifiedName) =
                qualifiedName.right.text :: (flattenEntityName acc qualifiedName.left)
            and flattenJSDocMemberName acc (jsdoc: Ts.JSDocMemberName) =
                match unbox jsdoc.left with
                | Patterns.Node.JSDocMemberName node ->
                    jsdoc.right.text :: (flattenJSDocMemberName acc node)
                | node ->
                    match unbox<Ts.EntityName> node with
                    | Patterns.Node.EntityNamePatterns.Identifier identifier ->
                        jsdoc.right.text :: identifier.text :: acc
                    | Patterns.Node.EntityNamePatterns.QualifiedName qualifiedName ->
                        jsdoc.right.text :: (flattenQualifiedName acc qualifiedName)
            linkName
            |> Option.map (unbox >> function
                | Patterns.Node.Identifier _ | Patterns.Node.QualifiedName _ as node ->
                    flattenEntityName [] (unbox node)
                    |> List.rev
                | node ->
                    unbox node |> flattenJSDocMemberName []
                    |> List.rev
                )
        let unsafeCreate (program: Ts.Program) = function
            | Patterns.Node.JSDocLink _ | Patterns.Node.JSDocLinkCode _ | Patterns.Node.JSDocLinkPlain _ as node ->
                let node = node :> Ts.JSDocLink
                match 
                    node.name
                    |> Option.bind (unbox<Ts.Node> >> program.getTypeChecker().getSymbolAtLocation)
                with
                | Some symbol ->
                    Symbol.Kind.createFromSymbol program symbol
                    |> Choice1Of2
                    |> Some
                | None ->
                    node.name
                    |> flattenLinkName
                    |> Option.map (String.concat "." >> Choice2Of2)
                |> fun name ->
                    {
                        Text =
                            if String.IsNullOrWhiteSpace(node.text) then
                                None
                            else Some node.text
                        Name = name
                    }
                    |> match node with
                        | Patterns.Node.JSDocLink _ -> CommentPart.Link
                        | Patterns.Node.JSDocLinkCode _ -> CommentPart.LinkCode
                        | Patterns.Node.JSDocLinkPlain _ -> CommentPart.LinkPlain
                        | _ -> failwith "Unexpected node type"
            | _ -> failwith "Unexpected node kind"
    module CommentPart =
        let internal kindMap = Dictionary [
            SK.JSDocText ==> (fun _ -> unbox<Ts.JSDocText> >> _.text >> String.splitLines >> Node.CommentPart.Text)
            SK.JSDocLink ==> CommentLink.unsafeCreate
            SK.JSDocLinkCode ==> CommentLink.unsafeCreate
            SK.JSDocLinkPlain ==> CommentLink.unsafeCreate
        ]
        let kindSet = dictToSet kindMap
        let isCommentPart node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isCommentPart program node
        let create program (comment: Ts.JSDocComment) =
            tryCreate program !!comment
            |> Option.defaultWith (fun () -> failwith "Unexpected JSDocComment")
        let fromJSDocTag jstag: NonEmptyArray<Node.CommentPart> option =
            let tag = JSDoc.toJSDocTag jstag
            let program = jstag.Program
            tag.comment
            |> Option.map (fun comment ->
                if jsTypeof comment = "string" then
                    unbox<string> comment
                    |> String.splitLines
                    |> Node.CommentPart.Text
                    |> Array.singleton
                else
                    unbox<Ts.JSDocComment array> comment
                    |> Array.map (create program)
                )
            |> Option.defaultValue [||]
            |> NonEmptyArray.create
    module JSDoc =
        open Xantham.TypeScript.Node
        let inline (>->) a b = a ==> (fun program -> b >> InlinedProgram.inject program)
        let internal kindMap: Dictionary<Ts.SyntaxKind, Ts.Program -> obj -> JSDoc> = Dictionary [
            Ts.SyntaxKind.JSDocParameterTag >-> JSDoc.Parameter
            Ts.SyntaxKind.JSDocThrowsTag >-> JSDoc.Throws
            Ts.SyntaxKind.JSDocReturnTag >-> JSDoc.Return
            Ts.SyntaxKind.JSDocTypeTag >-> JSDoc.Type
            Ts.SyntaxKind.JSDocTemplateTag >-> JSDoc.Template
            Ts.SyntaxKind.JSDocDeprecatedTag >-> JSDoc.Deprecated
            Ts.SyntaxKind.JSDocCallbackTag >-> JSDoc.Callback
            Ts.SyntaxKind.JSDocTypedefTag >-> JSDoc.Typedef
            Ts.SyntaxKind.JSDocAugmentsTag >-> JSDoc.Augments
            Ts.SyntaxKind.JSDocSeeTag >-> JSDoc.See
            Ts.SyntaxKind.JSDocOverrideTag >-> JSDoc.Override
            Ts.SyntaxKind.JSDocClassTag >-> JSDoc.Class
            Ts.SyntaxKind.JSDocPublicTag >-> JSDoc.Public
            Ts.SyntaxKind.JSDocPrivateTag >-> JSDoc.Private
            Ts.SyntaxKind.JSDocReadonlyTag >-> JSDoc.Readonly
            Ts.SyntaxKind.JSDocImportTag >-> JSDoc.Import
            Ts.SyntaxKind.JSDocTag >-> JSDoc.Tag
            Ts.SyntaxKind.JSDocOverloadTag >-> JSDoc.Overload
        ]
        let kindSet = dictToSet kindMap
        let isJSDoc node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isJSDoc program node
        let create program (node: Ts.JSDocTag) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwith "JSDocTag should be one of the known types")
        let inline getAllTagsFor (nodeLike: ^T when ^T :> IErasedWrapper<^Node> and ^Node :> Ts.Node) =
            ts.getAllJSDocTags(nodeLike.Value, fun _ -> true).AsArray
            |> Array.map (create nodeLike.Program)
            |> NonEmptyArray.create
        let toJSDocTag: JSDoc -> Ts.JSDocTag = _.Value
        let tag: JSDoc -> _ = IFastUnionUnwrappable.map _.tagName.text
        let comment = CommentPart.fromJSDocTag
            
    module Script =
        let isValid = ts.isExternalModule >> not
        let isScript = function Patterns.Node.SourceFile node -> isValid node | _ -> false
        let tryCreate program = function
            | Patterns.Node.SourceFile node when isValid node ->
                IErasedWrapper.create program node
                |> Some : Node.Script option
            | _ -> None
        let getSymbolLocals: Node.Script -> _ = _.MapWithProgram(LocalSymbolTable.fromSourceFile)
        let getStatements: Node.Script -> _ = IErasedWrapper.map (_.statements.AsArray >> Array.map TopLevelStatements.Create >> NonEmptyArray.create)
        let isDefaultLib: Node.Script -> _ = _.MapWithProgram(_.isSourceFileDefaultLibrary)
        
        // -----------------------------------------------------------------------------------
        // NOTE FOR `getVersionedPackageJson`, `getNamedPackageJson` and `getPackageJson`:
        // typescript scripts are considered to be part of the default lib, and they can have
        // - or can lift to - a valid package.json.
        // The intention behind our api is to hide default lib details. For this reason, these
        // will still return none
        // -----------------------------------------------------------------------------------
        
        /// Only provides a value in non-default lib scripts
        [<EditorBrowsable(EditorBrowsableState.Never); System.Obsolete("Is never different from `getNamedAndVersionedPackageJson`")>]
        let getVersionedPackageJson (script: Node.Script) =
            if isDefaultLib script then None else
            script.Value.closestVersionedPackageJsonFields
            |> Option.map (fun v -> v :?> PackageJsonPathFields.VerifiedVersion)
        /// Only provides a value in non-default lib scripts
        [<EditorBrowsable(EditorBrowsableState.Never); System.Obsolete("Is never different from `getNamedAndVersionedPackageJson`")>]
        let getNamedPackageJson (script: Node.Script) =
            if isDefaultLib script then None else
            script.Value.closestNamedPackageJsonFields
            |> Option.map (fun v -> v :?> PackageJsonPathFields.VerifiedName)
        /// Only provides a value in non-default lib scripts
        [<System.Obsolete("Does not necessarily have a name or version. Use `getNamedAndVersionedPackageJson` instead")>]
        let getPackageJson (script: Node.Script) =
            if isDefaultLib script then None else
            script.Value.packageJsonFields
        /// Only provides a value in non-default lib scripts
        let getNamedAndVersionedPackageJson (script: Node.Script): PackageJsonPathFields.VerifiedNameAndVersion option =
            if isDefaultLib script then None else
            script.Value.closestNamedAndVersionedPackageJsonFields
            |> Option.map (fun v -> v :?> PackageJsonPathFields.VerifiedNameAndVersion)
        /// Only provides a value in non-default lib scripts
        let getPackageName (script: Node.Script) = getNamedAndVersionedPackageJson script |> Option.map _.name
        /// Only provides a value in non-default lib scripts
        let getPackageVersion (script: Node.Script) = getNamedAndVersionedPackageJson script |> Option.map _.version
        let getPackageId (script: Node.Script) =
            getPackageName script
            |> Option.map (fun name ->
                // guaranteed to succeed
                getPackageVersion script
                |> Option.get
                |> Packages.Version.read
                |> Packages.PackageId.create name
                )
            
        
    module ExternalModule =
        let isValid = ts.isExternalModule
        let isExternalModule: Ts.Node -> _ = Patterns.Node.(|SourceFile|_|) >> Option.exists isValid
        let tryCreate program: _ -> Node.ExternalModule option = function
            | Patterns.Node.SourceFile node when isValid node -> IErasedWrapper.create program node |> Some
            | _ -> None
        let getEmbeddedSymbol: Node.ExternalModule -> _ = Internal.unsafeGetEmbeddedSymbol
        let getSymbol (script: Node.ExternalModule) =
            script
            |> IErasedWrapper.map (
                script.Checker.getSymbolAtLocation
                >> Option.defaultWith (fun () ->
                    script.Value
                    |> _.fileName
                    |> Logging.Log.Default.logfe "SourceFile marked as external module had no symbol associated: %s{fileName}"
                    failwith "SourceFile marked as external module had no symbol associated"
                    )
                )
        let getSymbolExports externalModule =
            getSymbol externalModule
            |> _.exports
            |> Option.map (ExportSymbolTable.create externalModule.Program)
            |> Option.defaultWith (fun () -> failwith "ExternalModule should have globalExports")
        let getSymbolGlobalExports externalModule =
            getSymbol externalModule
            |> _.globalExports
            |> Option.map (ExportSymbolTable.create externalModule.Program)
        let getSymbolLocals: Node.ExternalModule -> _ = _.MapWithProgram(LocalSymbolTable.fromSourceFile)
        let getModuleSpecifiers (externalModule: Node.ExternalModule) =
            let symbol = getSymbol externalModule
            let specifier = externalModule.Program.GetModuleSpecifier symbol
            specifier.kind
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module symbol had no module specifier kind associated: %s{fileName}" symbol.name
                failwith $"SourceFile marked as external module symbol had no module specifier kind associated: {symbol.name}"
            ),
            specifier.moduleSpecifiers
            |> NonEmptyArray.create
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module symbol had no module specifiers associated: %s{fileName}" symbol.name
                failwith $"SourceFile marked as external module symbol had no module specifiers associated: {symbol.name}"
                )
        let getStatements: Node.ExternalModule -> _ = IErasedWrapper.map (_.statements.AsArray >> Array.map TopLevelStatements.Create >> NonEmptyArray.create >> Option.defaultWith (fun () -> failwith "ExternalModule should have statements"))
        [<EditorBrowsable(EditorBrowsableState.Never); System.Obsolete("Is never different from `getNamedAndVersionedPackageJson`")>]
        let getVersionedPackageJson (externalModule: Node.ExternalModule) =
            externalModule.Value.closestVersionedPackageJsonFields
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module had no package.json associated with a version: %s{fileName}" externalModule.Value.fileName
                failwith $"SourceFile marked as external module had no package.json associated with a version: {externalModule.Value.fileName}"
                )
            :?> PackageJsonPathFields.VerifiedVersion
        [<EditorBrowsable(EditorBrowsableState.Never); System.Obsolete("Is never different from `getNamedAndVersionedPackageJson`")>]
        let getNamedPackageJson (externalModule: Node.ExternalModule) =
            externalModule.Value.closestNamedPackageJsonFields
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module had no package.json associated with a name: %s{fileName}" externalModule.Value.fileName
                failwith $"SourceFile marked as external module had no package.json associated with a name: {externalModule.Value.fileName}"
                )
            :?> PackageJsonPathFields.VerifiedName
        [<System.Obsolete("Does not necessarily have a name or version. Use `getNamedAndVersionedPackageJson` instead")>]
        let getPackageJson (externalModule: Node.ExternalModule) =
            externalModule.Value.packageJsonFields
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module had no package.json associated: %s{fileName}" externalModule.Value.fileName
                failwith $"SourceFile marked as external module had no package.json associated: {externalModule.Value.fileName}"
                )
        let getNamedAndVersionedPackageJson (externalModule: Node.ExternalModule): PackageJsonPathFields.VerifiedNameAndVersion =
            externalModule.Value.closestNamedAndVersionedPackageJsonFields
            |> Option.defaultWith (fun () ->
                Logging.Log.Default.logfe "SourceFile marked as external module had no package.json associated with a name and version: %s{fileName}" externalModule.Value.fileName
                failwith $"SourceFile marked as external module had no package.json associated with a name and version: {externalModule.Value.fileName}"
                )
            :?> PackageJsonPathFields.VerifiedNameAndVersion
        let getPackageName (externalModule: Node.ExternalModule) = getNamedAndVersionedPackageJson externalModule |> _.name
        let getPackageVersion (externalModule: Node.ExternalModule) = getNamedAndVersionedPackageJson externalModule |> _.version
        let getPackageId (externalModule: Node.ExternalModule) =
            let json = getNamedAndVersionedPackageJson externalModule
            json.version
            |> Packages.Version.read
            |> Packages.PackageId.create json.name
            
    module SourceKind =
        let isSourceKind = ts.isSourceFile
        let tryCreate program node =
            ExternalModule.tryCreate program node
            |> Option.map Node.SourceKind.ExternalModule
            |> Option.orElseWith (fun () ->
                Script.tryCreate program node
                |> Option.map Node.SourceKind.Script
                )
            |> Option.map (InlinedProgram.inject program)
        let create (program: Ts.Program) (node: Ts.SourceFile) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwithf "Unexpected SourceFile kind: %s" node.kind.Name)
            |> InlinedProgram.inject program
        let isDefaultLib: Node.SourceKind -> bool = IFastUnionUnwrappable.mapWithProgram _.isSourceFileDefaultLibrary
        let getStatements: Node.SourceKind -> NonEmptyArray<TopLevelStatements> option = function
            | Node.SourceKind.Script script -> Script.getStatements script
            | Node.SourceKind.ExternalModule externalModule -> ExternalModule.getStatements externalModule |> Some
        let getStatementKinds = fun sk ->
            getStatements sk
            |> Option.map (NonEmptyArray.map (_.Value >> Kind.createFromNode sk.Program))
        let registerToPackageCollection (sourceKind: Node.SourceKind) =
            match sourceKind with
            | Node.SourceKind.Script script when [
                    Script.getPackageName script
                    Script.getPackageVersion script
                ] |> List.forall Option.isSome ->
                let name = Script.getPackageName script |> Option.get
                let version = Script.getPackageVersion script |> Option.get |> Measures.annotate<Packages.packageVersion>
                sourceKind.Program.resol
                Packages.PackageId.create name version
            | Node.SourceKind.Script script ->
                Script.get
            
    module MethodKind =
        let internal kindMap = Dictionary [
            SK.MethodDeclaration ==* (MethodDeclaration.create, Node.MethodKind.Class)
            SK.MethodSignature ==* (MethodSignature.create, Node.MethodKind.Type)
        ]
        let kindSet = dictToSet kindMap
        let isMethodKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isMethodKind program node
        let fromMethodDeclaration (node: Node.MethodDeclaration) = Node.MethodKind.Class node |> InlinedProgram.inject node.Program
        let fromMethodSignature (node: Node.MethodSignature) = Node.MethodKind.Type node |> InlinedProgram.inject node.Program
        let isOptional = function
            | Node.MethodKind.Type method -> method |> IErasedWrapper.map _.questionToken.IsSome
            | Node.MethodKind.Class method -> method |> IErasedWrapper.map _.questionToken.IsSome
        let typeParameters = function
            | Node.MethodKind.Type method -> MethodSignature.typeParameters method
            | Node.MethodKind.Class method -> MethodDeclaration.typeParameters method
    module ParameterKind =
        let internal kindMap = Dictionary [
            SK.Parameter ==* (ParameterDeclaration.create, Node.ParameterKind.Simple)
            SK.BindingElement ==* (BindingElement.create, Node.ParameterKind.Binding)
        ]
        let kindSet = dictToSet kindMap
        let isParameterKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isParameterKind program node
        let fromParameterDeclaration (node: Node.ParameterDeclaration) = Node.ParameterKind.Simple node |> InlinedProgram.inject node.Program
        let fromBindingElement (node: Node.BindingElement) = Node.ParameterKind.Binding node |> InlinedProgram.inject node.Program
    module SignatureKind =
        let internal kindMap = Dictionary [
            SK.CallSignature ==* (CallSignature.create, Node.SignatureKind.Call)
            SK.ConstructSignature ==* (ConstructSignature.create, Node.SignatureKind.Construct)
            SK.IndexSignature ==* (IndexSignature.create, Node.SignatureKind.Index)
        ]
        let kindSet = dictToSet kindMap
        let isSignatureKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isSignatureKind program node
        let typeParameters = function
            | Node.SignatureKind.Call signature -> CallSignature.typeParameters signature
            | Node.SignatureKind.Construct signature -> ConstructSignature.typeParameters signature
            | Node.SignatureKind.Index signature -> IndexSignature.typeParameters signature
    module PropertyKind =
        let internal kindMap = Dictionary [
            SK.PropertyDeclaration ==* (PropertyDeclaration.create, Node.PropertyKind.Class)
            SK.PropertySignature ==* (PropertySignature.create, Node.PropertyKind.Type)
        ]
        let kindSet = dictToSet kindMap
        let isPropertyKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isPropertyKind program node
    module ClassMemberKind =
        let internal kindMap = Dictionary [
            SK.MethodDeclaration ==* (MethodDeclaration.create, Node.ClassMemberKind.Method)
            SK.PropertyDeclaration ==* (PropertyDeclaration.create, Node.ClassMemberKind.Property)
            SK.GetAccessor ==* (GetAccessorDeclaration.create, Node.ClassMemberKind.GetAccessor)
            SK.SetAccessor ==* (SetAccessorDeclaration.create, Node.ClassMemberKind.SetAccessor)
            SK.Constructor ==* (ConstructorDeclaration.create, Node.ClassMemberKind.Constructor)
        ]
        let kindSet = dictToSet kindMap
        let fromPropertyDeclaration (prop: Node.PropertyDeclaration) =
            Node.ClassMemberKind.Property prop
            |> InlinedProgram.inject prop.Program
        let isClassMemberKind node = setContainsNodeKind kindSet node
        let tryCreate program (node: Ts.Node) = tryCreateWithNodeCheck kindMap isClassMemberKind program node
        let create program (node: Ts.ClassElement) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwithf "Unexpected ClassElement kind: %s" node.kind.Name)
    module TypeMemberKind =
        let internal kindMap = Dictionary [
            SK.MethodSignature ==* (MethodSignature.create, Node.TypeMemberKind.Method)
            SK.CallSignature ==* (CallSignature.create, Node.TypeMemberKind.Call)
            SK.ConstructSignature ==* (ConstructSignature.create, Node.TypeMemberKind.Construct)
            SK.PropertySignature ==* (PropertySignature.create, Node.TypeMemberKind.Property)
            SK.GetAccessor ==* (GetAccessorDeclaration.create, Node.TypeMemberKind.GetAccessor)
            SK.SetAccessor ==* (SetAccessorDeclaration.create, Node.TypeMemberKind.SetAccessor)
            SK.IndexSignature ==* (IndexSignature.create, Node.TypeMemberKind.Index)
        ]
        let kindSet = dictToSet kindMap
        let isTypeMemberKind node = setContainsNodeKind kindSet node
        let tryCreate program (node: Ts.Node) = tryCreateWithNodeCheck kindMap isTypeMemberKind program node
        let create program (node: Ts.TypeElement) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwith "Unexpected TypeElement kind")
    module ModuleKind =
        let internal kindMap = Dictionary [
            SK.SourceFile ==* (SourceKind.create, Node.ModuleKind.Source)
            SK.ModuleDeclaration ==* (ModuleDeclaration.create, Node.ModuleKind.Declaration)
            // module block -> module declaration
            SK.ModuleBlock ==* ((fun program -> ModuleBlock.create program >> ModuleBlock.parent), Node.ModuleKind.Declaration)
        ]
        let kindSet = dictToSet kindMap
        let isModuleKind node = setContainsNodeKind kindSet node
        let tryCreate program (node: Ts.Node) = tryCreateWithNodeCheck kindMap isModuleKind program node
        let unsafeCreate program (node: Ts.Node) = tryCreate program node |> Option.defaultWith (fun () -> failwith "ModuleKind should be created")
        let toDeclaration = function
            | Node.ModuleKind.Source source -> source.Value :> Ts.Declaration
            | Node.ModuleKind.Declaration declaration -> declaration.Value :> Ts.Declaration
    module ModuleMemberKind =
        let internal kindMap = Dictionary [
            SK.VariableDeclaration ==* (Variable.create, Node.ModuleMemberKind.Variable)
            SK.FunctionDeclaration ==* (FunctionDeclaration.create, Node.ModuleMemberKind.Function)
            SK.ClassDeclaration ==* (ClassDeclaration.create, Node.ModuleMemberKind.Class)
            SK.InterfaceDeclaration ==* (InterfaceDeclaration.create, Node.ModuleMemberKind.Interface)
            SK.EnumDeclaration ==* (EnumDeclaration.create, Node.ModuleMemberKind.Enum)
            SK.TypeAliasDeclaration ==* (TypeAliasDeclaration.create, Node.ModuleMemberKind.TypeAlias)
            SK.ModuleDeclaration ==* (ModuleDeclaration.create, Node.ModuleMemberKind.Module)
        ]
        let kindSet = dictToSet kindMap
        let isModuleMemberKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isModuleMemberKind program node
    module ImportDeclarationKind =
        let internal kindMap = Dictionary [
            SK.ImportSpecifier ==* (ImportSpecifier.create, Node.ImportDeclarationKind.Specifier)
            SK.NamespaceImport ==* (NamespaceImport.create, Node.ImportDeclarationKind.Namespace)
            SK.ImportClause ==* (ImportClause.create, Node.ImportDeclarationKind.Clause)
            SK.ImportEqualsDeclaration ==* (ImportEqualsDeclaration.create, Node.ImportDeclarationKind.ImportEquals)
            SK.NamedImports ==* (NamedImports.create, Node.ImportDeclarationKind.Named)
            SK.ImportDeclaration ==* (ImportDeclaration.create, Node.ImportDeclarationKind.Declaration)
        ]
        let kindSet = dictToSet kindMap
        let isImportDeclarationKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isImportDeclarationKind program node
        let unsafeCreate program (node: Ts.Node) = tryCreate program node |> Option.defaultWith (fun () -> failwithf "ImportDeclarationKind should be created, got: %s" node.kind.Name)
    module ExportDeclarationKind =
        let internal kindMap = Dictionary [
            SK.ExportSpecifier ==* (ExportSpecifier.create, Node.ExportDeclarationKind.Specifier)
            SK.NamespaceExport ==* (NamespaceExport.create, Node.ExportDeclarationKind.Namespace)
            SK.NamespaceExportDeclaration ==* (NamespaceExportDeclaration.create, Node.ExportDeclarationKind.NamespaceDeclaration)
            SK.ExportAssignment ==* (ExportAssignment.create, Node.ExportDeclarationKind.Assignment)
            SK.ExportDeclaration ==* (ExportDeclaration.create, Node.ExportDeclarationKind.Declaration)
            SK.NamedExports ==* (NamedExports.create, Node.ExportDeclarationKind.Named)
        ]
        let kindSet = dictToSet kindMap
        let isExportDeclarationKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isExportDeclarationKind program node
        let unsafeCreate program (node: Ts.Node) = tryCreate program node |> Option.defaultWith (fun () -> failwithf "ExportDeclarationKind should be created, got: %s" node.kind.Name)
        let embeddedSymbol = function
            | Node.ExportDeclarationKind.Specifier node -> ExportSpecifier.embeddedSymbol node |> Some
            | Node.ExportDeclarationKind.Namespace node -> NamespaceExport.embeddedSymbol node |> Some
            | Node.ExportDeclarationKind.NamespaceDeclaration node -> NamespaceExportDeclaration.embeddedSymbol node |> Some
            | Node.ExportDeclarationKind.Assignment node -> ExportAssignment.embeddedSymbol node |> Some
            | Node.ExportDeclarationKind.Declaration node -> ExportDeclaration.embeddedSymbol node
            | Node.ExportDeclarationKind.Named _ -> None
    module ImportExportDeclarationKind =
        let internal kindMap = Dictionary [
            yield! fetchMapAndInject ExportDeclarationKind.kindMap Node.ImportExportDeclarationKind.Export
            yield! fetchMapAndInject ImportDeclarationKind.kindMap Node.ImportExportDeclarationKind.Import
        ]
        let kindSet = dictToSet kindMap
        let isImportExportDeclarationKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isImportExportDeclarationKind program node
        let unsafeCreate program (node: Ts.Node) = tryCreate program node |> Option.defaultWith (fun () -> failwithf "ImportExportDeclarationKind should be created, got: %s" node.kind.Name)
#nowarn 00040
    module DeclarationKind =
        let internal kindMap = Dictionary [
            SK.VariableDeclaration ==* (Variable.create, Node.DeclarationKind.Variable)
            SK.TypeAliasDeclaration ==* (TypeAliasDeclaration.create, Node.DeclarationKind.TypeAlias)
            SK.FunctionDeclaration ==* (FunctionDeclaration.create, Node.DeclarationKind.Function)
            SK.InterfaceDeclaration ==* (InterfaceDeclaration.create, Node.DeclarationKind.Interface)
            SK.ClassDeclaration ==* (ClassDeclaration.create, Node.DeclarationKind.Class)
            SK.TypeParameter ==* ( TypeParameterDeclaration.create, Node.DeclarationKind.TypeParameter )
            SK.GetAccessor ==* ( GetAccessorDeclaration.create, Node.DeclarationKind.GetAccessor )
            SK.SetAccessor ==* ( SetAccessorDeclaration.create, Node.DeclarationKind.SetAccessor )
            SK.Constructor ==* ( ConstructorDeclaration.create, Node.DeclarationKind.Constructor )
            SK.EnumDeclaration ==* ( EnumDeclaration.create, Node.DeclarationKind.Enum )
            SK.EnumMember ==* ( EnumMember.create, Node.DeclarationKind.EnumMember )
            yield! fetchMap ImportExportDeclarationKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.ImportExport)
            yield! fetchMap ParameterKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Parameter)
            yield! fetchMap MethodKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Method)
            yield! fetchMap PropertyKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Property)
            yield! fetchMap SignatureKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Signature)
            yield! fetchMap ModuleKind.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Module)
            yield! fetchMap Type.kindMap (fun program -> InlinedProgram.inject program >> Node.DeclarationKind.Type)
        ]
        let kindSet = dictToSet kindMap
        let isDeclarationKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isDeclarationKind program node
        let create program (node: Ts.Declaration) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwith "Unexpected Declaration kind")
#warnon 00040
    module TypeKeyword =
        let internal kindMap = Dictionary [
            Ts.SyntaxKind.StringKeyword ==>! Node.TypeKeyword.String 
            Ts.SyntaxKind.NumberKeyword ==>! Node.TypeKeyword.Number 
            Ts.SyntaxKind.BooleanKeyword ==>! Node.TypeKeyword.Boolean 
            Ts.SyntaxKind.AnyKeyword ==>! Node.TypeKeyword.Any 
            Ts.SyntaxKind.VoidKeyword ==>! Node.TypeKeyword.Void 
            Ts.SyntaxKind.UndefinedKeyword ==>! Node.TypeKeyword.Undefined 
            Ts.SyntaxKind.NullKeyword ==>! Node.TypeKeyword.Null 
            Ts.SyntaxKind.NeverKeyword ==>! Node.TypeKeyword.Never 
            Ts.SyntaxKind.UnknownKeyword ==>! Node.TypeKeyword.Unknown 
            Ts.SyntaxKind.ObjectKeyword ==>! Node.TypeKeyword.Object 
            Ts.SyntaxKind.SymbolKeyword ==>! Node.TypeKeyword.Symbol 
            Ts.SyntaxKind.IntrinsicKeyword ==>! Node.TypeKeyword.Intrinsic 
            Ts.SyntaxKind.BigIntKeyword ==>! Node.TypeKeyword.BigInt 
        ]
        let kindSet = dictToSet kindMap
        let isTypeKeyword node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isTypeKeyword program node
        let create program (node: #Ts.KeywordTypeNode) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwith "Unexpected TypeKeyword kind")
        /// <summary>
        /// Keyword is ANY or UNKNOWN (can be represented by a null or a value)
        /// </summary>
        let canBeNull = function
            | Node.TypeKeyword.Unknown
            | Node.TypeKeyword.Any -> true
            | _ -> false
        /// <summary>
        /// Keyword is a type that is representative of a null-like state: null, undefined, never, void
        /// </summary>
        let isNullish = function
            | Node.TypeKeyword.Void
            | Node.TypeKeyword.Never
            | Node.TypeKeyword.Undefined
            | Node.TypeKeyword.Null -> true
            | _ -> false
        /// <summary>
        /// Keyword is a type that reprs a null-like state, or can be null
        /// </summary>
        /// <param name="keyword"></param>
        let isNullable = fun keyword -> isNullish keyword || canBeNull keyword
        
        
    module KeyOf =
        let create program (typeNode: Ts.TypeNode) =
            Type.create program typeNode
            |> Node.KeyOf.Generic
    module TypeOperator =
        open Xantham.TypeScript.Node
        let isTypeOperator = ts.isTypeOperatorNode
        let create program (node: Ts.TypeOperatorNode) =
            match node.operator with
            | SK.KeyOfKeyword -> KeyOf.create program node.``type`` |> Node.TypeOperator.KeyOf
            | SK.UniqueKeyword -> Type.create program node.``type`` |> Node.TypeOperator.Unique
            | SK.ReadonlyKeyword -> Type.create program node.``type`` |> Node.TypeOperator.Readonly
            | _ -> failwith "Unexpected TypeOperatorNode"
        let tryCreate program node =
            if not <| isTypeOperator node then None else
            Some <| create program node
        let isUniqueESSymbolPattern = function
            | TypeOperator.Unique (Type.Keyword (TypeKeyword.Symbol, _)) -> true | _ -> false
            
    module ModifierKeyword =
        let internal kindMap = Dictionary [
            SK.ExportKeyword ==>! Node.ModifierKeyword.Export
            SK.DeclareKeyword ==>! Node.ModifierKeyword.Declare
            SK.DefaultKeyword ==>! Node.ModifierKeyword.Default
            SK.AbstractKeyword ==>! Node.ModifierKeyword.Abstract
            SK.StaticKeyword ==>! Node.ModifierKeyword.Static
            SK.PublicKeyword ==>! Node.ModifierKeyword.Public
            SK.ProtectedKeyword ==>! Node.ModifierKeyword.Protected
            SK.PrivateKeyword ==>! Node.ModifierKeyword.Private
            SK.OverrideKeyword ==>! Node.ModifierKeyword.Override
            SK.ReadonlyKeyword ==>! Node.ModifierKeyword.ReadOnly
            SK.ConstKeyword ==>! Node.ModifierKeyword.Const
            SK.InKeyword ==>! Node.ModifierKeyword.In
            SK.OutKeyword ==>! Node.ModifierKeyword.Out
        ]
        let kindSet = dictToSet kindMap
        let isModifierKeyword node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isModifierKeyword program node
        let create program (node: Ts.Modifier) =
            tryCreate program node
            |> Option.defaultWith (fun () -> failwith "Unexpected Modifier kind")
        let createFromModifierLike program (node: Ts.ModifierLike) =
            tryCreate program !!node
            |> Option.defaultWith (fun () -> failwith "Unexpected Modifier kind")
    module Type =
        let inline getTypeNode (node: Node.Type) = emitJsExpr node "$0.fields[1]"
        let inline getType (node: Node.Type) =
            getTypeNode node
            |> node.Checker.getTypeFromTypeNode
        let inline private constructorHelper initialiser unionCase program node =
            let value = initialiser program node
            unionCase(value, TypeNode.create program node)
            |> InlinedProgram.inject program
        let createTypeReference program node = constructorHelper TypeReference.create Node.Type.Reference program node
        let createTypeKeyword program node = constructorHelper TypeKeyword.create Node.Type.Keyword program node
        let createLiteralType program node = constructorHelper LiteralType.create ((fun (n,tn) -> LiteralType.literal n, tn) >> Node.Type.Literal) program node
        let createUnionType program node = constructorHelper UnionType.create Node.Type.Union program node
        let createFunctionType program node = constructorHelper FunctionType.create Node.Type.Function program node
        let createTypeLiteral program node = constructorHelper TypeLiteralType.create Node.Type.TypeLiteral program node
        let createExpressionWithTypeArguments program node = constructorHelper ExpressionWithTypeArguments.create Node.Type.ExpressionWithTypeArguments program node
        let createArrayType program node = constructorHelper ArrayType.create Node.Type.Array program node
        let createTypeOperator program node = constructorHelper TypeOperator.create Node.Type.TypeOperator program node
        let createIndexedAccess program node = constructorHelper IndexedAccessType.create Node.Type.IndexedAccess program node
        let createParenthesizedType program node = constructorHelper ParenthesizedType.create Node.Type.Parenthesized program node
        let createThisType program node = constructorHelper ThisType.create Node.Type.This program node
        let createTupleType program node = constructorHelper TupleType.create Node.Type.Tuple program node
        let createConditionalType program node = constructorHelper ConditionalType.create Node.Type.Conditional program node
        let createIntersectionType program node = constructorHelper IntersectionType.create Node.Type.Intersection program node
        let createTypePredicate program node = constructorHelper TypePredicate.create Node.Type.TypePredicate program node
        let createTypeQuery program node = constructorHelper TypeQuery.create Node.Type.TypeQuery program node
        let createInferType program node = constructorHelper InferType.create Node.Type.Infer program node
        let createTemplateLiteralSpan program node = constructorHelper TemplateLiteralTypeSpan.create Node.Type.TemplateLiteralSpan program node
        let createMapped program node = constructorHelper MappedType.create Node.Type.Mapped program node
        let createNamedTuple program node = constructorHelper NamedTupleMember.create Node.Type.NamedTuple program node
        let createTemplateLiteral program node = constructorHelper TemplateLiteralType.create Node.Type.TemplateLiteral program node
        let createRestType program node = constructorHelper RestType.create Node.Type.Rest program node
        let createImportType program node = constructorHelper ImportType.create Node.Type.Import program node
        let createConstructorType program node = constructorHelper ConstructorType.create Node.Type.Constructor program node
        let createOptionalType program node = constructorHelper OptionalType.create Node.Type.Optional program node
        let internal kindMap = Dictionary [
            SK.TypeReference ==> createTypeReference // ~39% of cases
            for key in TypeKeyword.kindSet do // ~29% of cases
                key ==> createTypeKeyword
            SK.LiteralType ==> createLiteralType // ~10% of type nodes
            SK.UnionType ==> createUnionType // ~7% of type nodes
            SK.FunctionType ==> createFunctionType // ~3% of type nodes
            SK.TypeLiteral ==> createTypeLiteral // ~2% of type nodes
            SK.ExpressionWithTypeArguments ==> createExpressionWithTypeArguments // ~2% of type nodes
            SK.ArrayType ==> createArrayType // ~1.5% of type nodes
            SK.TypeOperator ==> createTypeOperator // ~1% of type nodes
            SK.IndexedAccessType ==> createIndexedAccess // ~1% of type nodes
            SK.ParenthesizedType ==> createParenthesizedType // ~1% of type nodes
            SK.ThisType ==> createThisType // ~1% of type nodes
            SK.TupleType ==> createTupleType // ~0.5% of type nodes
            SK.ConditionalType ==> createConditionalType // ~0.25% of type nodes
            SK.IntersectionType ==> createIntersectionType // ~0.25% of type nodes
            SK.TypeQuery ==> createTypeQuery // ~0.25% of type nodes
            SK.InferType ==> createInferType // ~0.1% of type nodes
            SK.TypePredicate ==> createTypePredicate // ~0.1% of type nodes
            SK.TemplateLiteralTypeSpan ==> createTemplateLiteralSpan // ~0.1% of type nodes
            SK.MappedType ==> createMapped // ~0.1% of type nodes
            SK.NamedTupleMember ==> createNamedTuple // ~0.05% of type nodes
            SK.TemplateLiteralType ==> createTemplateLiteral // ~0.05% of type nodes
            SK.RestType ==> createRestType // ~0.05% of type nodes
            SK.ImportType ==> createImportType // ~0.03% of type nodes
            SK.ConstructorType ==> createConstructorType // ~0.01% of type nodes
            SK.OptionalType ==> createOptionalType // ~0.01% of type nodes
        ]
        let kindSet = dictToSet kindMap
        let isTypeKind node = setContainsNodeKind kindSet node
        let tryCreate program node = tryCreateWithNodeCheck kindMap isTypeKind program node
        let create program (node: Ts.TypeNode) =
            kindMap[node.kind] program node
    module JSDocIdentifier =
        let fromIdentifierKind node = Node.JSDocIdentifier.Identifier node |> InlinedProgram.inject node.Program
        let internal kindMap = Dictionary [
            SK.JSDocMemberName ==* (JSDocMemberName.create, Node.JSDocIdentifier.JSDoc)
            yield! fetchMap IdentifierKind.kindMap (fun _ -> fromIdentifierKind)
        ]
    module TopLevelExportSymbolKind =
        let fromInterfaceDeclaration program node =
            InterfaceDeclaration.create program node |> Node.TopLevelExportSymbolKind.Interface
        let fromTypeAlias program node =
            TypeAliasDeclaration.create program node |> Node.TopLevelExportSymbolKind.TypeAlias
        let fromClassDeclaration program node =
            ClassDeclaration.create program node |> Node.TopLevelExportSymbolKind.Class
        let fromEnumDeclaration program node =
            EnumDeclaration.create program node |> Node.TopLevelExportSymbolKind.Enum
        let fromVariableDeclaration program node =
            Variable.create program node |> Node.TopLevelLocalSymbolKind.Variable
        let fromModuleKind _ node = Node.TopLevelExportSymbolKind.Module node
        let fromFunctionDeclaration program node =
            FunctionDeclaration.create program node |> Node.TopLevelExportSymbolKind.Function
        let fromNamespaceExportDeclaration program node =
            NamespaceExportDeclaration.create program node |> Node.TopLevelExportSymbolKind.NamespaceExportDeclaration
        let fromExportDeclaration program node =
            ExportDeclaration.create program node |> Node.TopLevelExportSymbolKind.ExportDeclaration
        let fromImportEquals program node =
            ImportEqualsDeclaration.create program node |> Node.TopLevelExportSymbolKind.ImportEquals
        let fromImportDeclaration program node =
            ImportDeclaration.create program node |> Node.TopLevelExportSymbolKind.ImportDeclaration
        let fromExportAssignment program node =
            ExportAssignment.create program node |> Node.TopLevelExportSymbolKind.ExportAssignment
        let fromExportSpecifier program node =
            ExportSpecifier.create program node |> Node.TopLevelExportSymbolKind.ExportSpecifier
        let fromNamespaceExport program node =
            NamespaceExport.create program node |> Node.TopLevelExportSymbolKind.NamespaceExport
        let internal kindMap: Dictionary<SK, Ts.Program -> obj -> Node.TopLevelExportSymbolKind> = Dictionary [
            SK.InterfaceDeclaration ==> fromInterfaceDeclaration
            SK.TypeAliasDeclaration ==> fromTypeAlias
            SK.ClassDeclaration ==> fromClassDeclaration
            SK.EnumDeclaration ==> fromEnumDeclaration
            SK.VariableDeclaration ==> fromVariableDeclaration
            SK.FunctionDeclaration ==> fromFunctionDeclaration
            SK.NamespaceExportDeclaration ==> fromNamespaceExportDeclaration
            SK.ExportDeclaration ==> fromExportDeclaration
            SK.ImportEqualsDeclaration ==> fromImportEquals
            SK.ImportDeclaration ==> fromImportDeclaration
            SK.ExportAssignment ==> fromExportAssignment
            SK.ExportSpecifier ==> fromExportSpecifier
            SK.NamespaceExport ==> fromNamespaceExport
            yield! fetchMap ModuleKind.kindMap fromModuleKind
        ]
        let kindSet = kindMap.Keys |> Set
        let isKind (node: Ts.Node) = kindSet.Contains node.kind
        let rawTryCreate (node: INode) =
            if not <| isKind (INode.toNode node) then None else
            let program = INode.program node
            let node = INode.toNode node
            kindMap[node.kind] program node
            |> Some
        let rawCreate (node: INode) =
            rawTryCreate node
            |> Option.defaultWith(fun () ->
                INode.kind node
                |> failwithf "Could not create TopLevelExportSymbolKind from node %s"
                )
        let create (node: INode) =
            INode.program node
            |> _.CompositeCollection
            |> CompositeCollection.Node.registerToWrapper
            |> funApply node
        let createFromNode program (node: Ts.Node) =
            INode.create program node |> create
    module TopLevelLocalSymbolKind =
        let fromInterfaceDeclaration program node = InterfaceDeclaration.create program node |> Node.TopLevelLocalSymbolKind.Interface
        let fromTypeAlias program node = TypeAliasDeclaration.create program node |> Node.TopLevelLocalSymbolKind.TypeAlias
        let fromClassDeclaration program node = ClassDeclaration.create program node |> Node.TopLevelLocalSymbolKind.Class
        let fromEnumDeclaration program node = EnumDeclaration.create program node |> Node.TopLevelLocalSymbolKind.Enum
        let fromVariableDeclaration program node = Variable.create program node |> Node.TopLevelLocalSymbolKind.Variable
        let fromModuleKind _ node = Node.TopLevelLocalSymbolKind.Module node
        let fromFunctionDeclaration program node = FunctionDeclaration.create program node |> Node.TopLevelLocalSymbolKind.Function
        let internal kindMap: Dictionary<SK, Ts.Program -> obj -> Node.TopLevelLocalSymbolKind> = Dictionary [
            SK.InterfaceDeclaration ==> fromInterfaceDeclaration
            SK.TypeAliasDeclaration ==> fromTypeAlias
            SK.ClassDeclaration ==> fromClassDeclaration
            SK.EnumDeclaration ==> fromEnumDeclaration
            SK.VariableDeclaration ==> fromVariableDeclaration
            SK.ModuleDeclaration ==> fromModuleKind
            SK.FunctionDeclaration ==> fromFunctionDeclaration
            yield! fetchMap ModuleKind.kindMap fromModuleKind
        ]
        let kindSet = kindMap.Keys |> Set
        let isKind (node: Ts.Node)  = kindSet.Contains node.kind
        let rawTryCreate (node: INode) =
            if not <| isKind (INode.toNode node) then None else
            let program = INode.program node
            let node = INode.toNode node
            kindMap[node.kind] program node
            |> Some
        let rawCreate (node: INode) =
            rawTryCreate node
            |> Option.defaultWith(fun () ->
                INode.kind node
                |> failwithf "Could not create TopLevelExportSymbolKind from node %s"
                )
        let create (node: INode) =
            INode.program node
            |> _.CompositeCollection
            |> CompositeCollection.Node.registerToWrapper
            |> funApply node
        let createFromNode program (node: Ts.Node) =
            INode.create program node |> create
    
    module TopLevelStatementsKind =
        let fromInterface program node = Node.TopLevelStatementsKind.Interface <| InterfaceDeclaration.create program node
        let fromTypeAlias program node = Node.TopLevelStatementsKind.TypeAlias <| TypeAliasDeclaration.create program node
        let fromClass program node = Node.TopLevelStatementsKind.Class <| ClassDeclaration.create program node
        let fromEnum program node = Node.TopLevelStatementsKind.Enum <| EnumDeclaration.create program node
        let fromVariable program node = Node.TopLevelStatementsKind.Variable <| VariableStatement.create program node
        let fromVariableDeclaration program node = Node.TopLevelStatementsKind.VariableDeclaration <| Variable.create program node
        let fromFunction program node = Node.TopLevelStatementsKind.Function <| FunctionDeclaration.create program node
        let fromExportDeclaration program node = Node.TopLevelStatementsKind.ExportDeclaration <| ExportDeclaration.create program node
        let fromNamespaceExportDeclaration program node = Node.TopLevelStatementsKind.NamespaceExportDeclaration <| NamespaceExportDeclaration.create program node
        let fromImportDeclaration program node = Node.TopLevelStatementsKind.ImportDeclaration <| ImportDeclaration.create program node
        let fromImportEqualsDeclaration program node = Node.TopLevelStatementsKind.ImportEqualsDeclaration <| ImportEqualsDeclaration.create program node
        let fromExportAssignment program node = Node.TopLevelStatementsKind.ExportAssignment <| ExportAssignment.create program node
        let fromModuleKind _ node = Node.TopLevelStatementsKind.Module node
        let internal kindMap: Dictionary<SK, Ts.Program -> obj -> Node.TopLevelStatementsKind> = Dictionary [
            SK.InterfaceDeclaration ==> fromInterface
            SK.TypeAliasDeclaration ==> fromTypeAlias
            SK.ClassDeclaration ==> fromClass
            SK.EnumDeclaration ==> fromEnum
            SK.VariableDeclaration ==> fromVariableDeclaration
            SK.VariableStatement ==> fromVariable
            SK.FunctionDeclaration ==> fromFunction
            SK.ExportDeclaration ==> fromExportDeclaration
            SK.NamespaceExportDeclaration ==> fromNamespaceExportDeclaration
            SK.ImportDeclaration ==> fromImportDeclaration
            SK.ImportEqualsDeclaration ==> fromImportEqualsDeclaration
            SK.ExportAssignment ==> fromExportAssignment
            yield! fetchMap ModuleKind.kindMap fromModuleKind
        ]
        let kindSet = kindMap.Keys |> Set
        let isKind (node: Ts.Node) = kindSet.Contains node.kind
        let rawTryCreate (node: INode) =
            if not <| isKind (INode.toNode node) then None else
            let program = INode.program node
            let node = INode.toNode node
            kindMap[node.kind] program node
            |> Some
        let rawCreate (node: INode) =
            rawTryCreate node
            |> Option.defaultWith (fun () ->
                INode.kind node
                |> failwithf "Could not create toplevelstatmentkind from patriots."
                )
        let tryCreateFromNode program node =
            INode.create program node
            |> rawTryCreate
        let createFromNode program node =
            INode.create program node
            |> rawCreate
    module Kind =
        let fromModifier _ node = Node.Kind.Modifier node
        let fromSemanticToken _ node = Node.Kind.Semantic node
        let fromBindingPattern _ node = Node.Kind.BindingPattern node
        let fromExpression _ node = Node.Kind.Expression node
        let fromIdentifierKind _ node = Node.Kind.Identifier node
        let fromTemplatePart _ node = Node.Kind.TemplatePart node
        let fromJSDoc _ node = Node.Kind.JSDoc node
        let fromContainer _ node = Node.Kind.Container node
        let private createHeritageClause program node =
            HeritageClause.create program node
            |> HeritageClauseKind.fromHeritageClause
            |> Node.Kind.HeritageClause
        let fromLiteral _ node = Node.Kind.Literal node
        let fromJSDocIdentifier _ node = Node.Kind.JSDocIdentifier node
        let fromDeclarationKind _ node = Node.Kind.DeclarationOrType node
        let internal kindMap: Dictionary<SK, Ts.Program -> obj -> Node.Kind> = Dictionary [
            yield! fetchMapAndInject DeclarationKind.kindMap Node.Kind.DeclarationOrType
            yield! fetchMapAndInject ModifierKeyword.kindMap Node.Kind.Modifier
            yield! fetchMapAndInject SemanticToken.kindMap Node.Kind.Semantic
            yield! fetchMapAndInject BindingPattern.kindMap Node.Kind.BindingPattern
            yield! fetchMapAndInject Expression.kindMap Node.Kind.Expression
            yield! fetchMapAndInject TemplatePart.kindMap Node.Kind.TemplatePart
            yield! fetchMapAndInject JSDoc.kindMap Node.Kind.JSDoc
            yield! fetchMapAndInject Container.kindMap Node.Kind.Container
            SK.ExternalModuleReference ==* (ExternalModuleReference.create, Node.Kind.ExternalModuleReference)
            Ts.SyntaxKind.HeritageClause ==> createHeritageClause
            yield! fetchMapAndInject Literal.kindMap Node.Kind.Literal
            yield! fetchMapAndInject IdentifierKind.kindMap Node.Kind.Identifier
            // This is a superset of Identifier. The distinction is present because
            // not all identifiers in the JSDocs resolve to symbols unlike the rest of the identifiers.
            for KeyValue(k, v) in JSDocIdentifier.kindMap do
                if IdentifierKind.kindMap.ContainsKey k |> not then
                    k ==> (v >> fromJSDocIdentifier)
        ]
        let kindSet = kindMap.Keys |> Set
        let isKind (node: Ts.Node) = kindSet.Contains node.kind
        /// <summary>
        /// No collection interactions involved. Directly attempts creation of the wrapper value.
        /// </summary>
        let rawTryCreate (node: INode) =
            if not <| isKind (INode.toNode node) then None else
            let program = INode.program node
            let node = INode.toNode node
            kindMap[node.kind] program node
            |> Some
        let rawCreate (node: INode) =
            rawTryCreate node
            |> Option.defaultWith (fun () ->
                INode.kind node
                |> failwithf "Could not create kind from node %s"
                )
        let create (node: INode) =
            INode.program node
            |> _.CompositeCollection
            |> CompositeCollection.Node.registerToWrapper
            |> funApply node
        let createFromNode program (node: Ts.Node) =
            INode.create program node |> create
    let typeWrapper: NodeTypeWrapper = {
        IntermediateFn = INode.create
        WrapFn = Kind.rawCreate
        KeyFn = INode.nodeKey
    }
[<RequireQualifiedAccess>]
module Symbol =
    /// <summary>
    /// Maps the name of the type, to the transient and nontransient symbol kind constructors.
    /// </summary>
    let private kindConstructor: Dictionary<string, (obj -> Symbol.Kind) * (obj -> Symbol.Transient.Kind)> = Dictionary [
        let inline (==>) a b = KeyValuePair(a, b)
        let inline (</>) (a: 'T -> Symbol.Kind) (b: 'U -> Symbol.Transient.Kind): (obj -> Symbol.Kind) * (obj -> Symbol.Transient.Kind) = (unbox a, unbox b)
        nameof Symbol.Interface ==> (Symbol.Kind.Interface </> Symbol.Transient.Kind.Interface)
        nameof Symbol.Variable ==> (Symbol.Kind.Variable </> Symbol.Transient.Kind.Variable)
        nameof Symbol.Function ==> (Symbol.Kind.Function </> Symbol.Transient.Kind.Function)
        nameof Symbol.Parameter ==> (Symbol.Kind.Parameter </> Symbol.Transient.Kind.Parameter)
        nameof Symbol.Method ==> (Symbol.Kind.Method </> Symbol.Transient.Kind.Method)
        nameof Symbol.TypeAlias ==> (Symbol.Kind.TypeAlias </> Symbol.Transient.Kind.TypeAlias)
        nameof Symbol.TypeParameter ==> (Symbol.Kind.TypeParameter </> Symbol.Transient.Kind.TypeParameter)
        nameof Symbol.ValueModule ==> (Symbol.Kind.ValueModule </> Symbol.Transient.Kind.ValueModule)
        nameof Symbol.NamespaceModule ==> (Symbol.Kind.NamespaceModule </> Symbol.Transient.Kind.NamespaceModule)
        nameof Symbol.GetAccessor ==> (Symbol.Kind.GetAccessor </> Symbol.Transient.Kind.GetAccessor)
        nameof Symbol.SetAccessor ==> (Symbol.Kind.SetAccessor </> Symbol.Transient.Kind.SetAccessor)
        nameof Symbol.Class ==> (Symbol.Kind.Class </> Symbol.Transient.Kind.Class)
        nameof Symbol.Property ==> (Symbol.Kind.Property </> Symbol.Transient.Kind.Property)
        nameof Symbol.TypeEnum ==> (Symbol.Kind.TypeEnum </> Symbol.Transient.Kind.TypeEnum)
        nameof Symbol.ConstEnum ==> (Symbol.Kind.ConstEnum </> Symbol.Transient.Kind.ConstEnum)
        nameof Symbol.EnumMember ==> (Symbol.Kind.EnumMember </> Symbol.Transient.Kind.EnumMember)
        nameof Symbol.TypeLiteral ==> (Symbol.Kind.TypeLiteral </> Symbol.Transient.Kind.TypeLiteral)
    ]
    /// <summary>
    /// Uses the type variable to look up the constructor for the kind.
    /// If the symbol is a transient symbol, it will apply the transient constructor.
    /// The transient kind is wrapped in the kind transient case.
    /// </summary>
    /// <param name="symbol"></param>
    let inline private constructKind<'T> (symbol: ISymbol) =
        if ISymbol.hasFlag Ts.SymbolFlags.Transient symbol then
            (kindConstructor[typeof<'T>.Name] |> snd)
            >> Symbol.Kind.Transient
            >> Some
        else
            (kindConstructor[typeof<'T>.Name] |> fst)
            >> Some
        |> funApply symbol
        
    let inline private returnNone _ = None
    /// <summary>
    /// We determine the type of our symbol based on the canonical declaration.
    /// Otherwise, it is arbitrary.
    /// Either way, we can have full access to any API we create for compatible symbol types
    /// through the API defined on the specific symbol type interfaces.
    /// </summary>
    /// <param name="symbol"></param>
    /// <param name="declaration">The first 'canonical' declaration for a symbol</param>
    let private makeKind (symbol: ISymbol) (declaration: Ts.Declaration) =
        symbol
        |> match Node.DeclarationKind.create (ISymbol.program symbol) declaration with
            | Node.DeclarationKind.Variable _ -> constructKind<Symbol.Variable> 
            | Node.DeclarationKind.TypeAlias _ -> constructKind<Symbol.TypeAlias>
            | Node.DeclarationKind.Function _ -> constructKind<Symbol.Function>
            | Node.DeclarationKind.Parameter _ -> constructKind<Symbol.Parameter>
            | Node.DeclarationKind.Interface _ -> constructKind<Symbol.Interface> 
            | Node.DeclarationKind.Property _ -> constructKind<Symbol.Property>
            | Node.DeclarationKind.Method _ -> constructKind<Symbol.Method>
            | Node.DeclarationKind.Signature _ -> constructKind<Symbol.Signature>
            | Node.DeclarationKind.TypeParameter _ -> constructKind<Symbol.TypeParameter>
            | Node.DeclarationKind.Module _ ->
                if symbol |> ISymbol.hasFlag Ts.SymbolFlags.ValueModule
                then constructKind<Symbol.ValueModule>
                else constructKind<Symbol.NamespaceModule>
            | Node.DeclarationKind.GetAccessor _ -> constructKind<Symbol.GetAccessor>
            | Node.DeclarationKind.SetAccessor _ -> constructKind<Symbol.SetAccessor>
            | Node.DeclarationKind.Class _ -> constructKind<Symbol.Class>
            | Node.DeclarationKind.EnumMember _ -> constructKind<Symbol.EnumMember>
            | Node.DeclarationKind.Enum _ ->
                if symbol |> ISymbol.hasFlag Ts.SymbolFlags.ConstEnum
                then constructKind<Symbol.ConstEnum>
                else constructKind<Symbol.TypeEnum>
            // Not implemented, looking for fails to investigate shape
            | Node.DeclarationKind.Type _ -> constructKind<Symbol.TypeLiteral>
            // Not implemented, looking for fails to investigate shape
            | Node.DeclarationKind.Constructor _ -> constructKind<Symbol.Constructor> 
            | Node.DeclarationKind.ImportExport _ -> returnNone
    let private createFallback (symbol: ISymbol) =
        let program = ISymbol.program symbol
        symbol
        |> program.getTypeChecker().getRootSymbols
        |> _.AsArray
        |> Array.tryPick (_.getDeclarations() >> Option.bind (_.AsArray >> Array.tryHead))
        |> Option.bind (makeKind symbol)
        |> Option.defaultValue (Symbol.Transient.Kind.Unknown symbol |> Symbol.Kind.Transient)
    /// <summary>
    /// Use this to simplify your pattern matching.
    /// The motivation for this is the material difference between transient
    /// and non transient symbols only being the guarantee of the value declaration node.
    /// If this difference is not a deterrent for you, then you can half the number of pattern matches
    /// using this pattern. You can disambiguate true transients still via the _.isTransient property
    /// </summary>
    let foldToTransientKind = function
        | Symbol.Kind.Class kind -> kind :> Symbol.Transient.Class |> Symbol.Transient.Kind.Class
        | Symbol.Kind.Parameter kind -> kind :> Symbol.Transient.Parameter |> Symbol.Transient.Kind.Parameter 
        | Symbol.Kind.Variable kind -> kind :> Symbol.Transient.Variable |> Symbol.Transient.Kind.Variable 
        | Symbol.Kind.Property kind -> kind :> Symbol.Transient.Property |> Symbol.Transient.Kind.Property 
        | Symbol.Kind.EnumMember kind -> kind :> Symbol.Transient.EnumMember |> Symbol.Transient.Kind.EnumMember 
        | Symbol.Kind.Function kind -> kind :> Symbol.Transient.Function |> Symbol.Transient.Kind.Function 
        | Symbol.Kind.Interface kind -> kind :> Symbol.Transient.Interface |> Symbol.Transient.Kind.Interface 
        | Symbol.Kind.ConstEnum kind -> kind :> Symbol.Transient.ConstEnum |> Symbol.Transient.Kind.ConstEnum 
        | Symbol.Kind.TypeEnum kind -> kind :> Symbol.Transient.TypeEnum |> Symbol.Transient.Kind.TypeEnum 
        | Symbol.Kind.ValueModule kind -> kind :> Symbol.Transient.ValueModule |> Symbol.Transient.Kind.ValueModule 
        | Symbol.Kind.NamespaceModule kind -> kind :> Symbol.Transient.NamespaceModule |> Symbol.Transient.Kind.NamespaceModule 
        | Symbol.Kind.TypeLiteral kind -> kind :> Symbol.Transient.TypeLiteral |> Symbol.Transient.Kind.TypeLiteral 
        | Symbol.Kind.ObjectLiteral kind -> kind :> Symbol.Transient.ObjectLiteral |> Symbol.Transient.Kind.ObjectLiteral 
        | Symbol.Kind.Method kind -> kind :> Symbol.Transient.Method |> Symbol.Transient.Kind.Method 
        | Symbol.Kind.Constructor kind -> kind :> Symbol.Transient.Constructor |> Symbol.Transient.Kind.Constructor 
        | Symbol.Kind.GetAccessor kind -> kind :> Symbol.Transient.GetAccessor |> Symbol.Transient.Kind.GetAccessor 
        | Symbol.Kind.SetAccessor kind -> kind :> Symbol.Transient.SetAccessor |> Symbol.Transient.Kind.SetAccessor 
        | Symbol.Kind.Signature kind -> kind :> Symbol.Transient.Signature |> Symbol.Transient.Kind.Signature 
        | Symbol.Kind.TypeParameter kind -> kind :> Symbol.Transient.TypeParameter |> Symbol.Transient.Kind.TypeParameter 
        | Symbol.Kind.TypeAlias kind -> kind :> Symbol.Transient.TypeAlias |> Symbol.Transient.Kind.TypeAlias 
        | Symbol.Kind.Transient kind -> kind
        
    let inline failIfNone<'T> (symbol: ISymbol) (value: 'T option) =
        value
        |> Option.defaultWith(fun () ->
            let genArgs =
                typeof<'T>.GenericTypeArguments
                |> NonEmptyArray.create
            let name =
                genArgs
                |> Option.map _.Value.Name
                |> Option.defaultValue typeof<'T>.Name
            let declarations =
                ISymbol.declarations symbol
                |> Option.map (_.Values >> Array.map _.kind.Name)
                |> Option.defaultValue [||]
            Logging.Log.Default.logfe "Symbol.Kind: Failed to retrieve a canonical declaration of kind %s. Found declarations: %A" name declarations
            failwithf $"Symbol.Kind: Failed to retrieve a canonical declaration of kind %s{name}"
        )
    let inline private pairWithType (symbol: ISymbol) (node: ^T option when ^T :> IErasedWrapper<^U> and ^U :> Ts.Node) =
        node
        |> Option.map (fun node ->
            node, (ISymbol.checker symbol).getTypeOfSymbolAtLocation(symbol, node.Value))
    let inline private pairWithTypes (symbol: ISymbol) nodes =
        nodes
        |> Option.map (NonEmptyArray.map (Some >> pairWithType symbol >> Option.get))
    let inline private pairWithTypeByMap (symbol: ISymbol) map node =
        node |> Option.map (fun node ->
            node, (ISymbol.checker symbol).getTypeOfSymbolAtLocation(symbol, map node))
    let inline private pairWithTypesByMap (symbol: ISymbol) map nodes =
        nodes |> Option.map (NonEmptyArray.map (Some >> pairWithTypeByMap symbol map >> Option.get))
        
    let tryValueDeclaration (symbol: #Symbol.Transient.IValue) =
        symbol
        |> ISymbol.toSymbol
        |> _.valueDeclaration
        // |> Option.map (Node.DeclarationKind.create (ISymbol.program symbol))
        
    let tryParameterDeclaration (symbol: #Symbol.Transient.IParameter) =
        ISymbol.tryPickDeclaration (Node.ParameterKind.tryCreate (ISymbol.program symbol)) symbol
    let tryParameterDeclarationAndType (symbol: #Symbol.Transient.IParameter) =
        tryParameterDeclaration symbol
        |> pairWithTypeByMap symbol _.Value
        
    let tryVariableDeclaration (variable: #Symbol.Transient.IVariable) =
        ISymbol.tryPickDeclaration (Node.Variable.tryCreate (ISymbol.program variable)) variable
    let tryVariableDeclarationAndType (variable: #Symbol.Transient.IVariable) =
        tryVariableDeclaration variable
        |> pairWithType variable
        
    let tryPropertyDeclarations (property: #Symbol.Transient.IProperty) =
        ISymbol.chooseDeclarations (Node.PropertyKind.tryCreate (ISymbol.program property)) property
    let tryPropertyDeclarationsAndTypes (property: #Symbol.Transient.IProperty) =
        tryPropertyDeclarations property
        |> pairWithTypesByMap property _.Value
    
    let tryEnumMemberDeclaration (enumMember: #Symbol.Transient.IEnumMember) =
        ISymbol.tryPickDeclaration (Node.EnumMember.tryCreate (ISymbol.program enumMember)) enumMember
    let tryEnumMemberDeclarationAndType (enumMember: #Symbol.Transient.IEnumMember)  =
        tryEnumMemberDeclaration enumMember
        |> pairWithTypeByMap enumMember _.Value
    
    let tryFunctionDeclarations (symbol: #Symbol.Transient.IFunction) =
        ISymbol.chooseDeclarations (
            Patterns.Node.(|FunctionDeclaration|_|)
            >> Option.map (Node.FunctionDeclaration.create (ISymbol.program symbol))
            ) symbol
    let tryFunctionDeclarationAndTypes (symbol: #Symbol.Transient.IFunction) =
        tryFunctionDeclarations symbol
        |> pairWithTypes symbol 
    
    let tryClassDeclaration (symbol: #Symbol.Transient.IClass) =
        ISymbol.tryPickDeclaration
            (
                Patterns.Node.(|ClassDeclaration|_|)
                >> Option.map (Node.ClassDeclaration.create (ISymbol.program symbol))
            )
            symbol
    let tryClassDeclarationAndType (symbol: #Symbol.Transient.IClass) =
        tryClassDeclaration symbol
        |> pairWithType symbol
    
    let tryMethodDeclarations (symbol: #Symbol.Transient.IMethod) =
        ISymbol.chooseDeclarations (Node.MethodKind.tryCreate (ISymbol.program symbol)) symbol
    let tryMethodDeclarationsAndTypes (symbol: #Symbol.Transient.IMethod) =
        tryMethodDeclarations symbol
        |> pairWithTypesByMap symbol _.Value
    
    let tryConstructorDeclaration (symbol: #Symbol.Transient.IConstructor) =
        ISymbol.tryPickDeclaration (
            Patterns.Node.(|ConstructorDeclaration|_|)
            >> Option.map (Node.ConstructorDeclaration.create (ISymbol.program symbol))
            ) symbol
    let tryConstructorDeclarationAndType (symbol: #Symbol.Transient.IConstructor) =
        tryConstructorDeclaration symbol
        |> pairWithType symbol
        
    let trySignatureDeclarations (symbol: #Symbol.Transient.ISignature) =
        ISymbol.tryPickDeclaration (Node.SignatureKind.tryCreate  (ISymbol.program symbol)) symbol
    let trySignatureDeclarationsAndTypes (symbol: #Symbol.Transient.ISignature) =
        trySignatureDeclarations symbol
        |> pairWithTypeByMap symbol _.Value
    
    let tryEnumDeclaration (symbol: #Symbol.Transient.IEnum) =
        ISymbol.chooseDeclarations (
            Patterns.Node.(|EnumDeclaration|_|)
            >> Option.map (Node.EnumDeclaration.create (ISymbol.program symbol))
            ) symbol
    let tryEnumDeclarationAndType (symbol: #Symbol.Transient.IEnum) =
        tryEnumDeclaration symbol
        |> pairWithTypes symbol
    
    let tryNamespaceDeclarations (symbol: #Symbol.Transient.INamespace) =
        ISymbol.chooseDeclarations
            (
            Patterns.Node.(|ModuleDeclaration|_|)
            >> Option.map (Node.ModuleDeclaration.create (ISymbol.program symbol))
            )
            symbol
    let tryNamespaceDeclarationsAndTypes (symbol: #Symbol.Transient.INamespace) =
        tryNamespaceDeclarations symbol
        |> pairWithTypes symbol
    
    let tryModuleDeclarations (symbol: #Symbol.Transient.IValueModule) =
        ISymbol.chooseDeclarations (Node.ModuleKind.tryCreate (ISymbol.program symbol)) symbol
    let tryModuleDeclarationsAndTypes (symbol: #Symbol.Transient.IValueModule) =
        tryModuleDeclarations symbol
        |> pairWithTypesByMap symbol Node.ModuleKind.toDeclaration
    
    let tryTypeParameterDeclarations (symbol: #Symbol.Transient.ITypeParameter) =
        ISymbol.chooseDeclarations (Patterns.Node.(|TypeParameterDeclaration|_|) >> Option.map (Node.TypeParameterDeclaration.create (ISymbol.program symbol))) symbol
    let tryTypeParameterDeclarationsAndTypes (symbol: #Symbol.Transient.ITypeParameter) =
        tryTypeParameterDeclarations symbol
        |> pairWithTypes symbol
    
    let tryTypeAliasDeclaration (symbol: #Symbol.Transient.ITypeAlias) =
        ISymbol.tryPickDeclaration (Patterns.Node.(|TypeAliasDeclaration|_|) >> Option.map (Node.TypeAliasDeclaration.create (ISymbol.program symbol))) symbol
    let tryTypeAliasDeclarationAndType (symbol: #Symbol.Transient.ITypeAlias) =
        tryTypeAliasDeclaration symbol
        |> pairWithType symbol
    
    let tryInterfaceDeclarations (symbol: #Symbol.Transient.IInterface) =
        ISymbol.chooseDeclarations (Patterns.Node.(|InterfaceDeclaration|_|) >> Option.map (Node.InterfaceDeclaration.create (ISymbol.program symbol))) symbol
    let tryInterfaceDeclarationsAndTypes (symbol: #Symbol.Transient.IInterface) =
        tryInterfaceDeclarations symbol
        |> pairWithTypes symbol
    
    let tryGetAccessorDeclaration (symbol: #Symbol.Transient.IGetAccessor) =
        ISymbol.tryPickDeclaration (Patterns.Node.(|GetAccessorDeclaration|_|) >> Option.map (Node.GetAccessorDeclaration.create (ISymbol.program symbol))) symbol
    let tryGetAccessorDeclarationAndType (symbol: #Symbol.Transient.IGetAccessor) =
        tryGetAccessorDeclaration symbol
        |> pairWithType symbol
    
    let trySetAccessorDeclaration (symbol: #Symbol.Transient.ISetAccessor) =
        ISymbol.tryPickDeclaration (Patterns.Node.(|SetAccessorDeclaration|_|) >> Option.map (Node.SetAccessorDeclaration.create (ISymbol.program symbol))) symbol
    let trySetAccessorDeclarationAndType (symbol: #Symbol.Transient.ISetAccessor) =
        trySetAccessorDeclaration symbol
        |> pairWithType symbol
    
    let tryClassMemberDeclarations (symbol: #Symbol.Transient.IClassMember) =
        ISymbol.chooseDeclarations (Node.ClassMemberKind.tryCreate (ISymbol.program symbol)) symbol
    let tryClassMemberDeclarationsAndTypes (symbol: #Symbol.Transient.IClassMember) =
        tryClassMemberDeclarations symbol
        |> pairWithTypesByMap symbol _.Value
    
    let parameterDeclarations (symbol: #Symbol.Transient.Parameter) =
        tryParameterDeclaration symbol |> failIfNone symbol
    let parameterDeclarationAndType (symbol: #Symbol.Transient.Parameter) =
        tryParameterDeclarationAndType symbol |> failIfNone symbol
    
    let methodDeclarations (symbol: #Symbol.Transient.Method) =
        tryMethodDeclarations symbol |> failIfNone symbol
    let methodDeclarationsAndTypes (symbol: #Symbol.Transient.Method) =
        tryMethodDeclarationsAndTypes symbol |> failIfNone symbol
    
    let propertyDeclarations (symbol: #Symbol.Transient.Property) =
        tryPropertyDeclarations symbol |> failIfNone symbol
    let propertyDeclarationsAndTypes (symbol: #Symbol.Transient.Property) =
        tryPropertyDeclarationsAndTypes symbol |> failIfNone symbol
    
    let variableDeclaration (symbol: #Symbol.Transient.Variable) =
        tryVariableDeclaration symbol |> failIfNone symbol
    let variableDeclarationAndType (symbol: #Symbol.Transient.Variable) =
        tryVariableDeclarationAndType symbol |> failIfNone symbol
    
    let enumMemberDeclaration (symbol: #Symbol.Transient.EnumMember) =
        tryEnumMemberDeclaration symbol |> failIfNone symbol
    let enumMemberDeclarationAndType (symbol: #Symbol.Transient.EnumMember) =
        tryEnumMemberDeclarationAndType symbol |> failIfNone symbol
    
    let functionDeclarations (symbol: #Symbol.Transient.Function) =
        tryFunctionDeclarations symbol |> failIfNone symbol
    let functionDeclarationAndTypes (symbol: #Symbol.Transient.Function) =
        tryFunctionDeclarationAndTypes symbol |> failIfNone symbol
    
    let classDeclaration (symbol: #Symbol.Transient.Class) =
        tryClassDeclaration symbol |> failIfNone symbol
    let classDeclarationAndType (symbol: #Symbol.Transient.Class) =
        tryClassDeclarationAndType symbol |> failIfNone symbol
    
    let interfaceDeclarations (symbol: #Symbol.Transient.Interface) =
        tryInterfaceDeclarations symbol |> failIfNone symbol
    let interfaceDeclarationsAndTypes (symbol: #Symbol.Transient.Interface) =
        tryInterfaceDeclarationsAndTypes symbol |> failIfNone symbol

    let typeAliasDeclarations (symbol: #Symbol.Transient.TypeAlias) =
        tryTypeAliasDeclaration symbol |> failIfNone symbol
    let typeAliasDeclarationAndType (symbol: #Symbol.Transient.TypeAlias) =
        tryTypeAliasDeclarationAndType symbol |> failIfNone symbol
    
    let constructorDeclaration (symbol: #Symbol.Transient.Constructor) =
        tryConstructorDeclaration symbol |> failIfNone symbol
    let constructorDeclarationAndType (symbol: #Symbol.Transient.Constructor) =
        tryConstructorDeclarationAndType symbol |> failIfNone symbol
    
    let signatureDeclarations (symbol: #Symbol.Transient.Signature) =
        trySignatureDeclarations symbol |> failIfNone symbol
    let signatureDeclarationAndType (symbol: #Symbol.Transient.Signature) =
        trySignatureDeclarationsAndTypes symbol |> failIfNone symbol
    
    let constEnumDeclarations (symbol: #Symbol.Transient.ConstEnum) =
        tryEnumDeclaration symbol |> failIfNone symbol
    let constEnumDeclarationAndTypes (symbol: #Symbol.Transient.ConstEnum) =
        tryEnumDeclarationAndType symbol |> failIfNone symbol
    
    let typeEnumDeclarations (symbol: #Symbol.Transient.TypeEnum) =
        tryEnumDeclaration symbol |> failIfNone symbol
    let typeEnumDeclarationAndTypes (symbol: #Symbol.Transient.TypeEnum) =
        tryEnumDeclarationAndType symbol |> failIfNone symbol
    
    let moduleDeclarations (symbol: #Symbol.Transient.ValueModule) =
        tryModuleDeclarations symbol |> failIfNone symbol
    let moduleDeclarationAndTypes (symbol: #Symbol.Transient.ValueModule) =
        tryModuleDeclarationsAndTypes symbol |> failIfNone symbol
    
    let namespaceDeclarations (symbol: #Symbol.Transient.NamespaceModule) =
        tryNamespaceDeclarations symbol |> failIfNone symbol
    let namespaceDeclarationsAndTypes (symbol: #Symbol.Transient.NamespaceModule) =
        tryNamespaceDeclarationsAndTypes symbol |> failIfNone symbol
    
    let getAccessorDeclaration (symbol: #Symbol.Transient.GetAccessor) =
        tryGetAccessorDeclaration symbol |> failIfNone symbol
    let getAccessorDeclarationAndType (symbol: #Symbol.Transient.GetAccessor) =
        tryGetAccessorDeclarationAndType symbol |> failIfNone symbol
    
    let setAccessorDeclaration (symbol: #Symbol.Transient.SetAccessor) =
        trySetAccessorDeclaration symbol |> failIfNone symbol
    let setAccessorDeclarationAndType (symbol: #Symbol.Transient.SetAccessor) =
        trySetAccessorDeclarationAndType symbol |> failIfNone symbol
    
    let typeParameterDeclarations (symbol: #Symbol.Transient.TypeParameter) =
        tryTypeParameterDeclarations symbol |> failIfNone symbol
    let typeParameterDeclarationsAndTypes (symbol: #Symbol.Transient.TypeParameter) =
        tryTypeParameterDeclarationsAndTypes symbol |> failIfNone symbol
    
    type CanonicalDeclarationSRTPHelper =
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.TypeParameter) = typeParameterDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.TypeAlias) = typeAliasDeclarations symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Interface) = interfaceDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Class) = classDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Method) = methodDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Property) = propertyDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Variable) = variableDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Function) = functionDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Constructor) = constructorDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Signature) = signatureDeclarations symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.GetAccessor) = getAccessorDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.SetAccessor) = setAccessorDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.EnumMember) = enumMemberDeclaration symbol
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.ConstEnum) = constEnumDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.TypeEnum) = typeEnumDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.ValueModule) = moduleDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.NamespaceModule) = namespaceDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclaration (symbol: Symbol.Transient.Parameter) = parameterDeclarations symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Parameter) = parameterDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.TypeParameter) = typeParameterDeclarationsAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.TypeAlias) = typeAliasDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Interface) = interfaceDeclarationsAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Class) = classDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Method) = methodDeclarationsAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Property) = propertyDeclarationsAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Variable) = variableDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Function) = functionDeclarationAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Constructor) = constructorDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.Signature) = signatureDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.GetAccessor) = getAccessorDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.SetAccessor) = setAccessorDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.EnumMember) = enumMemberDeclarationAndType symbol
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.ConstEnum) =
            constEnumDeclarationAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.TypeEnum) =
            typeEnumDeclarationAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.ValueModule) =
            moduleDeclarationAndTypes symbol |> _.Value
        static member inline GetCanonicalDeclarationAndType (symbol: Symbol.Transient.NamespaceModule) = namespaceDeclarationsAndTypes symbol |> _.Value
    
    let inline canonicalDeclaration symbol =
        ((^T or CanonicalDeclarationSRTPHelper):(static member GetCanonicalDeclaration: ^T -> ^U) symbol)
    let inline canonicalDeclarationAndType symbol =
        ((^T or CanonicalDeclarationSRTPHelper):(static member GetCanonicalDeclarationAndType: ^T -> ^U) symbol)
    
    let parameterValueDeclaration (symbol: Symbol.Parameter) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Node.ParameterKind.tryCreate (ISymbol.program symbol)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Parameter was an unexpected kind")
    let methodValueDeclaration (symbol: Symbol.Method) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Node.MethodKind.tryCreate (ISymbol.program symbol)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Method was an unexpected kind")
    let propertyValueDeclaration (symbol: Symbol.Property) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Node.PropertyKind.tryCreate (ISymbol.program symbol)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Property was an unexpected kind")
    let classValueDeclaration (symbol: Symbol.Class) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|ClassDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Class was an unexpected kind")
    let variableValueDeclaration (symbol: Symbol.Variable) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|VariableDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Variable was an unexpected kind")
    let enumMemberValueDeclaration (symbol: Symbol.EnumMember) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|EnumMember|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of EnumMember was an unexpected kind")
    let functionValueDeclaration (symbol: Symbol.Function) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|FunctionDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of Function was an unexpected kind")
    let constEnumValueDeclaration (symbol: Symbol.ConstEnum) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|EnumDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of ConstEnum was an unexpected kind")
    let valueModuleValueDeclaration (symbol: Symbol.ValueModule) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Node.ModuleKind.tryCreate (ISymbol.program symbol)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of ValueModule was an unexpected kind")
    let getAccessorValueDeclaration (symbol: Symbol.GetAccessor) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|GetAccessorDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of GetAccessor was an unexpected kind")
    let setAccessorValueDeclaration (symbol: Symbol.SetAccessor) =
        tryValueDeclaration symbol
        |> failIfNone symbol
        |> Patterns.Node.(|SetAccessorDeclaration|_|)
        |> Option.defaultWith (fun () -> failwith "ValueDeclaration for a canonical symbol of SetAccessor was an unexpected kind")
    
    type ValueDeclarationSRTPHelper =
        static member inline GetValueDeclaration symbol = parameterValueDeclaration symbol
        static member inline GetValueDeclaration symbol = methodValueDeclaration symbol
        static member inline GetValueDeclaration symbol = propertyValueDeclaration symbol
        static member inline GetValueDeclaration symbol = classValueDeclaration symbol
        static member inline GetValueDeclaration symbol = variableValueDeclaration symbol
        static member inline GetValueDeclaration symbol = enumMemberValueDeclaration symbol
        static member inline GetValueDeclaration symbol = functionValueDeclaration symbol
        static member inline GetValueDeclaration symbol = constEnumValueDeclaration symbol
        static member inline GetValueDeclaration symbol = valueModuleValueDeclaration symbol
        static member inline GetValueDeclaration symbol = getAccessorValueDeclaration symbol
        static member inline GetValueDeclaration symbol = setAccessorValueDeclaration symbol
    let inline valueDeclaration symbol =
        ((^T or ValueDeclarationSRTPHelper):(static member GetValueDeclaration: ^T -> ^U) symbol)
    
    let isTransient (symbol: #Symbol.ITransient) = ISymbol.hasFlag Ts.SymbolFlags.Transient symbol
    let isOptional (symbol: #Symbol.IOptional) = ISymbol.hasFlag Ts.SymbolFlags.Optional symbol
    
    let inline private wrapImpl (symbol: ISymbol) =
        symbol
        |> ISymbol.toSymbol
        |> _.getDeclarations()
        |> Option.bind (_.AsArray >> Array.tryHead)
        |> Option.bind (makeKind symbol)
        |> Option.defaultWith(fun () -> createFallback symbol)
        
    let typeWrapper: SymbolTypeWrapper = {
        IntermediateFn = ISymbol.create
        WrapFn = wrapImpl
        KeyFn = ISymbol.symbolKey
    }
    
    module Kind =
        let create (symbol: ISymbol): Symbol.Kind =
            ISymbol.program symbol
            |> _.CompositeCollection
            |> CompositeCollection.Symbol.registerToWrapper
            |> funApply symbol
            
        let createFromSymbol program symbol =
            ISymbol.create program symbol |> create
            

[<RequireQualifiedAccess>]
module Type =
    module Internal =
        let inline unsafeGetCanonicalSymbol (typ: ^T when ^T :> IErasedWrapper<^U> and ^T :> IAlwaysSymbol and ^U :> Ts.Type) = typ.Value.unsafeGetCanonicalSymbol() |> Symbol.Kind.createFromSymbol typ.Program
        let inline getCanonicalSymbol (typ: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Type and ^T :> ICanHaveSymbol) = typ.Value.getCanonicalSymbol() |> Option.map (Symbol.Kind.createFromSymbol typ.Program)
        let inline getTypeKey (typ: ^T when ^T :> IErasedWrapper<^U> and ^U :> Ts.Type) = typ.Value.TypeKey
        let inline getUnionTypeKey (typ: ^T when ^T :> IFastUnionUnwrappable<^U> and ^U :> Ts.Type) = typ.Value.TypeKey
    [<Literal>]
    let private primitiveSingletonFlags =
        TF.Number
        ||| TF.String
        ||| TF.Boolean
        ||| TF.ESSymbol
        ||| TF.NonPrimitive
        ||| TF.BigInt
        ||| TF.Undefined
        ||| TF.Null
        ||| TF.Void
        ||| TF.Never
        ||| TF.Any
        ||| TF.Unknown
    [<Literal>]
    let private primitiveInstantiableFlags =
        TF.Index
        ||| TF.TemplateLiteral
        ||| TF.StringMapping
    [<Literal>]
    let private nonPrimitiveInstantiableFlags =
        TF.TypeParameter
        ||| TF.IndexedAccess
        ||| TF.Conditional
        ||| TF.Substitution
    [<Literal>]
    let private literalFlags =
        TF.StringLiteral
        ||| TF.NumberLiteral
        ||| TF.BigIntLiteral
        ||| TF.BooleanLiteral
        ||| TF.EnumLiteral
        ||| TF.UniqueESSymbol
    [<Literal>]
    let private enumFlags = TF.Enum 
    [<Literal>]
    let private structuralFlags =
        TF.Object
        ||| TF.Union
        ||| TF.Intersection
    module StringLiteralType =
        let create program typ: Type.StringLiteralType = IErasedWrapper.create program typ
        let value: Type.StringLiteralType -> _ = IErasedWrapper.map _.value
        let typeKey: Type.StringLiteralType -> _ = Internal.getTypeKey
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.StringLiteralType -> _ = ignore
    module NumberLiteralType =
        let create program typ: Type.NumberLiteralType = IErasedWrapper.create program typ
        let value: Type.NumberLiteralType -> _ = IErasedWrapper.map _.value
        let typeKey: Type.NumberLiteralType -> _ = Internal.getTypeKey
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.NumberLiteralType -> _ = ignore
    module BigIntLiteralType =
        let create program typ: Type.BigIntLiteralType = IErasedWrapper.create program typ
        let value: Type.BigIntLiteralType -> _ = IErasedWrapper.map _.value
        let typeKey: Type.BigIntLiteralType -> _ = Internal.getTypeKey
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.BigIntLiteralType -> _ = ignore
    module LiteralType =
        let create program typ: Type.LiteralType = IErasedWrapper.create program typ
        let value: Type.LiteralType -> _ = IErasedWrapper.map _.value
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.LiteralType -> _ = ignore
        let typeKey: Type.LiteralType -> _ = Internal.getTypeKey
    module UniqueESSymbol =
        let create program typ: Type.UniqueESSymbol = IErasedWrapper.create program typ
        let name: Type.UniqueESSymbol -> _ = IErasedWrapper.map (_.escapedName >> SymbolName.Create)
        let typeKey: Type.UniqueESSymbol -> _ = Internal.getTypeKey
        let symbol: Type.UniqueESSymbol -> _ = Internal.unsafeGetCanonicalSymbol
    module StringMappingType =
        let create program typ: Type.StringMappingType = IErasedWrapper.create program typ
        let typeKey: Type.StringMappingType -> _ = Internal.getTypeKey
        let symbol: Type.StringMappingType -> _ = Internal.unsafeGetCanonicalSymbol
        let type': Type.StringMappingType -> _ = _.MapWithProgram(fun program -> _.``type`` >> Kind.createFromType program)
    module TypeParameter =
        let create program typ: Type.TypeParameter = IErasedWrapper.create program typ
        let symbol: Type.TypeParameter -> _ = Internal.unsafeGetCanonicalSymbol
        let isThisTypeParameter: Type.TypeParameter -> bool = _.MapWithProgram(fun program ->
            _.symbol >> ISymbol.create program >> ISymbol.hasFlag SF.TypeParameter >> not)
        let typeKey: Type.TypeParameter -> _ = Internal.getTypeKey
    module Intersection =
        let create program typ: Type.Intersection = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.Intersection -> _ = ignore
        let types: Type.Intersection -> _ = _.MapWithProgram(fun program ->
            _.types.AsArray
            >> Array.map (Kind.createFromType program)
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "Intersection type has no members")
            )
        let typeKey: Type.Intersection -> _ = Internal.getTypeKey
    module Union =
        let create program typ: Type.Union = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.Union -> _ = ignore
        let types: Type.Union -> _ = _.MapWithProgram(fun program ->
            _.types.AsArray
            >> Array.map (Kind.createFromType program)
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "Union type has no members")
            )
        let typeKey: Type.Union -> _ = Internal.getTypeKey
    module TemplateLiteral =
        let create program typ: Type.TemplateLiteral = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.TemplateLiteral -> _ = ignore
        let texts: Type.TemplateLiteral -> _ = IErasedWrapper.map (_.texts >> NonEmptyArray.create >> Option.defaultWith (fun () -> failwith "TemplateLiteral type has no texts"))
        let types: Type.TemplateLiteral -> _ = _.MapWithProgram(fun program ->
            _.types.AsArray
            >> Array.map (Kind.createFromType program)
            >> NonEmptyArray.create
            >> Option.defaultWith (fun () -> failwith "TemplateLiteral type has no types")
            )
        let typeKey: Type.TemplateLiteral -> _ = Internal.getTypeKey
    module Index =
        let create program typ: Type.Index = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.Index -> _ = ignore
        // TODO - narrow
        let type': Type.Index -> _ = _.MapWithProgram(fun program -> _.``type`` >> unbox<Ts.Type> >> Kind.createFromType program)
        let typeKey: Type.Index -> _ = Internal.getTypeKey
    module Substitution =
        let create program typ: Type.Substitution = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.Substitution -> _ = ignore
        let ``constraint``: Type.Substitution -> _ = _.MapWithProgram(fun program -> _.``constraint`` >> Kind.createFromType program)
        let baseType: Type.Substitution -> _ = _.MapWithProgram(fun program -> _.baseType >> Kind.createFromType program)
        let typeKey: Type.Substitution -> _ = Internal.getTypeKey
    module Conditional =
        let create program typ: Type.Conditional = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.Conditional -> _ = ignore
        let checkType: Type.Conditional -> _ = _.MapWithProgram(fun program -> _.checkType >> Kind.createFromType program)
        let extendsType: Type.Conditional -> _ = _.MapWithProgram(fun program -> _.extendsType >> Kind.createFromType program)
        let resolvedFalseType: Type.Conditional -> _ = _.MapWithProgram(fun program -> _.resolvedFalseType >> Option.map (Kind.createFromType program))
        let resolvedTrueType: Type.Conditional -> _ = _.MapWithProgram(fun program -> _.resolvedTrueType >> Option.map (Kind.createFromType program))
        let root: Type.Conditional -> IErasedWrapper<Ts.ConditionalRoot> = IErasedWrapper.wrappedMap _.root
        let typeKey: Type.Conditional -> _ = Internal.getTypeKey
    module IndexedAccess =
        let create program typ: Type.IndexedAccess = IErasedWrapper.create program typ
        [<System.Obsolete("No symbol", true)>]
        let symbol: Type.IndexedAccess -> _ = ignore
        [<System.Obsolete("Never seems to provide a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let ``constraint``: Type.IndexedAccess -> _ = _.MapWithProgram(fun program -> _.``constraint`` >> Option.map (Kind.createFromType program))
        let indexType: Type.IndexedAccess -> _ = _.MapWithProgram(fun program -> _.indexType >> Kind.createFromType program)
        let objectType: Type.IndexedAccess -> _ = _.MapWithProgram(fun program -> _.objectType >> Kind.createFromType program)
        let typeKey: Type.IndexedAccess -> _ = Internal.getTypeKey
    module Interface =
        let create program typ: Type.Interface = IErasedWrapper.create program typ
        let symbol: Type.Interface -> _ = Internal.unsafeGetCanonicalSymbol
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let typeParameters: Type.Interface -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.typeParameters
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let localTypeParameters: Type.Interface -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.localTypeParameters
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let outerTypeParameters: Type.Interface -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.outerTypeParameters
        let thisType: Type.Interface -> Type.TypeParameter option = IErasedWrapper.wrappedMapMaybe _.thisType
        let typeKey: Type.Interface -> _ = Internal.getTypeKey
    module Class =
        let create program typ: Type.Class = IErasedWrapper.create program typ
        let symbol: Type.Class -> _ = Internal.unsafeGetCanonicalSymbol
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let typeParameters: Type.Class -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.typeParameters
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let localTypeParameters: Type.Class -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.localTypeParameters
        [<System.Obsolete("Never provides a value"); EditorBrowsable(EditorBrowsableState.Never)>]
        let outerTypeParameters: Type.Class -> Type.TypeParameter NonEmptyArray option = IErasedWrapper.wrappedArrayMapMaybe _.outerTypeParameters
        let thisType: Type.Class -> Type.TypeParameter = IErasedWrapper.wrappedMapMaybe _.thisType >> Option.defaultWith(fun () -> failwith "Class has no thisType")
        let typeKey: Type.Class -> _ = Internal.getTypeKey
    module PureTypeReference =
        let create program typ: Type.PureTypeReference = IErasedWrapper.create program typ
        let symbol: Type.PureTypeReference -> _ = Internal.unsafeGetCanonicalSymbol
        let node: Type.PureTypeReference -> _ = _.MapWithProgram(fun program typ ->
            typ.node
            |> Option.map (
                unbox<Ts.Node>
                >> function
                    | Patterns.Node.TypeReferenceNode node ->
                        Node.TypeReference.create program node
                        |> Choice1Of3
                    | Patterns.Node.TupleTypeNode node ->
                        Node.TupleType.create program node
                        |> Choice2Of3
                    | Patterns.Node.ArrayTypeNode node ->
                        Node.ArrayType.create program node
                        |> Choice3Of3
                    | node ->
                        failwithf "TypeReferenceNode expected for pure type reference, got %s. \n\tObject flags: %A\n\tType flags:%A\n\tHas symbol:%b\n\tType string: %s\n\tNode string: %s"
                            node.kind.Name
                            (typ.objectFlags.ToStringArray())
                            (typ.flags.ToStringArray())
                            (typ.getCanonicalSymbol().IsSome)
                            (program.getTypeChecker().typeToString typ)
                            (node.getText())
                )
            )
        let target: Type.PureTypeReference -> _ = _.MapWithProgram(fun program -> _.target >> Kind.createFromType program)
        let typeArguments: Type.PureTypeReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray >> Array.map (Kind.createFromType program)
                >> NonEmptyArray.create
                )
            >> Option.defaultWith (fun () -> failwith "PureTypeReference type has no type arguments")
            )
        let typeKey: Type.PureTypeReference -> _ = Internal.getTypeKey
    module InterfaceReference =
        let create program typ: Type.InterfaceReference = IErasedWrapper.create program typ
        let symbol: Type.InterfaceReference -> _ = Internal.unsafeGetCanonicalSymbol
        let target: Type.InterfaceReference -> _ = _.MapWithProgram(fun program -> _.target >> Kind.createFromType program)
        let typeArguments: Type.InterfaceReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray >> Array.map (Kind.createFromType program)
                >> NonEmptyArray.create
                )
            >> Option.defaultWith (fun () -> failwith "InterfaceReference type has no type arguments")
            )
        let typeKey: Type.InterfaceReference -> _ = Internal.getTypeKey
    module ClassReference =
        let create program typ: Type.ClassReference = IErasedWrapper.create program typ
        let symbol: Type.ClassReference -> _ = Internal.unsafeGetCanonicalSymbol
        let target: Type.ClassReference -> _ = _.MapWithProgram(fun program -> _.target >> Kind.createFromType program)
        let typeArguments: Type.ClassReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray >> Array.map (Kind.createFromType program)
                >> NonEmptyArray.create
                )
            )
        let typeKey: Type.ClassReference -> _ = Internal.getTypeKey
    module ArrayReference =
        let create program typ: Type.ArrayReference = IErasedWrapper.create program typ
        let symbol: Type.ArrayReference -> _ = Internal.unsafeGetCanonicalSymbol
        let target: Type.ArrayReference -> _ = _.MapWithProgram(fun program -> _.target >> Kind.createFromType program)
        let node: Type.ArrayReference -> _ = _.MapWithProgram(fun program ->
            _.node
            >> Option.map (
                unbox<Ts.Node>
                >> function
                    | Patterns.Node.TupleTypeNode node ->
                        Node.TupleType.create program node
                        |> Choice1Of3
                    | Patterns.Node.ArrayTypeNode node ->
                        Node.ArrayType.create program node
                        |> Choice2Of3
                    | Patterns.Node.TypeReferenceNode node ->
                        Node.TypeReference.create program node
                        |> Choice3Of3
                    | node -> failwithf "TupleReferenceNode expected for tuple reference, got %s" node.kind.Name
                )
            )
        let typeArguments: Type.ArrayReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray >> Array.map (Kind.createFromType program)
                >> NonEmptyArray.create
                )
            >> Option.defaultWith (fun () -> failwith "ArrayReference type has no type arguments")
            )
        let typeKey: Type.ArrayReference -> _ = Internal.getTypeKey
    module TupleReference =
        let create program typ: Type.TupleReference = IErasedWrapper.create program typ
        [<System.Obsolete(message = "No symbol", error = true)>]
        let symbol: Type.TupleReference -> _ = ignore
        let aliasSymbol: Type.TupleReference -> _ = _.MapWithProgram(fun program -> _.aliasSymbol >> Option.map (ISymbol.create program >> ISymbol.toSymbol))
        let target: Type.TupleReference -> _ = _.MapWithProgram(fun program -> _.target >> Kind.createFromType program)
        let node: Type.TupleReference -> _ = _.MapWithProgram(fun program ->
            _.node
            >> Option.map (
                unbox<Ts.Node>
                >> function
                    | Patterns.Node.TupleTypeNode node ->
                        Node.TupleType.create program node
                        |> Choice1Of3
                    | Patterns.Node.ArrayTypeNode node ->
                        Node.ArrayType.create program node
                        |> Choice2Of3
                    | Patterns.Node.TypeReferenceNode node ->
                        Node.TypeReference.create program node
                        |> Choice3Of3
                    | node -> failwithf "TupleReferenceNode expected for tuple reference, got %s" node.kind.Name
                )
            )
        let typeArguments: Type.TupleReference -> _ = _.MapWithProgram(fun program ->
            _.typeArguments
            >> Option.bind (
                _.AsArray >> Array.map (Kind.createFromType program)
                >> NonEmptyArray.create
                )
            )
        let typeKey: Type.TupleReference -> _ = Internal.getTypeKey
    module ObjectType =
        let create program typ: Type.ObjectType = IErasedWrapper.create program typ
        let typeFlag: Type.ObjectType -> _ = IErasedWrapper.map _.flags
        let objectFlags: Type.ObjectType -> _ = IErasedWrapper.map _.objectFlags
        let typeKey: Type.ObjectType -> _ = Internal.getTypeKey
    module ObjectRest =
        let create program typ: Type.ObjectRest = IErasedWrapper.create program typ
        let typeKey: Type.ObjectRest -> _ = Internal.getTypeKey
    module InstantiationExpression =
        let create program typ: Type.InstantiationExpression = IErasedWrapper.create program typ
        let typeKey: Type.InstantiationExpression -> _ = Internal.getTypeKey
    module AnonymousType =
        let create program typ: Type.AnonymousType = IErasedWrapper.create program typ
        let typeKey: Type.AnonymousType -> _ = Internal.getTypeKey
    module InstantiatedAnonymousType =
        let create program typ: Type.InstantiatedAnonymousType = IErasedWrapper.create program typ
        let typeKey: Type.InstantiatedAnonymousType -> _ = Internal.getTypeKey
    module MappedType =
        let create program typ: Type.MappedType = IErasedWrapper.create program typ
        let typeKey: Type.MappedType -> _ = Internal.getTypeKey
    module InstantiatedMappedType =
        let create program typ: Type.InstantiatedMappedType = IErasedWrapper.create program typ
        let typeKey: Type.InstantiatedMappedType -> _ = Internal.getTypeKey
    module Enum =
        let create (program: Ts.Program) typ: Type.Enum = IErasedWrapper.create program typ
        let isStandardEnumType (typ: Ts.Type) = typ.flags |> Enum.hasFlag TF.Enum
        let isAlternateEnum (typ: Ts.Type) =
            let mask = TF.Union ||| TF.EnumLiteral
            typ.flags |> Enum.mask mask |> (=) mask
        let isEnum typ = isStandardEnumType typ || isAlternateEnum typ
        let symbol: Type.Enum -> _ = Internal.unsafeGetCanonicalSymbol
        let typeKey: Type.Enum -> _ = Internal.getTypeKey
    module PrimitiveSingleton =
        let isPrimitiveSingleton: Ts.Type -> _ = _.flags >> Enum.hasMask primitiveSingletonFlags
        let unsafeCreate (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.Any -> Type.PrimitiveSingleton.Any typ
            | Enum.HasFlag TF.Unknown -> Type.PrimitiveSingleton.Unknown typ
            | Enum.HasFlag TF.Never -> Type.PrimitiveSingleton.Never typ
            | Enum.HasFlag TF.Void -> Type.PrimitiveSingleton.Void typ
            | Enum.HasFlag TF.Undefined -> Type.PrimitiveSingleton.Undefined typ
            | Enum.HasFlag TF.Null -> Type.PrimitiveSingleton.Null typ
            | Enum.HasFlag TF.String -> Type.PrimitiveSingleton.String typ
            | Enum.HasFlag TF.Number -> Type.PrimitiveSingleton.Number typ
            | Enum.HasFlag TF.Boolean -> Type.PrimitiveSingleton.Boolean typ
            | Enum.HasFlag TF.BigInt -> Type.PrimitiveSingleton.BigInt typ
            | Enum.HasFlag TF.ESSymbol -> Type.PrimitiveSingleton.ESSymbol typ
            | Enum.HasFlag TF.NonPrimitive -> Type.PrimitiveSingleton.NonPrimitive typ
            | _ -> failwith "Unexpected type flags"
        let tryCreate _ (typ: Ts.Type) =
            if not <| isPrimitiveSingleton typ then None else
            Some (unsafeCreate typ)
        let typeKey: Type.PrimitiveSingleton -> _ = Internal.getUnionTypeKey
    module PrimitiveLiteral =
        let isPrimitiveLiteral: Ts.Type -> _ = _.flags >> Enum.hasMask literalFlags
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.StringLiteral ->
                typ :?> Ts.StringLiteralType
                |> StringLiteralType.create program
                |> Type.PrimitiveLiteral.String
            | Enum.HasFlag TF.NumberLiteral ->
                typ :?> Ts.NumberLiteralType
                |> NumberLiteralType.create program
                |> Type.PrimitiveLiteral.Number
            | Enum.HasFlag TF.BigIntLiteral ->
                typ :?> Ts.BigIntLiteralType
                |> BigIntLiteralType.create program
                |> Type.PrimitiveLiteral.BigInt
            | Enum.HasFlag TF.BooleanLiteral ->
                typ :?> Ts.LiteralType
                |> LiteralType.create program
                |> Type.PrimitiveLiteral.Boolean
            | _ -> failwith "Unexpected type flags"
        let typeKey: Type.PrimitiveLiteral -> _ = Internal.getUnionTypeKey
    module EnumMember =
        let isEnumMember (typ: Ts.Type) =
            typ.flags.HasFlag TF.EnumLiteral
            && (typ.flags |> Enum.hasMask (TF.Enum ||| TF.Union) |> not)
        let unsafeCreate program typ =
            PrimitiveLiteral.unsafeCreate program typ
            |> Type.EnumMember
            |> InlinedProgram.inject program
        let typeKey: Type.EnumMember -> _ = _.Value >> Internal.getUnionTypeKey
    module Literal =
        let isLiteral (typ: Ts.Type) =
            Enum.hasMask (literalFlags ||| TF.UniqueESSymbol) typ.flags
            || EnumMember.isEnumMember typ
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.EnumLiteral ->
                EnumMember.unsafeCreate program typ
                |> Type.Literal.EnumMember
            | Enum.HasFlag TF.UniqueESSymbol ->
                typ :?> Ts.UniqueESSymbolType
                |> UniqueESSymbol.create program
                |> Type.Literal.UniqueESSymbol
            | Enum.HasMatch literalFlags ->
                PrimitiveLiteral.unsafeCreate program typ
                |> Type.Literal.PrimitiveLiteral
            | _ -> failwith "Unexpected type flags"
    module Primitive =
        let isPrimitive (typ: Ts.Type) = typ.flags |> Enum.hasMask (primitiveSingletonFlags ||| literalFlags)
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasMatch primitiveSingletonFlags ->
                PrimitiveSingleton.unsafeCreate typ
                |> Type.Primitive.Singleton
            | Enum.HasMatch literalFlags ->
                Literal.unsafeCreate program typ
                |> Type.Primitive.Literal
            | _ -> failwith "Unexpected type flags"
    module InstantiableNonPrimitive =
        let isInstantiableNonPrimitive (typ: Ts.Type) =
            typ.flags |> Enum.hasMask nonPrimitiveInstantiableFlags
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.Conditional ->
                typ :?> Ts.ConditionalType
                |> Conditional.create program
                |> Type.InstantiableNonPrimitive.Conditional
            | Enum.HasFlag TF.IndexedAccess ->
                typ :?> Ts.IndexedAccessType
                |> IndexedAccess.create program
                |> Type.InstantiableNonPrimitive.IndexedAccess
            | Enum.HasFlag TF.Substitution ->
                typ :?> Ts.SubstitutionType
                |> Substitution.create program
                |> Type.InstantiableNonPrimitive.Substitution
            | Enum.HasFlag TF.TypeParameter ->
                typ :?> Ts.TypeParameter
                |> TypeParameter.create program
                |> Type.InstantiableNonPrimitive.TypeParameter
            | _ -> failwith "Unexpected type flags"
    module StringMapping =
        let isStringMapping (typ: Ts.Type) = typ.flags |> Enum.hasFlag TF.StringMapping
        let fromStringMappingType (typ: Type.StringMappingType) =
            match (typ.Value.symbol |> ISymbol.create typ.Program |> ISymbol.toSymbol) |> _.name with
            | "Capitalize" -> Type.StringMapping.Capitalize typ
            | "Lowercase" -> Type.StringMapping.Lowercase typ
            | "Uppercase" -> Type.StringMapping.Uppercase typ
            | "Uncapitalize" -> Type.StringMapping.Uncapitalize typ
            | _ -> failwith "Unexpected string mapping type"
        let unsafeCreate program (typ: Ts.Type) =
            typ :?> Ts.StringMappingType
            |> StringMappingType.create program
            |> fromStringMappingType
        let tryCreate program typ =
            if not <| isStringMapping typ then None
            else unsafeCreate program typ |> Some
    module InstantiablePrimitive =
        let isInstantiablePrimitive (typ: Ts.Type) =
            typ.flags |> Enum.hasMask primitiveInstantiableFlags
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.Index ->
                typ :?> Ts.IndexType
                |> Index.create program
                |> Type.InstantiablePrimitive.Index
            | Enum.HasFlag TF.TemplateLiteral ->
                typ :?> Ts.TemplateLiteralType
                |> TemplateLiteral.create program
                |> Type.InstantiablePrimitive.TemplateLiteral
            | Enum.HasFlag TF.StringMapping ->
                StringMapping.unsafeCreate program typ
                |> Type.InstantiablePrimitive.StringMapping
            |_ -> failwith "Unexpected type flags"
    module Instantiable =
        let isInstantiable (typ: Ts.Type) =
            typ.flags |> Enum.hasMask (primitiveInstantiableFlags ||| nonPrimitiveInstantiableFlags)
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasMatch primitiveInstantiableFlags ->
                InstantiablePrimitive.unsafeCreate program typ
                |> Type.Instantiable.Primitive
            | Enum.HasMatch nonPrimitiveInstantiableFlags ->
                InstantiableNonPrimitive.unsafeCreate program typ
                |> Type.Instantiable.NonPrimitive
            | _ -> failwith "Unexpected type flags for instantiable"
    module Anonymous =
        let isAnonymous (typ: Ts.Type) =
            Structural.isObject typ
            && typ :?> Ts.ObjectType |> _.objectFlags |> Enum.hasFlag OF.Anonymous
        let unsafeCreate program (typ: Ts.ObjectType) =
            match typ.objectFlags with
            | Enum.HasFlag OF.ObjectRestType ->
                ObjectRest.create program typ
                |> Type.Anonymous.ObjectRest
            | Enum.HasFlag OF.InstantiationExpressionType ->
                InstantiationExpression.create program typ
                |> Type.Anonymous.InstantiationExpression
            | Enum.HasFlag OF.Instantiated ->
                InstantiatedAnonymousType.create program typ
                |> Type.Anonymous.Instantiated
            | Enum.HasFlag OF.Anonymous ->
                AnonymousType.create program typ
                |> Type.Anonymous.Anonymous
            | _ -> failwith "Unexpected object flags for anonymous"
    module Mapped =
        let isMapped (typ: Ts.Type) =
            Structural.isObject typ
            && typ :?> Ts.ObjectType |> _.objectFlags |> Enum.hasFlag OF.Mapped
        let unsafeCreate program (typ: Ts.ObjectType) =
            match typ.objectFlags with
            | Enum.HasFlag OF.Instantiated ->
                InstantiatedMappedType.create program typ
                |> Type.Mapped.Instantiated
            | Enum.HasFlag OF.Mapped ->
                MappedType.create program typ
                |> Type.Mapped.Mapped
            | _ -> failwith "Unexpected object flags for mapped"
    module TypeReference =
        let isTypeReference (typ: Ts.Type) =
            Structural.isObject typ
            && typ :?> Ts.ObjectType |> _.objectFlags |> Enum.hasFlag OF.Reference
            && (
                if typ :?> Ts.ObjectType |> _.objectFlags |> Enum.hasMask OF.ClassOrInterface
                then typ :?> Ts.TypeReference |> _.typeArguments.IsSome
                else true
            )
        let unsafeCreate program (typ: Ts.TypeReference) =
            // let typeNode =
            //     let inline makeResult enum arr = {| isTupleTypeNode = enum; isArrayTypeNode = arr |}
            //     lazy
            //     typ.node
            //     |> Option.map (
            //         unbox<Ts.Node>
            //         >> function
            //             | Patterns.Node.ArrayTypeNode _ -> makeResult false true
            //             | Patterns.Node.TupleTypeNode _ -> makeResult true false
            //             | _ -> makeResult false false
            //         )
            //     |> Option.defaultValue (makeResult false false)
                
            match typ.objectFlags with
            | Enum.HasFlag OF.Tuple ->
                TupleReference.create program typ
                |> Type.TypeReference.Tuple
            | _ when typ.checker.isTupleType typ ->
                TupleReference.create program typ
                |> Type.TypeReference.Tuple
            | Enum.HasFlag OF.Class ->
                ClassReference.create program typ
                |> Type.TypeReference.Class
            | Enum.HasFlag OF.Interface ->
                InterfaceReference.create program typ
                |> Type.TypeReference.Interface
            | _ when typ.checker.isArrayType typ ->
                ArrayReference.create program typ
                |> Type.TypeReference.Array
            | Enum.HasFlag OF.Reference ->
                PureTypeReference.create program typ
                |> Type.TypeReference.Pure
            | _ -> failwith "Unexpected object flags for type reference"
    module Structural =
        let isStructural (typ: Ts.Type) = typ.flags |> Enum.hasMask structuralFlags
        let isObject (typ: Ts.Type) = typ.flags |> Enum.hasFlag TF.Object
        let unsafeCreateObjectType program (typ: Ts.ObjectType) =
            match typ.objectFlags with
            | Enum.HasFlag OF.Anonymous ->
                Anonymous.unsafeCreate program typ
                |> Type.Structural.Anonymous
            | Enum.HasFlag OF.Mapped ->
                Mapped.unsafeCreate program typ
                |> Type.Structural.Mapped
            | Enum.HasFlag OF.Interface as flags
                when not(
                    flags
                    |> Enum.hasFlag OF.Reference
                    && typ :?> Ts.TypeReference
                       |> typ.checker.getTypeArguments
                       |> Seq.isEmpty
                       |> not) ->
                typ :?> Ts.InterfaceType
                |> Interface.create program
                |> Type.Structural.Interface
            | Enum.HasFlag OF.Class as flags
                when not(
                    flags
                    |> Enum.hasFlag OF.Reference
                    && typ :?> Ts.TypeReference
                       |> typ.checker.getTypeArguments
                       |> Seq.isEmpty
                       |> not) ->
                typ :?> Ts.InterfaceType
                |> Class.create program
                |> Type.Structural.Class
            | Enum.HasFlag OF.Reference ->
                typ :?> Ts.TypeReference
                |> TypeReference.unsafeCreate program 
                |> Type.Structural.TypeReference
            | _ -> failwith "Unexpected object flags for structural"
        let unsafeCreate program (typ: Ts.Type) =
            match typ.flags with
            | Enum.HasFlag TF.Union ->
                typ :?> Ts.UnionType
                |> Union.create program
                |> Type.Structural.Union
            | Enum.HasFlag TF.Intersection ->
                typ :?> Ts.IntersectionType
                |> Intersection.create program
                |> Type.Structural.Intersection
            | Enum.HasFlag TF.Object ->
                typ :?> Ts.ObjectType
                |> unsafeCreateObjectType program
            | _ -> failwith "Unexpected type flags for structural"
    module Kind =
        let isErrorStub (typ: Ts.Type) =
            typ.flags |> Enum.hasFlag TF.Any
            && typ?intrinsicName = "error"
        let internal rawCreate (typ: IType) =
            let program = IType.program typ
            let typ = IType.toType typ
            if Enum.isEnum typ then
                typ :?> Ts.EnumType
                |> Enum.create program
                |> Type.Kind.Enum
            elif Primitive.isPrimitive typ then
                Primitive.unsafeCreate program typ
                |> Type.Kind.Primitive
            elif Instantiable.isInstantiable typ then
                Instantiable.unsafeCreate program typ
                |> Type.Kind.Instantiable
            elif Structural.isStructural typ then
                Structural.unsafeCreate program typ
                |> Type.Kind.Structural
            else failwith "Unexpected type kind"
        let create (typ: IType): Type.Kind =
            IType.program typ
            |> _.CompositeCollection
            |> CompositeCollection.Type.registerToWrapper
            |> funApply typ
        let createFromType (program: Ts.Program) (typ: Ts.Type) =
            IType.create program typ |> create
        let tryCreateFromNode (program: Ts.Program) (node: Ts.Node) =
            try
                if ts.isTypeNode node
                then program.getTypeChecker().getTypeFromTypeNode (node :?> Ts.TypeNode)
                else program.getTypeChecker().getTypeAtLocation(node)
                |> Ok
            with _ -> Result.Error(None)
            |> Result.bind (function
                | typ when typ.getCanonicalSymbol().IsNone ->
                    Some Type.TypeValidationError.SymbolessErrorType
                    |> Result.Error
                | typ when isErrorStub typ ->
                    Some Type.TypeValidationError.ErrorType
                    |> Result.Error
                | typ ->
                    createFromType program typ
                    |> Ok
                )
        let tryCreateFromSymbol (program: Ts.Program) (symbol: Ts.Symbol) =
            ISymbol.create program symbol
            |> ISymbol.declaredType
            |> function
                | typ when isErrorStub typ.toType ->
                    Type.TypeValidationError.ErrorType |> Result.Error
                | typ ->
                    rawCreate typ
                    |> Ok
    let typeWrapper: TypeTypeWrapper = {
        IntermediateFn = IType.create
        WrapFn = Kind.rawCreate
        KeyFn = IType.typeKey
    }

module SymbolTable =
    let create program (symbolTable: Ts.SymbolTable) =
        symbolTable.entries()
        |> Seq.map (fun (symbolName, symbol) ->
            SymbolName.Create symbolName, ISymbol.create program symbol
            )
        |> Map
    
    let createOrFail program (symbolTable: Ts.SymbolTable option) =
        symbolTable
        |> Option.defaultWith (fun () ->
            Logging.Log.Default.logfe "SymbolTable.fromOption: unexpected failure to find symbol table. Please raise an issue."
            failwith "SymbolTable.fromOption: unexpected failure to find symbol table. Please raise an issue."
            )
        |> create program

module ExportSymbolTable =
    let create (program: Ts.Program) symbolTable: ExportSymbolTable =
        SymbolTable.create program symbolTable
        |> unbox
    let createOrFail program = SymbolTable.createOrFail program >> unbox<ExportSymbolTable>
    let toSymbolTable: ExportSymbolTable -> SymbolTable = unbox

module LocalSymbolTable =
    let create (program: Ts.Program) symbolTable: LocalSymbolTable =
        SymbolTable.create program symbolTable
        |> unbox
    let createOrFail program = SymbolTable.createOrFail program >> unbox<LocalSymbolTable>
    let toSymbolTable: LocalSymbolTable -> SymbolTable = unbox
    let fromSourceFile (program: Ts.Program) (sourceFile: Ts.SourceFile): LocalSymbolTable =
        sourceFile?locals
        |> Option.ofObj
        |> createOrFail program
        

[<EditorBrowsable(EditorBrowsableState.Never)>]
let typeWrappers: TypeWrappers = TypeWrappers.create Symbol.typeWrapper Node.typeWrapper Type.typeWrapper

type Ts.Symbol with
    member inline this.symbolName = this.escapedName |> SymbolName.Create
type ISymbol with
    member inline this.program = ISymbol.program this
    member inline this.checker = ISymbol.checker this
    member inline this.toSymbol = ISymbol.toSymbol this
type INode with
    member inline this.program = INode.program this
    member inline this.checker = INode.checker this
    member inline this.toNode = INode.toNode this
    member inline this.flags = (INode.toNode this).flags
type IType with
    member inline this.program = IType.program this
    member inline this.checker = IType.checker this
    member inline this.toType = IType.toType this
    member inline this.flags = (IType.toType this).flags
type Ts.Program with
    member inline this.TypeWrappers =
        this
        |> SymbolTypeKey.accessOrInit TypeWrappers.symbolTypeKey (fun _ -> typeWrappers)
    member this.CompositeCollection =
        this |> SymbolTypeKey.accessOrInit CompositeCollection.symbolTypeKey (fun _ -> CompositeCollection.init this.TypeWrappers)
    member this.PackageCollection =
        this |> SymbolTypeKey.accessOrInit PackageCollection.symbolTypeKey PackageCollection.init
type Symbol.LocalTableSymbol with
    member inline this.Declarations =
        this.Value.getDeclarations().Value.AsArray
        |> Array.map (Node.TopLevelLocalSymbolKind.createFromNode this.Program)
        |> NonEmptyArray.create
type Symbol.ExportTableSymbol with
    member inline this.Declarations =
        this.Value.getDeclarations().Value.AsArray
        |> Array.map (Node.TopLevelExportSymbolKind.createFromNode this.Program)
        |> NonEmptyArray.create

type Symbol.ITransient with
    member this.isTransient = ISymbol.hasFlag SF.Transient this
    member this.hasDeclaration = ISymbol.declarations this |> Option.isSome
type Symbol.Transient.IValue with
    member inline this.valueDeclaration = Symbol.tryValueDeclaration this |> Option.map (Node.DeclarationKind.create this.program)
type Symbol.Transient.IParameter with
    member this.parameterDeclaration: Node.ParameterKind option = Symbol.tryParameterDeclaration this
    member this.parameterDeclarationAndType = Symbol.tryParameterDeclarationAndType this
type Symbol.Transient.IVariable with
    member this.variableDeclaration: Node.Variable option = Symbol.tryVariableDeclaration this
    member this.variableDeclarationAndType = Symbol.tryVariableDeclarationAndType this
type Symbol.Transient.IProperty with
    member this.propertyDeclarations: NonEmptyArray<Node.PropertyKind> option = Symbol.tryPropertyDeclarations this
    member this.propertyDeclarationAndTypes = Symbol.tryPropertyDeclarationsAndTypes this
type Symbol.Transient.IEnumMember with
    member this.enumMemberDeclaration = Symbol.tryEnumMemberDeclaration this
    member this.enumMemberDeclarationAndType = Symbol.tryEnumMemberDeclarationAndType this
type Symbol.Transient.IFunction with
    member this.functionDeclarations: NonEmptyArray<Node.FunctionDeclaration> option = Symbol.tryFunctionDeclarations this
    member this.functionDeclarationAndTypes = Symbol.tryFunctionDeclarationAndTypes this
type Symbol.Transient.IClass with
    member this.classDeclaration: Node.ClassDeclaration option = Symbol.tryClassDeclaration this
    member this.classDeclarationAndType = Symbol.tryClassDeclarationAndType this
type Symbol.Transient.IMethod with
    member this.methodDeclarations: NonEmptyArray<Node.MethodKind> option = Symbol.tryMethodDeclarations this
    member this.methodDeclarationAndTypes = Symbol.tryMethodDeclarationsAndTypes this
type Symbol.Transient.IConstructor with
    member this.constructorDeclaration: Node.ConstructorDeclaration option = Symbol.tryConstructorDeclaration this
    member this.constructorDeclarationAndType = Symbol.tryConstructorDeclarationAndType this
type Symbol.Transient.ISignature with
    member this.signatureDeclarations: Node.SignatureKind option = Symbol.trySignatureDeclarations this
    member this.signatureDeclarationAndType = Symbol.trySignatureDeclarationsAndTypes this
type Symbol.Transient.IEnum with
    member this.enumDeclarations: NonEmptyArray<Node.EnumDeclaration> option = Symbol.tryEnumDeclaration this
    member this.enumDeclarationAndTypes = Symbol.tryEnumDeclarationAndType this
type Symbol.Transient.INamespace with
    member this.namespaceDeclarations: NonEmptyArray<Node.ModuleDeclaration> option = Symbol.tryNamespaceDeclarations this
    member this.namespaceDeclarationAndTypes = Symbol.tryNamespaceDeclarationsAndTypes this
type Symbol.Transient.IValueModule with
    member this.moduleDeclarations: NonEmptyArray<Node.ModuleKind> option = Symbol.tryModuleDeclarations this
    member this.moduleDeclarationAndTypes = Symbol.tryModuleDeclarationsAndTypes this
type Symbol.Transient.ITypeParameter with
    member this.typeParameterDeclarations: NonEmptyArray<Node.TypeParameterDeclaration> option = Symbol.tryTypeParameterDeclarations this
    member this.typeParameterDeclarationAndTypes = Symbol.tryTypeParameterDeclarationsAndTypes this
type Symbol.Transient.ITypeAlias with
    member this.typeAliasDeclaration: Node.TypeAliasDeclaration option = Symbol.tryTypeAliasDeclaration this
    member this.typeAliasDeclarationAndType = Symbol.tryTypeAliasDeclarationAndType this
type Symbol.Transient.IInterface with
    member this.interfaceDeclarations: NonEmptyArray<Node.InterfaceDeclaration> option = Symbol.tryInterfaceDeclarations this
    member this.interfaceDeclarationAndTypes = Symbol.tryInterfaceDeclarationsAndTypes this
type Symbol.Transient.IGetAccessor with
    member this.getAccessorDeclaration: Node.GetAccessorDeclaration option = Symbol.tryGetAccessorDeclaration this
    member this.getAccessorDeclarationAndType = Symbol.tryGetAccessorDeclarationAndType this
type Symbol.Transient.ISetAccessor with
    member this.setAccessorDeclaration: Node.SetAccessorDeclaration option = Symbol.trySetAccessorDeclaration this
    member this.setAccessorDeclarationAndType = Symbol.trySetAccessorDeclarationAndType this
type Symbol.Transient.IAccessor with
    member this.accessorDeclarations =
        ISymbol.tryPickDeclaration (Patterns.Node.(|GetAccessorDeclaration|_|) >> Option.map (Node.GetAccessorDeclaration.create (ISymbol.program this))) this,
        ISymbol.tryPickDeclaration (Patterns.Node.(|SetAccessorDeclaration|_|) >> Option.map (Node.SetAccessorDeclaration.create (ISymbol.program this))) this
    member this.accessorDeclarationAndTypes =
        this :?> Symbol.Transient.IGetAccessor |> _.getAccessorDeclarationAndType,
        this :?> Symbol.Transient.ISetAccessor |> _.setAccessorDeclarationAndType
type Symbol.Transient.IClassMember with
    member this.classMemberDeclarations: NonEmptyArray<Node.ClassMemberKind> option = Symbol.tryClassMemberDeclarations this
    member this.classMemberDeclarationAndTypes = Symbol.tryClassMemberDeclarationsAndTypes this
        
// ----------------------
type Symbol.IOptional with
    member inline this.isOptional = this |> ISymbol.hasFlag Ts.SymbolFlags.Optional
// Ensure the overloads for the declaration of the canonical kind does not go onto
// the inherited concrete I___ interface kind, as that is not the correct intention.
// Because we can provide more guarantees with the 'concrete'/'canonical' symbol kinds,
// we overload many of the transient members which provide generic options with non-option
// return values, and concrete types.
type Symbol.Transient.Parameter with
    member this.parameterDeclaration = Symbol.parameterDeclarations this
    member this.parameterDeclarationAndType = Symbol.parameterDeclarationAndType this
    member inline this.canonical = this.parameterDeclaration
    member inline this.canonicalWithType = this.parameterDeclarationAndType
type Symbol.Parameter with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.Method with
    member this.methodDeclarations = Symbol.methodDeclarations this
    member this.methodDeclarationAndTypes = Symbol.methodDeclarationsAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Method with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.Property with
    member this.propertyDeclarations = Symbol.propertyDeclarations this
    member this.propertyDeclarationAndTypes = Symbol.propertyDeclarationsAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Property with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.TypeAlias with
    member this.typeAliasDeclaration = Symbol.typeAliasDeclarations this
    member this.typeAliasDeclarationAndType = Symbol.typeAliasDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.Interface with
    member this.interfaceDeclarations = Symbol.interfaceDeclarations this
    member this.interfaceDeclarationAndTypes = Symbol.interfaceDeclarationsAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.Class with
    member this.classDeclaration = Symbol.classDeclaration this
    member this.classDeclarationAndType = Symbol.classDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Class with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.Constructor with
    member this.constructorDeclaration = Symbol.constructorDeclaration this
    member this.constructorDeclarationAndType = Symbol.constructorDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.Signature with
    member this.signatureDeclarations = Symbol.signatureDeclarations this
    member this.signatureDeclarationAndType = Symbol.signatureDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.Variable with
    member this.variableDeclaration = Symbol.variableDeclaration this
    member this.variableDeclarationAndType = Symbol.variableDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Variable with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.EnumMember with
    member this.enumMemberDeclaration = Symbol.enumMemberDeclaration this
    member this.enumMemberDeclarationAndType = Symbol.enumMemberDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.EnumMember with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.Function with
    member this.functionDeclarations = Symbol.functionDeclarations this
    member this.functionDeclarationAndTypes = Symbol.functionDeclarationAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Function with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.ConstEnum with
    member this.enumDeclarations = Symbol.constEnumDeclarations this
    member this.enumDeclarationAndTypes = Symbol.constEnumDeclarationAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.ConstEnum with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.TypeEnum with
    member this.enumDeclarations = Symbol.typeEnumDeclarations this 
    member this.enumDeclarationAndTypes = Symbol.typeEnumDeclarationAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.ValueModule with
    member this.moduleDeclarations = Symbol.moduleDeclarations this
    member this.moduleDeclarationAndTypes = Symbol.moduleDeclarationAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.ValueModule with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.NamespaceModule with
    member this.namespaceDeclarations = Symbol.namespaceDeclarations this
    member this.namespaceDeclarationAndTypes = Symbol.namespaceDeclarationsAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.Transient.GetAccessor with
    member this.getAccessorDeclaration = Symbol.getAccessorDeclaration this
    member this.getAccessorDeclarationAndType = Symbol.getAccessorDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.GetAccessor with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.SetAccessor with
    member this.setAccessorDeclaration = Symbol.setAccessorDeclaration this
    member this.setAccessorDeclarationAndType = Symbol.setAccessorDeclarationAndType this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
type Symbol.SetAccessor with
    member this.valueDeclaration = Symbol.valueDeclaration this
type Symbol.Transient.TypeParameter with
    member this.typeParameterDeclarations = Symbol.typeParameterDeclarations this 
    member this.typeParameterDeclarationAndTypes = Symbol.typeParameterDeclarationsAndTypes this
    member inline this.canonical = Symbol.canonicalDeclaration this
    member inline this.canonicalWithType = Symbol.canonicalDeclarationAndType this
