module Xantham.SimpleGenerator.Generator.TypeResolver

open System.Collections.Generic
open Fabulous.AST
open Xantham.Decoder
open Xantham.SimpleGenerator
open Xantham.SimpleGenerator.Patterns

(* Rendering output for a master key/type needs to provide the following:
    - A 'reference' to the type, or the type that is rendered when it is referenced
    - Possibly a type render DU, which supplies information to render type definitions
    
Both of these are supplied the path at which the type ref or definition is rendered.

Type definitions CAN contain cyclical references, so they are delayed using a
void lambda.

We refer to the 'type reference' rendering as 'short circuiting', since it
is minimal computation that does not trigger endless recursion with
cyclic references.

Short circuit results are always cached prior to computing the void lambda
containing the full type definition. This way, a self-referencing cycle
will not result in an infinite loop, since the 'minimal' computation
is already guaranteed to be available.

== References ==
Type References come in two kinds:
    - Paths
    - Prerendered widgets (such as `string`, `int` etc)
These form the DU `TypeRefKind`.

This is paired in a struct with a `Nullable` flag, which indicates whether
the type instance is considered nullable, and can/should be rendered as
an option. In the case of rendering a nullable type for an optional parameter,
we can opt out of rendering the option, since it is implied by the `?` on the
parameter.

== Renders ==
Renders in the DU `TypeRender` are the precomputed type render structs; a combination
of keys which can be expanded upon with the `KeyResolutionContext` to
produce a final render in different formats (ie abstract, abbrevs, signatures, etc)
Named types such as interfaces and classes will localise the name into the
struct.

Since all AST nodes are represented with unique key structs, a property
across multiple types referencing an anonymous type with the same shape
will only cause that shape to be precomputed once. Each time
the type is referenced, another computation occurs where path reliant
information is computed.

This makes it beneficial to do as much computing prior to path-related
rendering as possible before returning the computation for caching.
*)

module Primitives =
    let resolve (isNullable: bool) (key: PatternContextHolder<MasterKey>) =
        match key with
        | MasterKey.Primitive.NonPrimitive
        | MasterKey.Primitive.ESSymbol ->
            Render.createShortOnly isNullable Types.obj
        | MasterKey.Primitive.BigInt ->
            Render.createShortOnly isNullable Types.bigint
        | MasterKey.Primitive.Boolean ->
            Render.createShortOnly isNullable Types.bool
        | MasterKey.Primitive.Void
        | MasterKey.Primitive.Never
        | MasterKey.Primitive.Undefined
        | MasterKey.Primitive.Null ->
            Render.createShortOnly false Types.unit
        | MasterKey.Primitive.Any 
        | MasterKey.Primitive.Unknown ->
            Render.createShortOnly true Types.obj
        | MasterKey.Primitive.Number ->
            Render.createShortOnly isNullable Types.float
        | MasterKey.Primitive.Integer ->
            Render.createShortOnly isNullable Types.int
        | MasterKey.Primitive.String ->
            Render.createShortOnly isNullable Types.string
        | _ -> failwith $"Unknown primitive type for key %i{key.Value}"
    
module Optional =
    let resolve genCache =
        GeneratorContext.visit genCache
        >> function
            | Choice1Of2 compShortCircuit ->
                {
                    ShortCircuit = fun parentPath ->
                        { compShortCircuit parentPath with Nullable = true }
                    Full = ValueNone
                }
            | Choice2Of2 compFull ->
                {
                    ShortCircuit = 
                        match compFull with
                        | Pathed { Path = path } ->
                            fun parentPath ->
                                genCache.pathResolver.Localiser parentPath path
                                |> genCache.pathResolver.Renderer
                                |> TypeRefRender.create true
                        | RefOnly refRender ->
                            fun _ ->
                                { refRender with Nullable = true }
                    Full = ValueNone
                }

// TASK - need to allow this to lift multiple renders, or at least be able to register multiple renders
// to the cache
let rec resolvePattern (genCache: GeneratorContext) (isNullable: bool) (key: PatternContextHolder<MasterKey>) =
    match key with
    | Patterns.OptionalUnion (Value key) -> Optional.resolve genCache key
    // Handle most global types
    | MasterKey.Primitives -> Primitives.resolve isNullable key
    | MasterKey.KeyType.Global -> Render.createShortOnly isNullable Types.globalThis
    | MasterKey.KeyType.Array arrayKey ->
        let prerender = GeneratorContext.getTypeRef genCache arrayKey.Value
        {
            ShortCircuit = fun parentPath ->
                prerender parentPath
                |> TypeRefRender.toWidget genCache parentPath
                |> Types.array
                |> TypeRefKind.Widget
                |> TypeRefRender.create isNullable
            Full = ValueNone
        }
    // Handle type references
    | MasterKey.KeyType.TypeReference key ->
        match key with
        | TypeReference.TypeArguments (Array.Length 0) & TypeReference.Type typeKey ->
            GeneratorContext.touch genCache typeKey.Value
            {
                ShortCircuit = GeneratorContext.getTypeRef genCache typeKey.Value
                Full = ValueNone
            }
        | TypeReference.TypeArguments typeArguments & TypeReference.Type typeKey ->
            let typeArgumentShortCircuitsFns =
                typeArguments
                |> PatternContext.Array.map (GeneratorContext.getTypeRef genCache)
                |> PatternContext.value
            let typePrefixShortCircuit = GeneratorContext.getTypeRef genCache typeKey.Value
            {
                ShortCircuit = fun parentPath ->
                    Ast.AppPrefix(
                        typePrefixShortCircuit parentPath
                        |> TypeRefRender.toWidget genCache parentPath
                        ,
                        typeArgumentShortCircuitsFns
                        |> Array.map ((fun fn -> fn parentPath) >> TypeRefRender.toWidget genCache parentPath)
                        )
                    |> TypeRefRender.createWidget isNullable
                Full = ValueNone
            }
            // Render.createShortOnly isNullable (Ast.AppPrefix(typePrefixShortCircuit, typeArgumentShortCircuits))
    // Handle EsLibTypes with concrete paths
    | MasterKey.VisitationFlags.IsEsLib as concreteKey ->
        let name =
            MasterKey.hasName concreteKey
            |> ValueOption.map PatternContext.value
        match name with
        | ValueSome "Array" ->
            Ast.Anon "ResizeArray"
            |> Render.createShortOnly isNullable
        | ValueSome "ReadonlyArray" ->
            Ast.Anon "array"
            |> Render.createShortOnly isNullable
        | _ ->
            concreteKey.Destructure
            ||> KeyPath.init
            |> fun ({ Qualifiers = quals } as keyPath) ->
                { keyPath with Qualifiers = quals[ quals.Length - 1 .. ] }
            |> KeyPath.addMeasure<concreteTypePath>
            |> ConcreteType
            |> Render.createShortOnly isNullable
    
    // Handle cases without members which are reduced to obj types
    | MasterKey.KeyType.Union (Union.Types (Array.Length 0))
    | MasterKey.KeyType.Enum (Enum.Members (Array.Length 0))
    | MasterKey.KeyType.Intersection (Intersection.Types (Array.Length 0)) 
    | MasterKey.KeyType.TypeLiteral (TypeLiteral.Members (Array.Length 0)) -> Render.createShortOnly isNullable Types.obj
    
    // Handle enums
    | MasterKey.KeyType.Enum keyEnum & Value masterKey ->
        let enumPath =
            KeyPath.initConcreteTypeWithMasterKey masterKey keyEnum
            |> ConcreteType
        let enumRender = EnumRender.prerender keyEnum |> TypeRender.Enum
        Render.create genCache isNullable enumPath <| fun _ -> enumPath, fun _ -> enumRender
    
    // Handle unions where all types are concrete, or primitives
    | MasterKey.KeyType.Union (Union.Types types)
        when
            types
            |> PatternContext.Array.cforall (function
             | MasterKey.Primitives | MasterKey.IsDeclaration -> true
             | _ -> false
             )
             ->
        let mutable isNullable = isNullable
        let typeFns =
            types
            |> PatternContext.Array.cfilter (function
                | MasterKey.IsNullish -> isNullable <- true; false
                | MasterKey.IsNullable -> isNullable <- true; true
                | _ -> true
                )
            |> PatternContext.Array.cbind (function
                | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (EnumCaseKey.ParentEnumMasterKey enumKey)) -> enumKey
                | key -> key
                )
            |> PatternContext.map Array.distinct
            |> PatternContext.Array.map (GeneratorContext.getTypeRef genCache)
            |> PatternContext.value
        let isNullable = isNullable
        {
            ShortCircuit = fun parentPath ->
                    typeFns
                    |> Array.mapApply parentPath
                    |> Array.map (TypeRefRender.toWidget genCache parentPath)
                    |> Types.union
                    |> TypeRefRender.create isNullable
            Full = ValueNone
        }
    | MasterKey.KeyType.Union unionKey ->
        let resultPath =
            KeyPath.initEmptyTransientTypePath()
            |> TransientType
        let prerender = UnionRender.prerender unionKey
        let typeRender =
            UnionRender.toRender genCache isNullable (prerender resultPath)
        typeRender resultPath
        // {
        //     ShortCircuit = fun parentPath ->
        //         prerender parentPath
        //         |> UnionRender.toTypeRef genCache
        //         |> funApply resultPath
        //     Full = ValueNone
        // }
    // Handle conditionals
    | MasterKey.KeyType.Conditional conditionalKey ->
        let resultPath =
            KeyPath.initEmptyTransientTypePath()
            |> TransientType
        // Erased union builder
        let prerender = lazy ErasedUnionRender.prerenderFromConditional genCache conditionalKey
        {
            ShortCircuit = fun parentPath ->
                prerender.Value parentPath
                |> ErasedUnionRender.toWidget genCache
                |> funApply parentPath
                |> TypeRefRender.create isNullable
            Full = ValueNone
        }
        // Render.create isNullable resultPath <| fun _ ->
        //     resultPath,
        //     let prerender = ErasedUnionRender.prerenderFromConditional genCache conditionalKey
        //     fun parentPath -> prerender parentPath |> TypeRender.ErasedUnion
        
    | MasterKey.KeyType.Index keyIndex ->
        let resultPath = KeyPath.initEmptyTransientTypePath() |> TransientType
        let prerender = IndexRender.prerender genCache keyIndex
        Render.create genCache isNullable resultPath <| fun _ ->
            resultPath,
            fun parentPath ->
                prerender parentPath
                |> IndexRender.tryToTypeRender genCache <| parentPath
                |> function
                    | Ok value -> value
                    | Error error ->
                        TypeRender.TypeAlias {
                            TypeAliasRender.Name =
                                parentPath.Value
                                |> KeyPath.popQualifier
                                |> snd
                                |> ValueOption.defaultWith (fun () -> genCache.ctx.createNameKey "Index")
                                |> KeyResolution.getNamePascalCase genCache.ctx
                            TypeParameters = [||]
                            UnderlyingType = TypeAliasUnderlyingType.TypeReference error
                        }
    | MasterKey.KeyType.IndexAccess indexAccess ->
        let resultPath = KeyPath.initEmptyTransientTypePath() |> TransientType
        let prerender = IndexAccessRender.prerender genCache indexAccess
        {
            ShortCircuit = fun parentPath ->
                prerender parentPath
                |> IndexAccessRender.toTypeRef genCache
                |> funApply resultPath
            Full = ValueNone
        }
        
    | MasterKey.KeyType.Variable (Variable.Type key as keyVariable) & Value masterKey ->
        let path =
            KeyPath.initConcretePathWithMasterKey masterKey keyVariable
            |> KeyPathKind.ConcreteMember

        let shortCircuit =
            GeneratorContext.getTypeRef genCache key.Value path
            |> _.Type
        // Short circuit should render the variable type rather than the variable itself
        Render.create genCache isNullable shortCircuit <| fun _ ->
            path,
            let prerender = VariableRender.prerender genCache keyVariable
            fun parentPath ->
                prerender parentPath
                |> TypeRender.Variable
    
    
    | MasterKey.KeyType.TypeParameter keyTypeParameter ->
        let shortCircuit =
            TypeParameter.name keyTypeParameter
            |> PatternContext.value
            |> Name.Typar.create
            |> Name.Case.valueOrModified
            |> Ast.LongIdent
        {
            ShortCircuit = fun _ ->
                TypeRefRender.create isNullable shortCircuit
            Full =
                ValueNone
        }

    | MasterKey.KeyType.Interface interfaceKey & Value masterKey ->
        let resultPath =
            KeyPath.initConcreteTypeWithMasterKey masterKey interfaceKey
            |> ConcreteType
        Render.create genCache isNullable resultPath <| fun _ ->
            resultPath,
            let prerender = InterfaceRender.prerender genCache interfaceKey
            fun parentPath ->
            prerender parentPath
            |> TypeRender.Interface
    
    | MasterKey.KeyType.Class classKey & Value masterKey ->
        let resultPath =
            KeyPath.initConcreteTypeWithMasterKey masterKey classKey
            |> ConcreteType
        Render.create genCache isNullable resultPath <| fun _ ->
            resultPath,
            let prerender = ClassRender.prerender genCache classKey
            fun parentPath ->
                prerender parentPath
                |> TypeRender.Class
    
    | MasterKey.KeyType.Function functionKey & Value masterKey ->
        let resultPath =
            KeyPath.initConcretePathWithMasterKey masterKey functionKey
            |> ConcreteMember

        let shortCircuit = fun parentPath ->
            match functionKey with
            | Function.ShouldRenderDelegate ->
                KeyPath.initConcreteTypeWithMasterKey masterKey functionKey
                |> ConcreteType
                |> TypeRefRender.create isNullable
            | Function.DelegateNotRequired
                & Function.Parameters parameters
                & Function.Type returnType ->
                let resultPath = keyPathFunc parentPath key
                let returnType =
                    GeneratorContext.getTypeRef genCache returnType.Value resultPath
                    |> TypeRefRender.toWidget returnType.Context resultPath
                match parameters with
                | Array.Length 0 ->
                    Ast.Funs(
                        Types.unit,
                        returnType
                        )
                    |> TypeRefRender.create isNullable
                | _ ->
                    let getParameterShortCircuit (parameterKey: PatternContextHolder<KeyParameter>) =
                        let path =
                            KeyPath.initEmptyTransientPath()
                            |> KeyPath.appendQualifierKeyMeasure parameterKey.Value.Name
                            |> KeyPathKind.TransientMember

                        Parameter.type' parameterKey
                        |> PatternContext.value
                        |> GeneratorContext.getTypeRef genCache
                        |> fun fn -> fn path
                        |> TypeRefRender.toWidget parameterKey.Context path
                    Ast.Funs(
                        parameters
                        |> PatternContext.Array.cmap getParameterShortCircuit
                        |> PatternContext.value
                        , returnType
                        )
                    |> TypeRefRender.create isNullable
        {
            ShortCircuit = shortCircuit
            Full = ValueSome (lazy
                    {
                        Path = resultPath
                        Render =
                            let prerender = FunctionRender.prerender genCache functionKey
                            fun parentPath ->
                                prerender parentPath
                                |> TypeRender.Function
                    }
                    |> Pathed
                )
        }
    
    | MasterKey.KeyType.TypeLiteral keyTypeLiteral ->
        let resultPath =
            KeyPath.initEmptyTransientTypePath()
            |> TransientType
        {
            ShortCircuit = fun _ ->
                TypeRefRender.create isNullable resultPath
            Full = ValueSome(lazy
                let prerender = TypeLiteralRender.prerender genCache keyTypeLiteral
                Pathed {
                    Path = resultPath
                    Render = fun parentPath ->
                        let name =
                            parentPath.Value
                            |> KeyPath.popQualifier
                            |> snd
                            |> ValueOption.defaultWith (fun () -> genCache.ctx.createNameKey "Typar")
                            |> KeyResolution.getNamePascalCase genCache.ctx
                        TypeAlias {
                            TypeAliasRender.Name = name
                            TypeParameters = [||]
                            UnderlyingType =
                                prerender parentPath
                                |> TypeAliasUnderlyingType.TypeLiteral
                                
                        }
                }
                )
        }
        // Render.create isNullable resultPath <| fun _ ->
        //     resultPath,
        //     let prerender = TypeLiteralRender.prerender genCache keyTypeLiteral
        //     fun parentPath ->
        //         prerender parentPath
        //         |> TypeRender.TypeLiteral
    
    | MasterKey.KeyType.Constructor _ ->
        // TODO
        Render.createShortOnly isNullable Types.obj
    
    | MasterKey.KeyType.Tuple tupleKey ->
        let resultPath =
            KeyPath.initEmptyTransientTypePath()
            |> TransientType
        let prerender = lazy TupleRender.prerender genCache tupleKey
        {
            ShortCircuit = fun parentPath ->
                prerender.Value parentPath
                |> TupleRender.toTupleRender
                |> TupleRender.toTypes genCache
                |> funApply resultPath
                |> Ast.Tuple
                |> TypeRefRender.create isNullable
            Full = ValueNone
        }
        // Render.create isNullable resultPath <| fun _ ->
        //     resultPath,
        //     let prerender = TupleRender.prerender genCache tupleKey
        //     fun parentPath ->
        //     prerender parentPath
        //     |> TupleRender.toTypeRender
    | MasterKey.KeyType.MemberKey memberKey ->
        {
            ShortCircuit = fun parentPath ->
                // If this is prerendered outside a deferred evaluation, then it will
                // cause a stack overflow. Do not lift this out of the delayed render.
                MemberRender.prerender genCache memberKey parentPath
                |> MemberRender.toTypeRef genCache
                |> funApply parentPath
            Full = ValueNone
        }
    
    | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (EnumCaseKey.ParentEnum keyEnum)) ->
        let resultPath =
            // keyPathFunc keyPath key
            keyEnum
            |> Enum.toMasterKey
            |> PatternContext.mapc KeyPath.init
            |> PatternContext.value
            |> KeyPath.addMeasure
            |> ConcreteType
        let render =
            EnumRender.prerender keyEnum
            |> EnumRender.toTypeRender
        Render.create genCache isNullable resultPath <| fun () -> resultPath, fun _ -> render
    
    | MasterKey.KeyType.Intersection intersectionKey ->
        let prerender = InterfaceRender.prerenderFromIntersection genCache intersectionKey
        let resultPath = KeyPath.initEmptyTransientTypePath() |> TransientType
        Render.create genCache isNullable resultPath <| fun () ->
            resultPath,
            fun parentPath -> prerender parentPath |> TypeRender.Interface
    
    | MasterKey.KeyType.Literal literalKey ->
        match literalKey with
        | Literal.Int _ -> Types.int |> Render.createShortOnly isNullable
        | Literal.Float _ -> Types.float |> Render.createShortOnly isNullable
        | Literal.String _ -> Types.string |> Render.createShortOnly isNullable
        | Literal.Bool _ -> Types.bool |> Render.createShortOnly isNullable
        | Literal.Null -> Types.unit |> Render.createShortOnly isNullable
        | Literal.BigInt _ -> Types.bigint |> Render.createShortOnly isNullable
    
    | MasterKey.KeyType.Module moduleKey ->
        moduleKey
        |> ConcreteTypePath.initFromConcreteTypeKey Module.toMasterKey
        |> ConcreteType
        |> Render.createShortOnly isNullable
    
    // Type alias is a much lengthier match sequence, so we have separated the concerns
    // to the relevant type render file.
    | MasterKey.KeyType.TypeAlias typeAliasKey ->
        TypeAliasRender.createRender genCache isNullable typeAliasKey
    | MasterKey.KeyType.Predicate predicateKey ->
        Ast.Funs(Ast.Obj(), Ast.Boolean())
        |> Render.createShortOnly isNullable
    | MasterKey.MasterBuilder (Value builder) ->
        failwithf $"%A{builder}"
        
    
        

let resolve genCache (key: MasterKey) =
    PatternContext.prepare genCache.ctx key
    |> resolvePattern genCache false

let initWithInterceptors interceptors (ctx: KeyResolutionContext) =
    GeneratorContext.Create(ctx, resolve, interceptors = interceptors)

let init ctx = initWithInterceptors [] ctx

let rec renderKey (genCache: GeneratorContext) (concretePathStack: Stack<ConcretePathKind>) (tree: SourceTree) (key: MasterKey) =
    match GeneratorContext.tryGetTypeRender genCache key with
    | ValueSome (TypeMaybePathedRender.Pathed render) ->
        let keyPath = render.Path
        render.Path
        |> SourceTree.getModule genCache.ctx
        |> funApply tree
        |> ValueOption.map (ModuleTree.getModule genCache render.Path)
        |> ValueOption.defaultWith (fun () ->
            tree.Branches
            |> Dictionary.tryItem KeyNodeHashing.SourceKey.nullKey
            |> ValueOption.defaultWith (fun () ->
                tree.Branches
                |> Dictionary.tryAddOrGet KeyNodeHashing.SourceKey.nullKey { Branches = Dictionary(); Keys = HashSet() }
            )
        )
        |> _.Keys.Add(key)
        |> function
        | false -> ()
        | true ->
            match keyPath with
            | ConcreteParameter keyPath ->
                ConcretePathKind.ConcreteParameter keyPath
                |> concretePathStack.Push
            | ConcreteMember keyPath ->
                ConcretePathKind.ConcreteMember keyPath
                |> concretePathStack.Push
            | ConcreteType keyPath ->
                ConcretePathKind.ConcreteType keyPath
                |> concretePathStack.Push
            | Module keyPath ->
                ConcretePathKind.Module keyPath
                |> concretePathStack.Push
            | _ -> ()
            genCache.keyDependencies
            |> Dictionary.tryItem key
            |> ValueOption.iter (Seq.iter (renderKey genCache concretePathStack tree))
            if keyPath.IsConcrete then
                concretePathStack
                |> Stack.popAndForget
    | _ -> ()
let renderSourceTree (genCache: GeneratorContext) (topLevelKeys: MasterKey seq) =
    let sourceTree = { Branches = Dictionary() }
    let concretePathStack = Stack([ ConcretePathKind.Module (KeyPath.replaceMeasure KeyPathKind.emptyModule.Value) ])
    topLevelKeys
    |> Seq.iter (renderKey genCache concretePathStack sourceTree)
    sourceTree
    // moduleTree

let private getPascalName (genCache: GeneratorContext) (name: NameKey) =
    KeyResolution.getNamePascalCase genCache.ctx name

let typeBuilder (genCache: GeneratorContext) (path: KeyPathKind) (key: MasterKey) =
    if PatternContext.prepare genCache.ctx key |> MasterKey.KeyType.(|EnumCase|_|) |> ValueOption.isSome then
        ValueNone
    else
    let renderer, renderPath =
        match
            GeneratorContext.tryGetTypeRender genCache key
            |> ValueOption.defaultWith (fun () -> failwith "TypeRender not found")
        with
        | Pathed render ->
            render.Render path, render.Path
        | _ -> failwith "TypeRender not found"
    match renderer with
    | Interface interfaceRender ->
        InterfaceRender.renderInterfaceTypeDefn genCache interfaceRender renderPath
        |> ValueSome
    | Class classRender ->
        ClassRender.renderTypeDefn genCache classRender renderPath
        |> ValueSome
    | LiteralUnion unionRender ->
        EnumRender.toTypeDefn unionRender
        |> ValueSome
    | Enum unionRender ->
        EnumRender.toTypeDefn unionRender
        |> ValueSome
    | TypeAlias typeAliasRender ->
        TypeAliasRender.toTypeDefn genCache typeAliasRender renderPath
        |> ValueSome
    | Variable _ 
    | Function _
    | TypeAlias _ ->
        ValueNone


let rec foldBuilder (genCache: GeneratorContext) (path: KeyPathKind) (KeyValue(nameKey: NameKey, branch: ModuleTree)) =
    let path =
        path.Value
        |> KeyPath.appendQualifierKey nameKey
        |> KeyPath.addMeasure
        |> KeyPathKind.Module
    let modules =
        branch.Branches
        |> Seq.map (foldBuilder genCache path)
    let types =
        branch.Keys
        |> Seq.choose (typeBuilder genCache path >> ValueOption.toOption)
    let name =
        getPascalName genCache nameKey
        |> Name.Case.valueOrModified
    Ast.Module(name) {
        yield! types
        yield! modules
    }

let buildSource (genCache: GeneratorContext) (sourceTree: SourceTree) =
    Ast.Oak() {Ast.AnonymousModule() {
        yield!
            sourceTree.Branches
            |> Seq.collect (fun kv ->
                let path =
                    { KeyPath.empty with Source = if kv.Key = KeyNodeHashing.SourceKey.nullKey then ValueNone else ValueSome kv.Key }
                    |> KeyPath.addMeasure
                    |> KeyPathKind.Module
                kv.Value.Branches
                |> Seq.map (foldBuilder genCache path)
            )
    }}