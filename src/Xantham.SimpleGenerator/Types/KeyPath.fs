/// <summary>
/// Provides the core definitions and logic for representing, manipulating, and resolving paths
/// to types and members within the <c>Xantham.SimpleGenerator</c> framework.
/// </summary>
///
/// <remarks>
/// <para>This file defines the <c>KeyPath</c> structure, which serves as a unique identifier and location
/// descriptor for elements being generated. It includes:</para>
/// - <see cref="KeyPath"/>: A lightweight struct tracking the source origin (<see cref="SourceKey"/>),
///   name qualifiers (<see cref="NameKey"/>), and the identity of the last element visited (<see cref="MasterKey"/>).<br/>
/// - Path Kinds: Discrimination between `Concrete` paths (fully resolved), `Transient` paths (short-lived
///   or partially qualified), and `Module` paths.<br/>
/// - Measure Annotations: Type-safe path differentiation using F# units of measure (e.g., <see cref="concretePath"/>,
///   <see cref="transientPath"/>, <see cref="modulePath"/>).<br/>
/// - Resolution Logic: Functions to initialize, append to, and resolve paths relative to other paths,
///   essential for generating correct import/export statements and type references.<br/>
/// - Rendering Support: Integration with `Fabulous.AST` and `Fantomas` for transforming paths into
///   rendered code widgets.<br/>
/// </remarks>
[<AutoOpen>]
module Xantham.SimpleGenerator.AutoOpenKeyPathType

open FSharp.Logf
open Fabulous.AST
open Fantomas.Core.SyntaxOak
open Xantham.Decoder
open Xantham.SimpleGenerator.Patterns

[<Struct>]
type KeyPath = {
    Source: SourceKey voption
    Qualifiers: NameKey array
    LastVisited: MasterKey voption
    #if DEBUG
    Path: MasterKey array
    #endif
}

[<MeasureAnnotatedAbbreviation>] type KeyPath<[<Measure>]'u> = KeyPath

[<Measure>] type concreteMemberPath
[<Measure>] type concreteParameterPath
[<Measure>] type concreteTypePath
[<Measure>] type transientMemberPath
[<Measure>] type transientTypePath
[<Measure>] type transientParameterPath
[<Measure>] type localisedRenderedPath
[<Measure>] type modulePath

[<Struct>]
type ConcretePathKind =
    | ConcreteParameter of KeyPath<concreteParameterPath>
    | ConcreteMember of KeyPath<concreteMemberPath>
    | ConcreteType of KeyPath<concreteTypePath>
    | Module of KeyPath<modulePath>
    
[<Struct>]
type KeyPathKind =
    | ConcreteParameter of KeyPath<concreteParameterPath>
    | ConcreteMember of KeyPath<concreteMemberPath>
    | ConcreteType of KeyPath<concreteTypePath>
    /// Usually paths with a single qualifier.
    | TransientMember of KeyPath<transientMemberPath>
    | TransientParameter of KeyPath<transientParameterPath>
    /// Usually empty paths.
    | TransientType of KeyPath<transientTypePath>
    | Module of KeyPath<modulePath>
    member this.IsTransient =
        match this with
        | TransientMember _
        | TransientParameter _
        | TransientType _ -> true
        | _ -> false
    member this.IsConcrete = this.IsTransient |> not

    member this.Value: KeyPath =
        match this with
        | ConcreteMember keyPath -> unbox keyPath
        | ConcreteType keyPath -> unbox keyPath
        | TransientMember keyPath -> unbox keyPath
        | TransientType keyPath -> unbox keyPath
        | Module keyPath -> unbox keyPath
        | ConcreteParameter keyPath -> unbox keyPath
        | TransientParameter keyPath -> unbox keyPath


type ShapeConcreteDefinition<^T when
    ^T:(member Qualifiers: NameKey array)
    and ^T:(member Source: SourceKey)
    and ^T:(member Name: NameKey)
    > = ^T
type ShapeNamedDefinition<^T when ^T:(member Name: NameKey)> = ^T

/// <summary>
/// A function that can provide a path given only the master key.
/// This should only be able to provide concrete path types.
/// </summary>
type KeyPathFuncInitializer = PatternContextHolder<MasterKey> -> Result<ConcretePathKind, MasterKey>

/// <summary>
/// A function that takes a key path for a container of a type/path,
/// the type/path itself, and returns the key path for the type/path.
/// </summary>
type KeyPathFunc = KeyPathKind -> PatternContextHolder<MasterKey> -> KeyPathKind

/// <summary>
/// A function that takes the path of the container for the key,
/// the type render path of that key, and returns the path should
/// be rendered as a type for that container of the key.
/// </summary>
type PathLocaliserFunc = KeyPathKind -> KeyPathKind -> KeyPath<localisedRenderedPath>
/// <summary>
/// A function that takes the path of a parent, the path of a child, and returns
/// the <c>absolute</c> concrete path of the child (if it is descendant of a concrete path)
/// </summary>
type PathAbsolutionFunc = KeyPathKind -> KeyPathKind -> KeyPathKind
type ComputedLocaliserFunc = KeyPathKind -> KeyPath<localisedRenderedPath>

/// <summary>
/// A function that takes the prepared localised path for the type at its position,
/// and renders a type widget for that path.
/// </summary>
type PathRenderingFunc = KeyPath<localisedRenderedPath> -> WidgetBuilder<Type>

/// <summary>
/// All the requirements for functions that are needed
/// to utilise the key paths correctly.
/// </summary>
type KeyPathContext = {
    Initializer: KeyPathFuncInitializer
    Prerenderer: KeyPathFunc
    Localiser: PathLocaliserFunc
    Renderer: PathRenderingFunc
}

module KeyPath =
    let inline addMeasure<[<Measure>] 'u> (keyPath: KeyPath): KeyPath<'u> = unbox keyPath
    let inline withoutMeasure (keyPath: KeyPath<_>): KeyPath = unbox keyPath
    let removeMeasure = withoutMeasure
    let inline replaceMeasure<[<Measure>]'u, [<Measure>] 't>: KeyPath<'t> -> KeyPath<'u> = withoutMeasure >> addMeasure
    let empty = {
        Source = ValueNone
        Qualifiers = Array.empty
        LastVisited = ValueNone
        #if DEBUG
        Path = Array.empty
        #endif
    }
    
    let mapMeasured (f: KeyPath -> KeyPath) (keyPath: KeyPath<'u>): KeyPath<'u> =
        f (withoutMeasure keyPath) |> addMeasure
    let applyMeasured (f: KeyPath -> 'T) (keyPath: KeyPath<'u>): 'T =
        f (withoutMeasure keyPath)
    let inline appendQualifierKey (key: NameKey) (value: KeyPath) =
        { value with Qualifiers = value.Qualifiers |> Array.appendOne  key }
    let inline appendQualifierKeyRange (keys: NameKey array) (value: KeyPath) =
        { value with Qualifiers = Array.append value.Qualifiers keys }
    let inline setSourceKey (key: SourceKey) (value: KeyPath) =
        { value with Source = ValueSome key }
    let inline tryAddSourceKey (key: SourceKey) (value: KeyPath) =
        match value with
        | { Source = ValueNone } -> setSourceKey key value
        | _ -> value
    let inline clearSourceKey (value: KeyPath) =
        { value with Source = ValueNone }
    let clearSource = clearSourceKey
    let inline setLastVisited (key: MasterKey) (value: KeyPath) =
        { value with LastVisited = ValueSome key }
    let inline popQualifier (value: KeyPath): KeyPath * NameKey voption =
        match value.Qualifiers with
        | [||] -> value, ValueNone
        | [| key |] -> { value with Qualifiers = Array.empty }, ValueSome key
        | arr ->
            { value with Qualifiers = Array.truncate (arr.Length - 1) arr },
            ValueSome arr[ arr.Length - 1 ]
    
    let popQualifierMeasure (value: KeyPath<'u>): KeyPath<'u> * NameKey voption =
        withoutMeasure value
        |> popQualifier
        ||> fun value key ->
            addMeasure value, key
    
    let appendQualifierKeyMeasure (key: NameKey) =
        mapMeasured (appendQualifierKey key)
        
    let appendQualifierKeyRangeMeasure (keys: NameKey array) =
        mapMeasured (appendQualifierKeyRange keys)
        
    let setSourceKeyMeasure (key: SourceKey) =
        mapMeasured (setSourceKey key)
        
    let tryAddSourceKeyMeasure (key: SourceKey) =
        mapMeasured (tryAddSourceKey key)
        
    let clearSourceKeyMeasure (value: KeyPath<'u>) =
        mapMeasured clearSourceKey value
    
    let setLastVisitedMeasure (key: MasterKey) = mapMeasured (setLastVisited key)
        
    let getQualifiersMeasure value = applyMeasured _.Qualifiers value
    let getLastVisitedMeasure value = applyMeasured _.LastVisited value
    let getSourceMeasure value = applyMeasured _.Source value
    #if DEBUG
    let getPathMeasure value = applyMeasured _.Path value
    #endif
        
    /// <summary>
    /// Initializes a keypath from a masterkey using the given key resolution context.
    /// Returns an empty keypath if the masterkey has no associated qualifiers, source keys, or name keys.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="key"></param>
    let init = fun ctx key ->
        let patternContext = PatternContext.prepare ctx key
        let source =
            patternContext
            |> MasterKey.hasSourceKey
            |> ValueOption.map PatternContext.value
        let create = fun qualifiers -> {
            Source = source
            Qualifiers = qualifiers
            LastVisited = ValueSome key
            #if DEBUG
            Path = [| key |]
            #endif
        }
        
        #if DEBUG
        let log = fun path ->
            logft ctx.Logger "KeyPath.init (%i{masterKey}): %A{keyPath}" key path
            path
        #else
        let log = id
        #endif
        // match masterkey to determine what to do with the keypath
        match patternContext with
        | MasterKey.HasQualifiers (Value path) & MasterKey.HasNameKey (Value name) ->
            // has both qualifiers and name key, so we need to append the qualifiers to the path
            path
            |> Array.appendOne name
            |> create
        | MasterKey.HasNameKey (Value name) ->
            // has only a name key, create a new path with the name key
            Array.singleton name
            |> create
        | MasterKey.HasQualifiers (Value path) ->
            // has only qualifiers, so append the qualifiers to the path
            create path
        | _ -> empty // no qualifiers or name key, so create an empty path
        |> log // null op in prod   
    let inline initConcreteType<^T when ShapeConcreteDefinition<^T>>(key: PatternContextHolder<^T>) =
        {
            Source =
                ValueSome key.Value.Source
                |> ValueOption.filter ((<>) KeyNodeHashing.SourceKey.nullKey)
            Qualifiers =
                [|
                    yield! key.Value.Qualifiers
                    key.Value.Name
                |]
            LastVisited = ValueNone
            #if DEBUG
            Path = [||]
            #endif
        }
        |> addMeasure<concreteTypePath>
        
    let inline initConcreteTypeWithMasterKey (key: MasterKey) keyType =
        {
            withoutMeasure(initConcreteType keyType) with
                LastVisited = ValueSome key
                #if DEBUG
                Path = [| key |]
                #endif
        }
        |> addMeasure<concreteTypePath>
        
    let inline initConcretePath key =
        initConcreteType key
        |> replaceMeasure : KeyPath<concreteMemberPath>
        
    let inline initConcretePathWithMasterKey key keyType =
        initConcreteTypeWithMasterKey key keyType
        |> replaceMeasure : KeyPath<concreteMemberPath>
    
    let inline appendToConcreteTypeFromNamedKey<^T when ShapeNamedDefinition<^T>> (path: KeyPath<concreteTypePath>) (masterKey: MasterKey) (key: PatternContextHolder<^T>) =
        path
        |> mapMeasured (appendQualifierKey key.Value.Name >> setLastVisited masterKey)
        |> replaceMeasure : KeyPath<concreteMemberPath>
    
    let toTransientType (path: KeyPath) =
        match path with
        | { Qualifiers = [||] | [| _ |] | [| _; _ |] } as value -> value
        | value -> { value with Qualifiers = value.Qualifiers[ value.Qualifiers.Length - 2 .. ] }
        |> addMeasure : KeyPath<localisedRenderedPath>
    
    let toTransientTypeWithDefault (defaultTransientName: NameKey) (path: KeyPath<concreteMemberPath>) =
        path
        |> mapMeasured (function
            | { Qualifiers = [||] } as value ->
                { value with Qualifiers = [| defaultTransientName |] }
            | { Qualifiers = [| _ |] | [| _; _ |] } as value -> value
            | value -> { value with Qualifiers = value.Qualifiers[ value.Qualifiers.Length - 2 .. ] }
            )
        |> replaceMeasure: KeyPath<localisedRenderedPath>
    let toTransientTypeWithDefaultString (defaultTransientName: string) = toTransientTypeWithDefault (KeyNodeHashing.NameKey.createFromString defaultTransientName)
    
    let initEmptyTransientPath(): KeyPath<transientMemberPath> = addMeasure empty
        
    let initEmptyTransientTypePath(): KeyPath<transientTypePath> = addMeasure empty
    
    let inline appendToTransientTypePathFromNamedKey<^T when ShapeNamedDefinition<^T>> (path: KeyPath<transientTypePath>) (masterKey: MasterKey) (key: PatternContextHolder<^T>) =
        path
        |> mapMeasured (appendQualifierKey key.Value.Name >> setLastVisited masterKey)
        |> replaceMeasure : KeyPath<transientMemberPath>
    
    let toTransientPathWithMasterKey (defaultTransientPathName: string) (masterKey: MasterKey) (path: KeyPath<transientTypePath>) =
        match defaultTransientPathName with
        | null -> path
        | value ->
            let nameKey = KeyNodeHashing.NameKey.createFromString value
            path |> mapMeasured (appendQualifierKey nameKey >> setLastVisited masterKey)
        |> replaceMeasure : KeyPath<transientMemberPath>
    
    let toTransientPath (pathName: NameKey) (path: KeyPath<transientTypePath>) =
        path
        |> mapMeasured (appendQualifierKey pathName)
        |> replaceMeasure : KeyPath<transientMemberPath>
        

        
    /// <summary>
    /// Appends a key to a keypath.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="key"></param>
    /// <param name="path"></param>
    let append = fun ctx key path ->
        // If the key was the last visited, then we can return the path as is.
        if path.LastVisited |> ValueOption.exists ((=) key) then path else
        let patternContext = PatternContext.prepare ctx key // prepare pattern
        
        // has a key
        let source =
            patternContext
            |> MasterKey.hasSourceKey
            |> ValueOption.map PatternContext.value
            
        // closure with source key and last key set
        let create = fun qualifiers -> {
            Source = source |> ValueOption.orElse path.Source // Overwrite source key if available
            Qualifiers = qualifiers
            LastVisited = ValueSome key
            #if DEBUG
            Path = Array.appendOne key path.Path
            #endif
        }
        
        #if DEBUG
        let log = fun path ->
            logft ctx.Logger "KeyPath.append (%i{masterKey}): %A{keyPath}" key path
            path
        #else
        let log = id
        #endif
        
        match patternContext with
        | MasterKey.HasQualifiers (Value path) & MasterKey.HasNameKey (Value name) -> // if the key has qualifiers
            path |> Array.appendOne name |> create                                  // then it replaces the path
        | MasterKey.HasQualifiers (Value path) ->
            create path
        | MasterKey.HasNameKey (Value name) -> // if the key only has a name, then we append it.
            path.Qualifiers
            |> Array.appendOne name
            |> create
        | _ -> path
        |> log
    
    /// <summary>
    /// Computes a keypath into a tuple of the source and the qualifiers.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="path"></param>
    let resolve: KeyResolutionContext -> KeyPath -> string list voption * string list = fun (ctx: KeyResolutionContext) path ->
        let source =
            path.Source
            |> ValueOption.bind (Dictionary.Flip.tryItem ctx.cache.sourceKeys)
            |> ValueOption.map (_.Split('/', '\\') >> Seq.toList)
        let path =
            path.Qualifiers
            |> Seq.choose (Dictionary.Flip.tryItem ctx.cache.nameKeys >> ValueOption.toOption)
            |> Seq.toList
        source, path
    
    /// <summary>
    /// Converts a keypath to a relative keypath.
    /// </summary>
    /// <param name="comparitor"></param>
    /// <param name="renderedPath"></param>
    let toRelative (comparitor: KeyPath) (renderedPath: KeyPath) =
        match comparitor.Source, renderedPath.Source with
        | ValueSome source, ValueSome target when source <> target ->
            renderedPath
        | ValueNone, ValueSome _
        | ValueNone, ValueNone
        | ValueSome _, ValueNone
        | ValueSome _, ValueSome _ ->
            let renderedLength = renderedPath.Qualifiers.Length
            let comparitorLength = comparitor.Qualifiers.Length
            if renderedPath.Qualifiers.Length <= comparitor.Qualifiers.Length then
                comparitor.Qualifiers
                |> Array.truncate renderedLength
                |> Array.fold2 (fun (pass,acc: ResizeArray<NameKey>) rend comp ->
                    if pass || comp <> rend then
                        acc.Add(rend)
                        true, acc
                    else pass, acc
                    ) (false,ResizeArray<NameKey>()) renderedPath.Qualifiers
                |> fun (_,qualifiers) ->
                    {
                        Source = ValueNone
                        Qualifiers = qualifiers.ToArray()
                        LastVisited = renderedPath.LastVisited
                        #if DEBUG
                        Path = renderedPath.Path
                        #endif
                    }
            else
                comparitor.Qualifiers
                |> Array.fold2 (fun (pass,acc: ResizeArray<NameKey>) rend comp ->
                    if pass || comp <> rend then
                        acc.Add(rend)
                        true, acc
                    else pass, acc
                    ) (false,ResizeArray<NameKey>()) (renderedPath.Qualifiers |> Array.truncate comparitorLength)
                |> fun (_,qualifiers) ->
                    qualifiers.AddRange renderedPath.Qualifiers[comparitorLength..]
                    {
                        Source = ValueNone
                        Qualifiers = qualifiers.ToArray()
                        LastVisited = renderedPath.LastVisited
                        #if DEBUG
                        Path = renderedPath.Path
                        #endif
                    }
    let relativeResolve = fun ctx comparitor path ->
        toRelative comparitor path
        |> resolve ctx
    let resolveAndConcat: KeyResolutionContext -> KeyPath -> string list = fun ctx path ->
        let source, path = resolve ctx path
        match source with
        | ValueSome source -> source @ path
        | ValueNone -> path
    
    let relativeResolveAndConcat = fun ctx comparitor path ->
        let source,path = relativeResolve ctx comparitor path
        match source with
        | ValueSome source -> source @ path
        | ValueNone -> path

    /// <summary>
    /// Determines if two key paths share a common source.
    /// Returns true if the sources are both null.
    /// </summary>
    /// <param name="comparitor"></param>
    /// <param name="value"></param>
    let inline isCommonSourceOrNull (comparitor: KeyPath) (value: KeyPath) =
        comparitor.Source = value.Source
    
    /// <summary>
    /// Determines if two key paths share a common source.
    /// Returns false if the sources are both null.
    /// </summary>
    /// <remarks>
    /// Use <c>isCommonSourceOrNull</c> if the absence of a value does not matter.
    /// </remarks>
    /// <param name="comparitor"></param>
    /// <param name="value"></param>
    let inline isCommonSource (comparitor: KeyPath) (value: KeyPath) =
        comparitor.Source
        |> ValueOption.exists (fun key -> ValueOption.contains key value.Source)
    
    /// <summary>
    /// Determines if two key paths share the head of their qualifiers.
    /// Essentially compares the heads of two keypaths.
    /// </summary>
    /// <param name="comparitor"></param>
    /// <param name="value"></param>
    let inline hasCommonPath (comparitor: KeyPath) (value: KeyPath) =
        comparitor.Qualifiers
        |> Array.tryHead
        |> Option.exists (fun compKey ->
            value.Qualifiers
            |> Array.tryHead
            |> Option.exists ((=) compKey)
            )
        
    /// <summary>
    /// Determines if a keypath is a parent of another.
    /// This is true if the comparitor keypath is a shortened version of the value keypath.
    /// </summary>
    let inline isParentPath (comparitor: KeyPath) (value: KeyPath) =
        isCommonSourceOrNull comparitor value
        && hasCommonPath comparitor value
        && value.Qualifiers.Length < comparitor.Qualifiers.Length
        &&
        value.Qualifiers
        |> Seq.indexed
        |> Seq.forall (fun (i, compKey) -> comparitor.Qualifiers |> Array.item i |> (=) compKey)
        
    /// <summary>
    /// Determines if a keypath is a child of another.
    /// This is true if the value keypath is a shortened version of the comparitor keypath.
    /// </summary>
    let inline isChildPath (comparitor: KeyPath) (value: KeyPath) =
        isParentPath value comparitor
        
    /// <summary>
    /// Finds the common path between two keypaths if one exists.
    /// Failure to do so returns <c>ValueNone</c>. This uses <c>isCommonSourceOrNull</c> to determine
    /// if the sources are the same.
    /// </summary>
    /// <param name="comparitor"></param>
    /// <param name="value"></param>
    let tryGetCommonPath (comparitor: KeyPath) (value: KeyPath) =
        if not (isCommonSourceOrNull comparitor value) then ValueNone else
        { value with
                Qualifiers =
                    comparitor.Qualifiers
                    |> Seq.indexed
                    |> Seq.takeWhile (fun (i, compKey) ->
                        value.Qualifiers
                        |> Array.tryItem i
                        |> Option.exists ((=) compKey)
                        )
                    |> Seq.map snd
                    |> Seq.toArray }
        |> ValueSome
        
    /// <summary>
    /// Finds the common path between two keypaths if one exists.
    /// Otherwise returns an empty keypath.
    /// </summary>
    /// <param name="comparitor"></param>
    /// <param name="value"></param>
    let inline getCommonPath (comparitor: KeyPath) (value: KeyPath) =
        tryGetCommonPath comparitor value |> ValueOption.defaultValue empty
        
    /// <summary>
    /// Finds the most common sourcekey in a sequence of keypaths.
    /// Any occurence of a source ranks higher than a non-occurence of a source.
    /// This means if there are 5 paths without a source, and 1 path with a source,
    /// this function would return the source of the 1 path.
    /// </summary>
    /// <param name="values"></param>
    let inline findMostCommonSource (values: KeyPath seq) =
        values
        |> Seq.countBy _.Source
        |> Seq.maxBy (function ValueSome _, count -> count | _ -> 0)
        |> fst
        
    /// <summary>
    /// Filters a sequence of keypaths to only those that have a specific source.
    /// </summary>
    /// <param name="source"></param>
    /// <param name="values"></param>
    let inline filterPathsForSource (source: SourceKey) (values: KeyPath seq) =
        values
        |> Seq.filter (_.Source >> (=) (ValueSome source))
        
    /// <summary>
    /// Reduces a sequence of keypaths to a single keypath that is the most common from the source
    /// </summary>
    /// <param name="values"></param>
    let inline reduceToMostCommon (values: KeyPath seq) =
        values |> Seq.reduce getCommonPath
        
    /// <summary>
    /// Determines if a keypath is empty.
    /// </summary>
    let isEmpty = (=) empty
    
    /// <summary>
    /// Determines if a keypath is not empty.
    /// </summary>
    let isNotEmpty = (<>) empty
    
    /// <summary>
    /// Converts a keypath to a value option by filtering out empty keypaths.
    /// </summary>
    let toValueOption = ValueSome >> ValueOption.filter isNotEmpty
    
    /// <summary>
    /// From an array of KeyPaths, picks the keypath that is most common from the source
    /// that occurs the most. It is a combination of the following functions:<br/>
    /// <code>
    /// KeyPath.findMostCommonSource paths
    /// |> ValueOption.bind (fun source ->
    ///     KeyPath.filterPathsForSource source paths
    ///     |> KeyPath.reduceToMostCommon
    ///     |> KeyPath.toValueOption
    /// ) // : KeyPath voption
    /// </code>
    /// </summary>
    let tryPickCommonPathFromSeq (values: KeyPath seq) =
        findMostCommonSource values
        |> ValueOption.bind (fun source ->
            filterPathsForSource source values
            |> reduceToMostCommon
            |> toValueOption
            )
        
    let private (|WithoutMeasure|) = withoutMeasure
    
    module Relative =
        let inline popFromTypedPath (path: KeyPath) =
            popQualifier path
        let inline popFromTypedPathMeasure path =
            withoutMeasure path
            |> popFromTypedPath
        let inline popFromModulePath (path: KeyPath) = path
        let inline popFromModulePathMeasure path = withoutMeasure path
        let inline popFromMemberPath (path: KeyPath) =
            let typePath, memberName = popQualifier path
            let modulePath, typeName = popQualifier typePath
            modulePath, typeName, memberName
        let inline popFromMemberPathMeasure (path: KeyPath<_>) =
            withoutMeasure path
            |> popFromMemberPath
        let inline popFromParameterPath (path: KeyPath) =
            let memberPath, parameterName = popQualifier path
            let modulePath, typeName, memberName = popFromMemberPath memberPath
            modulePath, typeName, memberName, parameterName
        let inline popFromParameterPathMeasure (path: KeyPath<_>) =
            withoutMeasure path
            |> popFromParameterPath
        let inline appendQualifierIfSome (qualifier: NameKey voption) (path: KeyPath<_>) =
            if qualifier.IsSome then appendQualifierKeyMeasure qualifier.Value path else path
        module ConcreteType =
            let relativeToModule (comparitor: KeyPath<modulePath>) (value: KeyPath<concreteTypePath>) =
                let comparitor = withoutMeasure comparitor
                let value, typeName = popFromTypedPath (withoutMeasure value)
                if isCommonSourceOrNull comparitor value && hasCommonPath comparitor value then
                    toRelative comparitor value
                    |> addMeasure
                elif isCommonSourceOrNull comparitor value then
                    value
                    |> clearSourceKey
                    |> addMeasure
                else
                    value
                    |> addMeasure : KeyPath<localisedRenderedPath>
                |> appendQualifierIfSome typeName
            let relativeToConcreteType (comparitor: KeyPath<concreteTypePath>) (value: KeyPath<concreteTypePath>) =
                let comparitor, _ = popFromTypedPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let relativeToConcreteMember (comparitor: KeyPath<concreteMemberPath>) (value: KeyPath<concreteTypePath>) =
                let comparitor, _ = popQualifier (withoutMeasure comparitor)
                relativeToConcreteType (addMeasure comparitor) value
            let relativeToConcreteParameter (comparitor: KeyPath<concreteParameterPath>) (value: KeyPath<concreteTypePath>) =
                let comparitor, _, _, _ = popFromParameterPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let inline relativeToTransientMember (comparitor: KeyPath<transientMemberPath>) (value: KeyPath<concreteTypePath>) =
                relativeToConcreteMember (replaceMeasure comparitor) value
            let inline relativeToTransientParameter (comparitor: KeyPath<transientParameterPath>) (value: KeyPath<concreteTypePath>) =
                relativeToConcreteParameter (replaceMeasure comparitor) value
            let inline relativeToTransientType (comparitor: KeyPath<transientTypePath>) (value: KeyPath<concreteTypePath>) =
                relativeToConcreteType (replaceMeasure comparitor) value
        module ConcreteMember =
            let relativeToModule (comparitor: KeyPath<modulePath>) (value: KeyPath<concreteMemberPath>) =
                // if we're rendering a 'type' located at value, then we need to render the type of the
                // container (assuming it is a enum or union).
                ConcreteType.relativeToModule comparitor (value |> popQualifierMeasure |> fst |> replaceMeasure)
            let relativeToConcreteType (comparitor: KeyPath<concreteTypePath>) (value: KeyPath<concreteMemberPath>) =
                let comparitor, _ = popFromTypedPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let relativeToConcreteMember (comparitor: KeyPath<concreteMemberPath>) (value: KeyPath<concreteMemberPath>) =
                let comparitor, _ =
                    popQualifier (withoutMeasure comparitor)
                relativeToConcreteType (addMeasure comparitor) value
            let relativeToConcreteParameter (comparitor: KeyPath<concreteParameterPath>) (value: KeyPath<concreteMemberPath>) =
                let comparitor, _, _, _ = popFromParameterPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let inline relativeToTransientMember (comparitor: KeyPath<transientMemberPath>) (value: KeyPath<concreteMemberPath>) =
                relativeToConcreteMember (replaceMeasure comparitor) value
            let inline relativeToTransientParameter (comparitor: KeyPath<transientParameterPath>) (value: KeyPath<concreteMemberPath>) =
                relativeToConcreteParameter (replaceMeasure comparitor) value
            let inline relativeToTransientType (comparitor: KeyPath<transientTypePath>) (value: KeyPath<concreteMemberPath>) =
                relativeToConcreteType (replaceMeasure comparitor) value
        module ConcreteParameter =
            let relativeToModule (comparitor: KeyPath<modulePath>) (value: KeyPath<concreteParameterPath>) =
                ConcreteType.relativeToModule comparitor (value |> popQualifierMeasure |> fst |> popQualifierMeasure |> fst |> replaceMeasure)
            let relativeToConcreteType (comparitor: KeyPath<concreteTypePath>) (value: KeyPath<concreteParameterPath>) =
                let comparitor, _ = popFromTypedPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let relativeToConcreteMember (comparitor: KeyPath<concreteMemberPath>) (value: KeyPath<concreteParameterPath>) =
                let comparitor, _ =
                    popQualifier (withoutMeasure comparitor)
                relativeToConcreteType (addMeasure comparitor) value
            let relativeToConcreteParameter (comparitor: KeyPath<concreteParameterPath>) (value: KeyPath<concreteParameterPath>) =
                let comparitor, _, _, _ = popFromParameterPath (withoutMeasure comparitor)
                relativeToModule (addMeasure comparitor) value
            let inline relativeToTransientMember (comparitor: KeyPath<transientMemberPath>) (value: KeyPath<concreteParameterPath>) =
                relativeToConcreteMember (replaceMeasure comparitor) value
            let inline relativeToTransientParameter (comparitor: KeyPath<transientParameterPath>) (value: KeyPath<concreteParameterPath>) =
                relativeToConcreteParameter (replaceMeasure comparitor) value
            let inline relativeToTransientType (comparitor: KeyPath<transientTypePath>) (value: KeyPath<concreteParameterPath>) =
                relativeToConcreteType (replaceMeasure comparitor) value
        
        
        module TransientType =
            let inline relativeToModule (comparitor: KeyPath<modulePath>) (value: KeyPath<transientTypePath>) =
                // a transient type should be placed inside the module. If we are rendering an empty pathed type
                // in a module, we should throw an error. Otherwise, simply append the qualifiers to the comparitor.
                let value = withoutMeasure value
                if Array.isEmpty value.Qualifiers then failwith "Cannot render a transient type without a type name inside a module." else
                if value.Source.IsSome then ConcreteType.relativeToModule comparitor (replaceMeasure value) else
                replaceMeasure value
            
            let inline relativeToConcreteType (comparitor: KeyPath<concreteTypePath>) (value: KeyPath<transientTypePath>) =
                let value = withoutMeasure value
                if value.Source.IsSome then ConcreteType.relativeToConcreteType comparitor (replaceMeasure value) else
                // referencing a transient type from another type usually means we are dealing with typars or others.
                let _, comparitorTypeName = popFromTypedPath (withoutMeasure comparitor)
                { empty with Qualifiers = [| if comparitorTypeName.IsSome then comparitorTypeName.Value else KeyNodeHashing.NameKey.typarTransientKey |] }
                |> if Array.isEmpty value.Qualifiers then appendQualifierKey KeyNodeHashing.NameKey.typarTransientKey else appendQualifierKeyRange value.Qualifiers
                |> addMeasure
            
            let inline relativeToConcreteMember (comparitor: KeyPath<concreteMemberPath>) (value: KeyPath<transientTypePath>) =
                let value = withoutMeasure value
                if value.Source.IsSome then ConcreteType.relativeToConcreteMember comparitor (replaceMeasure value) else
                let _, typeName, memberName = popFromMemberPath (withoutMeasure comparitor)
                {
                    value with
                        Qualifiers = Array.append [|
                            if typeName.IsSome then typeName.Value else KeyNodeHashing.NameKey.typarTransientKey
                            if memberName.IsSome then memberName.Value else KeyNodeHashing.NameKey.moduleMagicKey
                        |] value.Qualifiers
                }
                |> addMeasure
            
            let inline relativeToConcreteParameter (comparitor: KeyPath<concreteParameterPath>) (value: KeyPath<transientTypePath>) =
                let value = withoutMeasure value
                if value.Source.IsSome then ConcreteType.relativeToConcreteParameter comparitor (replaceMeasure value) else
                let _, typeName, memberName, parameterName = popFromParameterPath (withoutMeasure comparitor)
                {
                    value with
                        Qualifiers = Array.append [|
                            if typeName.IsSome then typeName.Value else KeyNodeHashing.NameKey.typarTransientKey
                            if memberName.IsSome then memberName.Value else KeyNodeHashing.NameKey.moduleMagicKey
                            if parameterName.IsSome then parameterName.Value else KeyNodeHashing.NameKey.moduleMagicKey
                        |] value.Qualifiers
                }
                |> addMeasure
            
            let inline relativeToTransientType (comparitor: KeyPath<transientTypePath>) (value: KeyPath<transientTypePath>) =
                relativeToConcreteType (replaceMeasure comparitor) value
            
            let inline relativeToTransientPath (comparitor: KeyPath<transientMemberPath>) (value: KeyPath<transientTypePath>) =
                relativeToConcreteMember (replaceMeasure comparitor) value
                
            let inline relativeToTransientParameter (comparitor: KeyPath<transientParameterPath>) (value: KeyPath<transientTypePath>) =
                relativeToConcreteParameter (replaceMeasure comparitor) value
        
                
    let localiser: PathLocaliserFunc = fun pathHoldingType pathOfType ->
        match pathHoldingType, pathOfType with
        | ConcreteType comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToConcreteType comparitor value
        | ConcreteMember comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToConcreteMember comparitor value
        | ConcreteParameter comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToConcreteParameter comparitor value
        | TransientParameter comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToTransientParameter comparitor value
        | TransientMember comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToTransientMember comparitor value
        | TransientType comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToTransientType comparitor value
        | Module comparitor, ConcreteType value ->
            Relative.ConcreteType.relativeToModule comparitor value
        | ConcreteType comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToConcreteType comparitor value
        | ConcreteMember comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToConcreteMember comparitor value
        | TransientMember comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToTransientMember comparitor value
        | TransientType comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToTransientType comparitor value
        | Module comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToModule comparitor value
        | ConcreteParameter comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToConcreteParameter comparitor value
        | TransientParameter comparitor, ConcreteMember value ->
            Relative.ConcreteMember.relativeToTransientParameter comparitor value
        | ConcreteType comparitor, TransientType value ->
            Relative.TransientType.relativeToConcreteType comparitor value
        | ConcreteMember comparitor, TransientType value ->
            Relative.TransientType.relativeToConcreteMember comparitor value
        | ConcreteParameter comparitor, TransientType value ->
            Relative.TransientType.relativeToConcreteParameter comparitor value
        | TransientParameter comparitor, TransientType value ->
            Relative.TransientType.relativeToTransientParameter comparitor value
        | TransientMember comparitor, TransientType value ->
            Relative.TransientType.relativeToTransientPath comparitor value
        | TransientType comparitor, TransientType value ->
            Relative.TransientType.relativeToTransientType comparitor value
        | Module comparitor, TransientType value ->
            Relative.TransientType.relativeToModule comparitor value
        | ConcreteType comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToConcreteType comparitor value
        | ConcreteMember comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToConcreteMember comparitor value
        | ConcreteParameter comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToConcreteParameter comparitor value
        | TransientParameter comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToTransientParameter comparitor value
        | TransientMember comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToTransientMember comparitor value
        | TransientType comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToTransientType comparitor value
        | Module comparitor, ConcreteParameter value ->
            Relative.ConcreteParameter.relativeToModule comparitor value
        
            
        // Any reference to a concrete type should just render the
        // relative path.
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), ConcreteType (WithoutMeasure concretePath)
            // if no common source, then just render the concrete path
            when not (isCommonSourceOrNull comparitorPath concretePath) ->
            replaceMeasure concretePath
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), ConcreteType (WithoutMeasure concretePath)
            // if common source, but no common path, then
            // render the concrete path without the source
            when not (hasCommonPath comparitorPath concretePath) ->
            concretePath |> clearSource |> addMeasure
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), ConcreteType (WithoutMeasure concretePath) ->
            // otherwise, clear the source and render the relative path
            toRelative comparitorPath concretePath
            |> clearSource
            |> addMeasure
        
        // Any reference to a module should render the
        // module 'type' (prefaced with I). We do this by
        // adding a magic qualifier __MODULE__. This should be
        // recognised in rendering to modify the type name.
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), Module (WithoutMeasure modulePath)
            when not (isCommonSource comparitorPath modulePath) ->
            replaceMeasure modulePath
            |> appendQualifierKeyMeasure KeyNodeHashing.NameKey.moduleMagicKey
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), Module (WithoutMeasure modulePath)
            // if no common source, but a common path, then
            // render the concrete path without the source
            when not (hasCommonPath comparitorPath modulePath) ->
            modulePath |> clearSource |> addMeasure
            |> appendQualifierKeyMeasure KeyNodeHashing.NameKey.moduleMagicKey
        | (TransientType (WithoutMeasure comparitorPath)
          | ConcreteType (WithoutMeasure comparitorPath)
          | TransientMember (WithoutMeasure comparitorPath)
          | ConcreteMember (WithoutMeasure comparitorPath)
          | Module (WithoutMeasure comparitorPath)), Module (WithoutMeasure modulePath) ->
            // otherwise, render the relative path
            toRelative comparitorPath modulePath
            |> addMeasure
            |> appendQualifierKeyMeasure KeyNodeHashing.NameKey.moduleMagicKey
            
        // when rendering a transient type from a concrete path, we convert the
        // path into a transient type path by taking the last two qualifiers of the concrete path (module name - type name)
        | ConcreteMember concretePath, TransientType (WithoutMeasure transientPath) when isEmpty transientPath ->
            // if the transient path is empty, then we proceed as normal
            toTransientTypeWithDefaultString "TransientType" concretePath
        | ConcreteMember concretePath, TransientType (WithoutMeasure transientPath) ->
            // if the transient path is not empty, and it has a source path, then we render the transient path
            // as is. It is likely a generated transient type in a 'concrete' path.
            if
                withoutMeasure concretePath
                |> isCommonSourceOrNull transientPath
                |> not
            then replaceMeasure transientPath
            else
            // otherwise, we append the qualifiers of the transient path to the path generated from the
            // concrete path (module name - type name - ... qualifiers)
            withoutMeasure concretePath
            |> toTransientType 
            |> appendQualifierKeyRangeMeasure transientPath.Qualifiers
        // When resolving a transient type from a transient path, we treat it the same as we would the concrete path.
        | TransientMember (WithoutMeasure transientPath), TransientType  (WithoutMeasure transientTypePath) when isEmpty transientTypePath ->
            addMeasure transientPath
            |> toTransientTypeWithDefaultString "TransientType"
        | TransientMember (WithoutMeasure transientPath), TransientType (WithoutMeasure transientTypePath) ->
            transientPath
            |> clearSource
            |> toTransientType
            |> appendQualifierKeyRangeMeasure transientTypePath.Qualifiers
        // When resolving a transient type path from a concrete or transient type path, we assume that we are handling a
        // type parameter, so we append a qualifier 'Typar' to the path so that the generated types are [ type name - Typar ] (module name - type name)
        | (ConcreteType (WithoutMeasure concreteTypePath) | TransientType (WithoutMeasure concreteTypePath)),
            TransientType (WithoutMeasure transientTypePath)
            when isEmpty transientTypePath ->
            concreteTypePath
            |> appendQualifierKey (KeyNodeHashing.NameKey.createFromString "Typar")
            |> toTransientType
        | (ConcreteType (WithoutMeasure concreteTypePath) | TransientType (WithoutMeasure concreteTypePath)),
            TransientType (WithoutMeasure transientTypePath) ->
            concreteTypePath
            |> appendQualifierKeyRange transientTypePath.Qualifiers
            |> toTransientType
            |> appendQualifierKeyRangeMeasure transientTypePath.Qualifiers
        | v1, v2 -> failwithf $"Unhandled combination of paths: {v1} and {v2}"

    let createPathTypeRenderer (ctx: KeyResolutionContext): PathRenderingFunc =
        withoutMeasure
        >> resolveAndConcat ctx
        >> function
            | [] -> Ast.Obj() // fallback
            | [ value ] when value = "__MODULE__" -> Ast.Obj() // fallback to global
            | nameList ->
                // check for magic
                if nameList |> List.last |> (=) KeyNodeHashing.NameKey.moduleMagicKeyString
                then
                    let length = List.length nameList
                    let moduleTypeName =
                        nameList[length - 1]
                        |> Name.Module.create
                    nameList
                    |> Seq.take (length - 2)
                    |> Seq.map Name.Pascal.create
                    |> Seq.insertAt (length - 1) (Case.unboxMeasure moduleTypeName)
                else nameList |> Seq.map Name.Pascal.create
                |> Seq.map Name.Case.valueOrModified
                |> Ast.LongIdent
                
    let appendQualifierWithName (ctx: KeyResolutionContext) (name: Name<_>) (parentPath: KeyPathKind) =
        ctx.createNameKey (Name.Case.valueOrSource name)
        |> appendQualifierKey
        |> match parentPath with
            | KeyPathKind.ConcreteMember keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> TransientParameter
            | KeyPathKind.TransientMember keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> TransientParameter
            | KeyPathKind.TransientType keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> TransientMember
            | KeyPathKind.Module keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> ConcreteMember
            | KeyPathKind.ConcreteType keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> ConcreteMember
            | KeyPathKind.ConcreteParameter keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> ConcreteParameter
            | KeyPathKind.TransientParameter keyPath ->
                funApply (withoutMeasure keyPath)
                >> addMeasure
                >> TransientParameter

    let inline appendQualifierFromNamedRender<^T, [<Measure>] ^U when ^T:(member Name: Name<^U>)> (ctx: KeyResolutionContext) (namedRender: ^T) (parentPath: KeyPathKind) =
        appendQualifierWithName ctx namedRender.Name parentPath

module KeyPathKind =
    let emptyModule =
        KeyPath.empty
        |> KeyPath.addMeasure
        |> KeyPathKind.Module

module ConcreteTypePath =
    let inline initFromConcreteTypeKey (masterKeyFn: PatternContextHolder<^T> -> PatternContextHolder<MasterKey>) (value: PatternContextHolder<^T>) =
        KeyPath.initConcreteTypeWithMasterKey (masterKeyFn value |> PatternContext.value) value
        
    let inline appendFromNamedKey path (masterKeyFn: PatternContextHolder<^T> -> PatternContextHolder<MasterKey>) (key: PatternContextHolder<^T>) =
        KeyPath.appendToConcreteTypeFromNamedKey path (masterKeyFn key |> PatternContext.value) key
        
    let toConcretePath (step: NameKey) (path: KeyPath<concreteTypePath>) =
        path
        |> KeyPath.mapMeasured (KeyPath.appendQualifierKey step)
        |> KeyPath.replaceMeasure : KeyPath<concreteMemberPath>

module ConcreteMember =
    let inline initTransientType() = KeyPath.initEmptyTransientPath()
    let append (step: NameKey) (path: KeyPath<concreteMemberPath>) =
        path
        |> KeyPath.mapMeasured (KeyPath.appendQualifierKey step)
        |> KeyPath.replaceMeasure : KeyPath<concreteParameterPath>

module ModulePath =
    let init (value: PatternContextHolder<KeyModule>) =
        KeyPath.initConcreteTypeWithMasterKey 
            (Module.toMasterKey value |> PatternContext.value)
            value
        |> KeyPath.replaceMeasure : KeyPath<modulePath>
        

module TransientPath =
    let append (step: NameKey) (path: KeyPath<transientMemberPath>) =
        path
        |> KeyPath.mapMeasured (KeyPath.appendQualifierKey step)

module TransientTypePath =
    let append (step: NameKey) (path: KeyPath<transientTypePath>): KeyPath<transientMemberPath> =
        path
        |> KeyPath.mapMeasured (KeyPath.appendQualifierKey step)
        |> KeyPath.replaceMeasure
        
let keyPathInitFunc: KeyPathFuncInitializer = function
    | MasterKey.KeyType.Class concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcretePathKind.ConcreteType
        |> Ok
    | MasterKey.KeyType.Interface concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcretePathKind.ConcreteType
        |> Ok
    | MasterKey.KeyType.TypeAlias concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcretePathKind.ConcreteType
        |> Ok
    | MasterKey.KeyType.Module concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> ConcretePathKind.Module
        |> Ok
    | MasterKey.KeyType.Enum concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcretePathKind.ConcreteType
        |> Ok
    | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (EnumCaseKey.ParentEnum concreteKey) & EnumCase.ToMasterKey (Value masterKey) as enumCaseKey) ->
        KeyPath.appendToConcreteTypeFromNamedKey 
            (KeyPath.initConcreteType concreteKey)
            masterKey
            enumCaseKey
        |> ConcretePathKind.ConcreteMember
        |> Ok
    | MasterKey.KeyType.Variable concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> ConcretePathKind.ConcreteMember
        |> Ok
    | MasterKey.KeyType.Function concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> ConcretePathKind.ConcreteMember
        |> Ok
    | Value masterKey -> Error masterKey
let keyPathFunc: KeyPathFunc = fun parentPath -> function
    // handle concrete types:
    // class, interface, enum (technically enum cases too), variables, functions, modules,
    // type aliases
    | MasterKey.KeyType.Class concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcreteType
    | MasterKey.KeyType.Interface concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcreteType
    | MasterKey.KeyType.TypeAlias concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcreteType
    | MasterKey.KeyType.Module concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> Module
    | MasterKey.KeyType.Enum concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> ConcreteType
    | MasterKey.KeyType.EnumCase (EnumCase.ToEnumCaseKey (EnumCaseKey.ParentEnum concreteKey) & EnumCase.ToMasterKey (Value masterKey) as enumCaseKey) ->
        KeyPath.appendToConcreteTypeFromNamedKey 
            (KeyPath.initConcreteType concreteKey)
            masterKey
            enumCaseKey
        |> ConcreteMember
    | MasterKey.KeyType.Variable concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> ConcreteMember
    | MasterKey.KeyType.Function concreteKey ->
        KeyPath.initConcreteType concreteKey
        |> KeyPath.replaceMeasure
        |> ConcreteMember
    | MasterKey.KeyType.Parameter (Parameter.NameKey (Value nameKey)) & Value masterKey ->
        parentPath.Value
        |> KeyPath.appendQualifierKey nameKey
        |> KeyPath.setLastVisited masterKey
        |> if parentPath.IsTransient then
            KeyPath.addMeasure
            >> TransientParameter
           else KeyPath.addMeasure >> ConcreteParameter
    | MasterKey.HasNameKey (Value nameKey) & Value(masterKey) ->
        match parentPath with
        | ConcreteMember path ->
            ConcreteMember.append nameKey path
            |> KeyPath.setLastVisitedMeasure masterKey
            |> ConcreteParameter
        | ConcreteType keyPath ->
            ConcreteTypePath.toConcretePath nameKey keyPath
            |> KeyPath.setLastVisitedMeasure masterKey
            |> ConcreteMember
        | ConcreteParameter keyPath ->
            keyPath
            |> KeyPath.appendQualifierKeyMeasure nameKey
            |> KeyPath.setLastVisitedMeasure masterKey
            |> ConcreteParameter
        | KeyPathKind.TransientParameter path ->
            path
            |> KeyPath.appendQualifierKeyMeasure nameKey
            |> KeyPath.setLastVisitedMeasure masterKey
            |> TransientParameter
        | KeyPathKind.TransientMember keyPath ->
            TransientPath.append nameKey keyPath
            |> KeyPath.setLastVisitedMeasure masterKey
            |> KeyPath.replaceMeasure
            |> KeyPathKind.TransientParameter
        | TransientType keyPath ->
            TransientTypePath.append nameKey keyPath
            |> KeyPath.setLastVisitedMeasure masterKey
            |> KeyPathKind.TransientMember
        | Module _ ->
            failwith "unhandled; modules should only have concrete paths"
    | _ ->
        KeyPath.initEmptyTransientTypePath()
        |> KeyPathKind.TransientType
        // parentPath.Value
        // |> KeyPath.toTransientType
        // |> match parentPath with
        //     | Concrete _ -> 
        //         KeyPath.replaceMeasure
        //         >> TransientType
        //     | ConcreteType _ ->
        //         KeyPath.replaceMeasure
        //         >> TransientType
        //     | Transient _ ->
        //         KeyPath.replaceMeasure
        //         >> Transient
        //     | TransientType _ ->
        //         KeyPath.replaceMeasure
        //         >> TransientType
        //     | Module _ ->
        //         KeyPath.replaceMeasure
        //         >> TransientType
    
    | MasterKey.MasterBuilder (Value builder) -> failwithf $"unhandled %A{parentPath} with child %A{builder}"

let keyPathAbsolution: PathAbsolutionFunc = fun parentPath ->
    let (|WithoutMeasure|) = KeyPath.withoutMeasure
    function
    // Concrete paths are returned as is.
    | ConcreteMember _ | ConcreteType _ | ConcreteParameter _ | Module _ as path -> path
    | TransientType (WithoutMeasure path) ->
        match parentPath with
        | Module (WithoutMeasure parentPath) 
        | ConcreteMember (WithoutMeasure parentPath)
        | ConcreteType (WithoutMeasure parentPath)
        | ConcreteParameter (WithoutMeasure parentPath) ->
            parentPath
            |> KeyPath.appendQualifierKeyRangeMeasure path.Qualifiers
            |> KeyPath.addMeasure
            |> ConcreteType
        | TransientParameter (WithoutMeasure parentPath) 
        | TransientType (WithoutMeasure parentPath) 
        | TransientMember (WithoutMeasure parentPath) ->
            parentPath
            |> KeyPath.appendQualifierKeyRangeMeasure path.Qualifiers
            |> KeyPath.addMeasure
            |> TransientType
    | TransientMember (WithoutMeasure childPath) ->
        match parentPath with
        | Module (WithoutMeasure keyPath) 
        | ConcreteMember (WithoutMeasure keyPath) 
        | ConcreteType (WithoutMeasure keyPath) 
        | ConcreteParameter (WithoutMeasure keyPath) ->
            keyPath
            |> KeyPath.appendQualifierKeyRangeMeasure childPath.Qualifiers
            |> KeyPath.addMeasure
            |> ConcreteMember
        | TransientMember (WithoutMeasure keyPath) 
        | TransientParameter (WithoutMeasure keyPath) 
        | TransientType (WithoutMeasure keyPath) ->
            keyPath
            |> KeyPath.appendQualifierKeyRangeMeasure childPath.Qualifiers
            |> KeyPath.addMeasure
            |> TransientMember
    | TransientParameter (WithoutMeasure childPath) ->
        match parentPath with
        | Module (WithoutMeasure keyPath) 
        | ConcreteMember (WithoutMeasure keyPath) 
        | ConcreteType (WithoutMeasure keyPath) 
        | ConcreteParameter (WithoutMeasure keyPath) ->
            keyPath
            |> KeyPath.appendQualifierKeyRangeMeasure childPath.Qualifiers
            |> KeyPath.addMeasure
            |> ConcreteParameter
        | TransientMember (WithoutMeasure keyPath) 
        | TransientParameter (WithoutMeasure keyPath) 
        | TransientType (WithoutMeasure keyPath) ->
            keyPath
            |> KeyPath.appendQualifierKeyRangeMeasure childPath.Qualifiers
            |> KeyPath.addMeasure
            |> TransientParameter
        