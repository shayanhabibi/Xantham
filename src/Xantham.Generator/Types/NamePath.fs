module Xantham.Generator.NamePath

(*
This file contains the logic for type safe name generation via domain specific unions.
*)

open System.Collections.Generic
open System.ComponentModel
open Xantham.Decoder
open Xantham.Generator

// An attempt at deterministic name generation for types

type QualifiedNamePart =
    // Qualified name is a path to the file or a script
    | FilePath of string
    // Qualified name is a non-file-path string
    | MemberPath of string
    // The name of the member
    | Name of string

module QualifiedNamePart =
    let rec foldParse  (qualifiedName: string list) (acc: QualifiedNamePart list) =
        match qualifiedName with
        | [] -> acc
        | [ name ] -> Name name :: acc
        | path :: rest when System.IO.Path.IsPathFullyQualified path ->
            let acc = FilePath path :: acc
            foldParse rest acc
        | path :: rest ->
            let acc = MemberPath path :: acc
            foldParse rest acc
    
    let private normalizeParts (qualifiedName: ArenaInterner.QualifiedNamePart list) =
        qualifiedName
        |> List.map (function
            | ArenaInterner.QualifiedNamePart.Abnormal(value, diagnostic)
                when diagnostic.HasFlag(
                    ArenaInterner.QualifiedNamePartDiagnostic.ContainsQuotationMarks
                    ) ->
                value.Trim('"')
            | ArenaInterner.QualifiedNamePart.Abnormal(value, _) -> value
            | ArenaInterner.QualifiedNamePart.Normal(value) -> value)
    let parse (qualifiedName: ArenaInterner.QualifiedNamePart list) =
        normalizeParts qualifiedName
        |> foldParse
        |> funApply []
    
    // The name should always be the most specific part of a qualified name, and should be the last part of the list.
    // This will reorder the list if it's not in the correct order.
    let normalizeDirection (parts: QualifiedNamePart list) =
        match parts with
        | [] -> []
        | [ Name _ ] -> parts
        | Name _ :: _ -> List.rev parts
        | _ -> parts

type QualifiedName = {
    FilePath: string list
    MemberPath: string list
    Name: string voption
}

module QualifiedName =
    let create (parts: QualifiedNamePart list) =
        let parts = QualifiedNamePart.normalizeDirection parts
        let filePaths, rest = parts |> List.partition _.IsFilePath
        if filePaths |> List.length |> (<) 1 then failwith "Did not expect multiple file paths; raise an issue so this can be handled"
        {
            FilePath = filePaths |> List.map (function FilePath s -> s | _ -> failwith "Unreachable")
            MemberPath = rest |> List.choose (function MemberPath s -> Some s | _ -> None)
            Name =
                rest
                |> List.tryPick (function Name s -> Some s | _ -> None)
                |> Option.toValueOption
        }
    
    let parse (qualifiedName: ArenaInterner.QualifiedNamePart list) =
        QualifiedNamePart.parse qualifiedName
        |> create

type QualifiedName with
    static member Empty = {
        FilePath = []
        MemberPath = []
        Name = ValueNone
    }


type ModulePath = {
    Parent: ModulePath voption
    Name: Name<Case.pascal>
    Depth: int
}

type TypePath = {
    Parent: ModulePath
    Name: Name<Case.pascal>
}

[<RequireQualifiedAccess>]
type MemberPathParent =
    | Type of TypePath
    | Module of ModulePath
    
type MemberPath = {
    Parent: MemberPathParent
    Name: Name<Case.camel>
}

type ParameterPath = {
    Parent: MemberPath
    Name: Name<Case.camel>
    Index: int
}

[<RequireQualifiedAccess>]
type TypeParamPathParent =
    | Type of TypePath
    | Member of MemberPath
    | Parameter of ParameterPath

type TypeParamPath = {
    Parent: TypeParamPathParent
    Name: Name<Case.typar>
}

[<RequireQualifiedAccess>]
type AnchorPath =
    | TypeParam of TypeParamPath
    | Parameter of ParameterPath
    | Member of MemberPath
    | Type of TypePath
    | Module of ModulePath

[<RequireQualifiedAccess>]
type TransientModulePath =
    | Anchored
    | Moored of parent: TransientModulePath * name: Name<Case.pascal>
    | AnchoredAndMoored of name: Name<Case.pascal>

[<RequireQualifiedAccess>]
type TransientTypePath =
    | Anchored 
    | Moored of parent: TransientModulePath * name: Name<Case.pascal>
    | AnchoredAndMoored of name: Name<Case.pascal>

// Can render inline as parameters
[<RequireQualifiedAccess>]
type TransientMemberPath =
    | Anchored
    | Moored of parent: TransientTypePath * name: Name<Case.camel>
    | AnchoredAndMoored of name: Name<Case.camel>

[<RequireQualifiedAccess>]
type TransientParameterPath =
    | Anchored
    | Moored of parent: TransientMemberPath * name: Name<Case.camel>
    | AnchoredAndMoored of name: Name<Case.camel>

[<RequireQualifiedAccess>]
type TransientPath =
    // we should never raise a transient module path in isolation
    // | Module of TransientModulePath
    | Type of TransientTypePath
    | Member of TransientMemberPath
    | Parameter of TransientParameterPath
    | TypeParam of TransientTypePath

[<RequireQualifiedAccess>]
type Path =
    | Anchor of AnchorPath
    | Transient of TransientPath
    
[<RequireQualifiedAccess>]
type TypeLikePath =
    | Transient of TransientTypePath
    | Anchored of TypePath

[<RequireQualifiedAccess>]
type MemberLikePath =
    | Transient of TransientMemberPath
    | Anchored of MemberPath

[<RequireQualifiedAccess>]
type ParameterLikePath =
    | Transient of TransientParameterPath
    | Anchored of ParameterPath

[<RequireQualifiedAccess>]
type TypeParamLikePath =
    | Transient of TransientTypePath
    | Anchored of TypeParamPath

[<RequireQualifiedAccess>]
type ModuleLikePath =
    | Transient of TransientModulePath
    | Anchored of ModulePath
    
module ModulePath =
    let rec graft (parentPath: ModulePath) (childBranch: ModulePath) =
        match childBranch.Parent with
        | ValueSome parent ->
            { childBranch with
                  Parent = ValueSome (graft parentPath parent)
                  Depth = parentPath.Depth + childBranch.Depth }
        | ValueNone ->
            { childBranch with
                  Parent = ValueSome parentPath
                  Depth = parentPath.Depth + childBranch.Depth }
    let initWithName (name: Name<Case.pascal>) = {
        ModulePath.Parent = ValueNone
        Name = name
        Depth = 0
    }
    let init name = {
        ModulePath.Parent = ValueNone
        Name = Name.Pascal.create name
        Depth = 0
    }
    let createWithName (name: Name<Case.pascal>) parent = {
        ModulePath.Parent = ValueSome parent
        Name = name
        Depth = parent.Depth + 1
    }
    let create name parent =
        createWithName 
            (Name.Pascal.create name)
            parent
    let createFromList (parts: string list) =
        match parts with
        | [] -> init ""
        | head :: tail ->
            tail
            |> List.fold (fun acc part -> create part acc) (init head)
    let parent (path: ModulePath) = path.Parent
    let depth (path: ModulePath) = path.Depth
    let name (path: ModulePath) = path.Name
    let pruneParent (predicate: ModulePath -> bool) (modulePath: ModulePath) =
        let rec prune = function
            | { ModulePath.Parent = ValueSome parent; Name = name } when predicate parent ->
                true, initWithName name
            | { ModulePath.Parent = ValueSome parent; Name = name } as current ->
                match prune parent with
                | true, parentPath -> true, createWithName name parentPath
                | false, _ -> false, current
            | { ModulePath.Parent = ValueNone } as current ->
                false, current
        prune modulePath
        |> snd
    let rec mutateChain (map: ModulePath -> ModulePath) (modulePath: ModulePath) =
        let modulePath = map modulePath
        { modulePath with Parent = modulePath.Parent |> ValueOption.map (mutateChain map) }
    let pruneTopParent (modulePath: ModulePath) = pruneParent _.Parent.IsNone modulePath
    let flatten (modulePath: ModulePath) =
        let rec flattenModule (modulePath: ModulePath) =
            if modulePath.Name |> Name.Case.valueOrSource |> (=) ""
            then
                modulePath.Parent
                |> ValueOption.map flattenModule
                |> ValueOption.defaultValue []
            else
                modulePath.Name :: (
                    modulePath.Parent
                    |> ValueOption.map flattenModule
                    |> ValueOption.defaultValue []
                )
        flattenModule modulePath
        |> List.rev
    let flattenCaseless = flatten >> List.map Case.withoutMeasure

module TypePath =
    let flatten { TypePath.Parent = parent; Name = name } =
        ModulePath.flatten parent @ [ name ]
    let flattenCaseless = flatten >> List.map Case.withoutMeasure
    let inline traceToParentModule (typePath: TypePath) = typePath.Parent, [ typePath.Name ]
    let createWithName name parent = {
        TypePath.Parent = parent
        Name = name
    }
    let create name parent =
        Name.Pascal.create name
        |> createWithName
        |> funApply parent
    let pruneParent (predicate: ModulePath -> bool) (typePath: TypePath) =
        if predicate typePath.Parent then
            ModulePath.init ""
        else ModulePath.pruneParent predicate typePath.Parent
        |> createWithName typePath.Name
module MemberPath =
    let findParentModule (memberPath: MemberPath) =
        match memberPath.Parent with
        | MemberPathParent.Type typePath -> typePath.Parent
        | MemberPathParent.Module modulePath -> modulePath
    /// Does not include the name of the member, or the returned module.
    let traceToParentModule (memberPath: MemberPath) =
        match memberPath.Parent with
        | MemberPathParent.Type typePath -> typePath.Parent, [ Case.withoutMeasure typePath.Name ]
        | MemberPathParent.Module modulePath -> modulePath, []
    let createWithName name parent = {
        MemberPath.Parent = parent
        Name = name
    }
    let inline create name parent =
        createWithName (Name.Camel.create name) parent
    let inline createOnType name parent =
        MemberPathParent.Type parent
        |> create name
    let inline createOnModule name parent =
        MemberPathParent.Module parent
        |> create name
    let inline createOnTypeWithName name parent =
        MemberPathParent.Type parent
        |> createWithName name
    let inline createOnModuleWithName name parent =
        MemberPathParent.Module parent
        |> createWithName name
    let pruneParent (predicate: ModulePath -> bool) (memberPath: MemberPath) =
        let parent =
            match memberPath.Parent with
            | MemberPathParent.Type typePath ->
                TypePath.pruneParent predicate typePath
                |> MemberPathParent.Type
            | MemberPathParent.Module modulePath ->
                ModulePath.pruneParent predicate modulePath
                |> MemberPathParent.Module
        { memberPath with Parent = parent }

module ParameterPath =
    /// Does not include the name of the parameter, or the returned module.
    let traceToParentModule (parameterPath: ParameterPath) =
        MemberPath.traceToParentModule parameterPath.Parent
        ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure parameterPath.Parent.Name ]
    let findParentModule (parameterPath: ParameterPath) =
        parameterPath.Parent
        |> MemberPath.findParentModule
        
    let createWithName name parent index = {
        ParameterPath.Parent = parent
        Name = name
        Index = index
    }
    let create name parent =
        Name.Camel.create name
        |> createWithName
        |> funApply parent
        |> funApply 0

module TypeParamPath =
    let findParentModule (typeParamPath: TypeParamPath) =
        match typeParamPath.Parent with
        | TypeParamPathParent.Type typePath ->
            typePath.Parent
        | TypeParamPathParent.Member memberPath ->
            MemberPath.findParentModule memberPath
        | TypeParamPathParent.Parameter parameterPath ->
            ParameterPath.findParentModule parameterPath
    /// Does not include the name of the parameter, or the returned module.
    let traceToParentModule (typeParamPath: TypeParamPath) =
        match typeParamPath.Parent with
        | TypeParamPathParent.Type typePath ->
            typePath.Parent, [ Case.withoutMeasure typePath.Name ]
        | TypeParamPathParent.Member memberPath ->
            MemberPath.traceToParentModule memberPath
            ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure memberPath.Name ]
        | TypeParamPathParent.Parameter parameterPath ->
            ParameterPath.traceToParentModule parameterPath
            ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure parameterPath.Name ]
    let create name parent = {
        TypeParamPath.Parent = parent
        Name = name
    }
    let createOnType name parent =
        TypeParamPathParent.Type parent
        |> create name
    let createOnMember name parent =
        TypeParamPathParent.Member parent
        |> create name
    let createOnParameter name parent =
        TypeParamPathParent.Parameter parent
        |> create name

module AnchorPath =
    let createModule (modulePath: ModulePath) = AnchorPath.Module modulePath
    let createType (typePath: TypePath) = AnchorPath.Type typePath
    let createMember (memberPath: MemberPath) = AnchorPath.Member memberPath
    let createParameter (parameterPath: ParameterPath) = AnchorPath.Parameter parameterPath
    let createTypeParameter (typeParameterPath: TypeParamPath) = AnchorPath.TypeParam typeParameterPath
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(modulePath: ModulePath) = createModule modulePath
        static member inline Create(typePath: TypePath) = createType typePath
        static member inline Create(memberPath: MemberPath) = createMember memberPath
        static member inline Create(parameterPath: ParameterPath) = createParameter parameterPath
        static member inline Create(typeParameterPath: TypeParamPath) = createTypeParameter typeParameterPath
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> AnchorPath) value)
    
    let toTypePath = function
        | AnchorPath.Type typePath ->
            let parent = typePath.Parent
            ModulePath.createWithName typePath.Name parent
            |> TypePath.create "Typar"
        | AnchorPath.Member memberPath ->
            MemberPath.traceToParentModule memberPath
            ||> fun modulePath tracedPath ->
                tracedPath
                |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
                |> TypePath.createWithName (Name.Pascal.fromCase memberPath.Name)
        | AnchorPath.Parameter parameterPath ->
            ParameterPath.traceToParentModule parameterPath
            ||> fun modulePath tracedPath ->
                tracedPath
                |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
                |> TypePath.createWithName (Name.Pascal.fromCase parameterPath.Name)
        | AnchorPath.TypeParam typeParamPath ->
            TypeParamPath.traceToParentModule typeParamPath
            ||> fun modulePath tracedPath ->
                tracedPath
                |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
                |> TypePath.createWithName (Name.Pascal.fromCase typeParamPath.Name)
        | AnchorPath.Module modulePath ->
            modulePath.Name
            |> Name.Module.fromCase
            |> Name.Case.valueOrModified
            |> TypePath.create
            |> funApply modulePath
    let traceToParentModule (anchorPath: AnchorPath) =
        match anchorPath with
        | AnchorPath.TypeParam typeParamPath ->
            TypeParamPath.traceToParentModule typeParamPath
            ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure typeParamPath.Name ]
        | AnchorPath.Parameter parameterPath ->
            ParameterPath.traceToParentModule parameterPath
            ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure parameterPath.Name ]
        | AnchorPath.Member memberPath ->
            MemberPath.traceToParentModule memberPath
            ||> fun modulePath tracedPath -> modulePath, tracedPath @ [ Case.withoutMeasure memberPath.Name ]
        | AnchorPath.Type typePath ->
            typePath.Parent, [ Case.withoutMeasure typePath.Name ]
        | AnchorPath.Module modulePath ->
            modulePath, []
    let flatten =
        traceToParentModule
        >> fun (modulePath, pathTrace) ->
            ModulePath.flatten modulePath @ (pathTrace |> List.map Name.Pascal.fromName)
    let flattenCaseless =
        traceToParentModule
        >> fun (modulePath, pathTrace) ->
            ModulePath.flattenCaseless modulePath @ pathTrace
    let last =
        traceToParentModule
        >> function
            | modulePath, [] -> Case.withoutMeasure modulePath.Name
            | _, pathTrace -> pathTrace |> List.last
        
    
module TransientModulePath =
    let createOnTransientModule name transientParent =
        TransientModulePath.Moored(transientParent, Name.Pascal.create name)
    let rec toAnchored (transientModulePath: TransientModulePath) =
        match transientModulePath with
        | TransientModulePath.Anchored -> []
        | TransientModulePath.Moored(parent, name) -> toAnchored parent @ [ Case.withoutMeasure name ]
        | TransientModulePath.AnchoredAndMoored name -> [ Case.withoutMeasure name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientModulePath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        pathTrace @ List.map Case.withoutMeasure anchorTrace
        |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
    
    let graft (parent: TransientModulePath) (child: TransientModulePath) =
        toAnchored child
        |> List.fold (fun acc name -> createOnTransientModule name.ValueOrSource acc) parent

module TransientTypePath =
    let graft (parent: TransientModulePath) =
        match parent with
        | TransientModulePath.Anchored -> TransientTypePath.Anchored
        | TransientModulePath.Moored(_, name) 
        | TransientModulePath.AnchoredAndMoored name ->
            TransientTypePath.Moored(parent, name)

    let createOnTransientModuleWithName name transientParent =
        TransientTypePath.Moored(transientParent, name)
    let createOnTransientModule name transientParent =
        (Name.Pascal.create name, transientParent)
        ||> createOnTransientModuleWithName
    
    let rec toAnchored (transientTypePath: TransientTypePath) =
        match transientTypePath with
        | TransientTypePath.Anchored -> []
        | TransientTypePath.Moored(parent, name) ->
            TransientModulePath.toAnchored parent @ [ Case.withoutMeasure name ]
        | TransientTypePath.AnchoredAndMoored name -> [ Case.withoutMeasure name ]
    
    let anchor (anchorPath: AnchorPath) (transient: TransientTypePath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let combinedTrace = pathTrace @ anchorTrace
        let typeName = combinedTrace |> List.last
        let traceWithoutTypeName = combinedTrace |> List.removeAt (List.length combinedTrace - 1)
        traceWithoutTypeName
        |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
        |> TypePath.createWithName (Name.Pascal.fromName typeName)

module TransientMemberPath =
    let graft (parent: TransientModulePath) =
        match parent with
        | TransientModulePath.Anchored -> TransientMemberPath.Anchored
        | TransientModulePath.Moored(grandParent, name) ->
            TransientMemberPath.Moored(TransientTypePath.Moored(grandParent, name), Name.Camel.fromCase name)
        | TransientModulePath.AnchoredAndMoored name ->
            TransientMemberPath.Moored(TransientTypePath.AnchoredAndMoored name, Name.Camel.fromCase name)
    let createOnTransientTypeWithName name transientParent =
        TransientMemberPath.Moored(transientParent, name)
    let createOnTransientType name transientParent =
        (Name.Camel.create name, transientParent)
        ||> createOnTransientTypeWithName 
    let rec toAnchored (transientMemberPath: TransientMemberPath) =
        match transientMemberPath with
        | TransientMemberPath.Anchored -> []
        | TransientMemberPath.Moored(parent, name) ->
            TransientTypePath.toAnchored parent @ [ Case.withoutMeasure name ]
        | TransientMemberPath.AnchoredAndMoored name -> [ Case.withoutMeasure name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientMemberPath) =
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let anchorTrace =
            if transient.IsAnchored
            then pathTrace |> List.last |> List.singleton
            else toAnchored transient
        let trace = pathTrace @ anchorTrace |> List.rev
        match trace with
        | memberName :: typeName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
            |> TypePath.createWithName (Name.Pascal.fromName typeName)
            |> MemberPath.createOnTypeWithName (Name.Camel.fromName memberName)
        | memberName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
            |> MemberPath.createOnModuleWithName (Name.Camel.fromCase memberName)
        | [] -> failwith "Did not expect empty transient member path"

module TransientParameterPath =
    let createOnTransientMember name transientParent =
        TransientParameterPath.Moored(transientParent, Name.Camel.create name)
    let createOnTransientMemberWithName name transientParent =
        TransientParameterPath.Moored(transientParent, name)
    let rec toAnchored (transientParameterPath: TransientParameterPath) =
        match transientParameterPath with
        | TransientParameterPath.Anchored -> []
        | TransientParameterPath.Moored(parent, name) ->
            TransientMemberPath.toAnchored parent @ [ Case.withoutMeasure name ]
        | TransientParameterPath.AnchoredAndMoored name -> [ Case.withoutMeasure name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientParameterPath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let trace = pathTrace @ anchorTrace |> List.rev
        match trace with
        | [] -> failwith "Did not expect empty transient parameter path"
        | parameterName :: memberName :: typeName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
            |> TypePath.createWithName (Name.Pascal.fromName typeName)
            |> MemberPath.createOnTypeWithName (Name.Camel.fromCase memberName)
            |> ParameterPath.createWithName (Name.Camel.fromCase parameterName)
            |> funApply 0
        | parameterName :: memberName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath
            |> MemberPath.createOnModuleWithName (Name.Camel.fromCase memberName)
            |> ParameterPath.createWithName (Name.Camel.fromCase parameterName)
            |> funApply 0
        | _ -> failwith "Did not expect transient parameter path to have less than 3 elements"

module TransientTypeParameterPath =
    let graft (parent: TransientModulePath) =
        TransientTypePath.graft parent
        |> TransientPath.TypeParam

module TransientPath =
    let createType (typePath: TransientTypePath) = TransientPath.Type typePath
    let createMember (memberPath: TransientMemberPath) = TransientPath.Member memberPath
    let createParameter (parameterPath: TransientParameterPath) = TransientPath.Parameter parameterPath
    let createTypeParameter (typeParameterPath: TransientTypePath) = TransientPath.TypeParam typeParameterPath
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(typePath: TransientTypePath) = createType typePath
        static member inline Create(memberPath: TransientMemberPath) = createMember memberPath
        static member inline Create(parameterPath: TransientParameterPath) = createParameter parameterPath
    let inline create (value: ^T) = ((^T or SRTPHelper): (static member Create: ^T -> TransientPath) value)
    
    let toAnchored (transientPath: TransientPath) =
        match transientPath with
        | TransientPath.Type transientTypePath -> TransientTypePath.toAnchored transientTypePath
        | TransientPath.Member transientMemberPath -> TransientMemberPath.toAnchored transientMemberPath
        | TransientPath.Parameter transientParameterPath -> TransientParameterPath.toAnchored transientParameterPath
        | TransientPath.TypeParam transientTypePath -> TransientTypePath.toAnchored transientTypePath
    let anchor (anchorPath: AnchorPath) (transient: TransientPath) =
        match transient with
        | TransientPath.Type transientTypePath ->
            TransientTypePath.anchor anchorPath transientTypePath
            |> AnchorPath.Type
        | TransientPath.Member transientMemberPath ->
            TransientMemberPath.anchor anchorPath transientMemberPath
            |> AnchorPath.Member
        | TransientPath.Parameter transientParameterPath ->
            TransientParameterPath.anchor anchorPath transientParameterPath
            |> AnchorPath.Parameter
        | TransientPath.TypeParam transientTypePath ->
            TransientTypePath.anchor anchorPath transientTypePath
            |> AnchorPath.Type
    
    module Helpers =
        type FlattenedTransientPath =
            | Type of TransientTypePath
            | Member of TransientMemberPath
            | Parameter of TransientParameterPath
            | TypeParam of TransientTypePath
            | Module of TransientModulePath
        let toFlattenedPath = function
            | TransientPath.Type transientTypePath -> Type transientTypePath
            | TransientPath.Member transientMemberPath -> Member transientMemberPath
            | TransientPath.Parameter transientParameterPath -> Parameter transientParameterPath
            | TransientPath.TypeParam transientTypePath -> TypeParam transientTypePath
        let private flattenTransientPath (transientPath: TransientPath) =
                
            let rec flattenTransientPath (path: FlattenedTransientPath) =
                match path with
                | Type transientTypePath ->
                    match transientTypePath with
                    | TransientTypePath.AnchoredAndMoored _
                    | TransientTypePath.Anchored -> [ path ]
                    | TransientTypePath.Moored(parent, _) ->
                        path :: flattenTransientPath (Module parent)
                | Member transientMemberPath ->
                    match transientMemberPath with
                    | TransientMemberPath.Anchored
                    | TransientMemberPath.AnchoredAndMoored _ -> [ path ]
                    | TransientMemberPath.Moored(parent, _) ->
                        path :: flattenTransientPath (Type parent)
                | Parameter transientParameterPath ->
                    match transientParameterPath with
                    | TransientParameterPath.Anchored 
                    | TransientParameterPath.AnchoredAndMoored _ -> [ path ]
                    | TransientParameterPath.Moored(parent, _) ->
                        path :: flattenTransientPath (Member parent)
                | TypeParam transientTypePath ->
                    match transientTypePath with
                    | TransientTypePath.Anchored 
                    | TransientTypePath.AnchoredAndMoored _ -> [ path ]
                    | TransientTypePath.Moored(parent, _) ->
                        path :: flattenTransientPath (Module parent)
                | Module transientModulePath ->
                    match transientModulePath with
                    | TransientModulePath.Anchored 
                    | TransientModulePath.AnchoredAndMoored _ -> [ path ]
                    | TransientModulePath.Moored(parent, _) ->
                        path :: flattenTransientPath (Module parent)

            toFlattenedPath transientPath
            |> flattenTransientPath
            |> List.rev
        let private reformTransientPath (flattenedTransientPath: FlattenedTransientPath list) =
            let modules, nonModules =
                flattenedTransientPath
                |> List.partition _.IsModule
            let modules =
                modules
                |> List.map (function
                    | Module transientModulePath -> transientModulePath
                    | _ -> failwith "Expected only modules in transient path")
            let nonModules =
                nonModules
                |> List.map (function
                    | Type transientTypePath -> TransientPath.Type transientTypePath
                    | Member transientMemberPath -> TransientPath.Member transientMemberPath
                    | Parameter transientParameterPath -> TransientPath.Parameter transientParameterPath
                    | TypeParam transientTypePath -> TransientPath.TypeParam transientTypePath
                    | Module _ -> failwith "Expected only non-modules")
            let moduleRoot = 
                match modules with
                | [] -> ValueNone
                | headModule :: tailModules ->
                    let headModule =
                        match headModule with
                        | TransientModulePath.Moored(_, name) ->
                            TransientModulePath.AnchoredAndMoored(name)
                        | TransientModulePath.Anchored 
                        | TransientModulePath.AnchoredAndMoored _ -> headModule
                    if tailModules |> List.isEmpty then ValueSome headModule else
                    (headModule :: tailModules)
                    |> List.reduce (fun l r ->
                        match r with
                        | TransientModulePath.Moored(_, name) -> TransientModulePath.Moored(l, name)
                        | _ -> r
                        )
                    |> ValueSome
            match nonModules with
            | [] -> failwith "empty flattened path should not be possible"
            | head :: tail ->
                let head =
                    match head with
                    | TransientPath.Type (TransientTypePath.Moored(_, name)) when moduleRoot.IsSome ->
                        TransientTypePath.Moored(moduleRoot.Value, name)
                        |> TransientPath.Type
                    | _ -> head
                (head :: tail)
                |> List.reduce (fun l r ->
                    match l,r with
                    | TransientPath.Type typePath, TransientPath.Member (TransientMemberPath.Moored(_, name)) ->
                        TransientMemberPath.Moored(typePath, name)
                        |> TransientPath.Member
                    | TransientPath.Member memberPath, TransientPath.Parameter (TransientParameterPath.Moored(_, name)) ->
                        TransientParameterPath.Moored(memberPath, name)
                        |> TransientPath.Parameter
                    | _ -> r)
        let last (transientPath: TransientPath) =
            match transientPath with
            | TransientPath.Type (TransientTypePath.Moored(_, name) | TransientTypePath.AnchoredAndMoored name) -> name |> Case.withoutMeasure |> ValueSome
            | TransientPath.TypeParam (TransientTypePath.Moored(_, name) | TransientTypePath.AnchoredAndMoored name) -> name |> Case.withoutMeasure |> ValueSome
            | TransientPath.Member (TransientMemberPath.Moored(_, name) | TransientMemberPath.AnchoredAndMoored name ) -> name |> Case.withoutMeasure |> ValueSome
            | TransientPath.Parameter (TransientParameterPath.Moored(_, name) | TransientParameterPath.AnchoredAndMoored name) -> name |> Case.withoutMeasure |> ValueSome
            | _ -> ValueNone

        let removeCommonRoots (target: TransientPath) (transient: TransientPath) =
            let rec getCommonIdxOfTransientPathsImpl idx (l: FlattenedTransientPath list) (r: FlattenedTransientPath list) =
                match l, r with
                | [], _ | _, []
                | [], [] -> idx
                | head :: tail, head' :: tail' when head = head' ->
                    getCommonIdxOfTransientPathsImpl (idx + 1) tail tail'
                | _ -> idx
            let targetList = flattenTransientPath target
            let transientList = flattenTransientPath transient
            let commonIdx = getCommonIdxOfTransientPathsImpl -1 targetList transientList
            targetList
            |> List.skip (commonIdx + 1)
            |> reformTransientPath
    let rec toTransientModulePath (transientPath: TransientPath) =
        match transientPath with
        | TransientPath.Type transientTypePath ->
            match transientTypePath with
            | TransientTypePath.Anchored -> TransientModulePath.Anchored
            | TransientTypePath.Moored(parent, name) ->
                TransientModulePath.Moored(parent, name)
            | TransientTypePath.AnchoredAndMoored name -> TransientModulePath.AnchoredAndMoored name
        | TransientPath.Member transientMemberPath ->
            match transientMemberPath with
            | TransientMemberPath.Anchored -> TransientModulePath.Anchored
            | TransientMemberPath.Moored(parent, name) ->
                TransientPath.Type parent
                |> toTransientModulePath
                |> TransientModulePath.createOnTransientModule (Name.Case.valueOrSource name)
            | TransientMemberPath.AnchoredAndMoored name ->
                TransientModulePath.AnchoredAndMoored (Name.Pascal.fromCase name)
        | TransientPath.Parameter transientParameterPath ->
            match transientParameterPath with
            | TransientParameterPath.Anchored -> TransientModulePath.Anchored
            | TransientParameterPath.Moored(parent, name) ->
                TransientPath.Member parent
                |> toTransientModulePath
                |> TransientModulePath.createOnTransientModule (Name.Case.valueOrSource name)
            | TransientParameterPath.AnchoredAndMoored name ->
                TransientModulePath.AnchoredAndMoored (Name.Pascal.fromCase name)
        | TransientPath.TypeParam transientTypePath ->
            TransientPath.Type transientTypePath
            |> toTransientModulePath
    let last = Helpers.last

module Path =
    module RelativeHelper =
        // Returns the index of the last common element in the two lists.
        let rec idxOfLastCommonElement (state: int) (target: string list) (from: string list) =
            match target, from with
            | _, []
            | [], _ 
            | [], [] -> state
            | head :: tail, head' :: tail' when head = head' ->
                idxOfLastCommonElement (state + 1) tail tail'
            | _ -> state

    let getRelativePath (target: TypePath) (from: AnchorPath) =
        // get anchor paths module list
        let anchorModulePath =
            AnchorPath.traceToParentModule from
            |> fst |> ModulePath.flatten
            // we use modified, because transient names can be generated from camel cased names.
            // this means even if the actual paths are the same, their source strings may be different.
            |> List.map Name.Case.valueOrModified
        // get target module list
        let targetModulePath =
            ModulePath.flatten target.Parent
            // we use modified, because transient names can be generated from camel cased names.
            // this means even if the actual paths are the same, their source strings may be different.
            |> List.map Name.Case.valueOrModified
        // get relative root
        let relativeRoot =
            match RelativeHelper.idxOfLastCommonElement -1 anchorModulePath targetModulePath with
            | -1 -> targetModulePath
            // self reference?
            | idx when idx = List.length targetModulePath -> [ List.last targetModulePath ]
            | idx -> targetModulePath |> List.skip (idx + 1)
            |> List.map Name.Pascal.create
        // add leaf name
        relativeRoot @ [ target.Name ]
    let last (path: Path) =
        match path with
        | Path.Anchor anchored -> AnchorPath.last anchored |> ValueSome
        | Path.Transient transient -> TransientPath.last transient

    let createAnchor (anchorPath: AnchorPath) = Path.Anchor anchorPath
    let createTransient (transientPath: TransientPath) = Path.Transient transientPath
    
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline Create(anchorPath: AnchorPath) = createAnchor anchorPath
        static member inline Create(transientPath: TransientPath) = createTransient transientPath
        static member inline Create(path: Path) = path
        
    let inline create (value: ^T) =
        let ir = ((^T or SRTPHelper or AnchorPath.SRTPHelper or TransientPath.SRTPHelper): (static member Create: ^T -> ^U) value)
        ((^U or SRTPHelper): (static member Create: ^U -> Path) ir)

module TypeLikePath =
    type SRTPHelper =
        static member inline Create value = TypeLikePath.Anchored value
        static member inline Create value = TypeLikePath.Transient value
    let inline create value = ((^T or SRTPHelper): (static member Create: ^T -> TypeLikePath) value)

module MemberLikePath =
    type SRTPHelper =
        static member inline Create value = MemberLikePath.Anchored value
        static member inline Create value = MemberLikePath.Transient value
    let inline create value = ((^T or SRTPHelper): (static member Create: ^T -> MemberLikePath) value)

module ParameterLikePath =
    type SRTPHelper =
        static member inline Create value = ParameterLikePath.Anchored value
        static member inline Create value = ParameterLikePath.Transient value
        static member inline CreateParameter(name, parent) = ParameterPath.createWithName name parent 0 |> SRTPHelper.Create
        static member inline CreateParameter(name, parent) = TransientParameterPath.createOnTransientMember name parent |> SRTPHelper.Create
    let inline create value = ((^T or SRTPHelper): (static member Create: ^T -> ParameterLikePath) value)
    let inline createWithName name memberLikePath = ((^T or SRTPHelper): (static member CreateParameter: ^Y * ^T -> ParameterLikePath) (name, memberLikePath))

module TypeParamLikePath =
    type SRTPHelper =
        static member inline Create value = TypeParamLikePath.Anchored value
        static member inline Create value = TypeParamLikePath.Transient value
    let inline create value = ((^T or SRTPHelper): (static member Create: ^T -> TypeParamLikePath) value)

module ModuleLikePath =
    type SRTPHelper =
        static member inline Create value = ModuleLikePath.Anchored value
        static member inline Create value = ModuleLikePath.Transient value
    let inline create value = ((^T or SRTPHelper): (static member Create: ^T -> ModuleLikePath) value)

/// <summary>
/// Builder for constructing paths with easily identifiable syntax (originally made for spec testing, but
/// may be a useful abstraction for consumers who need to create paths for injected types or other purposes).
/// <br/><br/>
/// All concrete paths are preceded by a single <c>_</c>, while transient paths are preceded and followed by <c>_</c>.
/// <br/>
/// Therefor making a concrete path would be:
/// <code>
/// _module "Root"; _module "Module"; _type "Type"
/// </code>
/// And a transient path would be:
/// <code>
/// _module_ "Root"; _type_ "Type"
/// </code>
/// If you do not pass a string to the transient builder, it will be anchored.
/// <code>
/// _type_ // TransientTypePath.Anchored
/// _type_ "Type" // TransientTypePath.AnchoredAndMoored "Type"
/// _module_ "Module"; _type_ "Type" // TransientTypePath.Moored(TransientModulePath.AnchoredAndMoored "Module", "Type")
/// </code>
/// </summary>
type PathBuilder() =
    member inline _.Yield(a: unit) = ()
    member inline _.Yield(n: ModulePath) = n
    member inline _.Yield(n: TypePath) = n
    member inline _.Yield(n: MemberPath) = n
    member inline _.Yield(n: ParameterPath) = n
    member inline _.Yield(n: TypeParamPath) = n
    member inline _.Yield(n: TransientTypePath) = n
    member inline _.Yield(n: TransientModulePath) = n
    member inline _.Yield(n: TransientMemberPath) = n
    member inline _.Yield(n: TransientParameterPath) = n
    member inline _.Yield(n: AnchorPath) = n
    member inline _.Yield(n: TransientPath) = n
    member inline _.Yield(n: Path) = n
    member inline _.Yield(n: string) = n
    member inline _.Combine(a, b: unit) = a
    member inline _.Zero() = ()
    member inline this.Combine(a: ModulePath, b: string) = ModulePath.create b a
    member inline this.Combine(a: string, b: string) = ModulePath.init a |> ModulePath.create b
    member inline this.Combine(a: TypePath, b: string) = MemberPath.createOnType b a
    member inline this.Combine(a: MemberPath, b: string) = ParameterPath.create b a
    member inline this.Combine(a: TypePath, b: Name<Case.typar>) = TypeParamPath.createOnType b a
    member inline this.Combine(a: MemberPath, b: Name<Case.typar>) = TypeParamPath.createOnMember b a
    member inline this.Combine(a: ParameterPath, b: Name<Case.typar>) = TypeParamPath.createOnParameter b a
    member inline this.Combine(a: TransientModulePath, b: string) = TransientModulePath.createOnTransientModule b a
    member inline this.Combine(a: TransientTypePath, b: string) = TransientMemberPath.createOnTransientType b a
    member inline this.Combine(a: TransientMemberPath, b: string) = TransientParameterPath.createOnTransientMember b a
    member inline this.Delay([<InlineIfLambda>] a) = a()
    member inline this.For(a: ModulePath, b: unit -> string) = this.Combine(a, b())
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: unit, b: string) = TransientModulePath.AnchoredAndMoored(Name.Pascal.create b)
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: TransientModulePath, b: string) = TransientModulePath.Moored(a, Name.Pascal.create b)
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: string, b: string) =
        TransientModulePath.AnchoredAndMoored(Name.Pascal.create a)
        |> TransientModulePath.createOnTransientModule b
    [<CustomOperation "_module_">]
    member inline _.MakeTransientModule(a: unit) = TransientModulePath.Anchored
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: unit) = TransientTypePath.Anchored
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: TransientModulePath, b: string) = TransientTypePath.Moored(a, Name.Pascal.create b)
    [<CustomOperation "_type_">]
    member inline _.MakeTransientType(a: unit, b: string) = TransientTypePath.AnchoredAndMoored(Name.Pascal.create b)
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: unit) = TransientMemberPath.Anchored
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: TransientTypePath, b: string) = TransientMemberPath.Moored(a, Name.Camel.create b)
    [<CustomOperation "_member_">]
    member inline _.MakeTransientMember(a: unit, b: string) = TransientMemberPath.AnchoredAndMoored(Name.Camel.create b)
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: unit) = TransientParameterPath.Anchored
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: TransientMemberPath, b: string) = TransientParameterPath.Moored(a, Name.Camel.create b)
    [<CustomOperation "_parameter_">]
    member inline _.MakeTransientParameter(a: unit, b: string) = TransientParameterPath.AnchoredAndMoored(Name.Camel.create b)
    [<CustomOperation "_module">]
    member inline _.MakeModule(a: unit, b: string) = ModulePath.init b
    [<CustomOperation "_module">]
    member inline _.MakeModule(a: ModulePath, b: string) = ModulePath.create b a
    [<CustomOperation "_type">]
    member inline _.MakeType(a: ModulePath, b: string) = TypePath.create b a
    [<CustomOperation "_member">]
    member inline _.MakeMember(a: TypePath, b: string) = MemberPath.createOnType b a
    [<CustomOperation "_member">]
    member inline _.MakeMember(a: ModulePath, b: string) = MemberPath.createOnModule b a
    [<CustomOperation "_parameter">]
    member inline _.MakeParameter(a: MemberPath, b: string) = ParameterPath.create b a
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: TypePath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: MemberPath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "_typar">]
    member inline this.MakeTypeParam(a: ParameterPath, b: string) = this.Combine(a, Name.Typar.create b)
    [<CustomOperation "asAnchorPath">]
    member inline _.AsAnchorPath(a) = AnchorPath.create a
    [<CustomOperation "asTransientPath">]
    member inline _.AsTransientPath(a) = TransientPath.create a
    [<CustomOperation "asPath">]
    member inline _.AsPath(a) = Path.create a
    member inline _.Run(a) = a
/// <summary>
/// Builder for constructing paths with easily identifiable syntax (originally made for spec testing, but
/// may be a useful abstraction for consumers who need to create paths for injected types or other purposes).
/// <br/><br/>
/// All concrete paths are preceded by a single <c>_</c>, while transient paths are preceded and followed by <c>_</c>.
/// <br/>
/// Therefor making a concrete path would be:
/// <code>
/// _module "Root"; _module "Module"; _type "Type"
/// </code>
/// And a transient path would be:
/// <code>
/// _module_ "Root"; _type_ "Type"
/// </code>
/// If you do not pass a string to the transient builder, it will be anchored.
/// <code>
/// _type_ // TransientTypePath.Anchored
/// _type_ "Type" // TransientTypePath.AnchoredAndMoored "Type"
/// _module_ "Module"; _type_ "Type" // TransientTypePath.Moored(TransientModulePath.AnchoredAndMoored "Module", "Type")
/// </code>
/// </summary>
let pathCe = PathBuilder()
