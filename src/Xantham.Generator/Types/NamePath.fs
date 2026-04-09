module Xantham.Generator.NamePath

open System.Collections.Generic
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
    
    let parse (qualifiedName: string list) =
        foldParse qualifiedName []
    
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
    
    let parse (qualifiedName: string list) =
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
        | [] -> invalidArg "parts" "Cannot create module path from empty list"
        | head :: tail ->
            tail
            |> List.fold (fun acc part -> create part acc) (init head)
    let parent (path: ModulePath) = path.Parent
    let depth (path: ModulePath) = path.Depth
    let name (path: ModulePath) = path.Name
    let pruneParent (modulePath: ModulePath) =
        if modulePath.Parent.IsSome then
            initWithName modulePath.Name
        else modulePath
    let flatten (modulePath: ModulePath) =
        let rec flattenModule (modulePath: ModulePath) =
            modulePath.Name :: (
                modulePath.Parent
                |> ValueOption.map flattenModule
                |> ValueOption.defaultValue []
            )
        flattenModule modulePath
        |> List.rev

module TypePath =
    let flatten { TypePath.Parent = parent; Name = name } =
        ModulePath.flatten parent @ [ name ]
    let inline traceToParentModule (typePath: TypePath) = typePath.Parent, [ typePath.Name ]
    let createWithName name parent = {
        TypePath.Parent = parent
        Name = name
    }
    let create name parent =
        Name.Pascal.create name
        |> createWithName
        |> funApply parent
    
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
        
    
module TransientModulePath =
    let rec toAnchored (transientModulePath: TransientModulePath) =
        match transientModulePath with
        | TransientModulePath.Anchored -> []
        | TransientModulePath.Moored(parent, name) -> toAnchored parent @ [ name ]
        | TransientModulePath.AnchoredAndMoored name -> [ name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientModulePath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        pathTrace @ List.map Case.withoutMeasure anchorTrace
        |> List.fold (fun acc name -> ModulePath.createWithName (Name.Pascal.fromName name) acc) modulePath

module TransientTypePath =
    let rec toAnchored (transientTypePath: TransientTypePath) =
        match transientTypePath with
        | TransientTypePath.Anchored -> []
        | TransientTypePath.Moored(parent, name) ->
            TransientModulePath.toAnchored parent @ [ name ]
        | TransientTypePath.AnchoredAndMoored name -> [ name ]
    
    let anchor (anchorPath: AnchorPath) (transient: TransientTypePath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let combinedTrace = List.map Name.Pascal.fromName pathTrace @ anchorTrace
        let typeName = combinedTrace |> List.last
        let traceWithoutTypeName = combinedTrace |> List.removeAt (List.length combinedTrace - 1)
        traceWithoutTypeName
        |> List.fold (fun acc name -> ModulePath.createWithName name acc) modulePath
        |> TypePath.createWithName typeName

module TransientMemberPath =
    let rec toAnchored (transientMemberPath: TransientMemberPath) =
        match transientMemberPath with
        | TransientMemberPath.Anchored -> []
        | TransientMemberPath.Moored(parent, name) ->
            TransientTypePath.toAnchored parent @ [ Name.Pascal.fromCase name ]
        | TransientMemberPath.AnchoredAndMoored name -> [ Name.Pascal.fromCase name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientMemberPath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let trace = List.map Name.Pascal.fromName pathTrace @ anchorTrace |> List.rev
        match trace with
        | memberName :: typeName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName name acc) modulePath
            |> TypePath.createWithName typeName
            |> MemberPath.createOnTypeWithName (Name.Camel.fromCase memberName)
        | memberName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName name acc) modulePath
            |> MemberPath.createOnModuleWithName (Name.Camel.fromCase memberName)
        | [] -> failwith "Did not expect empty transient member path"

module TransientParameterPath =
    let rec toAnchored (transientParameterPath: TransientParameterPath) =
        match transientParameterPath with
        | TransientParameterPath.Anchored -> []
        | TransientParameterPath.Moored(parent, name) ->
            TransientMemberPath.toAnchored parent @ [ Name.Pascal.fromCase name ]
        | TransientParameterPath.AnchoredAndMoored name -> [ Name.Pascal.fromCase name ]
    let anchor (anchorPath: AnchorPath) (transient: TransientParameterPath) =
        let anchorTrace = toAnchored transient
        let modulePath, pathTrace = AnchorPath.traceToParentModule anchorPath
        let trace = List.map Name.Pascal.fromName pathTrace @ anchorTrace |> List.rev
        match trace with
        | [] -> failwith "Did not expect empty transient parameter path"
        | parameterName :: memberName :: typeName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName name acc) modulePath
            |> TypePath.createWithName typeName
            |> MemberPath.createOnTypeWithName (Name.Camel.fromCase memberName)
            |> ParameterPath.createWithName (Name.Camel.fromCase parameterName)
            |> funApply 0
        | parameterName :: memberName :: moduleTrace ->
            moduleTrace |> List.rev
            |> List.fold (fun acc name -> ModulePath.createWithName name acc) modulePath
            |> MemberPath.createOnModuleWithName (Name.Camel.fromCase memberName)
            |> ParameterPath.createWithName (Name.Camel.fromCase parameterName)
            |> funApply 0
        | data -> failwith "Did not expect transient parameter path to have less than 3 elements"

module TransientPath =
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

module Path =
    module private RelativeHelper =
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

        
        