// ReSharper disable FSharpInterpolatedString

namespace Xantham.Generator

open System.Collections.Frozen
open System.Collections.Generic
open Microsoft.Extensions.Logging
open Xantham
open Xantham.Decoder
open FSharp.Logf

type VisitationFlags =
    | None = 0
    | WouldYieldInt = ((<<<) 1 0)
    | WouldYieldBool = ((<<<) 1 1)
    | WouldYieldNull = ((<<<) 1 2)
    | WouldYieldString = ((<<<) 1 3)
    | WouldYieldFloat = ((<<<) 1 4)
    | MaybeParamObject  = ((<<<) 1 5)
    | ParamArray  = ((<<<) 1 6)
    | IsObsolete  = ((<<<) 1 7)
    | HasSource = ((<<<) 1 8)
    | HasMembers = ((<<<) 1 9)
    | HasName = ((<<<) 1 10)
    | HasTypeParameters = ((<<<) 1 11)
    | IsTypeParameter = ((<<<) 1 12)
    | IsGeneric = ((<<<) 1 13)
    | IsConstrained = ((<<<) 1 14)
    | IsSymbolMember = ((<<<) 1 15)
    | IsFullyQualified = ((<<<) 1 16)
    | IsEsLib = ((<<<) 1 17)
    | IsNeverTyped = ((<<<) 1 18)
    | HasDocs = (1 <<< 19)
    | ContainsCyclicReference = (1 <<< 20)
module VisitationFlags =
    [<Literal>]
    let HasDocumentation = VisitationFlags.IsObsolete ||| VisitationFlags.HasDocs
    [<Literal>]
    let WouldYieldLiteral: VisitationFlags =
        VisitationFlags.WouldYieldInt
        ||| VisitationFlags.WouldYieldBool
        ||| VisitationFlags.WouldYieldNull
        ||| VisitationFlags.WouldYieldString
        ||| VisitationFlags.WouldYieldFloat
    [<Literal>]
    let RequiresAttributes =
        VisitationFlags.ParamArray
        ||| VisitationFlags.MaybeParamObject
        ||| VisitationFlags.IsObsolete
        
    let toStringArray (flags: VisitationFlags) =
        [|
            if flags.HasFlag(VisitationFlags.WouldYieldInt) then nameof VisitationFlags.WouldYieldInt 
            if flags.HasFlag(VisitationFlags.WouldYieldBool) then nameof VisitationFlags.WouldYieldBool 
            if flags.HasFlag(VisitationFlags.WouldYieldNull) then nameof VisitationFlags.WouldYieldNull 
            if flags.HasFlag(VisitationFlags.WouldYieldString) then nameof VisitationFlags.WouldYieldString 
            if flags.HasFlag(VisitationFlags.WouldYieldFloat) then nameof VisitationFlags.WouldYieldFloat 
            if flags.HasFlag(WouldYieldLiteral) then nameof WouldYieldLiteral
            if flags.HasFlag(VisitationFlags.MaybeParamObject ) then nameof VisitationFlags.MaybeParamObject  
            if flags.HasFlag(VisitationFlags.ParamArray ) then nameof VisitationFlags.ParamArray  
            if flags.HasFlag(VisitationFlags.IsObsolete ) then nameof VisitationFlags.IsObsolete  
            if flags.HasFlag(VisitationFlags.HasSource) then nameof VisitationFlags.HasSource 
            if flags.HasFlag(VisitationFlags.HasMembers) then nameof VisitationFlags.HasMembers 
            if flags.HasFlag(VisitationFlags.HasName) then nameof VisitationFlags.HasName 
            if flags.HasFlag(VisitationFlags.HasTypeParameters) then nameof VisitationFlags.HasTypeParameters
            if flags.HasFlag(VisitationFlags.IsTypeParameter) then nameof VisitationFlags.IsTypeParameter
            if flags.HasFlag(VisitationFlags.IsGeneric) then nameof VisitationFlags.IsGeneric
            if flags.HasFlag(RequiresAttributes) then nameof RequiresAttributes
            if flags.HasFlag(HasDocumentation) then nameof HasDocumentation
            if flags.HasFlag(VisitationFlags.IsSymbolMember) then nameof VisitationFlags.IsSymbolMember
            if flags.HasFlag(VisitationFlags.IsFullyQualified) then nameof VisitationFlags.IsFullyQualified
            if flags.HasFlag(VisitationFlags.IsEsLib) then nameof VisitationFlags.IsEsLib
            if flags.HasFlag(VisitationFlags.IsNeverTyped) then nameof VisitationFlags.IsNeverTyped
            if flags.HasFlag(VisitationFlags.ContainsCyclicReference) then nameof VisitationFlags.ContainsCyclicReference
        |]
[<AutoOpen>]
module VisitationFlagDictionaryExtensions =
    module Dictionary =
        module Enum =
            let inline has (key: TypeKey) (value: VisitationFlags) (dict: Dictionary<TypeKey, VisitationFlags>) =
                Dictionary.tryItem key dict
                |> ValueOption.exists _.HasFlag(value)
            let inline hasMask (key: TypeKey) (mask: VisitationFlags) (dict: Dictionary<TypeKey, VisitationFlags>) =
                Dictionary.tryItem key dict
                |> ValueOption.exists ((&&&) mask >> int >> (<>) 0)
            let inline add (key: TypeKey) (value: VisitationFlags) (dict: Dictionary<TypeKey, VisitationFlags>) =
                let addResult: VisitationFlags = Dictionary.tryAddOrGet key value dict
                if addResult <> value then
                    dict[key] <- addResult ||| value
            let inline remove (key: TypeKey) (value: VisitationFlags) (dict: Dictionary<TypeKey, VisitationFlags>) =
                dict
                |> Dictionary.tryItem key
                |> ValueOption.iter (fun flags ->
                    dict[key] <- flags ^^^ (flags &&& value))
            let inline set (key: TypeKey) (value: VisitationFlags) (dict: Dictionary<TypeKey, VisitationFlags>) =
                dict
                |> Dictionary.tryItem key
                |> function
                    | ValueSome _ -> dict[key] <- value
                    | ValueNone -> dict.Add(key, value)
        module Flip =
            module Enum =
                let inline has dict key value = Enum.has key value dict
                let inline add dict key value = Enum.add key value dict
                let inline remove dict key value = Enum.remove key value dict
                let inline set dict key value = Enum.set key value dict