[<AutoOpen>]
module GlobalUtils

open System.Collections.Generic
open FSharp.SystemCommandLine
open FSharp.Reflection

/// Extensions to System.CommandLine
[<RequireQualifiedAccess>]
module Input =
    /// No field cases allowed
    let defaultUnionParser<'T>: ActionInput<'T> -> ActionInput<'T> =
        let cases =
            FSharpType.GetUnionCases(typeof<'T>)
            |> Array.map (fun caseInfo ->
                if caseInfo.GetFields().Length > 0 then failwith "No field cases allowed when using the defaultUnionParser!"
                caseInfo.Name, FSharpValue.PreComputeUnionConstructor(caseInfo))
        Input.customParser(
            _.Tokens
            >> Seq.head
            >> _.Value >> fun stringValue ->
                cases
                |> Array.find (fst >> (=) stringValue)
                |> snd
                |> fun fn -> fn [||] |> unbox<'T>
            )
        >> Input.acceptOnlyFromAmong (cases |> Array.map fst)

[<RequireQualifiedAccess>]
module Reflection =
    type private Computed = {
        Commands: System.CommandLine.Command array
        ActionInputs: ActionInput array
        Constructors: (string * (objnull[] -> objnull))[]
        DummyValues: objnull[]
    }
    type private Fns = {
        Command: (obj -> System.CommandLine.Command) option
        CommandMaybe: (obj -> System.CommandLine.Command option) option
        ActionInput: (obj -> ActionInput) option
        ActionInputMaybe: (obj -> ActionInput option) option
    }
    let private cache = Dictionary<System.Type, Computed>()
    
    let private cacheType<'T>() =
        let cases = FSharpType.GetUnionCases(typeof<'T>)
        let constructors =
            cases
            |> Array.map (fun caseInfo ->
                caseInfo.Name, FSharpValue.PreComputeUnionConstructor(caseInfo))
        let properties = typeof<'T>.GetProperties()
        let commandGetter =
            properties
            |> Array.tryFind(fun prop -> prop.PropertyType = typeof<System.CommandLine.Command>)
        let optionCommandGetter =
            if commandGetter.IsSome then None else
            properties
            |> Array.tryFind(fun prop -> prop.PropertyType = typeof<System.CommandLine.Command option>)
        let actionInputGetter =
            properties
            |> Array.tryFind(fun prop -> prop.PropertyType = typeof<ActionInput> || prop.PropertyType.BaseType = typeof<ActionInput>)
        let actionInputOptionGetter =
            properties
            |> Array.tryFind(fun prop ->
                prop.PropertyType = typeof<ActionInput option>
                || prop.PropertyType.GenericTypeArguments |> Array.tryHead |> Option.contains typeof<ActionInput>)
        let dummyValues = constructors |> Array.map snd |> Array.map (fun fn -> fn [||])
        let commands =
            dummyValues
            |> Array.choose (fun union ->
                commandGetter
                |> Option.map (fun fn -> fn.GetValue union |> unbox<System.CommandLine.Command>)
                |> Option.orElseWith (fun () ->
                    optionCommandGetter
                    |> Option.bind (fun fn -> fn.GetValue union |> unbox<System.CommandLine.Command option>)
                    )
                )
        let actions =
            dummyValues
            |> Array.choose (fun union ->
                actionInputGetter
                |> Option.map (fun fn -> fn.GetValue union |> unbox<ActionInput>)
                |> Option.orElseWith (fun () ->
                    actionInputOptionGetter
                    |> Option.bind (fun fn -> fn.GetValue union |> unbox<ActionInput option>)
                    )
                )
        let computed = {
            Commands = commands
            ActionInputs = actions
            Constructors = constructors
            DummyValues = dummyValues
        }
        cache.Add(typeof<'T>, computed)
        computed
    let commands<'T> =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.Commands
        | _ -> cacheType<'T>().Commands
    let commandsFrom<'T> (fn: 'T -> System.CommandLine.Command) =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.DummyValues
        | _ -> cacheType<'T>().DummyValues
        |> Array.map (unbox >> fn)
    let commandsFromMaybe<'T>(fn: 'T -> System.CommandLine.Command option) =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.DummyValues
        | _ -> cacheType<'T>().DummyValues
        |> Array.choose (unbox >> fn)
    let actions<'T> =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.ActionInputs
        | _ -> cacheType<'T>().ActionInputs
    let actionsFrom<'T> (fn: 'T -> ActionInput) =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.DummyValues
        | _ -> cacheType<'T>().DummyValues
        |> Array.map (unbox >> fn)
    let actionsFromMaybe<'T> (fn: 'T -> ActionInput option) =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.DummyValues
        | _ -> cacheType<'T>().DummyValues
        |> Array.choose (unbox >> fn)
    let constructors<'T> =
        match cache.TryGetValue(typeof<'T>) with
        | true, value -> value.Constructors
        | _ -> cacheType<'T>().Constructors
    let buildTargets<'T> (buildOp: 'T -> unit) =
        match cache.TryGetValue(typeof<'T>) with
        | true, value ->
            value.DummyValues
        | _ ->
            cacheType<'T>().DummyValues
        |> Array.iter (unbox >> buildOp)
