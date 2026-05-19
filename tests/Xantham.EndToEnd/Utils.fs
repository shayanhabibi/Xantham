[<AutoOpen>]
module Utils

open Fake.Core
open Fake.JavaScript

let private createProcess exe numItems args dir =
    CreateProcess.fromRawCommand exe args
    |> CreateProcess.withWorkingDirectory dir
    |> CreateProcess.redirectOutput
    |> CreateProcess.mapResult (function
        | { Error = null | ""; Output = text } ->
            Ok, text
        | { Error = text } ->
            Error, text
        >> fun (fn, text) ->
            if numItems = -1 then fn text else
            text
            |> String.convertTextToWindowsLineBreaks
            |> String.splitStr String.WindowsLineBreaks
            |> List.rev
            |> List.truncate numItems
            |> List.rev
            |> String.concat String.WindowsLineBreaks
            |> fn
            )
let inline private createProcessAndRun exe = fun numLines args dir ->
    createProcess exe numLines args dir |> Proc.run
let inline private createProcessAndRunOrThrow exe = fun numLines args dir ->
    createProcess exe numLines args dir |> CreateProcess.ensureExitCode |> Proc.run
let inline private createProcessAndRunOrThrowAndForget exe = fun numLines args dir ->
    createProcess exe numLines args dir |> CreateProcess.ensureExitCode |> Proc.run |> ignore
    

let dotnet numLines args dir = createProcessAndRun "dotnet" numLines args dir 
let dotnetOrThrow numLines args dir = createProcessAndRunOrThrow "dotnet" numLines args dir 
let dotnetOrThrowAndForget numLines args dir = createProcessAndRunOrThrowAndForget "dotnet" numLines args dir

let node numLines args dir = createProcessAndRun "node" numLines args dir 
let nodeOrThrow numLines args dir = createProcessAndRunOrThrow "node" numLines args dir 
let nodeOrThrowAndForget numLines args dir = createProcessAndRunOrThrowAndForget "node" numLines args dir
    
module Npm =
    let setDir dir = fun param -> { param with Npm.NpmParams.WorkingDirectory = dir }
