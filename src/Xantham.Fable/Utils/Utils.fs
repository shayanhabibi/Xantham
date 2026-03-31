[<AutoOpen>]
module Xantham.Fable.Utils

open Fable.Core

let inline funApply args f = f args

type System.Collections.Generic.List<'T> with
    [<Emit "$0">]
    member inline this.AsArray: 'T array = unbox this

module Log =
    open Glutinum.Chalk

    let inline emit text = JS.console.error text

    let success (text: string) = chalk.greenBright.Invoke text |> emit
    let log (text: string) = text |> emit
    let info (text: string) = chalk.blueBright.Invoke text |> emit
    let warn (text: string) = chalk.yellowBright.Invoke text |> emit
    let error (text: string) = chalk.redBright.Invoke text |> emit
    let debug (text: string) = chalk.gray.Invoke text |> emit
    
    let traceTo (depth: int) o = JS.console.dir(o, {| depth = depth; colors = true |}) 
    let traceInf o = JS.console.dir(o, {| depth = null; colors = true |})
    let trace o = traceTo 3 o


[<AutoOpen>]
module Report =
    open TypeScript
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type Report =
        static member readerError(
                errorContext: string,
                reason: string,
                node: Ts.Type,
                checker: Ts.TypeChecker,
                [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
                [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
            ) =
            let errorOrigin =
                let filePath =
                    let index = filePath.IndexOf("src/Glutinum.Converter")
                    "./" + filePath.Substring(index)

                $"%s{filePath}(%d{fileLine})".Replace("\\", "/")


            $"""%s{errorOrigin}: Error while reading %s{errorContext} from:
    (source file not available for report)
    %s{node.symbol.name}

    %s{reason}

    --- Text ---
    %s{checker.typeToString node}
    ---"""
        static member readerError
            (
                errorContext: string,
                reason: string,
                node: Ts.Node,
                [<CallerFilePath; Optional; DefaultParameterValue("")>] filePath: string,
                [<CallerLineNumber; Optional; DefaultParameterValue(0)>] fileLine: int
            )

            =

            let errorOrigin =
                let filePath =
                    let index = filePath.IndexOf("src/Glutinum.Converter")
                    "./" + filePath.Substring(index)

                $"%s{filePath}(%d{fileLine})".Replace("\\", "/")

            let sourceFile =
                try
                node.getSourceFile ()
                with _ -> null

            if isNull sourceFile then
                $"""%s{errorOrigin}: Error while reading %s{errorContext} from:
    (source file not available for report)

    %s{reason}"""
            else
                let lineAndChar = sourceFile.getLineAndCharacterOfPosition node.pos
                let line = int lineAndChar.line + 1
                let column = int lineAndChar.character + 1
                let typeFileName = sourceFile.fileName.Replace("\\", "/")

                let parentText =
                    if isNull node.parent then
                        ""
                    else
                        $"""
    --- Parent text ---
    %s{node.parent.getFullText ()}
    ---"""

                $"""%s{errorOrigin}: Error while reading %s{errorContext} from:
    %s{typeFileName}(%d{line},%d{column})

    %s{reason}

    --- Text ---
    %s{node.getFullText ()}
    ---%s{parentText}"""
