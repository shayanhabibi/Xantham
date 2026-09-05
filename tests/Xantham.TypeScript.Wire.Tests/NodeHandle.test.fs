/// Exercises `NodeHandle`, the string a checker response names a syntax node by.
///
/// The live half is the reason the type exists: a parameter's `?` reaches a caller only through
/// the declaration node, so a handle is the whole bridge from a symbol to the syntax that
/// declared it.
module Xantham.TypeScript.Wire.Tests.NodeHandle

open System.IO
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.TypeScript.Wire.Patterns

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")
let private exePath = Tsc.locate __SOURCE_DIRECTORY__
let private file name = DocumentIdentifier.FileName(Path.Combine(fixtures, name))

let private withSession (test: Session<TscChannel> -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use channel = new TscChannel(exe, fixtures)
            Api.initialize channel |> ignore
            let snapshot = channel.updateSnapshot(openProjects = [| file "tsconfig.json" |])
            test (channel.Session snapshot)

/// The parameters of the one call signature of an exported function, in declaration order.
let private parametersOf (session: Session<TscChannel>) (name: string) =
    let entry = file "optional.d.ts"

    let moduleSymbol =
        session.getSymbolOfSourceFile entry
        |> ValueOption.defaultWith (fun () -> failtest "optional.d.ts declares no module")

    let export =
        session.getExportsOfModule moduleSymbol.Id
        |> ValueOption.defaultValue [||]
        |> Array.find (fun symbol -> symbol.Name = name)

    let signature =
        session.getSignaturesOfType((session.getTypeOfSymbol export.Id).Id, SignatureKind.Call)
        |> Array.exactlyOne

    session.getParametersOfSignature signature.Id |> ValueOption.defaultValue [||]

[<Tests>]
let nodeHandleTests =
    testList "node handle" [
        testCase "a Windows path keeps its drive colon and its dots" <| fun _ ->
            let handle = "12.170.c:/packages/some.pkg/index.d.ts"

            match NodeHandle.parse handle with
            | ValueNone -> failtest "a well-formed handle was refused"
            | ValueSome parsed ->
                Expect.equal parsed.Index 12 "the index"
                Expect.equal parsed.Kind SyntaxKind.Parameter "the kind, by name rather than by ordinal"
                Expect.equal parsed.Path "c:/packages/some.pkg/index.d.ts" "everything after the second dot"
                Expect.equal (NodeHandle.format parsed) handle "and it round-trips"

        testCase "a string that is not a handle is refused" <| fun _ ->
            for candidate in [ ""; "main.ts"; "12.170"; "twelve.170.main.ts"; "12.kind.main.ts" ] do
                Expect.isTrue (NodeHandle.parse candidate).IsNone $"refused: {candidate}"

        testCase "an optional parameter is marked nowhere on its symbol"
        <| withSession (fun session ->
            for parameter in parametersOf session "marked" do
                Expect.equal parameter.Flags SymbolFlags.FunctionScopedVariable "the flags a parameter symbol carries"

                Expect.equal parameter.CheckFlags CheckFlags.None "and CheckFlags.OptionalParameter marks synthetics only")

        testCase "it is marked on the declaration the handle names"
        <| withSession (fun session ->
            let optionality name =
                parametersOf session name
                |> Array.map (fun parameter ->
                    let handle =
                        parameter.Declarations
                        |> ValueOption.defaultValue [||]
                        |> Array.head
                        |> NodeHandle.parse
                        |> ValueOption.defaultWith (fun () -> failtest "a declaration that is not a handle")

                    let ast =
                        session.getSourceFile(DocumentIdentifier.FileName handle.Path)
                        |> ValueOption.defaultWith (fun () -> failtest $"no blob for {handle.Path}")

                    match Node.ofIndex<AnyNode> ast handle.Index with
                    | ParameterDeclaration declaration ->
                        parameter.Name, (ParameterDeclaration.questionToken declaration |> ValueOption.isSome)
                    | _ -> failtest $"handle {NodeHandle.format handle} does not name a parameter")

            Expect.equal (optionality "marked") [| "a", false; "b", true |] "the ? marker, read off the syntax"

            Expect.equal
                (optionality "unioned")
                [| "a", false; "b", false |]
                "a declared type admitting undefined carries no marker")

        testCase "a property states the same fact on its symbol"
        <| withSession (fun session ->
            let declared =
                session.getSymbolOfSourceFile(file "optional.d.ts")
                |> ValueOption.defaultWith (fun () -> failtest "optional.d.ts declares no module")

            let marked =
                session.getExportsOfModule declared.Id
                |> ValueOption.defaultValue [||]
                |> Array.find (fun symbol -> symbol.Name = "Marked")

            let property =
                session.getPropertiesOfType (session.getDeclaredTypeOfSymbol marked.Id).Id
                |> ValueOption.defaultValue [||]
                |> Array.exactlyOne

            Expect.isTrue
                (property.Flags.HasFlag SymbolFlags.Optional)
                "SymbolFlags.Optional is a property's answer, which is why a parameter needs the syntax")
    ]
