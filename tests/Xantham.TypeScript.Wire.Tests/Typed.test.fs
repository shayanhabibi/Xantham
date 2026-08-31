/// Exercises the generated typed layer against `main.ts`.
///
/// The point of these is less the assertions than the code they are written in: every line below
/// would fail to compile if a tag were wrong, and none of it names a slot, an index or a kind
/// ordinal.
module Xantham.TypeScript.Wire.Tests.Typed

open System.IO
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Proto
open Xantham.TypeScript.Wire.Patterns

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")
let private exePath = Tsc.locate __SOURCE_DIRECTORY__
let private file name = DocumentIdentifier.FileName(Path.Combine(fixtures, name))

/// A fresh session per test, as in `Live.fs` and for the same reason.
let private withMain (test: Node<SourceFile> -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use channel = new TscChannel(exe, fixtures)
            Api.initialize channel |> ignore

            let snapshot =
                Api.updateSnapshot channel
                    { OpenProjects = ValueSome [| file "tsconfig.json" |]
                      CloseProjects = ValueNone
                      FileChanges = ValueNone
                      OpenFiles = ValueNone
                      CloseFiles = ValueNone }

            match
                Api.getSourceFile channel
                    { Snapshot = snapshot.Snapshot
                      Project = snapshot.Projects[0].Id
                      File = file "main.ts" }
            with
            | ValueNone -> failtest "expected an AST for main.ts"
            | ValueSome ast -> test (Node.root ast)

/// `export function distance(a: Point, b: Point): number` - the one function in the fixture.
let private distance (root: Node<SourceFile>) =
    SourceFile.statements root
    |> Seq.pick (fun statement ->
        match statement with
        | FunctionDeclaration declaration -> Some declaration
        | _ -> None)

[<Tests>]
let typedTests =
    testList "typed layer" [
        match exePath with
        | None ->
            testCase "native tsc not found - typed layer tests skipped" <| fun _ ->
                skiptest "run `npm install` in tests/Xantham.TypeScript.Wire.Tests, or set XANTHAM_TSGO_EXE"
        | Some _ ->

        // `name` is typed as `Node<Identifier>`, so `Identifier.text` applies without a check -
        // the schema already said the slot holds an identifier.
        testCase "a declaration's name is an Identifier by construction" <| withMain (fun root ->
            distance root
            |> FunctionDeclaration.name
            |> ValueOption.bind Identifier.text
            |> Flip.Expect.equal "the function's name" (ValueSome "distance"))

        testCase "parameters are ParameterDeclarations, and carry their own names" <| withMain (fun root ->
            distance root
            |> FunctionDeclaration.parameters
            |> Seq.map (fun parameter ->
                match ParameterDeclaration.name parameter with
                | ValueSome (Identifier name) -> Identifier.text name |> ValueOption.defaultValue ""
                | _ -> failtest "a parameter with no identifier name")
            |> List.ofSeq
            |> Flip.Expect.equal "both parameter names" [ "a"; "b" ])

        // The interesting walk: body is a `FunctionBody`, which narrows to `Block`, whose
        // statements narrow to `ReturnStatement`, whose expression is an `Expression`, which
        // narrows to `CallExpression`. Five tags, no ints.
        testCase "the body walks down to the call it returns" <| withMain (fun root ->
            let returned =
                match distance root |> FunctionDeclaration.body with
                | ValueSome (Block body) ->
                    Block.statements body
                    |> Seq.pick (fun statement ->
                        match statement with
                        | ReturnStatement statement -> ReturnStatement.expression statement |> ValueOption.toOption
                        | _ -> None)
                | _ -> failtest "expected a block body"

            match returned with
            | CallExpression call ->
                match CallExpression.expression call with
                | ValueSome (PropertyAccessExpression callee) ->
                    PropertyAccessExpression.expression callee
                    |> ValueOption.map (fun object' ->
                        match object' with
                        | Identifier name -> Identifier.text name
                        | _ -> failtest "expected an identifier callee object")
                    |> ValueOption.flatten
                    |> Flip.Expect.equal "the object of the call" (ValueSome "Math")
                | _ -> failtest "expected a property access callee"

                CallExpression.arguments call
                |> Seq.length
                |> Flip.Expect.equal "hypot's two arguments" 2
            | _ -> failtest "expected the return value to be a call")

        // Widening is explicit and erases: same node, wider tag.
        testCase "an Identifier widens to an Expression" <| withMain (fun root ->
            match distance root |> FunctionDeclaration.name with
            | ValueNone -> failtest "expected a name"
            | ValueSome name ->
                let expression: Node<Expression> = Expression.ofNode name

                name
                |> Node.sameAs expression
                |> Flip.Expect.isTrue "widening changes the tag and nothing else"

                expression.Kind |> Flip.Expect.equal "still an identifier" SyntaxKind.Identifier)

        // The claim the whole view design rests on. A `Choice`-returning total active pattern
        // would allocate once per match; these allocate nothing, so a match is a kind read and a
        // two-field copy. Measured rather than asserted, because it is the reason for the shape.
        testCase "matching allocates nothing" <| withMain (fun root ->
            let statement = SourceFile.statements root |> Seq.head
            let mutable matched = 0

            // Warm up, so the measurement is not paying for jitting the loop body.
            for _ in 1..1000 do
                match statement with
                | FunctionDeclaration _ | Block _ | ReturnStatement _ -> matched <- matched + 1
                | _ -> ()

            let before = System.GC.GetAllocatedBytesForCurrentThread()

            for _ in 1..100_000 do
                match statement with
                | FunctionDeclaration _ | Block _ | ReturnStatement _ -> matched <- matched + 1
                | _ -> ()

            System.GC.GetAllocatedBytesForCurrentThread() - before
            |> Flip.Expect.equal "bytes allocated by 100k pattern matches" 0L)

        // A pattern that does not match is a `ValueNone`, not an exception and not a wrong answer.
        testCase "a pattern declines rather than mis-reading" <| withMain (fun root ->
            match SourceFile.statements root |> Seq.head with
            | CallExpression _ -> failtest "the first statement is an interface, not a call"
            | _ -> ())
    ]
