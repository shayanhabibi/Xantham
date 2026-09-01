/// Exercises the generated typed layer against `main.ts`.
///
/// The point of these is less the assertions than the code they are written in: every line below
/// would fail to compile if a tag were wrong, and none of it names a slot, an index or a kind
/// ordinal.
module Xantham.TypeScript.Wire.Tests.Typed

open System.Collections.Generic
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

/// A whole file's worth of nodes, at the widest tag. Index 0 is the absent sentinel, so the
/// nodes themselves run from `Ast.Root` to the end of the node table.
let private allNodes (ast: Ast.SourceFile) =
    [| for index in Ast.Root .. ast.NodeCount - 1 -> Node.ofIndex<AnyNode> ast index |]

/// The generated table is only worth as much as the variety of kinds it is run over, and the
/// six fixture files are between them a narrow slice of the language. The program's own file
/// list is wider: `lib.es5.d.ts` and its siblings bring interfaces, call and construct
/// signatures, index signatures, mapped and conditional types, template literal types and the
/// rest, without vendoring a tree of our own.
///
/// What comes back is one node per kind, across every file: a view's answer depends on the kind
/// and nothing else, so a second node of a kind already seen would only cost time.
let private withRepresentatives (test: Node<AnyNode>[] -> unit) =
    fun () ->
        match exePath with
        | None -> ()
        | Some exe ->
            use channel = new TscChannel(exe, fixtures)
            Api.initialize channel |> ignore

            let snapshot = channel.updateSnapshot(openProjects = [| file "tsconfig.json" |])
            let project = snapshot.Projects[0].Id
            let names = channel.getSourceFileNames(snapshot.Snapshot, project)

            Expect.isGreaterThan names.Length 1 "the program has more than the one fixture file"

            let representatives =
                names
                |> Seq.collect (fun name ->
                    match channel.getSourceFile(snapshot.Snapshot, project, DocumentIdentifier.FileName name) with
                    | ValueNone -> Seq.empty
                    | ValueSome ast -> allNodes ast)
                |> Seq.distinctBy (fun node -> node.Kind)
                |> Array.ofSeq

            test representatives

[<Tests>]
let typedTests =
    testList "typed layer" [
        match exePath with
        | None ->
            testCase "native tsc not found - typed layer tests skipped" <| fun _ ->
                skiptest "run `npm install` at the repository root, or set XANTHAM_TSGO_EXE"
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

        // The generated half of this file. `Typed.table.generated.fs` carries one row per view:
        // the kinds `ast.json` gives the tag, and the view itself reduced to the index it
        // narrowed to. Checking the two against each other on a real tree is what says the
        // emitted predicate - a chain of up to 180 `||`s - actually matches that kind set, and
        // that narrowing hands back the node it was given rather than a neighbour.
        testCase "every view takes exactly the kinds its tag declares" <| withRepresentatives (fun representatives ->
            // A floor, so that a program which stopped covering the language could not make the
            // whole test pass by having nothing left to disagree about. 107 of the 351 kinds turn
            // up in `lib` and the fixtures; the rest need JSX, JSDoc or a syntax error to appear.
            Expect.isGreaterThan representatives.Length 100 "distinct kinds walked"

            let failures = ResizeArray()

            for name, kinds, view in TypedTable.views do
                let declared = HashSet kinds

                for node in representatives do
                    match view node with
                    | ValueSome index when not (declared.Contains node.Kind) ->
                        failures.Add $"%s{name} accepted %A{node.Kind}, which is not one of its kinds"
                    | ValueSome index when index <> Node.index node ->
                        failures.Add $"%s{name} narrowed node %d{Node.index node} to %d{index}"
                    | ValueNone when declared.Contains node.Kind ->
                        failures.Add $"%s{name} declined %A{node.Kind}, which is one of its kinds"
                    | _ -> ()

            // Joined rather than asserted empty, so a failure names the tag and the kind rather
            // than only saying that something disagreed.
            failures
            |> String.concat "
"
            |> Flip.Expect.equal "views disagreeing with their kind sets" "")

        // The invariant the tag hierarchy exists to encode: `'Tag :> Expression` is the
        // compile-time form of `AstKind.isExpression`. The two sides come from different parts of
        // the generator - the `inherit` lines from the tag emitter, the kind sets from the view
        // and guard emitters - so agreeing is a result rather than a restatement.
        //
        // Only aliases and `AnyNode` are ever supertags, and where two aliases have identical
        // kind sets the first-declared wins, which is why declaration order is in the table.
        testCase "tag inheritance is exactly kind-set inclusion" <| fun _ ->
            let assembly = typeof<AnyNode>.Assembly

            let tags =
                TypedTable.tags
                |> Array.map (fun (name, sort, order, kinds) ->
                    let tag = assembly.GetType $"Xantham.TypeScript.Wire.{name}"
                    if isNull tag then failtest $"no tag type for %s{name}"
                    name, sort, order, HashSet kinds, tag)

            let failures = ResizeArray()

            for subName, _, subOrder, subKinds, subType in tags do
                for superName, superSort, superOrder, superKinds, superType in tags do
                    if subName <> superName && (superSort = "alias" || superSort = "any") then
                        let expected =
                            subKinds.IsSubsetOf superKinds
                            && (subKinds.Count <> superKinds.Count || superOrder < subOrder)

                        if superType.IsAssignableFrom subType <> expected then
                            failures.Add
                                (if expected then $"%s{subName} should widen to %s{superName} but does not"
                                 else $"%s{subName} widens to %s{superName} but should not")

            failures
            |> Seq.truncate 10
            |> String.concat "
"
            |> Flip.Expect.equal $"%d{failures.Count} inheritances disagreeing with kind sets" ""

        // Widening is a function per alias rather than one generic one, so "every alias has an
        // `ofNode`" is a claim that can quietly stop being true. `Widenings` is auto-opened and
        // each module carries F#'s `Module` suffix, since it shares its name with the tag.
        testCase "every alias tag has a widening function" <| fun _ ->
            let assembly = typeof<AnyNode>.Assembly

            let missing =
                [ for name, sort, _, _ in TypedTable.tags do
                    if sort = "alias" then
                        let widening = assembly.GetType $"Xantham.TypeScript.Wire.Widenings+{name}Module"

                        if isNull widening || isNull (widening.GetMethod "ofNode") then
                            yield name ]

            missing |> Flip.Expect.isEmpty "aliases with no ofNode"

        // A pattern that does not match is a `ValueNone`, not an exception and not a wrong answer.
        testCase "a pattern declines rather than mis-reading" <| withMain (fun root ->
            match SourceFile.statements root |> Seq.head with
            | CallExpression _ -> failtest "the first statement is an interface, not a call"
            | _ -> ())
    ]
