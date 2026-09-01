module Xantham.TypeScript.Wire.Tests.Live

open System.IO
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Measures
open Xantham.TypeScript.Wire.Proto

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")

/// The compiler comes from the `typescript` npm package declared beside this file; run
/// `npm install` here to enable these tests. It is not vendored, so they are skipped rather
/// than failed when it is absent.
let private exePath = Tsc.locate __SOURCE_DIRECTORY__

let private file name = DocumentIdentifier.FileName(Path.Combine(fixtures, name))

/// A fresh session per test, for the same reason as the Xantham.TsGo suite: snapshot state
/// from a failing test would otherwise leak into its successors.
let private withSession (test: TscChannel -> UpdateSnapshotResponse -> string -> unit) =
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

            test channel snapshot snapshot.Projects[0].Id

let private unicodeText = File.ReadAllText(Path.Combine(fixtures, "unicode.ts"))

[<Tests>]
let liveTests =
    testList "wire api" [
        match exePath with
        | None ->
            testCase "native tsc not found - live tests skipped" <| fun _ ->
                skiptest "run `npm install` in tests/Xantham.TypeScript.Wire.Tests, or set XANTHAM_TSGO_EXE"
        | Some _ ->

        // The no-parameter request shape: the server rejects an empty payload, so this fails
        // unless `requestNoParams` sends the four bytes of a literal null.
        testCase "initialize takes no parameters" <| withSession (fun channel _ _ ->
            let response = Api.initialize channel
            response.CurrentDirectory
            |> Flip.Expect.stringContains "the cwd the channel was started in" "fixtures")

        testCase "a project loads and yields a snapshot" <| withSession (fun _ snapshot project ->
            snapshot.Snapshot > 0 |> Flip.Expect.isTrue $"snapshot id, got {snapshot.Snapshot}"
            project |> Flip.Expect.stringContains "project is the tsconfig" "tsconfig.json")

        // A ValueSome result, with the optional fields of the response populated.
        testCase "a symbol query returns a typed response" <| withSession (fun channel snapshot project ->
            let position = unicodeText.IndexOf "yy"

            match Api.getSymbolAtPosition channel
                      { Snapshot = snapshot.Snapshot; Project = project; File = file "unicode.ts"; Position = position } with
            | ValueNone -> failtest "expected a symbol at the UTF-16 offset"
            | ValueSome symbol ->
                symbol.Name |> Flip.Expect.equal "name" "yy"
                symbol.ValueDeclaration
                |> ValueOption.isSome
                |> Flip.Expect.isTrue "an optional field the server did populate"
                symbol.Parent |> Flip.Expect.equal "an optional field it did not" ValueNone)

        // The other half of the voption contract: nothing at this offset, and nothing is not
        // an error.
        testCase "an empty result is ValueNone, not an exception" <| withSession (fun channel snapshot project ->
            Api.getSymbolAtPosition channel
                { Snapshot = snapshot.Snapshot; Project = project; File = file "unicode.ts"; Position = 0 }
            |> ValueOption.isNone
            |> Flip.Expect.isTrue "no symbol at position 0")

        // An array-of-records result, and an optional *request* field that is present.
        testCase "diagnostics come back as typed records" <| withSession (fun channel snapshot project ->
            match Api.getSemanticDiagnostics channel
                      { Snapshot = snapshot.Snapshot; Project = project; Files = ValueSome [| file "main.ts" |] } with
            | ValueNone -> failtest "expected diagnostics"
            | ValueSome diagnostics ->
                // TS2322: type is not assignable. The code is a stabler assertion than the prose.
                [ for d in diagnostics -> d.Code ] |> Flip.Expect.contains "expected TS2322" 2322

                [ for d in diagnostics -> d.Text ]
                |> List.exists (fun text -> text.Contains "not assignable")
                |> Flip.Expect.isTrue "expected an assignability message")

        // The schema types this as a base64 blob in a JSON envelope; the transport actually
        // returns the AST bytes raw. This is the assertion that keeps `requestAst` honest.
        testCase "getSourceFile returns a decoded AST, not the schema's envelope" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->
                Ast.kind ast Ast.Root |> Flip.Expect.equal "root node kind" SyntaxKind.SourceFile
                ast.NodeCount > 20
                |> Flip.Expect.isTrue $"the whole file arrives in one blob, got {ast.NodeCount} nodes")

        // The generated slot numbers are positions in a bitmap that nothing in the blob labels,
        // so the only real check is to read a node whose shape we already know. main.ts has
        // exactly one function declaration: `distance(a: Point, b: Point): number`.
        testCase "named child slots read the node the schema says they do" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            let text node =
                match Ast.data ast node with
                | Ast.StringIndex index -> Ast.getString ast index
                | other -> failtest $"node %d{node} carries %A{other}, not a string"

            let declaration =
                Ast.descendants ast Ast.Root
                |> Seq.tryFind (AstNode.FunctionDeclaration.is ast)
                |> function
                    | Some node -> node
                    | None -> failtest "main.ts declares a function"

            AstNode.FunctionDeclaration.name ast declaration
            |> ValueOption.map text
            |> Flip.Expect.equal "the name slot holds the function's name" (ValueSome "distance")

            AstNode.FunctionDeclaration.parameters ast declaration
            |> Seq.map (fun parameter ->
                AstNode.ParameterDeclaration.name ast parameter |> ValueOption.map text)
            |> List.ofSeq
            |> Flip.Expect.equal "the parameter list slot holds both parameters"
                [ ValueSome "a"; ValueSome "b" ]

            // `type` and `body` are slots 5 and 6, past the `FullSignature` member that the
            // encoder skips - they are exactly what a naive slot numbering would get wrong.
            AstNode.FunctionDeclaration.``type`` ast declaration
            |> ValueOption.map (Ast.kind ast)
            |> Flip.Expect.equal "the type slot holds the return type" (ValueSome SyntaxKind.NumberKeyword)

            AstNode.FunctionDeclaration.body ast declaration
            |> ValueOption.map (Ast.kind ast)
            |> Flip.Expect.equal "the body slot holds the block" (ValueSome SyntaxKind.Block)

            // An absent optional slot reads as absent rather than as its neighbour.
            AstNode.FunctionDeclaration.asteriskToken ast declaration
            |> Flip.Expect.equal "distance is not a generator" ValueNone

            Slot.names SyntaxKind.FunctionDeclaration
            |> Flip.Expect.equal "the slot name table matches the encoder's"
                [| "modifiers"; "asteriskToken"; "name"; "typeParameters"; "parameters"; "type"; "body" |])

        // `SourceFile` spends its data word on an extended-data offset, so it has no slot bitmap.
        // Its children are still in the blob and every declared slot of it is present, which the
        // rest of the suite never noticed because it reaches children through `Ast.children` and
        // parent links - neither of which reads the mask.
        testCase "slots of a node whose data word is not a child mask" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            AstNode.SourceFile.statements ast Ast.Root
            |> Seq.map (Ast.kind ast)
            |> List.ofSeq
            |> Flip.Expect.equal "the statements slot holds main.ts's statements"
                (Ast.children ast Ast.Root
                 |> Seq.head
                 |> Ast.children ast
                 |> Seq.map (Ast.kind ast)
                 |> List.ofSeq)

            AstNode.SourceFile.endOfFileToken ast Ast.Root
            |> ValueOption.map (Ast.kind ast)
            |> Flip.Expect.equal "and the slot after it holds the EOF token" (ValueSome SyntaxKind.EndOfFile)

            // The guard the all-ones fallback needs: a leaf spends its word on a string index and
            // has no children at all, so every slot of it is absent rather than the next node.
            let identifier =
                Ast.descendants ast Ast.Root
                |> Seq.find (fun node -> Ast.kind ast node = SyntaxKind.Identifier)

            Ast.childAtOrder ast identifier 0<astSlot>
            |> Flip.Expect.equal "a childless leaf reports no child, not its neighbour" ValueNone)

        // The data word carries more than children: six commonData bits, and for literals an
        // offset into a separate extended-data record. data.ts is written so that each of those
        // has exactly one unambiguous answer.
        testCase "the data word decodes to literal text, flags and commonData bits" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "data.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            let ofKind kind =
                Ast.descendants ast Ast.Root
                |> Seq.filter (fun node -> Ast.kind ast node = kind)
                |> List.ofSeq

            let only kind =
                match ofKind kind with
                | [ node ] -> node
                | found -> failtest $"data.ts should hold one %A{kind}, found %d{found.Length}"

            // Extended data: text is unreachable without decoding the record the data word
            // points at, which is the whole reason this section exists.
            ofKind SyntaxKind.StringLiteral
            |> List.map (Ast.text ast)
            |> Flip.Expect.equal "every string literal's text"
                [ ValueSome "./main.js"; ValueSome "world"; ValueSome "quoted" ]

            // Numeric text is the cooked value, not the source spelling: `0x2a` is stored as
            // `42`, which is what the scanner wrote and what tokenFlags below is there to
            // recover the base from.
            Ast.text ast (only SyntaxKind.NumericLiteral)
            |> Flip.Expect.equal "the numeric literal's cooked text" (ValueSome "42")

            // The base the cooked text lost. Named rather than asserted as "nonzero", now that
            // `TokenFlags` is generated from the vendored enums.
            Ast.tokenFlags ast (only SyntaxKind.NumericLiteral)
            |> Flip.Expect.equal "the hex literal records its specifier" (ValueSome TokenFlags.HexSpecifier)

            ofKind SyntaxKind.StringLiteral
            |> List.map (Ast.tokenFlags ast)
            |> Flip.Expect.equal "ordinary string literals record none"
                [ ValueSome TokenFlags.None; ValueSome TokenFlags.None; ValueSome TokenFlags.None ]

            let head = only SyntaxKind.TemplateHead
            Ast.text ast head |> Flip.Expect.equal "the template head's cooked text" (ValueSome "hello ")
            Ast.rawText ast head |> Flip.Expect.equal "and its raw text" (ValueSome "hello ")
            Ast.templateFlags ast head
            |> ValueOption.isSome
            |> Flip.Expect.isTrue "template flags are present"

            Ast.rawText ast (only SyntaxKind.NumericLiteral)
            |> Flip.Expect.equal "raw text is template-only" ValueNone

            // commonData bits.
            ofKind SyntaxKind.ObjectLiteralExpression
            |> List.map (AstNode.ObjectLiteralExpression.multiLine ast)
            |> Flip.Expect.equal "multiLine is a commonData bit, not a guess" [ true; false ]

            AstNode.ImportClause.phaseModifier ast (only SyntaxKind.ImportClause)
            |> Flip.Expect.equal "an optional union: absent is 0, so type is 1"
                (ValueSome SyntaxKind.TypeKeyword)

            AstNode.PrefixUnaryExpression.operator ast (only SyntaxKind.PrefixUnaryExpression)
            |> Flip.Expect.equal "a non-optional union: the operator is index-encoded"
                (ValueSome SyntaxKind.MinusToken))

        // The `SourceFile` node's extended-data record: nineteen words of file-level metadata,
        // eight of them offsets into the msgpack structured-data section. sourcefile.ts carries
        // one of each thing an ordinary file can put there.
        testCase "the SourceFile record decodes to file-level metadata" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "sourcefile.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            let source = Ast.sourceText ast

            source
            |> Flip.Expect.equal "the record's text is the file, byte for byte"
                (File.ReadAllText(Path.Combine(fixtures, "sourcefile.ts")))

            Ast.originalText ast
            |> Flip.Expect.equal "and equals the original text, this file being no transformation" source

            Ast.fileName ast
            |> Flip.Expect.stringEnds "the file name the compiler knows it by" "fixtures/sourcefile.ts"

            // The path is the canonicalised form, which on Windows means case-folded.
            Ast.path ast
            |> Flip.Expect.stringEnds "the canonicalised path" "fixtures/sourcefile.ts"

            Ast.scriptKind ast |> Flip.Expect.equal "scriptKind is TS" ScriptKind.TS
            Ast.languageVariant ast |> Flip.Expect.equal "languageVariant is Standard" LanguageVariant.Standard

            // A node index rather than an offset or a string, so it resolves against the blob.
            Ast.externalModuleIndicator ast
            |> ValueOption.map (Ast.kind ast)
            |> Flip.Expect.equal "the import is what makes this file a module"
                (ValueSome SyntaxKind.ImportDeclaration)

            // Structured data: msgpack tuples, whose positions the writer has already converted
            // to UTF-16, so they index into the text above.
            let referencePosition = source.IndexOf "./main.ts"

            Ast.referencedFiles ast
            |> Flip.Expect.equal "the path reference, positioned at its own text"
                [| { Pos = referencePosition
                     End = referencePosition + "./main.ts".Length
                     FileName = "./main.ts"
                     ResolutionMode = 0u
                     Preserve = false } |]

            Ast.typeReferenceDirectives ast
            |> Array.map _.FileName
            |> Flip.Expect.equal "the types reference" [| "node" |]

            Ast.libReferenceDirectives ast
            |> Flip.Expect.isEmpty "no lib references, and absent decodes as empty"

            // Node index arrays, resolved back to nodes.
            Ast.imports ast
            |> Array.map (Ast.text ast)
            |> Flip.Expect.equal "the module specifiers this file imports" [| ValueSome "./main.js" |]

            Ast.moduleAugmentations ast
            |> Array.map (Ast.text ast)
            |> Flip.Expect.equal "both `declare module` specifiers"
                [| ValueSome "./main.js"; ValueSome "ambient-only" |]

            // The rest of the record belongs to virtual files, which `tsc` over a fixture never
            // produces. Asserting them absent is the most these can be held to, and it does pin
            // that the offsets are read rather than mistaken for data.
            Ast.spanMap ast |> Flip.Expect.isEmpty "no span map"
            Ast.supplementalSourceFileNames ast |> Flip.Expect.isEmpty "no supplemental files"
            Ast.canonicalSourceFileName ast |> Flip.Expect.equal "no canonical file" ValueNone
            Ast.contentMapper ast |> Flip.Expect.equal "no content mapper" ValueNone
            Ast.virtualFileName ast |> Flip.Expect.equal "no virtual file name" ValueNone

            // Despite the `@ts-expect-error` in the fixture: these are a virtual file's mapped
            // directives, not the suppression comments of an ordinary one.
            Ast.diagnosticDirectives ast |> Flip.Expect.isEmpty "no mapped diagnostic directives")

        // `ambientModuleNames` needs a file that is not itself a module, so its `declare module`
        // is an ambient declaration rather than an augmentation of something else.
        testCase "a declaration file's ambient modules" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "ambient.d.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            Ast.ambientModuleNames ast
            |> Flip.Expect.equal "the ambient module it declares" [| "virtual:greeting" |]

            Ast.moduleAugmentations ast
            |> Flip.Expect.isEmpty "which is a declaration, not an augmentation"

            Ast.externalModuleIndicator ast
            |> Flip.Expect.equal "and the file itself is not a module" ValueNone)

        // The string table is WTF-8, not UTF-8: a lone surrogate is the three bytes ED A0 80,
        // which `Encoding.UTF8.GetString` would replace with U+FFFD. TypeScript permits one in a
        // string literal and stores the cooked value, so this is the only way to see whether
        // `Wtf8.decode` is doing anything - a corrupted read is a valid string, not an error.
        testCase "a lone surrogate survives the string table" <| withSession (fun channel snapshot project ->
            match Api.getSourceFile channel { Snapshot = snapshot.Snapshot; Project = project; File = file "surrogate.ts" } with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->

            let literals =
                Ast.descendants ast Ast.Root
                |> Seq.filter (fun node -> Ast.kind ast node = SyntaxKind.StringLiteral)
                |> Seq.map (fun node -> Ast.text ast node)
                |> List.ofSeq

            // Built from chars, not written as `"\uD800"`: F# lowers a lone-surrogate escape in
            // its own source to U+FFFD, so a literal expectation would agree with a corrupted
            // read and the test would pass for the wrong reason.
            let lone = string (char 0xD800)
            let trailing = "ab" + string (char 0xDC00)

            literals
            |> Flip.Expect.equal "the cooked text of all three literals" [
                ValueSome lone
                ValueSome trailing
                ValueSome "\U0001F600"
            ]

            match literals.Head with
            | ValueSome text ->
                text.Length |> Flip.Expect.equal "one UTF-16 code unit, not a replacement" 1
                int text[0] |> Flip.Expect.equal "the high surrogate itself" 0xD800
            | ValueNone -> failtest "the first literal has text")

        // The synchronous surface comes in the same three layers as the asynchronous one, and
        // each has to reach the same server: the free function, the member taking the parameter
        // record, and the member taking that record's fields.
        testCase "the free function, the record overload and the flattened one agree" <| withSession (fun channel _ _ ->
            let parameters: ParseCommandLineParams = { CommandLine = ValueSome [| "--strict"; "main.ts" |] }

            let viaFunction = Api.parseCommandLine channel parameters
            let viaRecord = channel.parseCommandLine parameters
            let viaFields = channel.parseCommandLine(commandLine = [| "--strict"; "main.ts" |])

            viaRecord.FileNames |> Flip.Expect.equal "record overload" viaFunction.FileNames
            viaFields.FileNames |> Flip.Expect.equal "flattened overload" viaFunction.FileNames

            // An optional argument left out has to end up absent from the payload rather than
            // sent as null: `--strict` alone yields no file names, a null commandLine is an error.
            channel.parseCommandLine().FileNames.Length
            |> Flip.Expect.equal "no command line, no file names" 0)

        // The members delegate rather than re-implement, so the binary methods have to come back
        // as the decoded blob here too - not the schema's JSON envelope.
        testCase "an AST member returns the same tree as the free function" <| withSession (fun channel snapshot project ->
            let viaMember =
                channel.getSourceFile(snapshot = snapshot.Snapshot, project = project, file = file "main.ts")

            match viaMember with
            | ValueNone -> failtest "expected an AST"
            | ValueSome ast ->
                let request: GetSourceFileParams =
                    { Snapshot = snapshot.Snapshot; Project = project; File = file "main.ts" }

                let viaFunction = (Api.getSourceFile channel request).Value
                ast.NodeCount |> Flip.Expect.equal "node count" viaFunction.NodeCount
                Ast.sourceText ast |> Flip.Expect.equal "source text" (Ast.sourceText viaFunction))

        testCase "release frees the snapshot" <| withSession (fun channel snapshot _ ->
            Api.release channel { Snapshot = snapshot.Snapshot })
    ]
