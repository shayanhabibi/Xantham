/// Exercises the six `ContentMapperSourceFileInfo` fields of the SourceFile record.
///
/// These describe a virtual file - one the compiler synthesised out of part of another - and
/// nothing the API server exposes will make it write one: `dist/api/node/node.d.ts` offers a
/// `contentMapper` getter and no method that takes one, and `encoder.go:675-695` writes the six
/// words from a Go `SourceFile` that only the content-mapping path ever populates. So there is
/// no live fixture to be had, and the accessors would otherwise be six pieces of untested
/// decoding.
///
/// What is testable is the decoding itself. A real blob is fetched, checked to carry none of the
/// six, and then rewritten: the structured-data section grows by three hand-written msgpack
/// values and the root's extended record is pointed at them. The bytes below are the encoder's
/// side of the protocol written by hand, which is the whole point - if `spanMap` read its
/// elements in the wrong order, or `Features` were not optional, this is where it would show.
module Xantham.TypeScript.Wire.Tests.VirtualFile

open System
open System.Buffers.Binary
open System.IO
open System.Text
open Expecto
open Xantham.TypeScript.Wire
open Xantham.TypeScript.Wire.Measures
open Xantham.TypeScript.Wire.Proto

let private fixtures = Path.Combine(__SOURCE_DIRECTORY__, "fixtures")
let private exePath = Tsc.locate __SOURCE_DIRECTORY__
let private file name = DocumentIdentifier.FileName(Path.Combine(fixtures, name))

/// A fresh session per test, as in `Live.fs` and for the same reason.
let private withMain (test: Ast.SourceFile -> unit) =
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
            | ValueSome ast -> test ast

// ─────────────────────────────────────────────────────────────────────────────────────────────
// The encoder's side, by hand
//
// Only the three tags `encoder.go` uses for these fields are written: a fixarray header, a
// uint32, and a fixstr. The short forms are enough for values this size, and `Msgpack.Reader`
// accepts the wide ones too - a test that exercised `array16` would be testing the writer below
// rather than the reader.
// ─────────────────────────────────────────────────────────────────────────────────────────────

/// `0xCE` and four big-endian bytes. Always the wide form, so that a reader that mis-sized the
/// value could not accidentally agree.
let private uint32Value (value: uint32) =
    [| 0xCEuy; byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

let private fixstr (value: string) =
    let bytes = Encoding.UTF8.GetBytes value

    if bytes.Length > 31 then
        failtestf "%s is too long for a fixstr" value

    Array.append [| 0xA0uy ||| byte bytes.Length |] bytes

let private fixarray (elements: byte[] list) =
    if List.length elements > 15 then
        failtest "too many elements for a fixarray"

    Array.concat ([| 0x90uy ||| byte (List.length elements) |] :: elements)

/// The span map the patched file will claim: two segments, the second without the optional sixth
/// element, since the compiler omits it for segments it wrote no features for.
let private spanMapBlob =
    fixarray
        [ fixarray
              [ uint32Value 0u
                uint32Value 12u
                uint32Value 40u
                uint32Value 12u
                uint32Value (uint32 SpanMapKind.Atom)
                uint32Value (uint32 (SpanMapFeature.Hover ||| SpanMapFeature.Definition)) ]
          fixarray
              [ uint32Value 12u
                uint32Value 5u
                uint32Value 100u
                uint32Value 5u
                uint32Value (uint32 SpanMapKind.Alias) ] ]

let private directivesBlob =
    fixarray
        [ fixarray
              [ uint32Value 200u
                uint32Value 14u
                uint32Value 3u
                uint32Value 14u
                uint32Value (uint32 DiagnosticDirectivePolicy.Expect)
                uint32Value 2578u ] ]

let private namesBlob = fixarray [ fixstr "one.vue"; fixstr "two.vue" ]

/// <summary>
/// A copy of <paramref name="ast"/> with the six virtual-file fields filled in.
/// </summary>
/// <remarks>
/// The three arrays are appended to the structured-data section, which ends where the node
/// section begins, so only the node section moves and only the header word that locates it needs
/// rewriting. Offsets within structured data are relative to the section, so the ones already in
/// the blob are unaffected.
///
/// The three string fields are string-table indices rather than offsets, and adding to the string
/// table would mean rewriting its offset array as well - so they are pointed at strings the file
/// already contains, taken from three identifiers whose texts differ.
/// </remarks>
let private patch (ast: Ast.SourceFile) =
    let structuredLength = ast.Nodes - ast.StructuredData
    let additions = Array.concat [ spanMapBlob; directivesBlob; namesBlob ]

    let data = Array.zeroCreate<byte> (ast.Data.Length + additions.Length)
    Array.blit ast.Data 0 data 0 ast.Nodes
    Array.blit additions 0 data ast.Nodes additions.Length
    Array.blit ast.Data ast.Nodes data (ast.Nodes + additions.Length) (ast.Data.Length - ast.Nodes)
    // Header word 10: the start of the node section.
    BinaryPrimitives.WriteUInt32LittleEndian(Span(data, 40, 4), uint32 (ast.Nodes + additions.Length))

    let patched = Ast.read data

    let record =
        match Ast.data patched Ast.Root with
        | Ast.Extended record -> int record
        | other -> failtestf "the root node carries %A, not an extended record" other

    let write (field: int<byteOffset>) (value: uint32) =
        BinaryPrimitives.WriteUInt32LittleEndian(Span(data, patched.ExtendedData + record + int field, 4), value)

    write SourceFileRecord.SpanMap (uint32 structuredLength)
    write SourceFileRecord.DiagnosticDirectives (uint32 (structuredLength + spanMapBlob.Length))

    write
        SourceFileRecord.SupplementalSourceFileNames
        (uint32 (structuredLength + spanMapBlob.Length + directivesBlob.Length))

    // Three identifiers with three different texts, so that a field reading its neighbour's word
    // would come back with the wrong name rather than the right one by coincidence.
    let strings =
        [ for index in Ast.Root .. patched.NodeCount - 1 do
            match Ast.data patched index, Ast.text patched index with
            | Ast.StringIndex string, ValueSome text -> yield int string, text
            | _ -> () ]
        |> List.distinctBy snd

    match strings with
    | (canonical, canonicalText) :: (mapper, mapperText) :: (virtualName, virtualText) :: _ ->
        write SourceFileRecord.CanonicalSourceFileName (uint32 canonical)
        write SourceFileRecord.ContentMapper (uint32 mapper)
        write SourceFileRecord.VirtualFileName (uint32 virtualName)
        patched, canonicalText, mapperText, virtualText
    | _ -> failtest "main.ts should carry at least three distinctly named identifiers"

[<Tests>]
let virtualFileTests =
    testList "virtual files" [
        match exePath with
        | None ->
            testCase "native tsc not found - virtual file tests skipped" <| fun _ ->
                skiptest "run `npm install` in tests/Xantham.TypeScript.Wire.Tests, or set XANTHAM_TSGO_EXE"
        | Some _ ->

        // The claim the accessors are documented with: an ordinary file has none of this, and
        // absent reads as an empty collection rather than as a failure.
        testCase "an ordinary file carries none of the virtual-file fields" <| withMain (fun ast ->
            [ if not (Array.isEmpty (Ast.spanMap ast)) then "spanMap"
              if not (Array.isEmpty (Ast.diagnosticDirectives ast)) then "diagnosticDirectives"
              if not (Array.isEmpty (Ast.supplementalSourceFileNames ast)) then "supplementalSourceFileNames"
              if ValueOption.isSome (Ast.canonicalSourceFileName ast) then "canonicalSourceFileName"
              if ValueOption.isSome (Ast.contentMapper ast) then "contentMapper"
              if ValueOption.isSome (Ast.virtualFileName ast) then "virtualFileName" ]
            |> String.concat ", "
            |> Flip.Expect.equal "fields main.ts should not have" "")

        // Patching must leave everything else where it was, or the rest of this list is reading a
        // blob that no longer agrees with itself.
        testCase "patching moves the node section without disturbing it" <| withMain (fun ast ->
            let patched, _, _, _ = patch ast

            Expect.equal patched.NodeCount ast.NodeCount "node count"
            Expect.equal (Ast.fileName patched) (Ast.fileName ast) "file name"
            Expect.equal (Ast.sourceText patched) (Ast.sourceText ast) "source text"
            Expect.equal (Ast.contentHash patched) (Ast.contentHash ast) "content hash"

            [ for index in Ast.Root .. patched.NodeCount - 1 ->
                Ast.kind patched index, Ast.pos patched index, Ast.endPos patched index ]
            |> Flip.Expect.equal
                "every node"
                [ for index in Ast.Root .. ast.NodeCount - 1 ->
                    Ast.kind ast index, Ast.pos ast index, Ast.endPos ast index ])

        // Six uint32s in the order `encoder.go` writes them - the assertion is that none of them
        // is read out of place, so no two of the values above are equal.
        testCase "spanMap reads its segments field by field" <| withMain (fun ast ->
            let patched, _, _, _ = patch ast

            Ast.spanMap patched
            |> Flip.Expect.equal
                "the patched span map"
                [| { VirtualStart = 0
                     VirtualLength = 12
                     OriginalStart = 40
                     OriginalLength = 12
                     Kind = SpanMapKind.Atom
                     Features = ValueSome(SpanMapFeature.Hover ||| SpanMapFeature.Definition) }
                   { VirtualStart = 12
                     VirtualLength = 5
                     OriginalStart = 100
                     OriginalLength = 5
                     Kind = SpanMapKind.Alias
                     Features = ValueNone } |])

        // The `Features` element is optional and last, so the array header's length is the only
        // thing that says whether it is there. A reader that assumed six would run into the next
        // value in the section. `ValueNone` is the wire fact and not the meaning: the reference
        // client reads a five-element segment as `SpanMapFeature.All`.
        testCase "a segment written without features reads as ValueNone" <| withMain (fun ast ->
            let patched, _, _, _ = patch ast

            let segments = Ast.spanMap patched

            segments[1].Features
            |> Flip.Expect.equal "the second segment's features" ValueNone)

        testCase "diagnosticDirectives reads its directives field by field" <| withMain (fun ast ->
            let patched, _, _, _ = patch ast

            Ast.diagnosticDirectives patched
            |> Flip.Expect.equal
                "the patched directives"
                [| { OriginalStart = 200
                     OriginalLength = 14
                     VirtualStart = 3
                     VirtualLength = 14
                     Policy = DiagnosticDirectivePolicy.Expect
                     UnusedCode = 2578u } |])

        testCase "supplementalSourceFileNames reads a string array" <| withMain (fun ast ->
            let patched, _, _, _ = patch ast

            Ast.supplementalSourceFileNames patched
            |> Flip.Expect.equal "the patched names" [| "one.vue"; "two.vue" |])

        // The three string fields are one word each and adjacent in the record, so reading the
        // wrong offset is the failure to catch here.
        testCase "the three string fields read their own words" <| withMain (fun ast ->
            let patched, canonical, mapper, virtualName = patch ast

            [ Ast.canonicalSourceFileName patched
              Ast.contentMapper patched
              Ast.virtualFileName patched ]
            |> Flip.Expect.equal
                "canonical name, content mapper, virtual name"
                [ ValueSome canonical; ValueSome mapper; ValueSome virtualName ])
    ]
