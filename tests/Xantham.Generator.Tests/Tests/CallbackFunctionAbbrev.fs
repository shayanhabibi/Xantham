module Xantham.Generator.Tests.Tests.CallbackFunctionAbbrev

// Isolated characterization of the NAMED-CALLBACK -> FUNCTION-TYPE ABBREVIATION behaviour.
//
// THE BEHAVIOUR (Render.TypeAlias.fs, the pure-single-call-signature TypeLiteral alias arm):
//   A named type alias whose BODY is a `TypeLiteral` with EXACTLY ONE call signature and NOTHING
//   else IS a function type. It must render as an F# function-type ABBREVIATION
//       type ScheduledHandler = ScheduledController -> Env -> option<Promise<unit>>
//   so that an F# CALLBACK LAMBDA satisfies it — NOT as a nominal interface with an
//   `abstract Invoke:` member (a lambda cannot be passed where a nominal interface is demanded;
//   this is the single most usability-breaking output the generator can emit for the canonical
//   Cloudflare handler callbacks, ExportedHandler*Handler / PagesFunction / EventListener / ...).
//
// COUNTER-EXAMPLES that MUST STAY NOMINAL (an F# function-type abbreviation cannot express them):
//   (1) an OVERLOADED interface: TWO+ call signatures (e.g. `setTimeout`) — F# cannot abbreviate
//       to two function types, so it stays `abstract Invoke: <sig1>  abstract Invoke: <sig2>`.
//   (2) a call signature PLUS another member (a property / method) — abbreviating to a function
//       type would DROP the other member, so it stays nominal.
//
// The guard `moleculeCollapsesOntoAlias` additionally keeps a single-call-sig body NOMINAL when it
// embeds a nested anonymous structural type that would collapse onto the alias name as a bare
// abbreviation (the `ExportedHandlerFetchHandler` self-cycle, FS0033) — not exercised here; these
// tests pin the clean conversion + the two nominal counter-examples.

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

// Render a TypeAlias export end-to-end (identical pipeline to AliasBodyLiteralPlacement):
//   prerenderTypeAliases (register the alias-body remap) -> registerAnchorFromExport
//   (prerender + anchor) -> collectModules -> renderRoot.
let private renderTypeAliasSurface (alias: TypeAlias) : string =
    let ctx = GeneratorContext.Empty
    let export = ResolvedExport.TypeAlias alias
    if not (RenderScope_Prelude.ArenaInterner.isShareableAliasBody alias.Type.Value) then
        let path = Path.Interceptors.pipeTypeAlias ctx alias
        let aliasRef =
            RenderScopeStore.TypeRefAtom.Unsafe.createConcretePath path
            |> RenderScopeStore.TypeRef.Unsafe.createAtom
            |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind false
        GeneratorContext.Prelude.addTypeAliasRemap ctx alias.Type.Value aliasRef
    registerAnchorFromExport ctx export
    let root = RootModule.collectModules ctx
    Ast.Oak() {
        Ast.AnonymousModule() {
            renderRoot ctx root
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> _.Trim()

// A body `TypeLiteral` whose ONLY member is a single call signature with the given parameters and
// return type — the canonical callback shape.
let private singleCallSigBody (parameters: Parameter list) (returnType: ResolvedType) =
    TypeLiteral.empty
    |> TypeLiteral.withMembers [
        CallSignature.create returnType
        |> CallSignature.withParameters parameters
        |> List.singleton
        |> CallSignature.wrap
    ]
    |> TypeLiteral.wrap

// type ScheduledHandler = ((controller: ScheduledController), (env: Env)) => option<Promise<unit>>
// A single call signature, no other members → MUST become a function-type abbreviation.
let private scheduledHandlerAlias =
    let body =
        singleCallSigBody
            [ Parameter.create "controller" (Interface.create "ScheduledController" |> Interface.wrap)
              Parameter.create "env" (Interface.create "Env" |> Interface.wrap) ]
            (Interface.create "Outcome" |> Interface.wrap)
    TypeAlias.create body "ScheduledHandler"

// type SetTimeout = { (cb): float; (cb, delay): float }  — TWO overloaded call signatures.
// F# cannot abbreviate to two function types → MUST stay nominal (`abstract Invoke:` ×2).
let private overloadedAlias =
    let body =
        TypeLiteral.empty
        |> TypeLiteral.withMembers [
            Member.CallSignature [
                CallSignature.create (primitive TypeKindPrimitive.Number)
                |> CallSignature.withParameters [ Parameter.create "cb" (Interface.create "Cb" |> Interface.wrap) ]
                CallSignature.create (primitive TypeKindPrimitive.Number)
                |> CallSignature.withParameters
                    [ Parameter.create "cb" (Interface.create "Cb" |> Interface.wrap)
                      Parameter.create "delay" (primitive TypeKindPrimitive.Number) ]
            ]
        ]
        |> TypeLiteral.wrap
    TypeAlias.create body "SetTimeout"

// type Mixed = { (event): unit; readonly tag: string }  — a call signature PLUS a property.
// Abbreviating to a function type would DROP `tag` → MUST stay nominal.
let private callSigPlusPropertyAlias =
    let body =
        TypeLiteral.empty
        |> TypeLiteral.withMembers [
            CallSignature.create (primitive TypeKindPrimitive.Void)
            |> CallSignature.withParameters [ Parameter.create "event" (Interface.create "Event" |> Interface.wrap) ]
            |> List.singleton
            |> CallSignature.wrap
            Property.create "tag" (primitive TypeKindPrimitive.String) |> Property.wrap
        ]
        |> TypeLiteral.wrap
    TypeAlias.create body "Mixed"

[<Tests>]
let tests =
    testList "CallbackFunctionAbbrev" [

        // The single-call-signature alias must abbreviate to an F# function type — a curried arrow
        // chain of the params -> return — and must NOT emit a nominal `abstract Invoke:` interface.
        testCase "named single-call-signature alias renders as a function-type abbreviation" <| fun _ ->
            let out = renderTypeAliasSurface scheduledHandlerAlias
            Expect.isFalse (out.Contains("abstract Invoke"))
                (sprintf "single-call-sig alias must NOT be a nominal Invoke interface; got:\n%s" out)
            let abbrevLine =
                out.Split('\n')
                |> Array.map _.Trim()
                |> Array.tryFind (_.StartsWith("type ScheduledHandler ="))
            match abbrevLine with
            | Some line ->
                // A function-type abbreviation: the RHS is an arrow chain, not an interface body.
                Expect.isTrue (line.Contains("->"))
                    (sprintf "abbreviation RHS must be a function type (contains '->'); got: %s" line)
                Expect.isTrue
                    (line.Contains("ScheduledController") && line.Contains("Env"))
                    (sprintf "params must appear in the function type; got: %s" line)
            | None ->
                failtestf "expected `type ScheduledHandler =` function-type abbreviation; got:\n%s" out

        // An OVERLOADED (two call signatures) interface cannot be a single function-type
        // abbreviation — it MUST stay nominal, emitting the `abstract Invoke:` interface form.
        testCase "overloaded (two call-sig) alias stays a nominal Invoke interface" <| fun _ ->
            let out = renderTypeAliasSurface overloadedAlias
            Expect.isTrue (out.Contains("abstract Invoke"))
                (sprintf "two-call-sig alias must stay nominal (abstract Invoke); got:\n%s" out)
            let abbrevLine =
                out.Split('\n')
                |> Array.map _.Trim()
                |> Array.tryFind (fun l -> l.StartsWith("type SetTimeout =") && l.Contains("->"))
            Expect.isNone abbrevLine
                (sprintf "two-call-sig alias must NOT collapse to a function-type abbreviation; got:\n%s" out)

        // A call signature PLUS another member cannot be a function-type abbreviation (the abbrev
        // would drop the member) — it MUST stay nominal.
        testCase "call-signature-plus-property alias stays a nominal interface" <| fun _ ->
            let out = renderTypeAliasSurface callSigPlusPropertyAlias
            let abbrevLine =
                out.Split('\n')
                |> Array.map _.Trim()
                |> Array.tryFind (fun l -> l.StartsWith("type Mixed =") && l.Contains("->"))
            Expect.isNone abbrevLine
                (sprintf "call-sig + property alias must NOT collapse to a function-type abbreviation; got:\n%s" out)
            // The non-call-sig member (`tag`) must survive — proof it was not dropped.
            Expect.isTrue (out.Contains("tag"))
                (sprintf "the non-call-signature property `tag` must be preserved; got:\n%s" out)
    ]
