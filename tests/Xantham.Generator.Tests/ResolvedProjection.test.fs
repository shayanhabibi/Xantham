module Xantham.Generator.Tests.ResolvedProjectionTests

open System
open System.IO
open Expecto
open Xantham.Generator
open Xantham.Generator.Customization
open Xantham.Generator.Measure
open Xantham.TypeScript.Wire.Proto

let private package = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "../fixtures/projection-lab"))
let private config = { GeneratorConfig.Default with Lib = Some ["es5"]; Types = Some [] }

let private resolve path =
    async {
        let! mailbox, ctx = Bootstrap.start config path
        let! harvest, _ = Pipeline.runTier ctx Harvest.passes HarvestModel.Empty
        let! model, _ = Pipeline.runTier ctx Resolve.passes (Pipeline.toResolve harvest)
        return mailbox :> IDisposable, ctx, model
    } |> Async.RunSynchronously

let private snapshot ctx model = Semantics.projectResolved ctx model |> Async.RunSynchronously
let private select owner name model = Resolved.tryFind owner [name] model |> Option.defaultWith (fun () -> failtestf "Missing %s/%s" owner name)
let private fixtureSource name model = select "projection-lab" name model

let private hasCode code result =
    match result with
    | Error errors -> errors |> List.exists (fun diagnostic -> Diagnostic.code diagnostic = code)
    | Ok _ -> false

let private create source model = ProjectionCompanion.create source "Choice.fs" ["Projection.Choice"] "module Projection" model
let private fingerprint plan = let _, hash, _, _, _ = ContractData.projectionCompanionInfo plan in hash

let private choiceArms =
    [ ResolvedUnionArm.StringLiteral "auto"; ResolvedUnionArm.StringLiteral "manual"
      ResolvedUnionArm.Number; ResolvedUnionArm.Null; ResolvedUnionArm.Undefined ]

[<Tests>]
let tests =
    testList "projection resolved" [
        testCase "facts precede shaping and preserve both absence values" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let source = fixtureSource "Choice" model
            Expect.equal (Resolved.union source model) (Ok choiceArms) "original literals and both absence arms"
            Expect.equal (Resolved.package source model, Resolved.path source model) ("projection-lab", ["Choice"]) "source selection is package qualified"

        testCase "equal and overlapping named aliases retain independent declaration identity" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let a, b, c = fixtureSource "EqualA" model, fixtureSource "EqualB" model, fixtureSource "OverlapC" model
            Expect.equal (Resolved.union a model) (Resolved.union b model) "equal runtime sets"
            Expect.notEqual (Resolved.identity a model) (Resolved.identity b model) "equal sets do not merge source declarations"
            Expect.notEqual (Resolved.union a model) (Resolved.union c model) "overlap does not merge value sets"
            let next = snapshot ctx resolve
            let nextA = fixtureSource "EqualA" next
            Expect.equal (Resolved.identity a model) (Resolved.identity nextA next) "identity does not contain a session nonce"
            Expect.equal (fingerprint (create a model)) (fingerprint (create nextA next)) "fact fingerprint is deterministic"

        testCase "arbitrary string literal values reach the extension unchanged" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let model = snapshot ctx resolve
            let arms = Resolved.union (fixtureSource "Weird" model) model |> Result.defaultWith (fun _ -> failtest "supported weird union")
            for value in [""; "Number"; "Null"; "Undefined"; "auto-mode"; "auto_mode"; "quote\"and\\slash"] do
                Expect.contains arms (ResolvedUnionArm.StringLiteral value) "case naming is not a semantic transformation"

        testCase "unsupported selected unions diagnose without hiding valid neighbors" <| fun _ ->
            use scratch = Scratch.directory "projection-unsupported"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-controls\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export type Good = 'auto' | 'manual'; export type Alias = Good; export type Boolean = 'auto' | boolean; export type Generic<T> = 'auto' | T; export type NumericLiteral = 'auto' | 7; export type BroadString = 'auto' | string; export type ObjectArm = 'auto' | { x: string }; export type VoidArm = 'auto' | void;")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolve
            let source name = select "projection-controls" name model
            let good, alias = source "Good", source "Alias"
            Expect.isOk (Resolved.union good model) "unsupported unrelated exports do not block selection"
            Expect.equal (Resolved.union good model) (Resolved.union alias model) "alias has the same resolved values"
            Expect.notEqual (Resolved.identity good model) (Resolved.identity alias model) "named alias remains its own declaration"
            for name in ["Boolean"; "Generic"; "NumericLiteral"; "BroadString"; "ObjectArm"; "VoidArm"] do
                let selected = source name
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/unsupported-union") name
                Expect.throws (fun () -> create selected model |> ignore) "unsupported input cannot become an authenticated plan"
            Expect.isNonEmpty (Resolved.diagnostics model) "unsupported declarations remain visible as diagnostics"

        testCase "phantom generic declarations reject without poisoning a shared literal type" <| fun _ ->
            use scratch = Scratch.directory "projection-phantom-generic"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-phantom\",\"types\":\"index.d.ts\"}")
            File.WriteAllText(Path.Combine(scratch.Path, "index.d.ts"), "export type Phantom<T> = 'auto'; export type PhantomUnion<T> = 'auto' | 'manual'; export type Plain = 'auto';")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolve
            let source name = select "projection-phantom" name model
            let declared name =
                let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = name)
                resolve.ExportTypes[exported.Symbol.SymbolId].Declared |> Option.get
            Expect.equal (declared "Phantom") (declared "Plain") "control shares the normalized checker type"
            Expect.equal (Resolved.union (source "Plain") model) (Ok [ResolvedUnionArm.StringLiteral "auto"]) "nongeneric declaration remains supported"
            for name in ["Phantom"; "PhantomUnion"] do
                let selected = source name
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/unsupported-union") "genericity belongs to the declaration"
                Expect.throws (fun () -> create selected model |> ignore) "phantom parameters cannot escape the bounded contract"

        testCase "missing and deliberately unresolved arms reject the entire selected union" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = "Choice")
            let id = resolve.ExportTypes[exported.Symbol.SymbolId].Declared |> Option.get
            let arm = resolve.Types[id].UnionMembers.Head
            let controls =
                [ { resolve with Types = Map.remove arm resolve.Types }
                  { resolve with NotFollowed = Map.add arm "controlled unresolved constituent" resolve.NotFollowed }
                  { resolve with Types = Map.add id { resolve.Types[id] with UnionMembers = [] } resolve.Types }
                  { resolve with Types = Map.add id { resolve.Types[id] with UnionMembers = [id] } resolve.Types } ]
            for control in controls do
                let model = snapshot ctx control
                let selected = fixtureSource "Choice" model
                Expect.isTrue (Resolved.union selected model |> hasCode "projection/incomplete-union") "no incomplete arm set is accepted"
                Expect.throws (fun () -> create selected model |> ignore) "no partial companion plan"

        testCase "missing source metadata remains selectable and diagnoses" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let exported = resolve.Harvest.Exports |> List.find (fun item -> item.ExportName = "Choice")
            let exported = { exported with Symbol = { exported.Symbol with Declarations = ValueNone } }
            let model = snapshot ctx { resolve with Harvest = { resolve.Harvest with Exports = [exported] } }
            let selected = fixtureSource "Choice" model
            Expect.isTrue (Resolved.union selected model |> hasCode "projection/missing-source-metadata") "source is not silently omitted"
            Expect.throws (fun () -> create selected model |> ignore) "missing identity cannot authenticate a plan"

        testCase "source tokens and cached plans are sealed to the current snapshot" <| fun _ ->
            let server, ctx, resolve = resolve package
            use _ = server
            let first, second = snapshot ctx resolve, snapshot ctx resolve
            let selected = fixtureSource "Choice" first
            let plan = create selected first
            Expect.isTrue (ContractData.projectionCompanionIsCurrent first plan) "own plan is valid"
            Expect.isFalse (ContractData.projectionCompanionIsCurrent second plan) "cached plan is rejected even when values match"
            Expect.isTrue (Resolved.union selected second |> hasCode "projection/stale-source") "foreign token returns a diagnostic"
            Expect.throws (fun () -> create selected second |> ignore) "factory rejects a foreign token"

        testCase "source authentication changes even when union values do not" <| fun _ ->
            use scratch = Scratch.directory "projection-source-hash"
            File.WriteAllText(Path.Combine(scratch.Path, "package.json"), "{\"name\":\"projection-hash\",\"types\":\"index.d.ts\"}")
            let file = Path.Combine(scratch.Path, "index.d.ts")
            File.WriteAllText(file, "export type Choice = 'auto' | 'manual';")
            let server, ctx, resolve = resolve scratch.Path
            use _ = server
            let first = snapshot ctx resolve
            let firstSource = select "projection-hash" "Choice" first
            File.AppendAllText(file, "\n// changed source content\n")
            let second = snapshot ctx resolve
            let secondSource = select "projection-hash" "Choice" second
            Expect.equal (Resolved.union firstSource first) (Resolved.union secondSource second) "control retains the same facts"
            Expect.notEqual (fingerprint (create firstSource first)) (fingerprint (create secondSource second)) "content is part of authentication"

        testCase "unannotated existing extension records still infer GeneratorExtension" <| fun _ ->
            let existing =
                { Identity = { Id = "source-compatible"; Version = "1"; Configuration = Map.empty }
                  Transform = fun _ -> Ok Edits.empty }
            let requiresExisting (_: GeneratorExtension) = ()
            requiresExisting existing
    ]

let private operationPackage (path: string) (source: string) =
    File.WriteAllText(Path.Combine(path, "package.json"), "{\"name\":\"projection-operations\",\"types\":\"index.d.ts\"}")
    File.WriteAllText(Path.Combine(path, "index.d.ts"), source)

let private inputModel = """
export interface TextContent { type: 'text'; text: string; textSignature?: string; '__proto__'?: string; }
export interface ImageContent { type: 'image'; data: string; mimeType: string; }
export type UserInput = string | (TextContent | ImageContent)[];
export interface Options { whenBusy?: 'followUp' | 'steer'; session?: string; }
export interface Harness {
  submit(input: UserInput, options?: Options): Promise<void>;
  configure(change: { level?: 'low' | 'high' | null; other?: string }, context: string): void;
  equal(input: UserInput): void;
}
"""

let private operationSource name parameter model =
    Resolved.tryFindParameter "projection-operations" ["Harness"] name parameter model |> Option.defaultWith (fun () -> failtest "missing operation")

let private operationPlan source model =
    ProjectionCompanion.forOperation source "Raw.Harness" "Operation.fs" ["Projected.Value"] "module Projected" model

[<Tests>]
let operationTests =
    testList "projection resolved operations" [
        testCase "declared double-underscore methods and fields authenticate without checker escapes" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-declared-names"
            operationPackage scratch.Path """
export interface Harness {
  plain(change: { ordinary?: string; '__proto__'?: string }): void;
  '__method'(change: { ordinary?: string }): void;
}
"""
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            for methodName, field in ["plain", "__proto__"; "__method", "ordinary"] do
                let source = Resolved.tryFindParameterField "projection-operations" ["Harness"] methodName "change" field model |> Option.get
                Expect.equal (Resolved.shape source model) (Ok(ResolvedValueShape.Union [ResolvedValueShape.String; ResolvedValueShape.Undefined])) "declared keys select the original value shape"
                let operation = Resolved.operation source model |> Result.defaultWith (fun errors -> failtestf "%A" errors)
                Expect.equal (operation.MethodName, operation.FieldName) (methodName, Some field) "JavaScript call metadata retains declaration spelling"
                operationPlan source model |> ignore
            let whole = operationSource "__method" "change" model
            Expect.isOk (Resolved.shape whole model) "whole-parameter lookup also uses declaration spelling"
            operationPlan whole model |> ignore
            for methodName, field in ["plain", "___proto__"; "___method", "ordinary"] do
                let source = Resolved.tryFindParameterField "projection-operations" ["Harness"] methodName "change" field model |> Option.get
                Expect.isTrue (Resolved.shape source model |> hasCode "projection/unsupported-operation") "checker escapes cannot select a different JavaScript key"

        testCase "input arrays preserve tagged records and optional properties before Shape" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-shape"
            operationPackage scratch.Path inputModel
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            let source = operationSource "submit" "input" model
            let shape = Resolved.shape source model |> Result.defaultWith (fun errors -> failtestf "%A" errors)
            let records =
                match shape with
                | ResolvedValueShape.Union arms ->
                    Expect.contains arms ResolvedValueShape.String "plain text remains accepted"
                    arms |> List.pick (function ResolvedValueShape.Array(ResolvedValueShape.Union records) -> Some records | _ -> None)
                | _ -> failtest "expected text or content array"
            Expect.equal records.Length 2 "text and image remain distinct arms"
            for tag, required in ["text", ["text"]; "image", ["data"; "mimeType"]] do
                let fields = records |> List.pick (function ResolvedValueShape.Record fields when fields |> List.exists (fun field -> field.Name = "type" && field.Shape = ResolvedValueShape.StringLiteral tag) -> Some fields | _ -> None)
                for name in required do
                    Expect.contains fields {Name = name; Optional = false; Shape = ResolvedValueShape.String} "required payload has an exact primitive shape"
                if tag = "text" then
                    Expect.isTrue (fields |> List.exists (fun field -> field.Name = "__proto__")) "declared spelling replaces the compiler's escaped symbol name"
                    Expect.isFalse (fields |> List.exists (fun field -> field.Name = "___proto__")) "compiler escape is not a JavaScript property key"
                    let signature = fields |> List.find (fun field -> field.Name = "textSignature")
                    Expect.isTrue signature.Optional "property presence survives"
                    match signature.Shape with
                    | ResolvedValueShape.Union arms -> Expect.contains arms ResolvedValueShape.Undefined "present undefined remains distinct from absence"
                    | _ -> failtest "optional property must retain undefined"
            let operation = Resolved.operation source model |> Result.defaultWith (fun errors -> failtestf "%A" errors)
            Expect.equal operation.ParameterNames ["input"; "options"] "call argument order"
            Expect.equal operation.ParameterOptional [false; true] "optional argument can be forwarded as an option"
            Expect.equal operation.FieldName None "whole parameter projection"
            Expect.isSome (ContractData.projectionReceiver (operationPlan source model)) "late binding receives a sealed receiver assertion"
            Expect.throws (fun () -> create source model |> ignore) "operation source cannot bypass receiver authentication"

        testCase "field projection supports optional object parameters and tri-state values" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-fields"
            operationPackage scratch.Path inputModel
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            for methodName, parameter, field in ["configure", "change", "level"; "submit", "options", "whenBusy"] do
                let source = Resolved.tryFindParameterField "projection-operations" ["Harness"] methodName parameter field model |> Option.get
                let shape = Resolved.shape source model |> Result.defaultWith (fun errors -> failtestf "%A" errors)
                match shape with
                | ResolvedValueShape.Union arms ->
                    Expect.contains arms ResolvedValueShape.Undefined "optional property can explicitly carry undefined"
                    if field = "level" then Expect.contains arms ResolvedValueShape.Null "null is independent of undefined"
                | _ -> failtest "expected finite union"
                let operation = Resolved.operation source model |> Result.defaultWith (fun errors -> failtestf "%A" errors)
                Expect.equal operation.FieldName (Some field) "field plan is explicit"
                Expect.equal operation.ParameterIndex (if methodName = "submit" then 1 else 0) "selected parameter index"
                operationPlan source model |> ignore

        testCase "compiler array identity survives global augmentation but rejects a local namesake" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-arrays"
            operationPackage scratch.Path """
declare global { interface Array<T> { readonly projectionMarker?: string; } }
export interface Array<T> { item: T; }
export interface Harness {
  mutable(input: string[]): void;
  readonly(input: readonly string[]): void;
  namesake(input: Array<string>): void;
}
"""
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            for name in ["mutable"; "readonly"] do
                let source = operationSource name "input" model
                Expect.equal (Resolved.shape source model) (Ok(ResolvedValueShape.Array ResolvedValueShape.String)) "compiler array predicate governs the representation"
                operationPlan source model |> ignore
            let namesake = operationSource "namesake" "input" model
            Expect.isError (Resolved.shape namesake model) "a generic interface named Array does not establish JavaScript array semantics"

        testCase "operation selection refuses unsafe call and value shapes" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-negative"
            operationPackage scratch.Path """
export interface Recursive { next: Recursive; }
export type Phantom<T> = 'same';
declare const brand: unique symbol;
declare const level: 'low' | 'high';
export interface SymbolRecord { [brand]: 'x'; text: string; }
export interface Harness {
  required(change: { level?: 'low'; required: string }): void;
  selectedRequired(change: { level: 'low' }): void;
  escapedField(change: { '__proto__'?: 'low' }): void;
  '__proto__'(input: string): void;
  overloaded(input: string): void; overloaded(input: number): void;
  generic<T>(input: T): void;
  rest(...input: string[]): void;
  recursive(input: Recursive): void;
  indexed(input: { [key: string]: string }): void;
  callable(input: () => void): void;
  phantom(input: Phantom<string>): void;
  symbolic(input: SymbolRecord): void;
  queried(input: typeof level): void;
  imported(input: import('./values').Level): void;
}
"""
            File.WriteAllText(Path.Combine(scratch.Path, "values.d.ts"), "export type Level = 'low' | 'high';")
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            let required = Resolved.tryFindParameterField "projection-operations" ["Harness"] "required" "change" "level" model |> Option.get
            Expect.isTrue (Resolved.shape required model |> hasCode "projection/unsupported-operation") "required siblings cannot be dropped"
            let selectedRequired = Resolved.tryFindParameterField "projection-operations" ["Harness"] "selectedRequired" "change" "level" model |> Option.get
            Expect.isTrue (Resolved.shape selectedRequired model |> hasCode "projection/unsupported-operation") "the wrapper cannot offer omission of a required selected field"
            let escapedField = Resolved.tryFindParameterField "projection-operations" ["Harness"] "escapedField" "change" "___proto__" model |> Option.get
            Expect.isTrue (Resolved.shape escapedField model |> hasCode "projection/unsupported-operation") "checker-escaped field names cannot authenticate the wrong JavaScript key"
            let escapedMethod = operationSource "___proto__" "input" model
            Expect.isTrue (Resolved.shape escapedMethod model |> hasCode "projection/unsupported-operation") "checker-escaped method names cannot authenticate the wrong JavaScript call"
            for name in ["overloaded"; "generic"; "rest"; "recursive"; "indexed"; "callable"; "phantom"; "symbolic"; "queried"; "imported"] do
                let source = operationSource name "input" model
                Expect.isError (Resolved.shape source model) name
                Expect.isError (Resolved.operation source model) "invalid values do not yield an operation plan"
                Expect.throws (fun () -> operationPlan source model |> ignore) "invalid source cannot authenticate a companion"

        testCase "equal parameter values keep occurrence identity and reject stale plans" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-identities"
            operationPackage scratch.Path inputModel
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let model = snapshot ctx resolved
            let first, second = operationSource "submit" "input" model, operationSource "equal" "input" model
            Expect.equal (Resolved.shape first model) (Resolved.shape second model) "control uses the same value shape"
            Expect.notEqual (Resolved.identity first model) (Resolved.identity second model) "method and parameter declarations authenticate occurrence"
            let plan = operationPlan first model
            let next = snapshot ctx resolved
            Expect.isFalse (ContractData.projectionCompanionIsCurrent next plan) "cached operation plan is rejected"
            Expect.isTrue (Resolved.shape first next |> hasCode "projection/stale-source") "foreign operation token diagnoses"
            Expect.throws (fun () -> operationPlan first next |> ignore) "foreign operation token cannot authenticate a receiver"

        testCase "missing operation constituents cannot narrow the accepted value set" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-incomplete"
            operationPackage scratch.Path inputModel
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let exported = resolved.Harvest.Exports |> List.find (fun item -> item.ExportName = "Harness")
            let receiver = resolved.Types[resolved.ExportTypes[exported.Symbol.SymbolId].Declared |> Option.get]
            let method_ = receiver.Members |> List.find (fun member_ -> member_.Symbol.Name = "submit")
            let signature = resolved.Types[method_.TypeId].CallSignatures |> List.exactlyOne
            let input = signature.Parameters.Head.TypeId
            let arm = resolved.Types[input].UnionMembers.Head
            let controls =
                [ { resolved with Types = Map.remove arm resolved.Types }
                  { resolved with NotFollowed = Map.add arm "controlled unresolved operation constituent" resolved.NotFollowed }
                  { resolved with Types = Map.add input { resolved.Types[input] with UnionMembers = [] } resolved.Types }
                  { resolved with Types = Map.add input { resolved.Types[input] with UnionMembers = [input] } resolved.Types } ]
            for control in controls do
                let model = snapshot ctx control
                let source = operationSource "submit" "input" model
                Expect.isError (Resolved.shape source model) "incomplete values must not silently disappear"
                Expect.isError (Resolved.operation source model) "incomplete values cannot produce call metadata"
                Expect.throws (fun () -> operationPlan source model |> ignore) "no authenticated partial operation"

        testCase "selected imported declaration content authenticates the operation fingerprint" <| fun _ ->
            use scratch = Scratch.directory "projection-operation-import"
            operationPackage scratch.Path "import type {Level} from './values'; export interface Harness { set(change: { level?: Level|null }): void; }"
            let values = Path.Combine(scratch.Path, "values.d.ts")
            File.WriteAllText(values, "export type Level = 'low' | 'high';")
            let server, ctx, resolved = resolve scratch.Path
            use _ = server
            let selected snapshot = Resolved.tryFindParameterField "projection-operations" ["Harness"] "set" "change" "level" snapshot |> Option.get
            let first = snapshot ctx resolved
            let firstSource = selected first
            let firstHash = fingerprint (operationPlan firstSource first)
            File.AppendAllText(values, "\n// retained values, changed dependency source\n")
            let next = snapshot ctx resolved
            let nextSource = selected next
            Expect.equal (Resolved.shape firstSource first) (Resolved.shape nextSource next) "control preserves value facts"
            Expect.notEqual firstHash (fingerprint (operationPlan nextSource next)) "imported source content must affect operation provenance"
    ]
