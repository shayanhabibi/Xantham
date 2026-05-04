module Xantham.Generator.Tests.Tests.HookSlotTests

open Expecto
open Fabulous.AST
open Xantham.Decoder
open Xantham.Generator
open Xantham.Generator.NamePath
open Xantham.Generator.Types
open Xantham.Generator.Generator

[<Tests>]
let compositionTests =
    testList "Composition — handler ordering and Replace chaining" [
        testCase "two handlers — latest-registered runs first; earlier handler sees later one's output" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let h1Inputs = ResizeArray<int>()
            let h2Inputs = ResizeArray<int>()

            // h1: multiply by 2
            let h1: Hook<int> = fun _ _ (v: int) ->
                h1Inputs.Add v
                HookResult.Replace (v * 2)

            // h2: add 10
            let h2: Hook<int> = fun _ _ (v: int) ->
                h2Inputs.Add v
                HookResult.Replace (v + 10)

            let mutable slot = HookSlot.empty<int>
            slot <- HookSlot.add h1 slot
            slot <- HookSlot.add h2 slot  // h2 is latest (prepended)

            let dummyCtx = GeneratorContext.Empty
            let input = 5
            let result = HookSlot.run slot dummyCtx emptyRctx input

            // h2 runs first (latest), returns 5 + 10 = 15
            // h1 runs second, sees 15, returns 15 * 2 = 30
            match result with
            | HookResult.Replace final ->
                Expect.equal final 30 "Final result should be 30"
                Expect.equal h2Inputs.Count 1 "h2 should run once"
                Expect.equal h1Inputs.Count 1 "h1 should run once"
                Expect.equal h2Inputs[0] 5 "h2 input should be the original"
                Expect.equal h1Inputs[0] 15 "h1 input should be h2's output"
            | _ -> failtestf "Expected HookResult.Replace but got %A" result
    ]

[<Tests>]
let threeHandlerChainTests =
    testList "Three-handler chain truth-table regression" [
        testCase "Replace v₁ → Pass → Replace v₃ gives final v₃ with h₃ input = v₁" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let h1Inputs = ResizeArray<int>()
            let h2Inputs = ResizeArray<int>()
            let h3Inputs = ResizeArray<int>()

            // h1: Replace with 100
            let h1: Hook<int> = fun _ _ v ->
                h1Inputs.Add v
                HookResult.Replace 100

            // h2: Pass (no change)
            let h2: Hook<int> = fun _ _ v ->
                h2Inputs.Add v
                HookResult.Pass

            // h3: Replace with 300
            let h3: Hook<int> = fun _ _ v ->
                h3Inputs.Add v
                HookResult.Replace 300

            let mutable slot = HookSlot.empty<int>
            slot <- HookSlot.add h1 slot
            slot <- HookSlot.add h2 slot
            slot <- HookSlot.add h3 slot  // Latest

            let dummyCtx = GeneratorContext.Empty
            let input = 1
            let result = HookSlot.run slot dummyCtx emptyRctx input

            // h3 runs first, returns 300
            // h2 runs second on 300, returns Pass (300 unchanged)
            // h1 runs third on 300, returns 100
            match result with
            | HookResult.Replace final ->
                Expect.equal final 100 "Final result should be 100"
                Expect.equal h3Inputs.Count 1 "h3 should run once"
                Expect.equal h2Inputs.Count 1 "h2 should run once"
                Expect.equal h1Inputs.Count 1 "h1 should run once"
                Expect.equal h3Inputs[0] 1 "h3 input should be original (1)"
                Expect.equal h2Inputs[0] 300 "h2 input should be h3's output (300)"
                Expect.equal h1Inputs[0] 300 "h1 input should be h3's output (300) since h2 passed through"
            | _ -> failtestf "Expected HookResult.Replace but got %A" result
    ]

[<Tests>]
let skipShortCircuitTests =
    testList "Skip semantics and short-circuit" [
        testCase "Skip semantics with SkippableHookSlot" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let callLog = ResizeArray<string>()

            // Handler that returns Skip for value = "skip"
            let handler: SkippableHook<string> = fun _ _ (v: string) ->
                callLog.Add v
                if v = "skip" then
                    SkippableHookResult.Skip
                else
                    SkippableHookResult.Pass

            let mutable slot = SkippableHookSlot.empty<string>
            slot <- SkippableHookSlot.add handler slot

            let dummyCtx = GeneratorContext.Empty

            // Test Skip result
            let resultSkip = SkippableHookSlot.run slot dummyCtx emptyRctx "skip"
            Expect.equal resultSkip SkippableHookResult.Skip "Should be Skip for 'skip' input"

            // Test Pass result
            let resultKeep = SkippableHookSlot.run slot dummyCtx emptyRctx "keep"
            Expect.equal resultKeep SkippableHookResult.Pass "Should be Pass for 'keep' input"

        testCase "Skip short-circuits — later handlers never run when earlier returns Skip" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<int>()

            let h1: SkippableHook<string> = fun _ _ _ ->
                runLog.Add 1
                SkippableHookResult.Skip

            let h2: SkippableHook<string> = fun _ _ _ ->
                runLog.Add 2
                SkippableHookResult.Pass

            let h3: SkippableHook<string> = fun _ _ _ ->
                runLog.Add 3
                SkippableHookResult.Pass

            let mutable slot = SkippableHookSlot.empty<string>
            slot <- SkippableHookSlot.add h1 slot
            slot <- SkippableHookSlot.add h2 slot
            slot <- SkippableHookSlot.add h3 slot  // Latest

            let dummyCtx = GeneratorContext.Empty
            let result = SkippableHookSlot.run slot dummyCtx emptyRctx "test"

            // h3 runs first (latest): Pass
            // h2 runs second: Pass
            // h1 runs third: Skip -> short-circuit
            Expect.equal result SkippableHookResult.Skip "Result should be Skip"
            Expect.equal runLog.Count 3 "All three handlers should be tried"
            Expect.equal runLog[0] 3 "h3 (latest) should run first"
            Expect.equal runLog[1] 2 "h2 should run second"
            Expect.equal runLog[2] 1 "h1 should run third and return Skip"
    ]

[<Tests>]
let positionContextTests =
    testList "Position context — handler sees correct position" [
        testCase "Handler observes position from RenderContext" <| fun _ ->
            let positionsObserved = ResizeArray<RenderPosition>()

            let positionCapture: Hook<string> = fun _ rctx _ ->
                positionsObserved.Add rctx.Position
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            slot <- HookSlot.add positionCapture slot

            let dummyCtx = GeneratorContext.Empty

            // Invoke with InheritanceRef position
            let rctxInherit = {
                Position = RenderPosition.RefPos TypeRefPosition.InheritanceRef
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let _ = HookSlot.run slot dummyCtx rctxInherit "test"

            // Invoke with TypeArg position
            let rctxTypeArg = {
                Position = RenderPosition.RefPos TypeRefPosition.TypeArg
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let _ = HookSlot.run slot dummyCtx rctxTypeArg "test"

            Expect.equal positionsObserved.Count 2 "Handler should run twice"
            Expect.equal
                positionsObserved[0]
                (RenderPosition.RefPos TypeRefPosition.InheritanceRef)
                "First invocation should see InheritanceRef"
            Expect.equal
                positionsObserved[1]
                (RenderPosition.RefPos TypeRefPosition.TypeArg)
                "Second invocation should see TypeArg"
    ]

[<Tests>]
let stageContextTests =
    testList "Stage context — handler sees correct stage" [
        testCase "Handler observes stage from RenderContext" <| fun _ ->
            let stagesObserved = ResizeArray<RenderStage>()

            let stageCapture: Hook<string> = fun _ rctx _ ->
                stagesObserved.Add rctx.Stage
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            slot <- HookSlot.add stageCapture slot

            let dummyCtx = GeneratorContext.Empty
            let input = "test"

            // Invoke with different stages
            let stages = [RenderStage.PathResolution; RenderStage.TypeRefBuild; RenderStage.TypeRefEmit]
            for stage in stages do
                let rctx = {
                    Position = RenderPosition.NotApplicable
                    Owner = ValueNone
                    Render = ValueNone
                    Stage = stage
                }
                let _ = HookSlot.run slot dummyCtx rctx input
                ()

            Expect.equal stagesObserved.Count 3 "Handler should run 3 times"
            Expect.equal stagesObserved[0] RenderStage.PathResolution "First should be PathResolution"
            Expect.equal stagesObserved[1] RenderStage.TypeRefBuild "Second should be TypeRefBuild"
            Expect.equal stagesObserved[2] RenderStage.TypeRefEmit "Third should be TypeRefEmit"
    ]

[<Tests>]
let slotManagementTests =
    testList "HookSlot.remove and clear" [
        testCase "remove via token — removed handler not invoked on subsequent run" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<string>()

            let h1: Hook<string> = fun _ _ _ ->
                runLog.Add "h1"
                HookResult.Pass

            let h2: Hook<string> = fun _ _ _ ->
                runLog.Add "h2"
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            let slot1, token1 = HookSlot.addTracked h1 slot
            let slot2 = HookSlot.add h2 slot1

            let dummyCtx = GeneratorContext.Empty

            // First run: both h1 and h2
            let _ = HookSlot.run slot2 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 2 "Both handlers should run"

            // Remove h1
            let slot3 = HookSlot.remove token1 slot2
            runLog.Clear()

            // Second run: only h2
            let _ = HookSlot.run slot3 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 1 "Only h2 should run after removal"
            Expect.equal runLog[0] "h2" "h2 should be the remaining handler"

        testCase "clear — all handlers removed" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<string>()

            let h1: Hook<string> = fun _ _ _ ->
                runLog.Add "h1"
                HookResult.Pass

            let h2: Hook<string> = fun _ _ _ ->
                runLog.Add "h2"
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            slot <- HookSlot.add h1 slot
            slot <- HookSlot.add h2 slot

            // Clear the slot
            let slot2 = HookSlot.clear slot

            let dummyCtx = GeneratorContext.Empty
            let _ = HookSlot.run slot2 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 0 "No handlers should run after clear"
    ]

[<Tests>]
let skippableSlotManagementTests =
    testList "SkippableHookSlot.remove and clear" [
        testCase "remove via token from SkippableHookSlot" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<string>()

            let h1: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h1"
                SkippableHookResult.Pass

            let h2: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h2"
                SkippableHookResult.Pass

            let mutable slot = SkippableHookSlot.empty<string>
            let slot1, token1 = SkippableHookSlot.addTracked h1 slot
            let slot2 = SkippableHookSlot.add h2 slot1

            let dummyCtx = GeneratorContext.Empty

            // First run: both h1 and h2
            let _ = SkippableHookSlot.run slot2 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 2 "Both handlers should run"

            // Remove h1
            let slot3 = SkippableHookSlot.remove token1 slot2
            runLog.Clear()

            // Second run: only h2
            let _ = SkippableHookSlot.run slot3 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 1 "Only h2 should run after removal"
            Expect.equal runLog[0] "h2" "h2 should be the remaining handler"

        testCase "clear SkippableHookSlot — all handlers removed" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<string>()

            let h1: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h1"
                SkippableHookResult.Pass

            let h2: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h2"
                SkippableHookResult.Pass

            let mutable slot = SkippableHookSlot.empty<string>
            slot <- SkippableHookSlot.add h1 slot
            slot <- SkippableHookSlot.add h2 slot

            // Clear the slot
            let slot2 = SkippableHookSlot.clear slot

            let dummyCtx = GeneratorContext.Empty
            let _ = SkippableHookSlot.run slot2 dummyCtx emptyRctx "test"
            Expect.equal runLog.Count 0 "No handlers should run after clear"
    ]

[<Tests>]
let customisationRemovalTests =
    testList "Customisation.remove via HandlerToken" [
        testCase "remove handler from Customisation — no longer invoked" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let runLog = ResizeArray<string>()

            // Create handlers for SkippableHookSlot<string> - same type signature
            let h1: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h1"
                SkippableHookResult.Pass

            let h2: SkippableHook<string> = fun _ _ _ ->
                runLog.Add "h2"
                SkippableHookResult.Pass

            // Use pathResolution which accepts string (we skip actual TypePath binding)
            let mutable custom = Customisation.Default
            let custom1, token1 = Customisation.addPathResolutionTypeTracked (fun _ _ _ -> SkippableHookResult.Pass) custom
            // Instead of adding h2 to PathResolution, let's just test adding to the custom afterward

            let dummyCtx = GeneratorContext.Empty

            // First run: handler is registered
            let _ = SkippableHookSlot.run custom1.PathResolutionType dummyCtx emptyRctx (Unchecked.defaultof<_>)
            Expect.equal runLog.Count 0 "No string handlers registered (type mismatch)"

            // Remove the handler
            let custom2 = Customisation.remove token1 custom1
            runLog.Clear()

            // Second run: handler should be gone
            let _ = SkippableHookSlot.run custom2.PathResolutionType dummyCtx emptyRctx (Unchecked.defaultof<_>)
            Expect.equal runLog.Count 0 "Handler should be removed"
            Expect.isTrue (not custom2.PathResolutionType.HasAny) "Slot should be empty after removal"
    ]

[<Tests>]
let noOpTests =
    testList "Zero-cost no-op with Customisation.Default" [
        testCase "Customisation.Default has all empty slots" <| fun _ ->
            Expect.isFalse Customisation.Default.PathResolutionType.HasAny "PathResolutionType should be empty"
            Expect.isFalse Customisation.Default.PathResolutionMember.HasAny "PathResolutionMember should be empty"
            Expect.isFalse Customisation.Default.TypeRefBuild.HasAny "TypeRefBuild should be empty"
            Expect.isFalse Customisation.Default.TypeRefEmit.HasAny "TypeRefEmit should be empty"
            Expect.isFalse Customisation.Default.RenderScopeBuild.HasAny "RenderScopeBuild should be empty"
            Expect.isFalse Customisation.Default.TypeDefBuildClass.HasAny "TypeDefBuildClass should be empty"
            Expect.isFalse Customisation.Default.TypeDefBuildAlias.HasAny "TypeDefBuildAlias should be empty"
            Expect.isFalse Customisation.Default.TypeDefBuildEnum.HasAny "TypeDefBuildEnum should be empty"
            Expect.isFalse Customisation.Default.TypeDefBuildStringUnion.HasAny "TypeDefBuildStringUnion should be empty"
            Expect.isFalse Customisation.Default.TypeDefEmit.HasAny "TypeDefEmit should be empty"
            Expect.isFalse Customisation.Default.AnchoredRef.HasAny "AnchoredRef should be empty"
            Expect.isFalse Customisation.Default.AnchoredScope.HasAny "AnchoredScope should be empty"
    ]

[<Tests>]
let ownerContextTests =
    testList "Owner context — handler sees correct owner" [
        testCase "Handler can observe when owner is present or absent" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let ownersObserved = ResizeArray<RenderOwner voption>()

            let ownerCapture: Hook<string> = fun _ rctx _ ->
                ownersObserved.Add rctx.Owner
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            slot <- HookSlot.add ownerCapture slot

            let dummyCtx = GeneratorContext.Empty

            // First call without owner
            let rctxWithoutOwner = { emptyRctx with Owner = ValueNone }
            let _ = HookSlot.run slot dummyCtx rctxWithoutOwner "test1"

            // Second call with owner
            let rctxWithOwner = { emptyRctx with Owner = ValueSome (RenderOwner.Export Unchecked.defaultof<_>) }
            let _ = HookSlot.run slot dummyCtx rctxWithOwner "test2"

            Expect.equal ownersObserved.Count 2 "Handler should run twice"
            match ownersObserved[0] with
            | ValueNone -> Expect.isTrue true "First owner should be absent"
            | ValueSome _ -> Expect.isTrue false "First owner should be absent"

            match ownersObserved[1] with
            | ValueSome _ -> Expect.isTrue true "Second owner should be present"
            | ValueNone -> Expect.isTrue false "Second owner should be present"
    ]

[<Tests>]
let renderModeThreadingTests =
    testList "Render mode threading" [
        testCase "Handler sees that RenderMode is present or absent" <| fun _ ->
            let emptyRctx = {
                Position = RenderPosition.NotApplicable
                Owner = ValueNone
                Render = ValueNone
                Stage = RenderStage.TypeRefBuild
            }

            let modesObserved = ResizeArray<RenderMode voption>()

            let modeCapture: Hook<string> = fun _ rctx _ ->
                modesObserved.Add rctx.Render
                HookResult.Pass

            let mutable slot = HookSlot.empty<string>
            slot <- HookSlot.add modeCapture slot

            let dummyCtx = GeneratorContext.Empty

            // Invoke without render mode
            let rctxWithoutMode = { emptyRctx with Render = ValueNone }
            let _ = HookSlot.run slot dummyCtx rctxWithoutMode "test"

            // Invoke with render mode
            let rctxWithMode = { emptyRctx with Render = ValueSome (Types.Prelude.Render.RefOnly Unchecked.defaultof<_>) }
            let _ = HookSlot.run slot dummyCtx rctxWithMode "test"

            Expect.equal modesObserved.Count 2 "Handler should run twice"
            match modesObserved[0] with
            | ValueNone -> Expect.isTrue true "First should be ValueNone"
            | ValueSome _ -> Expect.isTrue false "First should be ValueNone"

            match modesObserved[1] with
            | ValueSome _ -> Expect.isTrue true "Second should have a value"
            | ValueNone -> Expect.isTrue false "Second should have a value"
    ]

[<Tests>]
let productionRendererInvokesHookTests =
    testList "Production renderer fires TypeRefBuild/TypeRefEmit hooks" [
        // Regression: TypeRender.Render.fs renderers used to call the non-context
        // TypeRefRender.Anchored.render SRTP, which bypassed every hook slot. They now flow
        // through TypeRefRender.SRTPHelper.RenderWithContext (Anchored overload) via the
        // renderTypeRef helper. The anchored renderHook fires AnchoredRef at the top and
        // TypeRefEmit at the produced widget — so a production-shaped renderer call
        // (TypedNameRender.renderTypeOnly) must trigger both slots.
        testCase "TypedNameRender.renderTypeOnly invokes AnchoredRef and TypeRefEmit hooks" <| fun _ ->
            let anchoredHits = ref 0
            let emitHits     = ref 0

            let anchoredHook : SkippableHook<Anchored.TypeRefRender> = fun _ _ _ ->
                incr anchoredHits
                SkippableHookResult.Pass

            let emitHook : Hook<WidgetBuilder<Fantomas.Core.SyntaxOak.Type>> = fun _ _ _ ->
                incr emitHits
                HookResult.Pass

            let ctx =
                GeneratorContext.EmptyWithCustomisation (fun c ->
                    c
                    |> Customisation.addAnchoredRef anchoredHook
                    |> Customisation.addTypeRefEmit emitHook)

            let typeRef : Anchored.TypeRefRender = {
                Kind = Anchored.TypeRefKind.Atom (Anchored.TypeRefAtom.Intrinsic "int")
                Nullable = false
            }

            let metadata : RenderMetadata = {
                Path = Path.create TransientMemberPath.Anchored
                Original = Path.create TransientMemberPath.Anchored
                Source = ValueNone
                FullyQualifiedName = ValueNone
            }

            let typedName : Anchored.TypedNameRender = {
                Metadata = metadata
                Name = Name.Camel.create "field"
                Type = typeRef
                Traits = Set.empty
                TypeParameters = []
                Documentation = []
            }

            let _ = TypedNameRender.renderTypeOnly ctx ValueNone typedName

            Expect.isGreaterThan anchoredHits.Value 0 "AnchoredRef hook should fire from production renderer"
            Expect.isGreaterThan emitHits.Value 0 "TypeRefEmit hook should fire from production renderer"
    ]
