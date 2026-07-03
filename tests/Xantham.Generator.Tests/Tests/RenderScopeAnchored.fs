module Xantham.Generator.Tests.Tests.RenderScopeAnchored

open Expecto
open Fabulous.AST
open Xantham
open Xantham.Decoder
open Xantham.Decoder.ArenaInterner
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.Generator.TypeRefRender
open Xantham.Generator.Types
open Xantham.Generator.NamePath
open Mocking.ArenaInterner.ResolvedType

// After upstream PR #55 wired exportdeclaration into export specifiers, a
// renamed re-export (`dispatcherBindingImpl as _dispatcherBindingImpl`) and the
// canonical declaration both flow into a single `ResolvedExport.Function` as a
// list of two reference-different Function records with structurally identical
// rendered signatures. Because Function carries [<ReferenceEquality>],
// List.distinct on the funcs themselves doesn't dedup; only the resulting
// FunctionLikeSignature records (plain F# records → structural equality) are
// deduplicable. Visible symptom: duplicate `_dispatcherBindingImpl` static
// member in dynamic-workflows.fs. Fix: List.distinct on the constructed
// Signatures list inside `registerAnchorFromExport`.

// MODULE-GLOBAL VARIABLE TYPE ANCHORING (2026-07-05, the rpcStub fix). A module-scope
// global's typed member lands in the globals-holder type emitted at namespace ROOT, so its
// type ref is LOCALISED against root (qualification survives — the A-1 CacheContext fix).
// But ANCHORING (transient resolution) must use the export's TRUE anchor — the same anchor
// the hoisted def is placed with — so a hoisted un-homed literal's def (`module M ... type X`)
// and its holder ref (`M.X`) agree by construction. Anchoring AND localising at one shared
// anchor breaks one side or the other: member-anchor strips the module prefix (bare `X` at
// root, FS0039); root-anchor roots the transient away from its module-placed def (bare ref,
// FS0039 — the rpcStub failure). Concrete atoms are anchor-invariant, so the split cannot
// regress the A-1 classes.
let private renderedVariableTypeText (ctx: GeneratorContext) =
    let typeRef =
        ctx.AnchorRenders
        |> Seq.choose (function
            | KeyValue(_, Choice2Of2 scope) -> Some scope
            | _ -> None)
        |> Seq.tryPick (fun scope ->
            match snd scope.Render |> _.Value with
            | Anchored.TypeRender.Variable v -> Some v.Type
            | _ -> None)
    match typeRef with
    | None -> failtest "expected a Variable render scope to be registered"
    | Some ref ->
        Ast.Oak() {
            Ast.AnonymousModule() {
                Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref)
            }
        }
        |> Gen.mkOak
        |> Gen.run
        |> _.Trim()

[<Tests>]
let variableAnchoringTests =
    testList "registerAnchorFromExport — module-global Variable type anchoring" [
        testCase "concrete module-qualified ref keeps its qualification (root localise)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let variable =
                Interface.create "Target"
                |> Interface.withPath [ "M" ]
                |> Interface.wrap
                |> Variable.create "cache"
                |> Variable.withPath [ "M" ]
            registerAnchorFromExport ctx (ResolvedExport.Variable variable)
            renderedVariableTypeText ctx
            |> Flip.Expect.equal "M.Target must not strip to bare Target" "let _: M.Target = JS.undefined"

        testCase "hoisted un-homed literal ref qualifies under the export's module (def/ref agreement)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let variable =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create "value" (primitive TypeKindPrimitive.String) |> Property.wrap)
                |> TypeLiteral.wrap
                |> Variable.create "rpcStub"
                |> Variable.withPath [ "M" ]
            registerAnchorFromExport ctx (ResolvedExport.Variable variable)
            renderedVariableTypeText ctx
            |> Flip.Expect.equal "hoisted literal must qualify under M, not render bare" "let _: M.RpcStub = JS.undefined"

        // PATH OCCUPANCY through the full anchor machinery (the merged-def family):
        // TWO structurally-distinct nameless literals under ONE context must render
        // DISTINCT ref paths (first keeps the context, second re-homes to a Case2
        // child) — not the U2<X, X> self-union whose defs merged (FS0438).
        testCase "two distinct hoisted literals under one context split into context + Case2 child" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let literalWith name typ =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create name (primitive typ) |> Property.wrap)
                |> TypeLiteral.wrap
            let variable =
                [ literalWith "a" TypeKindPrimitive.String
                  literalWith "b" TypeKindPrimitive.Number ]
                |> Union.create
                |> Variable.create "combo"
                |> Variable.withPath [ "M" ]
            registerAnchorFromExport ctx (ResolvedExport.Variable variable)
            renderedVariableTypeText ctx
            |> Flip.Expect.equal "union arms must not collapse onto one path" "let _: U2<M.Combo, M.Combo.Case2> = JS.undefined"

        // FUNCTION-ARM SPLIT (the PartyServer GetServerByName class — the Variable-arm
        // rpcStub fix's flagged latent twin): a module-global FUNCTION's hoisted param
        // and return literals must render refs at the export's TRUE anchor (where the
        // def walk places their defs: nameless ↔ context path, second literal ↔ Case2
        // child) with the module qualification kept — not rooted bare at the holder.
        testCase "function param and return literals qualify at the export anchor (def/ref agreement)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let literalWith name typ =
                TypeLiteral.empty
                |> TypeLiteral.addMember (Property.create name (primitive typ) |> Property.wrap)
                |> TypeLiteral.wrap
            let func =
                Function.create "getServerByName" (literalWith "id" TypeKindPrimitive.String)
                |> Function.withPath [ "M" ]
                |> Function.withParameters [ Parameter.create "options" (literalWith "retry" TypeKindPrimitive.Boolean) ]
            registerAnchorFromExport ctx (ResolvedExport.Function [ func ])
            let signature =
                ctx.AnchorRenders
                |> Seq.choose (function
                    | KeyValue(_, Choice2Of2 scope) -> Some scope
                    | _ -> None)
                |> Seq.tryPick (fun scope ->
                    match snd scope.Render |> _.Value with
                    | Anchored.TypeRender.Function f -> Some f.Signatures.Head
                    | _ -> None)
                |> Option.defaultWith (fun () -> failtest "expected a Function render scope")
            let text (ref: Xantham.Generator.Types.Anchored.TypeRefRender) =
                Ast.Oak() {
                    Ast.AnonymousModule() {
                        Ast.Value("_", Exprs.jsUndefined, TypeRefRender.render ref)
                    }
                }
                |> Gen.mkOak |> Gen.run |> _.Trim()
            // Both refs must resolve AT THE EXPORT ANCHOR — qualified `M.GetServerByName`,
            // neither bare `GetServerByName` (the pre-split root-localise failure) nor
            // param-rebased `M.GetServerByName.Options`. NB: production literals pre-mint
            // through the function's full SignatureKey (so a second literal in the scope
            // re-homes to Case2 — see the PartyServer emission); this mock's SignatureKey
            // is empty, so both literals resolve nameless at the anchor.
            text signature.Parameters.Head.Type
            |> Flip.Expect.equal "param literal ref must resolve at the export anchor" "let _: M.GetServerByName = JS.undefined"
            text signature.ReturnType
            |> Flip.Expect.equal "return literal ref must resolve at the export anchor" "let _: M.GetServerByName = JS.undefined"
    ]

[<Tests>]
let tests =
    testList "registerAnchorFromExport — Function dedup" [
        testCase "two reference-different Functions with same content collapse to one signature" <| fun _ ->
            let ctx = GeneratorContext.Empty
            // Two structurally identical Function values, different references
            // (mirrors the post-#55 case where canonical and renamed export
            // bind to the same shape).
            let func1 = Function.create "doThing" (primitive TypeKindPrimitive.String)
            let func2 = Function.create "doThing" (primitive TypeKindPrimitive.String)
            // Confirm they are reference-different but structurally equal in
            // the resulting paths (sanity: this is the regression scenario)
            Expect.isFalse (System.Object.ReferenceEquals(func1, func2))
                "test mocks must produce reference-different Function records"
            let export = ResolvedExport.Function [ func1; func2 ]
            registerAnchorFromExport ctx export
            // Find the registered FunctionLikeRender for this export.
            let renderScope =
                ctx.AnchorRenders
                |> Seq.choose (function
                    | KeyValue(_, Choice2Of2 scope) -> Some scope
                    | _ -> None)
                |> Seq.tryHead
            match renderScope with
            | None -> failtest "expected a render scope to be registered"
            | Some scope ->
                match snd scope.Render |> _.Value with
                | Anchored.TypeRender.Function functionLike ->
                    Flip.Expect.equal
                        "two duplicate input funcs should collapse to a single signature"
                        1
                        functionLike.Signatures.Length
                | other ->
                    failtestf "expected TypeRender.Function but got %A" other

        testCase "single Function produces one signature (baseline)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let func = Function.create "doThing" (primitive TypeKindPrimitive.String)
            let export = ResolvedExport.Function [ func ]
            registerAnchorFromExport ctx export
            let renderScope =
                ctx.AnchorRenders
                |> Seq.choose (function
                    | KeyValue(_, Choice2Of2 scope) -> Some scope
                    | _ -> None)
                |> Seq.tryHead
            match renderScope with
            | None -> failtest "expected a render scope to be registered"
            | Some scope ->
                match snd scope.Render |> _.Value with
                | Anchored.TypeRender.Function functionLike ->
                    Flip.Expect.equal "single func → single signature" 1 functionLike.Signatures.Length
                | other ->
                    failtestf "expected TypeRender.Function but got %A" other
    ]

// ---------------------------------------------------------------------------
// DANGLING SELF-NAMESPACE SCRUB (`Render.scrubDanglingSelfRefs`) — the ref-side
// closure of the cross-owner cache-hit class (the Agents createMcpHandler FS0039s):
// a path atom INSIDE the export's own anchor namespace that the export's store will
// NOT materialize is a cross-owner cached transient's per-site resolution — its def
// lives only under its first owner. Degrade to obj, ledgered foreign-transient-scrub.
//
// The KEEP-list is threefold, each pinned below:
//   - STORE-BACKED same-export transients (RpcStub, Case{n} children) materialize
//     under this anchor — kept;
//   - DECLARED types whose path coincides with the anchor namespace (`env:
//     Cloudflare.Env` under the Cloudflare.* anchors) are real defs — kept via
//     ctx.DeclaredTypePaths (the markDeclaredTypePaths pre-pass);
//   - atoms OUTSIDE the anchor namespace (concrete cross-unit refs) are not this
//     scrub's business — untouched.
//
// DEF-SIDE ADOPTION is the pinned 3-for-3 negative (walk-wide Workers 0->37;
// export-arm-gated 0->34; declared-guarded Agents 2->22): every variant collides
// with the def-placement universe, which is not enumerable at prerender time. The
// ref side is total and collision-free — this scrub is the design, not a fallback.
// ---------------------------------------------------------------------------

let private scrubAnchor =
    ModulePath.init "Agents"
    |> TypePath.create "CreateMcpHandler"
    |> AnchorPath.createType

let private pathAtomRender (path: TypePath) : Anchored.TypeRefRender =
    { Kind = Anchored.TypeRefKind.Atom(Anchored.TypeRefAtom.Path path); Nullable = false }

let private runScrub (ctx: GeneratorContext) (scope: RenderScopeStore) (render: Anchored.TypeRefRender) =
    Render.scrubDanglingSelfRefs ctx scope scrubAnchor render

let private expectAtom (render: Anchored.TypeRefRender) =
    match render.Kind with
    | Anchored.TypeRefKind.Atom a -> a
    | other -> failtestf "expected an Atom but got %A" other

[<Tests>]
let scrubDanglingSelfRefsTests =
    testList "Render.scrubDanglingSelfRefs (cross-owner dangling ref scrub)" [

        testCase "an anchor-namespace atom with no def degrades to obj, ledgered" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let dangling =
                ModulePath.createFromList [ "Agents"; "CreateMcpHandler" ]
                |> TypePath.create "Request"
                |> pathAtomRender
            match runScrub ctx (RenderScopeStore.create ()) dangling |> expectAtom with
            | Anchored.TypeRefAtom.Intrinsic s -> Flip.Expect.equal "degraded to obj" "obj" s
            | other -> failtestf "expected Intrinsic obj but got %A" other
            let ledger = GeneratorContext.Advisory.dump ctx |> Map.ofList
            Flip.Expect.equal "scrub is ledgered by subject" (Some 1) (Map.tryFind "foreign-transient-scrub:Request" ledger)

        testCase "the anchor path ITSELF with no def is scrubbed (self-name dangle)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let self = ModulePath.init "Agents" |> TypePath.create "CreateMcpHandler" |> pathAtomRender
            match runScrub ctx (RenderScopeStore.create ()) self |> expectAtom with
            | Anchored.TypeRefAtom.Intrinsic s -> Flip.Expect.equal "degraded to obj" "obj" s
            | other -> failtestf "expected Intrinsic obj but got %A" other

        testCase "a STORE-BACKED same-export transient is kept (it will materialize)" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let scope = RenderScopeStore.create ()
            scope.TypeStore[primitive TypeKindPrimitive.Any] <-
                TransientTypePath.Moored(TransientModulePath.Anchored, Name.Pascal.create "Stub")
            let stubRef =
                ModulePath.createFromList [ "Agents"; "CreateMcpHandler" ]
                |> TypePath.create "Stub"
                |> pathAtomRender
            match runScrub ctx scope stubRef |> expectAtom with
            | Anchored.TypeRefAtom.Path p -> Flip.Expect.equal "kept" "Stub" (Name.Case.valueOrModified p.Name)
            | other -> failtestf "expected the Path atom kept but got %A" other

        testCase "a DECLARED type coinciding with the anchor namespace is kept" <| fun _ ->
            let ctx = GeneratorContext.Empty
            ctx.DeclaredTypePaths.Add "Agents.CreateMcpHandler.Env" |> ignore
            let envRef =
                ModulePath.createFromList [ "Agents"; "CreateMcpHandler" ]
                |> TypePath.create "Env"
                |> pathAtomRender
            match runScrub ctx (RenderScopeStore.create ()) envRef |> expectAtom with
            | Anchored.TypeRefAtom.Path p -> Flip.Expect.equal "kept" "Env" (Name.Case.valueOrModified p.Name)
            | other -> failtestf "expected the Path atom kept but got %A" other

        testCase "an atom OUTSIDE the anchor namespace is untouched" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let foreign = ModulePath.init "Workers" |> TypePath.create "Request" |> pathAtomRender
            match runScrub ctx (RenderScopeStore.create ()) foreign |> expectAtom with
            | Anchored.TypeRefAtom.Path p -> Flip.Expect.equal "untouched" "Request" (Name.Case.valueOrModified p.Name)
            | other -> failtestf "expected the Path atom untouched but got %A" other

        // Guard against prefix false-positives: "AgentsX.CreateMcpHandler" shares the
        // anchor's leading TEXT but not its namespace — the scrub matches on the dotted
        // boundary, not raw StartsWith.
        testCase "a sibling namespace sharing the anchor's text prefix is untouched" <| fun _ ->
            let ctx = GeneratorContext.Empty
            let sibling =
                ModulePath.createFromList [ "AgentsX"; "CreateMcpHandler" ]
                |> TypePath.create "Request"
                |> pathAtomRender
            match runScrub ctx (RenderScopeStore.create ()) sibling |> expectAtom with
            | Anchored.TypeRefAtom.Path _ -> ()
            | other -> failtestf "expected the Path atom untouched but got %A" other
    ]
