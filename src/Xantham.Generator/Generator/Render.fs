module Xantham.Generator.Generator.Render

open System
open Xantham
open Xantham.Generator
open Xantham.Generator.Generator
open Xantham.Generator.NamePath
open Fabulous.AST
open Xantham.Decoder.ArenaInterner
open Xantham.Decoder

open Xantham.Generator.Types

//
[<EntryPoint>]
let main argv =
    let file = IO.Path.Join(__SOURCE_DIRECTORY__, "../../Xantham.Fable/output.json")
    let tree = Decoder.Runtime.create file
    let interner = tree.GetArenaInterner()
    let generatorContext: GeneratorContext =
         GeneratorContext.EmptyWithCustomisation (fun customiser ->
         {
             customiser with
                 Customisation.Interceptors.ResolvedTypePrelude = fun _ ->
                     // Faithful TS-stdlib (lib.es / lib.dom) -> F#/Fable name mappings live
                     // in LibEsSubstitution (single source of truth, unit-tested).
                     let libEsSubstitution = LibEsSubstitution.substitute
                     // `Array`/`Error`/... are stdlib types the TS checker attributes to a
                     // `typescript` source (not flagged IsLibEs). The substitution map is the
                     // safety gate: workers-types' own `typescript`-sourced types (Response,
                     // Request, ...) are NOT in the map, so they fall through untouched.
                     let isStdlibSourced (source: QualifiedNamePart option) =
                         match source with
                         | Some (QualifiedNamePart.Normal s | QualifiedNamePart.Abnormal(s, _)) ->
                             s.Contains("typescript", StringComparison.OrdinalIgnoreCase)
                         | None -> false
                     let substituteLibEs (libEsName: Name<Case.pascal>) renderScope =
                         match libEsSubstitution (Name.Case.valueOrSource libEsName) with
                         | Some target ->
                             let ref =
                                 RenderScopeStore.TypeRefAtom.Unsafe.createWidget (Ast.LongIdent target)
                                 |> RenderScopeStore.TypeRef.Unsafe.createAtom
                                 |> RenderScopeStore.TypeRefRender.Unsafe.createFromKind renderScope.TypeRef.Nullable
                             { renderScope with TypeRef = ref; Render = Render.RefOnly ref }
                         | None ->
                             { renderScope with Render = Render.RefOnly renderScope.TypeRef }
                     function
                     | ResolvedType.Interface { IsLibEs = true; Name = name } -> substituteLibEs name
                     | ResolvedType.Class { IsLibEs = true; Name = name } -> substituteLibEs name
                     | ResolvedType.Enum { IsLibEs = true } -> fun renderScope ->
                         { renderScope with Render = Render.RefOnly renderScope.TypeRef }
                     // Stdlib types attributed to a `typescript` source but not IsLibEs-flagged
                     // (Array, the typed arrays, ...) — substitute only when the name is in the map.
                     | ResolvedType.Interface { Source = src; Name = name } when isStdlibSourced src && (libEsSubstitution (Name.Case.valueOrSource name)).IsSome ->
                         substituteLibEs name
                     | ResolvedType.Class { Source = src; Name = name } when isStdlibSourced src && (libEsSubstitution (Name.Case.valueOrSource name)).IsSome ->
                         substituteLibEs name
                     | _ -> id
                 Customisation.Interceptors.IgnorePathRender.Source = function
                     | QualifiedNamePart.Normal(text)
                     | QualifiedNamePart.Abnormal(text,_) ->
                         text.Contains("babel", StringComparison.OrdinalIgnoreCase)
                         || text.Contains("typescript", StringComparison.OrdinalIgnoreCase)
                 // The synthetic "Typescript" module parent is never emitted as a
                 // real F# module: it is synthesized from a `typescript` source
                 // attribution (the same source the IgnorePathRender above already
                 // suppresses). Types carrying it — whether lib.es or workers-types'
                 // own `Response`/`Request`/`WebSocket` (which the TS checker
                 // attributes to a `typescript` origin) — would otherwise reference a
                 // `Typescript.X` that has no definition. Prune the phantom parent
                 // unconditionally so the reference resolves to the emitted type.
                 Customisation.Interceptors.Paths.TypePaths = fun ctx typ s ->
                     TypePath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                 Customisation.Interceptors.Paths.MemberPaths = fun ctx typ s ->
                     MemberPath.pruneParent (_.Name >> Name.Case.valueOrModified >> (=) "Typescript") s
                     
         })
    ArenaInterner.prerenderTypeAliases generatorContext interner
    // ArenaInterner.prerenderFromGraph generatorContext interner
    ArenaInterner.processExports generatorContext interner
    let renders =
        RootModule.collectModules generatorContext
        // |> _.Modules["Typescript"]
        // |> renderModule generatorContext
        |> renderRoot generatorContext
    // generatorContext.AnchorRenders
    // |> Seq.take 100
    // |> Seq.choose (_.Value >> function
    //     | Choice1Of2 x ->
    //         Ast.Value("_", "jsNative", TypeRefRender.Anchored.render x)
    //         |> Choice1Of2
    //         |> Some
    //     | Choice2Of2 x ->
    //         match x.Render.Deconstruct() |> snd |> _.Value with
    //         | TypeRender.EnumUnion enumUnion ->
    //             LiteralUnionRender.renderEnum generatorContext enumUnion
    //             |> Choice2Of2
    //             |> Some
    //         | TypeDefn typeLikeRender ->
    //             TypeLikeRender.renderClass generatorContext typeLikeRender
    //             |> Choice2Of2
    //             |> Some
    //         | _ -> None
    //         )
    // |> fun x ->
    Ast.Oak() {
        Ast.AnonymousModule() {
            // Emit definitions for any erased-union arity beyond Fable.Core's U2..U9
            // (recorded while rendering above). Without these, a TS union of 10+
            // members references an undefined `U10`/`U15`/... type.
            for i in erasedUnion.UnionLengths |> Seq.sort do
                SpecialRender.renderErasedUnion i
            renders
        }
    }
    |> Gen.mkOak
    |> Gen.run
    |> printfn "%s"
    // |> Seq.iter (printfn "%A")
    0