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
                     // Faithful TS-stdlib (lib.es / lib.dom) -> F#/Fable mappings. These
                     // names have no F# definition in the emitted surface, so a by-name
                     // RefOnly reference dangles. Substitute the real Fable equivalent
                     // (the prefix only — any type arguments are applied by the caller, so
                     // `PromiseLike<T>` -> `Promise<'T>` is preserved). `obj` is used ONLY
                     // for genuinely-dynamic `Function`; every other entry is a real type.
                     let libEsSubstitution (name: string) =
                         match name with
                         | "Error" -> Some "exn"
                         | "PromiseLike" -> Some "Promise"               // Fable.Core.JS.Promise (open Fable.Core.JS)
                         | "IterableIterator" | "Iterator"
                         | "ArrayIterator" | "AsyncIterableIterator" -> Some "seq"
                         | "ReadonlyArray" -> Some "System.Collections.Generic.IReadOnlyList"
                         | "Function" | "CallableFunction" | "NewableFunction" -> Some "obj"
                         | _ -> None
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