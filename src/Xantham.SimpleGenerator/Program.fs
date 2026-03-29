module Xantham.SimpleGenerator.Program

open System
open System.Linq
open Microsoft.Extensions.Logging
open Xantham.Decoder.Runtime
open Fabulous.AST
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak
open type Fabulous.AST.Ast
open Xantham.Decoder
open Xantham.SimpleGenerator.Generator
open Xantham.SimpleGenerator.KeyNodeHashing

let file = @"C:\Users\shaya\RiderProjects\Partas.TypeScript\src\Xantham.Fable\output.json"

let createXanthamTree = create
let createHashTypeStore (xantham: XanthamTree) =
    KeyResolutionContext.Create(xantham.TryGetTypeForKey, LoggerFactory.Create(fun builder ->
        builder.AddConsole().SetMinimumLevel(LogLevel.Debug) |> ignore
        builder.AddDebug().SetMinimumLevel(LogLevel.Debug) |> ignore
        // _.AddConsole().SetMinimumLevel(LogLevel.Debug) >> ignore
        ).CreateLogger<KeyResolutionContext>())
let processXanthamTreeWithHashTypeStore (xantham: XanthamTree) (ctx: KeyResolutionContext) =
    for typKey in xantham.ExportsDict.Keys.AsSpan() do
        ctx.visitType typKey
        |> ignore
    ctx
    

let processFile file =
    let runtime = Runtime.create file
    let rootPath = Path.Root.ToKey()
    runtime.ExportsAsPathTypes
    |> _.Keys
    |> Seq.toList
    |> List.iter (fun key ->
        ()
        )


// processFile file
// ||> generate 
// |> Gen.mkOak
// |> Gen.run
// |> printfn "%s"

[<EntryPoint>]
let main argv =
    let xanthamTree = createXanthamTree file
    let ctx = KeyResolutionContext.CreateAndResolve xanthamTree
    let genCache = TypeResolver.init ctx
    let exports =
        xanthamTree.ExportsDict.Keys.ToArray()
        |> Array.map (
            HashTypeKey.create
            >> Dictionary.Flip.item ctx.cache.typeToMasterKeys
            )
    exports
    |> Array.iter (GeneratorContext.touch genCache)
    
    // genCache.keyDependencies
    // |> Seq.map (fun kv -> kv.Key, kv.Value.ToArray())
    // |> Seq.toList
    TypeResolver.renderSourceTree genCache exports
    |> TypeResolver.buildSource genCache
    |> Gen.mkOak
    |> Gen.run
    |> printfn "%A"
        
    // ctx.cache.typeToMasterKeys[5123 |> HashTypeKey.create]
    // |> GeneratorContext.visit genCache
    // |> printfn "%A"
    // // |> Dictionary.Flip.item ctx.cache.masters
    // // |> genCache.GetTypeRender
    // // |> function
    // //     | ValueSome pathRender ->
    // //         match pathRender with
    // //         | Pathed pathedRender ->
    // //             match pathedRender.Render with
    // //             | LiteralUnion render
    // //             | Enum render ->
    // //                 EnumRender.toTypeDefn render
    // // |> TypeRefRender.toWidget ctx
    // // |> fun typ ->
    // //     Oak() {
    // //         AnonymousModule() {
    // //             typ
    // //             // Value("_", "()", typ)
    // //         }
    // //     }
    // //     |> Gen.mkOak
    // //     |> Gen.run
    // // ctx.cache.typeToMasterKeys[5122 |> HashTypeKey.create]
    // // |> GeneratorContext.getTypeRef genCache
    // // |> fun fn ->
    // //     fn KeyPathKind.emptyModule
    // // |> TypeRefRender.toWidget genCache KeyPathKind.emptyModule
    // // |> ValueOption.map (fun fn -> fn KeyPathKind.emptyModule)
    // let makeTypeDefn key =
    //     ctx.cache.typeToMasterKeys[key |> HashTypeKey.create]
    //     |> GeneratorContext.tryGetTypeRender genCache
    //     |> function
    //         | ValueSome (Pathed { Render = render; Path = path }) ->
    //             match render KeyPathKind.emptyModule with
    //             | TypeRender.Interface value ->
    //                 InterfaceRender.renderInterfaceTypeDefn genCache value path
    //         | value -> failwithf $"%A{value}"
    // let makeTypeDefnFromMasterKey key =
    //     GeneratorContext.tryGetTypeRender genCache key
    //     |> function
    //         | ValueSome (Pathed { Render = render; Path = path }) ->
    //             match render KeyPathKind.emptyModule with
    //             | TypeRender.Interface value ->
    //                 InterfaceRender.renderInterfaceTypeDefn genCache value path
    //             | Class classRender ->
    //                 ClassRender.renderTypeDefn genCache classRender path
    //             | LiteralUnion unionRender -> EnumRender.toTypeDefn unionRender
    //             | Enum unionRender -> EnumRender.toTypeDefn unionRender
    //             | ErasedUnion erasedUnionRender ->
    //                 Ast.Abbrev(
    //                     "ErasedUnion",
    //                     ErasedUnionRender.toWidget genCache erasedUnionRender path
    //                     )
    //             | TypeLiteral typeLiteralRender ->
    //                 Ast.TypeDefn("TypeLiteral") {
    //                     yield! TypeLiteralRender.collectMembers genCache typeLiteralRender path
    //                 }
    //             | Variable variableRender ->
    //                 Ast.TypeDefn("Variable") {
    //                     VariableRender.renderAbstract genCache variableRender path
    //                 }
    //             | TypeParameter typeParameterRender ->
    //                 Ast.Abbrev("Typar", TypeParameterRender.toWidget typeParameterRender)
    //             | Function functionRender ->
    //                 Ast.TypeDefn("Function") {
    //                     FunctionRender.renderStaticAbstract genCache functionRender path
    //                 }
    //             | TypeAlias typeAliasRender -> Ast.Abbrev("TypeAlias", "unit")
    //             | TypeReference typeReferenceRender -> Ast.Abbrev("TypeReference", TypeReferenceRender.toWidget genCache typeReferenceRender path)
    //             | Tuple tupleRender ->
    //                 TupleRender.toTypeDefnAbbrev genCache tupleRender path
    //             |> ValueSome
    //         | value -> ValueNone
    // // |> fun typ ->
    // Oak() {
    //     AnonymousModule() {
    //         for key in exports do
    //             match makeTypeDefnFromMasterKey key with
    //             | ValueSome typ -> typ
    //             | _ -> ()
    //         makeTypeDefn 5004
    //         makeTypeDefn 5122
    //         // Value("_", "()", typ)
    //     }
    // }
    // |> Gen.mkOak
    // |> Gen.run
    // |> printfn "%A"
    0