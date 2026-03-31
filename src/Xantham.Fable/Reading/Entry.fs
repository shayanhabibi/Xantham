module Xantham.Fable.Reading.Entry

open Xantham
open Xantham.Fable
open Xantham.Fable.Types
open TypeScript
open Fable.Core.JsInterop
open Fable.Core
open Xantham.Fable.Types.Signal

let inline private getText mapping (tag: ^T when ^T:(member comment: Option<U2<string, ResizeArray<Ts.JSDocComment>>>)) =
    tag.comment
    |> Option.bind (fun comment -> ts.getTextOfJSDocComment comment)
    |> Option.map mapping

let inline isModuleFileFast (sourceFile: Ts.SourceFile): bool =
    (sourceFile?externalModuleIndicator && true)
let inline isModuleFileStable (sourceFile: Ts.SourceFile) =
    sourceFile.statements.AsArray
    |> Array.exists (function
        | Patterns.Node.ImportDeclaration _
        | Patterns.Node.ExportDeclaration _
        | Patterns.Node.ExportAssignment _ -> true
        | _ -> false
        )

let isModuleFile (sourceFile: Ts.SourceFile) =
    isModuleFileFast sourceFile || isModuleFileStable sourceFile
let isAmbientFile: Ts.SourceFile -> _ = isModuleFile >> not 
let expandDeclarations (ctx: TypeScriptReader) (declarations: XanthamTag array): XanthamTag array =
    [|
        for declaration in declarations do
            let decl =
                match declaration.Value with
                | TypeDeclaration (TypeDeclaration.Interface interfaceDeclaration) -> interfaceDeclaration :> Ts.Node |> Some
                | TypeDeclaration (TypeDeclaration.FunctionDeclaration functionDeclaration) -> functionDeclaration :> Ts.Node |> Some
                | _ -> None
            if decl.IsNone then
                yield declaration
            else
            let decl = decl.Value
            yield!
                ctx.checker.getSymbolAtLocation decl
                |> Option.orElseWith (fun () ->
                    ctx.checker.getTypeAtLocation decl
                    |> fun symb ->
                        symb.aliasSymbol
                        |> Option.orElse (symb.getSymbol())
                    )
                |> Option.bind _.getDeclarations()
                |> Option.map (
                    _.AsArray
                    >> Array.map (
                        ctx.CreateXanthamTag
                        >> fst >> TagState.value)
                    )
                |> Option.defaultValue [| declaration |]
    |]

let getDeclarations (ctx: TypeScriptReader) (sourceFile: Ts.SourceFile) =
    let checker = ctx.checker
    if isModuleFile sourceFile then
        sourceFile
        |> checker.getSymbolAtLocation
        |> Option.map (
            ctx.checker.getExportsOfModule
            >> _.AsArray
            >> Array.choose (fun (sym: Ts.Symbol) ->
                sym.declarations
                |> Option.bind (fun decls -> decls |> Seq.tryHead)
                |> Option.map (fun decl -> ctx.CreateXanthamTag(decl) |> fst |> TagState.value))
            >> expandDeclarations ctx
            )
        |> Option.defaultWith(fun () -> failwith "Source file exported/declared no known declarations")
    else
        sourceFile.statements.AsArray
        |> Array.filter ts.isDeclarationStatement
        |> Array.map (fun statement -> XanthamTag.Create(statement, checker) |> fst |> TagState.value)

let inline setDeclarationsNodeBuilderSignal (declarations: XanthamTag array) =
    declarations
    |> Array.iter (fun tag -> GuardedData.AstNodeBuilder.getOrSetDefault tag |> ignore)
    declarations

let inline pushDeclarationsToStack (ctx: TypeScriptReader) (declarations: XanthamTag array) =
    declarations
    // push in reverse order
    |> Array.revApply ctx.stack.Push
    
    