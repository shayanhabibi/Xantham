[<AutoOpen>]
module Xantham.Fable.Types.AutoOpenReaderExtensions

open TypeScript
open Xantham.Fable
open Xantham.Fable.Types.SourceTag

type TypeScriptReader with
    member this.CreateXanthamTag(node: Ts.Node) =
        XanthamTag.Create(node, this.checker)
    member this.CreateXanthamTag(typ: Ts.Type) =
        XanthamTag.Create(typ, this.checker)

module XanthamTag =
    let createForNode (ctx: TypeScriptReader) (node: Ts.Node) = ctx.CreateXanthamTag node
    let createForType (ctx: TypeScriptReader) (node: Ts.Type) = ctx.CreateXanthamTag node
    
type TypeScriptReader with
    member this.CreateSourceTag(node: Ts.Declaration) =
        match node with
        | Patterns.Node.SourceFile sourceFile -> SourceTag.Create(this.program, sourceFile)
        | node -> SourceTag.Create(this.program, node.getSourceFile())
    member this.CreateSourceTag(node: Ts.SourceFile) =
        SourceTag.Create(this.program, node)
    member this.CreateSourceTag(kind: TypeDeclaration) =
        SourceTag.Create(this.program, kind.Value.getSourceFile())
    member this.CreateSourceTagValue(node: Ts.Declaration) =
        match node with
        | Patterns.Node.SourceFile sourceFile -> SourceTag.CreateValue(this.program, sourceFile)
        | node -> SourceTag.CreateValue(this.program, node.getSourceFile())
    member this.CreateSourceTagValue(node: Ts.SourceFile) =
        SourceTag.CreateValue(this.program, node)
    member this.CreateSourceTagValue(kind: TypeDeclaration) =
        SourceTag.CreateValue(this.program, kind.Value.getSourceFile())
        
module SourceTag =
    let createForSourceFile (ctx: TypeScriptReader) (sourceFile: Ts.SourceFile) =
        ctx.CreateSourceTag sourceFile
    let createForDeclaration (ctx: TypeScriptReader) (node: Ts.Declaration) =
        ctx.CreateSourceTag node
    let createForTypeDeclaration (ctx: TypeScriptReader) (node: TypeDeclaration) =
        ctx.CreateSourceTag node
    let createValueForSourceFile (ctx: TypeScriptReader) (sourceFile: Ts.SourceFile) =
        ctx.CreateSourceTagValue sourceFile
    let createValueForDeclaration (ctx: TypeScriptReader) (node: Ts.Declaration) =
        ctx.CreateSourceTagValue node
    let createValueForTypeDeclaration (ctx: TypeScriptReader) (node: TypeDeclaration) =
        ctx.CreateSourceTagValue node
    
    

