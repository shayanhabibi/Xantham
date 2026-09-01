namespace Xantham.TypeScript

open System.ComponentModel
open Fable.Core
open TypeScript

type SymbolKey = Xantham.SymbolKey
type NodeKey = Xantham.NodeKey
type TypeKey = Xantham.TypeKey

type CompositeKey = private {
    Symbol: SymbolKey option
    Node: NodeKey option
    Type: TypeKey option
} with
    static member Create(?symbol, ?node, ?typ) = { Symbol = symbol; Node = node; Type = typ }
    member this.symbolKey = this.Symbol
    member this.nodeKey = this.Node
    member this.typeKey = this.Type

module CompositeKey =
    module Builder =
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        type Builder = {
            Symbol: SymbolKey option
            Node: NodeKey option
            Type: TypeKey option
        }
        let private empty = {
            Symbol = None
            Node = None
            Type = None
        }
        let inline withSymbol symbol builder = { builder with Builder.Symbol = Some symbol }
        let inline withNode node builder = { builder with Builder.Node = Some node }
        let inline withType typ builder = { builder with Builder.Type = Some typ }
        let build builder: CompositeKey = CompositeKey.Create(?symbol = builder.Symbol, ?node = builder.Node, ?typ = builder.Type)
        
        let inline initWithSymbol symbol = withSymbol symbol empty
        let inline initWithNode node = withNode node empty
        let inline initWithType typ = withType typ empty
        
        [<EditorBrowsable(EditorBrowsableState.Never)>]
        type SRTPHelper =
            static member inline init key = initWithSymbol key
            static member inline init key = initWithNode key
            static member inline init key = initWithType key
            static member inline with'(key, builder) = withSymbol key builder
            static member inline with'(key, builder) = withNode key builder
            static member inline with'(key, builder) = withType key builder
            
        let inline initWith key = ((^T or SRTPHelper):(static member init: ^T -> Builder) key)
        let inline withKey key builder = ((^T or SRTPHelper):(static member with': ^T -> Builder -> Builder) (key, builder))
    let inline symbol (compositeKey: CompositeKey) = compositeKey.symbolKey
    let inline node (compositeKey: CompositeKey) = compositeKey.nodeKey
    let inline type' (compositeKey: CompositeKey) = compositeKey.typeKey
    let type_ = type'
    let inline createSymbol symbol = CompositeKey.Create(symbol = symbol)
    let inline createNode node = CompositeKey.Create(node = node)
    let inline createType typ = CompositeKey.Create(typ = typ)
    let inline createSymbolAndNode symbol node = CompositeKey.Create(symbol = symbol, node = node)
    let inline createSymbolAndType symbol typ = CompositeKey.Create(symbol = symbol, typ = typ)
    let inline createNodeAndType node typ = CompositeKey.Create(node = node, typ = typ)
    let inline createSymbolAndNodeAndType symbol node typ = CompositeKey.Create(symbol = symbol, node = node, typ = typ)

module NodeKey =
    let inline fromNode(node: Ts.Node): NodeKey = ts.getNodeId node |> NodeKey.Create
    let inline fromINode(node: INode): NodeKey = ts.getNodeId node |> NodeKey.Create
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline get node = fromNode node
        static member inline get iNode = fromINode iNode
    let inline get node = ((^T or SRTPHelper):(static member get: ^T -> NodeKey) node)
    
module SymbolKey =
    let inline fromSymbol(symbol: Ts.Symbol): SymbolKey = ts.getSymbolId symbol |> SymbolKey.Create
    let inline fromISymbol(symbol: ISymbol): SymbolKey = ts.getSymbolId symbol |> SymbolKey.Create
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline get node = fromSymbol node
        static member inline get iNode = fromISymbol iNode
    let inline get node = ((^T or SRTPHelper):(static member get: ^T -> SymbolKey) node)

module TypeKey =
    let inline fromType(typ: Ts.Type): TypeKey = typ.id |> TypeKey.Create
    let inline fromIType(typ: IType): TypeKey = fromType(IType.op_Implicit typ)
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type SRTPHelper =
        static member inline get node = fromType node
        static member inline get iNode = fromIType iNode
    let inline get node = ((^T or SRTPHelper):(static member get: ^T -> TypeKey) node)
    
[<AutoOpen>]
module UniqueIdentifierExtensions =
    type Ts.Type with member inline this.TypeKey = TypeKey.get this
    type Ts.Symbol with member inline this.SymbolKey = SymbolKey.get this
    type Ts.Node with member inline this.NodeKey = NodeKey.get this

