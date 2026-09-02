// `three`'s TSL shape, reduced: a polymorphic-`this` method whose return type is an
// intersection *containing* `this`, so every application produces a strictly larger
// anonymous type. The shaper hoists each one to a named declaration by appending the
// member name plus `Result`, and that declaration's own `toVar` produces another - 518
// declarations and 369,116 lines on the `three` rung, stopped only by the depth cutoff.
// Reduced from `three`'s `src/nodes/core/VarNode.d.ts`.
export interface NodeExtensions<TNodeType> {
    toVar: (name?: string | null) => VarNode<TNodeType, this>;
}
export type Node<TNodeType> = { readonly isNode: true } & NodeExtensions<TNodeType>;
export interface VarNodeInterface<TNode> {
    node: TNode;
    readonly isVarNode: true;
}
export type VarNode<TNodeType, TNode> = Node<TNodeType> & VarNodeInterface<TNode>;
export declare const seed: Node<number>;

// Negatives: neither of these closes a cycle, so neither may lose its name to a hoist.
export interface Plain<TValue> {
    // A generic method whose return type names an already-declared generic - an
    // application, not a new anonymous shape.
    wrap(value: TValue): VarNodeInterface<TValue>;
    // Polymorphic `this` on its own, with no intersection minting a larger type.
    self(): this;
}
