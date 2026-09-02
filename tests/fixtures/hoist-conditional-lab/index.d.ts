// `docs/plans/generator-three-rung.md` §11.4. `chain-lab` pins the runaway that
// blocker 1 named and the recogniser that bounds it; this lab pins the one operand
// that defeats the recogniser, and pins it against a control that differs in that
// operand alone.
//
// Reduced from `three`'s `src/nodes/core/Node.d.ts`, whose `Node<TNodeType>`
// intersects `(unknown extends TNodeType ? {} : NodeExtensions<TNodeType>)` into an
// alias body that a `this`-returning member then applies.

// --- The reproducer: the second operand is a conditional deferred on the alias's
// own parameter. ---

export interface CondExtensions<TNodeType> {
    toVar: (name?: string | null) => CondVarNode<TNodeType, this>;
}

export type CondNode<TNodeType> =
    & { readonly isNode: true }
    & (unknown extends TNodeType ? {} : CondExtensions<TNodeType>);

export interface CondVarNodeInterface<TNode> {
    node: TNode;
    readonly isVarNode: true;
}

export type CondVarNode<TNodeType, TNode> = CondNode<TNodeType> & CondVarNodeInterface<TNode>;

export const condSeed: CondNode<number>;

// --- The control: the same five declarations with the conditional's false branch
// written in place of the conditional. ---

export interface DirectExtensions<TNodeType> {
    toVar: (name?: string | null) => DirectVarNode<TNodeType, this>;
}

export type DirectNode<TNodeType> =
    & { readonly isNode: true }
    & DirectExtensions<TNodeType>;

export interface DirectVarNodeInterface<TNode> {
    node: TNode;
    readonly isVarNode: true;
}

export type DirectVarNode<TNodeType, TNode> = DirectNode<TNodeType> & DirectVarNodeInterface<TNode>;

export const directSeed: DirectNode<number>;
