// TEST TARGET: heritage clause (single extends) resolution
//
// Verifies that an extending interface carries the parent's TypeKey in its
// Heritage.Extends list and that the TypeKey resolves to a node in the result.
// Tests check heritage list length and own member presence — not TypeKey values.

export interface BaseInterface {
    parentProp: string;
}

export interface ExtendedInterface extends BaseInterface {
    childProp: number;
}
