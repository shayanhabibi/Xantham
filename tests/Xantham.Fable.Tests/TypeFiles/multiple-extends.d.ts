// TEST TARGET: multiple interfaces extending a common base
//
// Verifies that all heritage TypeKeys produced by ExtendedInterface and
// ExtendedInterface2 resolve to nodes in the result and that both extending
// interfaces are emitted with their own members intact.

export interface BaseInterface {
    parentProp: string;
}

export interface ExtendedInterface extends BaseInterface {
    childProp: number;
}

export interface ExtendedInterface2 extends BaseInterface {
    childProp2: number;
}
