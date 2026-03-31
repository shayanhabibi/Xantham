// TEST TARGET: basic interface and type alias emission
//
// Verifies that a top-level exported type alias and interface are both present
// in the result, that the interface has the correct property names, and that
// the total count of interface/alias nodes is correct.

export type BaseObject = {
    name: string;
}

export interface BaseInterface {
    name: string;
    age: number;
}
