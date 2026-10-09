// The checker's error type beside the `any` an author writes. Every member below reaches the
// shape tier flagged Any or Unknown; the intrinsic name the wire reports separates them.

import type { Schema } from "unresolved-any-lab-absent";

export interface Probe {
    // TS2307: the module is absent from the program.
    imported: Schema;
    // TS2304: no declaration of the name.
    undeclared: Undeclared;
    // TS2503: no declaration of the qualifier either.
    qualified: Absent.Timer;
    // The error type at an element position.
    elements: Schema[];
    // A union with an error arm reduces to the checker's nameless error type.
    either: Schema | string;
    written: any;
    omitted;
    opaque: unknown;
}
