// No import and no export at the top level of this file: a global script, so the ambient module
// below is harvested by specifier beside the globals.

/** A global interface holding the name the exported class below also carries. The class's
 * instance side is declared under a renamed name, and its statics have to reach *that*
 * declaration rather than this one. */
declare interface Depot {
    slot: string;
}

declare module "statics-lab:depot" {
    /** An exported class sharing its name with the global interface above. */
    export class Depot {
        constructor(slot: string);
        readonly slot: string;
        static readonly LIMIT: number;
        static open(slot: string): Depot;
    }
}
