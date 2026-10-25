/// <reference types="ns-owner-lab" />

declare global {
    /** The dependency owns this namespace; this package contributes an augmentation. */
    namespace NodeJS {
        interface PkgAdded { y: number }
    }
    /** This package owns this namespace. */
    namespace PkgNs {
        interface Inner { z: number }
    }
}

export declare function env(): NodeJS.ProcessEnv;
export declare function box(): NodeJS.Deep.Box<string>;
export declare function shape(): NodeJS.Deep.Shape<number>;
