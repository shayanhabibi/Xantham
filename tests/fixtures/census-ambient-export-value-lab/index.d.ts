/// <reference path="./globals.d.ts" />
declare module "census-root:direct" {
    const state: {
        value: string;
        readonly marker: string;
        update(value: string): string;
    };
    export = state;
}

declare const censusGlobalMarker: string;
declare const sharedRoot: { value: string };

declare module "census-root:shared-global" {
    export = sharedRoot;
}


declare module "census-root:alias" {
    import forwarded = require("census-root:direct");
    export = forwarded;
}

declare namespace LegacyNamespace {
    const version: string;
}

declare module "census-root:namespace" {
    export = LegacyNamespace;
}

declare module "census-root:class" {
    class LegacyClass { readonly label: string; }
    namespace LegacyClass { export { LegacyClass }; }
    export = LegacyClass;
}

declare module "census-root:empty" {}

declare module "census-root:augmented-global" {
    export = augmentedRoot;
}
