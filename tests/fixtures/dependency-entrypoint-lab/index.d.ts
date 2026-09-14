declare namespace EntrypointRuntime {
    export interface Branded { readonly brand: void; }
    export abstract class Actor<T = unknown> implements Branded {
        readonly brand: void;
        constructor(options: { seed: T });
        readonly seed: T;
        fetch?(value: T): T;
    }
}
declare module "entrypoint-lab:runtime" {
    export = EntrypointRuntime;
}
