declare namespace ClassRuntime {
    export abstract class Actor<T = unknown> {
        constructor(seed: T);
        readonly seed: T;
        fetch?(value: T): T;
    }
}
declare module "class-lab:runtime" { export = ClassRuntime; }
