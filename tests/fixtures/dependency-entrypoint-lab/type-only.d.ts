declare namespace TypeOnlyBody {
    export abstract class Hidden<T> {
        constructor(seed: T);
        readonly seed: T;
        fetch?(value: T): T;
    }
}
declare module "entrypoint-lab:types" {
    import Hidden = TypeOnlyBody.Hidden;
    export type { Hidden };
}
