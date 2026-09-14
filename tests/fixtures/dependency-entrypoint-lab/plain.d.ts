export abstract class Plain<T> {
    constructor(seed: T);
    readonly seed: T;
    fetch?(value: T): T;
}
