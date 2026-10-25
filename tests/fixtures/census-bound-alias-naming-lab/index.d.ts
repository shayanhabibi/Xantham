type Env = { Bindings?: object; Variables?: object; };
type OtherEnv = { Bindings?: object; Variables?: object; };
interface Holder {
    get<E extends Env>(value: E): E;
    inlineBound<T extends { value: string; }>(value: T): T;
}
