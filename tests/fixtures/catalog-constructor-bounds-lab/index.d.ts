export interface Base { value: string }
export interface Derived extends Base { extra: number }
export type Factory<T extends Base = Base> = { new (): T };
export declare function create<T extends Derived>(factory: Factory<T>): T;
