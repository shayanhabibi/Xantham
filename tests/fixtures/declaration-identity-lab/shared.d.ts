export declare class Client {
  send(message: string): void;
}
export interface Item { id: string; }
export interface Box<T extends Item> {
  value: T;
  options: { retry: boolean; };
}
export type Config<T> = { value: T };
export type TextConfig = Config<string>;
export interface Holder { config: Config<string>; }
export declare class GenericClient<T> {
  constructor(value: T);
  value: T;
}
export type StringConstructor = typeof GenericClient<string>;
export interface Left { left: string; }
export interface Right { right: number; }
export type Callback<T> = { run<U extends T>(value: U): U };
export type LeftCallback = Callback<Left>;
export type RightCallback = Callback<Right>;
