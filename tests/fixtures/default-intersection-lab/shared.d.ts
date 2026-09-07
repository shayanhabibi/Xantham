export interface Socket { send(message: string): void; }
export type State<T> = { readonly [K in keyof T]: T[K] } | null;
export type Connection<T = unknown> = Socket & {
  state: State<T>;
  setState(value: T | ((previous: State<T>) => T)): State<T>;
};
export interface Agent { onConnect(connection: Connection): void; }
