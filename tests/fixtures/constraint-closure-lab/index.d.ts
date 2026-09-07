export interface Box<T> { value: T; }

export function accept<T, E extends Box<T>>(env: E): void;
export function chain<T, E extends Box<T>, F extends Box<E>>(env: F): void;
export function unused<T, E extends Box<T>>(value: string): void;

export interface Client {
  accept<T, E extends Box<T>>(env: E): void;
}
