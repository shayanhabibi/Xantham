export interface Socket { send(message: string): void; }
export type Connection<T = unknown> = Socket & { state: T; setState(value: T | ((previous: T) => T)): T; };
export interface Agent { onConnect(connection: Connection): void; }
