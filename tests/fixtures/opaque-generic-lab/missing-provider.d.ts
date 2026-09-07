export interface Target<Value = unknown> { run(): Value; }
export interface Options<T extends Target = Target> { target: MissingNamespace<T>; name: string; }
export type Transport = (Options | { endpoint: string }) & { secure?: boolean };
declare class Connection { options: { transport: Transport }; }
declare class Manager { connections: Record<string, Connection>; }
export class Agent { readonly manager: Manager; }
