import { Internal, Payload } from "./shared.js";
export { Payload, Session } from "./shared.js";
export interface ClientOptions { retries: number; }
export declare function connect(options: ClientOptions): Internal;
export declare function describe(payload: Payload): string;
