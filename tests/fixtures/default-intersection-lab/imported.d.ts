import { Connection } from "./shared.js";
export interface ImportedAgent { onConnect(connection: Connection): void; }
