import type { Tool, ToolCallerDefinition } from "alias-defaults-owner-lab";
export type { Tool };
export type Described<T extends Tool> = { tool: T };
export declare function use(definition: ToolCallerDefinition): void;
