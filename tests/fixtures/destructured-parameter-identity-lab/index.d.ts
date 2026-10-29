export interface Tool { name: string; }
export interface Mapping { toProvider(name: string): string; }
export declare function createToolNameMapping({ tools, providerToolNames }: {
    tools: Tool[] | undefined;
    providerToolNames: Record<`${string}.${string}`, string>;
}): Mapping;
export declare function copyToolNameMapping({ tools, providerToolNames }: {
    tools: Tool[] | undefined;
    providerToolNames: Record<`${string}.${string}`, string>;
}): Mapping;
export declare function positions({ left }: { left: Record<`${string}.${string}`, string> }, { right }: { right: Record<`${string}.${string}`, string> }): { output: Record<`${string}.${string}`, string> };
export declare function overloaded({ strings }: { strings: Record<`${string}.${string}`, string> }): string;
export declare function overloaded({ counts }: { counts: Record<`${string}.${string}`, number> }): number;
export declare function genericMapping<T extends Tool>({ tools, providerToolNames }: {
    tools: T[];
    providerToolNames: Record<`${string}.${string}`, string>;
}): T;
