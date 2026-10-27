export declare const Kind: { a: "a"; b: "b" };
export interface Issue { kind: keyof typeof Kind; path: string[]; message: string; }
export type Stripped = Omit<Issue, "path"> & { fatal?: boolean };
export interface Holder { issue: Stripped; }
