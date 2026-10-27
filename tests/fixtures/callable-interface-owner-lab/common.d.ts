export interface IssueBase { code: string; }
export interface Issue extends IssueBase { message: string; }
export type RawIssue<T extends IssueBase = Issue> = T extends any ? Omit<T, "message"> & { extra?: unknown } : never;
export interface ErrorMap<T extends IssueBase = Issue> {
    (issue: RawIssue<T>): { message: string } | string | undefined | null;
}
export interface ParseContext<T extends IssueBase = never> { readonly error?: ErrorMap<T>; readonly jitless?: boolean; }
export interface ParseContextInternal<T extends IssueBase = never> extends ParseContext<T> { readonly async?: boolean; }
export interface Schema { run(ctx: ParseContextInternal): void; }
