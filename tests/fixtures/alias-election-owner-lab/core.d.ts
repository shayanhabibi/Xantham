export interface IssueBase { code: string; }
export interface IssueA extends IssueBase { code: "a"; a: number; }
export interface IssueB extends IssueBase { code: "b"; b: string; }
export type Issue = IssueA | IssueB;
type RawOf<T extends IssueBase> = T extends any ? Omit<T, "code"> & { code: T["code"]; extra?: unknown } : never;
export type InternalIssue<T extends IssueBase = Issue> = T extends any ? RawOf<T> : never;
export type RawIssue<T extends IssueBase = Issue> = InternalIssue<T>;
export interface ParsePayload<T = unknown> { value: T; issues: RawIssue[]; }
export interface Schema { run(payload: ParsePayload): ParsePayload; }
export interface TextContent { type: "text"; text: string; }
export interface ToolCall { type: "toolCall"; name: string; }
export interface AssistantMessage { content: (TextContent | ToolCall)[]; }
