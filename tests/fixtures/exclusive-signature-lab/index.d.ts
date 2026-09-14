export interface UrlPage { url: string; html?: never; }
export interface HtmlPage { url?: never; html: string; }
export type Page = UrlPage | HtmlPage;
export declare function render(page: Page): string;
