export type LocaleMatcher = "best fit" | "lookup";
export interface FirstOptions { localeMatcher?: LocaleMatcher; other?: string; }
export interface SecondOptions { localeMatcher?: "best fit" | "lookup"; other?: number; }
export interface First { supportedLocalesOf(options?: Pick<FirstOptions, "localeMatcher">): string[]; }
export interface Second { supportedLocalesOf(options?: Pick<SecondOptions, "localeMatcher">): string[]; }
