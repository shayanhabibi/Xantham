// A parameter's `?` and a property's `?`, so that a test can read the same question off both.
export declare function marked(a: string, b?: string): string;
export declare function unioned(a: string, b: string | undefined): string;

export interface Marked {
    property?: string;
}
