// TEST TARGET: leaf of transitive import chain
//
// Imported by views/dashboard.d.ts as "../components/widget".
// No imports of its own.

export interface Widget {
    id: string;
    render(): void;
}

export type WidgetSize = "small" | "medium" | "large";
