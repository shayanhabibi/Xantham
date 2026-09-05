export interface Div {
    div: string;
}

export interface Span {
    span: string;
}

export interface HtmlTags {
    div: Div;
    span: Span;
}

export interface SvgTags {
    circle: Div;
    rect: Span;
}

export interface Finder {
    /** Separated by which key set the parameter is bounded to, and by nothing else. */
    find<K extends keyof HtmlTags>(selector: K): HtmlTags[K];
    find<K extends keyof SvgTags>(selector: K): SvgTags[K];
    find(selector: string): Div;

    /** The negative: a nominal bound, erased the same way, still reports a plain drop. */
    pick<T extends Div>(value: T): void;
    pick<T extends Span>(value: T): void;
}
