export interface Elem {
    readonly tagName: string;
}
export interface DivElem extends Elem {
    align: string;
}
export interface CanvasElem extends Elem {
    width: number;
}
export interface MarqueeElem extends Elem {
    scrollAmount: number;
}
export interface CircleElem extends Elem {
    r: number;
}
export interface PathElem extends Elem {
    d: string;
}
/** A value that does not inherit Elem: the constraint is not carried over this map. */
export interface Loose {
    tagName: string;
}

export interface TagMap {
    div: DivElem;
    canvas: CanvasElem;
}
export interface DeprecatedTagMap {
    marquee: MarqueeElem;
}
export interface SvgTagMap {
    circle: CircleElem;
    path: PathElem;
}
export interface LooseTagMap {
    loose: Loose;
}

export interface ElemList<T extends Elem> {
    readonly length: number;
    item(index: number): T | null;
}
export interface CreateOptions {
    is?: string;
}

export interface Doc {
    createElement<K extends keyof TagMap>(tagName: K, options?: CreateOptions): TagMap[K];
    /** @deprecated */
    createElement<K extends keyof DeprecatedTagMap>(tagName: K, options?: CreateOptions): DeprecatedTagMap[K];
    createElement(tagName: string, options?: CreateOptions): Elem;
    getElementsByTagName<K extends keyof TagMap>(qualifiedName: K): ElemList<TagMap[K]>;
    getElementsByTagName<K extends keyof SvgTagMap>(qualifiedName: K): ElemList<SvgTagMap[K]>;
    getElementsByTagName(qualifiedName: string): ElemList<Elem>;
    /** One key overload: the map is inferred at the call. */
    createSvg<K extends keyof SvgTagMap>(tagName: K): SvgTagMap[K];
    /** A key used only as a key. */
    hasTag<K extends keyof TagMap>(tag: K): boolean;
    /** A map whose values do not inherit the bound: the result stays unconstrained and widens. */
    findLoose<K extends keyof LooseTagMap>(tag: K): ElemList<LooseTagMap[K]>;
    /** A map the compiler lib declares: its values are catalogued rather than derived. */
    libElements<K extends keyof HTMLElementTagNameMap>(qualifiedName: K): HTMLCollectionOf<HTMLElementTagNameMap[K]>;
}
export declare const doc: Doc;

export type ValueOf<ObjectType, ValueType extends keyof ObjectType = keyof ObjectType> = ObjectType[ValueType];
export type ValuesOf<T> = T[keyof T];
/** The negative: a key parameter with no default selects a narrower set than proptypelock states. */
export type Picked<T, K extends keyof T> = T[K];
export declare function valuesOf<T>(source: T): T[keyof T][];

/** A mapped type whose value does not read the key. */
export type Stringified<T> = { [P in keyof T]: string };
/** The negative: a value that reads the key has no single value type. */
export type Mirrored<T> = { [P in keyof T]: T[P] };
