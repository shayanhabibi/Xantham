export type RootAlias = HTMLElement;
export interface Input extends HTMLInputElement { custom?: string }
export interface Grandchild extends Input { extra: string }
export namespace Impostors {
    export interface HTMLElement { title: string; tagName: string }
    export interface Child extends HTMLElement { value: string }
}
export interface Lookalike { title: string; tagName: string; value: string }
export declare class ImplementsOnly implements Lookalike { title: string; tagName: string; value: string }
