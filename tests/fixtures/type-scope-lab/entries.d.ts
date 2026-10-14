export interface Entry { readonly id: string; }
export interface TaggedEntry extends Entry { readonly tag: string; }
/** Satisfies Entry structurally, inherits nothing. */
export interface EntryLike { readonly id: string; }
export interface EntryList<T extends Entry> { readonly length: number; item(index: number): T; }
export interface Shape { area(): number; }
