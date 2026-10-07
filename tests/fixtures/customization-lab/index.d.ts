export interface Properties<T> { value: T; readonly stamp: string }
export interface Named { title: string }
export interface Input extends Properties<string>, Named { disabled: boolean }
export interface OddKeys { "aria-label": string; "quote\"key": string }
export interface Left extends Properties<string> {}
export interface Right extends Properties<string> {}
export interface Diamond extends Left, Right { optional?: string }
export interface Override extends Properties<string> { value: "chosen" }
