export interface Upload {
    // selected, nullable
    onprogress: ((this: Upload, ev: ProgressEvent) => any) | null;
    // selected, optional, no argument
    onready?: () => any;
    // unselected name
    transform: (ev: ProgressEvent) => any;
    // selected name, not a callback
    onstate: any;
    // a nested callback's return stays obj
    onregister: (resolve: (value: string) => any) => void;
    // a method's return stays obj
    onclose(ev: ProgressEvent): any;
    // a named alias is a shared declaration and stays obj
    onnamed: Handler;
    // a two-argument callback hoists to a delegate declaration and stays obj
    onpair: (a: ProgressEvent, b: string) => any;
    // selected, a callback arm of an erased union
    onmixed: ((ev: ProgressEvent) => any) | string;
}
export type Handler = (ev: ProgressEvent) => any;
// selected value export
export declare let onload: ((ev: ProgressEvent) => any) | null;
