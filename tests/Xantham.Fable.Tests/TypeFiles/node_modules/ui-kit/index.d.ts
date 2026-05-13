// TEST TARGET: module source name from package.json
//
// Verifies that Source is set to the package name from the nearest
// package.json for all exported declaration kinds.
// Expected: Source = Some "ui-kit"

export interface Button {
    label: string;
    disabled: boolean;
}

export type ButtonSize = "small" | "medium" | "large";

export declare function createButton(size: ButtonSize): Button;

export declare const DEFAULT_SIZE: ButtonSize;

export declare enum ButtonVariant {
    Primary = 0,
    Secondary = 1,
}

export declare class ButtonGroup {
    buttons: Button[];
}
