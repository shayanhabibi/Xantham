// TEST TARGET: module source name from package.json
//
// Verifies that TsInterface.Source and TsTypeAlias.Source are set to the
// package name from the nearest package.json when processing types in a
// directory that contains a package.json with a "name" field.
// Expected: Source = Some "ui-kit"

export interface Button {
    label: string;
    disabled: boolean;
}

export type ButtonSize = "small" | "medium" | "large";
