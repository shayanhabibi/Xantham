// TEST TARGET: sub-package with its own package.json
//
// package.json name: "@app/framework-plugins"
// This subfolder is nested inside the framework package but has its own identity.
// When loaded as entry file directly, Source = "@app/framework-plugins".
// When loaded via import from parent, Source = import specifier.

export interface Plugin {
    name: string;
    init(): void;
}

export type PluginFactory = () => Plugin;
