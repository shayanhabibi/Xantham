// TEST TARGET: nested sub-package — framework imports from its own plugins/ subfolder
//
// package.json name: "@app/framework"
// The plugins/ subfolder has its OWN package.json with name "@app/framework-plugins".
// Framework types should get Source = "@app/framework" (fallback to own package.json).
// Plugin types should get Source = import specifier "./plugins/index".

import { Plugin } from "./plugins/index";

export interface Framework {
    name: string;
    version: string;
}

export interface FrameworkConfig {
    plugins: Plugin[];
    debug: boolean;
}
