// The runtime behind `declare module "ambient-lab:runtime"`, declared `export = AmbientLabRuntime`.
// The namespace is this module's body, so its members are reachable through the specifier alone.

export const version = "1.4.0";

export class Session {
    constructor(label) {
        this.label = label;
    }
}
