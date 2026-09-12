// Global script: ambient modules are harvested together.
declare module "static-reexport-lab" {
    export class Certificate {
        constructor();
        verify(spkac: string): boolean;
        static exportChallenge(spkac: string): string;
    }
}
declare module "node:static-reexport-lab" {
    export * from "static-reexport-lab";
}
