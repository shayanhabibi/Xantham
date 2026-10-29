declare module "virtual:backtick-worker" {
    export abstract class Worker {
        constructor();
        abstract "`fetch"(value: string): string;
    }
}
