declare module "implements-lab:runtime" {
    export interface Contract {
        run(): string;
    }

    export abstract class Station implements Contract {
        constructor(label: string);
        run(): string;
        fetch?(): string;
    }

    export abstract class Halt extends Error implements Contract {
        constructor(message: string);
        run(): string;
    }

    export class Client implements Station {
        run(): string;
    }
}
