import { Model, Options } from "./model";

export declare class Client implements Model<string> {
    generate(options: Options): string;
}

export declare class GenericClient<T extends Model<string>> implements Model<T> {
    generate(options: Options): T;
}

export declare class StructuralClient {
    generate(options: Options): string;
}
