import { Model, Part } from "alias-api-owner-lab";
export declare class Client {
    accept(part: Part): Part;
    generate(options: Parameters<Model["generate"]>[0]): Promise<Part>;
}
