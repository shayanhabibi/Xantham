export interface Message<Meta = unknown> {
    id: string;
    meta: Meta;
}
/** A function alias whose parameter is an anonymous intersection over the alias's parameter. */
export type PrepareRequest<M extends Message> = (options: {
    id: string;
    messages: M[];
} & {
    trigger: "submit" | "regenerate";
}) => { body: object };
export type InitOptions<M extends Message> = {
    api?: string;
    prepare?: PrepareRequest<M>;
};
/** Each class applies the options over its own `M`. */
export declare abstract class HttpTransport<M extends Message> {
    protected api: InitOptions<M>["api"];
    protected prepare?: PrepareRequest<M>;
    constructor(options: InitOptions<M>);
}
export declare class DefaultTransport<M extends Message> extends HttpTransport<M> {
    constructor(options?: InitOptions<M>);
}
/** An owner whose parameter has a different bound keeps its own copy. */
export declare class DirectTransport<M extends Message<string> = Message<string>> {
    protected prepare?: PrepareRequest<M>;
}
