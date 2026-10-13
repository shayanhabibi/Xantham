export declare class IncomingMessage {
    readonly url?: string;
}
export declare class ServerResponse<Request extends IncomingMessage = IncomingMessage> {
    readonly req: Request;
    end(): void;
}

// 1. http.RequestListener: InstanceType over two bounded parameters.
export type RequestListener<
    Request extends typeof IncomingMessage = typeof IncomingMessage,
    Response extends typeof ServerResponse<InstanceType<Request>> = typeof ServerResponse,
> = (req: InstanceType<Request>, res: InstanceType<Response> & { req: InstanceType<Request> }) => void;

// 2. http.Server: a generic class whose members read InstanceType<Request>.
export declare class Server<
    Request extends typeof IncomingMessage = typeof IncomingMessage,
    Response extends typeof ServerResponse<InstanceType<Request>> = typeof ServerResponse,
> {
    constructor(requestListener?: RequestListener<Request, Response>);
    on(event: "request", listener: (req: InstanceType<Request>) => void): this;
}
export declare function createServer<Request extends typeof IncomingMessage = typeof IncomingMessage>(
    listener?: RequestListener<Request>,
): Server<Request>;

// Negatives.
// 3. A concrete argument: the checker resolves it before Resolve reads it.
export type ConcreteListener = (req: InstanceType<typeof IncomingMessage>) => void;
// 4. Unbounded: the only constraint is the union of both branches.
export type Divergent<T> = T extends string ? number : boolean;
export declare function divergent<T>(value: Divergent<T>): void;
// 5. Bounded, but the bound instantiates to any.
export declare function returns<F extends (...args: any) => any>(value: ReturnType<F>): void;
// 6. The alias's own body stays deferred.
export type Instance<C extends typeof IncomingMessage> = C extends abstract new (...args: any) => infer R ? R : never;
