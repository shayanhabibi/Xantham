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
// 6. A union bound: the value at the bound is the union of each member's branch.
export declare class Outgoing {
    readonly sent: boolean;
}
export declare function either<C extends typeof IncomingMessage | typeof Outgoing>(value: InstanceType<C>): void;
// 7. The alias's own body stays deferred.
export type Instance<C extends typeof IncomingMessage> = C extends abstract new (...args: any) => infer R ? R : never;
// 8. An application of that alias over a bounded parameter: the taken branch is the `infer` variable.
export declare function instance<C extends typeof IncomingMessage>(value: Instance<C>): void;
// 9. A bound whose value is `boolean`: the one union the bound rung accepts.
export declare function flag<T extends boolean>(value: T extends string ? number : T): void;
// 10. A bound whose value is one of the conditional's own branches.
export declare function same<T extends number>(value: T extends string ? boolean : number): void;
