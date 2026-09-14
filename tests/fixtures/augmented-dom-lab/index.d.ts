declare global {
    interface Request<T = unknown> { cf?: T; }
    interface RequestInit<T = unknown> { cf?: T; }
}
export declare function request(value: Request<{ foo: string }>, init: RequestInit<{ bar: string }>): typeof fetch;
export declare function plain(): Response;
