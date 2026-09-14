export type Context<P = unknown> = { name: string } & (undefined extends P ? { params?: P } : { params: P });
export type Handler<E = unknown, P = unknown> = { bivarianceHack(env: E, ctx: Context<P>): void }['bivarianceHack'];
export type Handlers<Params extends Record<string, unknown>, E = unknown> = { [Method in keyof Params]?: Handler<E, Params[Method]> };
