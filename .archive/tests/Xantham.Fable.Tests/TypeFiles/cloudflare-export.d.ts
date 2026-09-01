declare module "cloudflare:sockets" {
    function _connect(
        address: string | any,
        options?: any,
    ): any;
    export { _connect as connect };
}