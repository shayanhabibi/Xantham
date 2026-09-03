// The runtime behind `declare module "ambient-lab:sockets"`, whose declaration exports
// `_connect` under the name `connect`: the import selector is the exported name.

export function connect(label) {
    return { label, at: { x: 1 } };
}
