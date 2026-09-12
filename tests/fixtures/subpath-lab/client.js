export function connect(options) { return { hidden: options.retries }; }
export function describe(payload) { return `client:${payload.value}`; }
