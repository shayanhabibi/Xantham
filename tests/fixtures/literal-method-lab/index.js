// The runtime behind literal-method-lab: each `on` call records its event and argument count.
class Emitter {
  constructor() { this.listeners = {}; this.calls = []; }
  on(event, listener) { this.calls.push([event, arguments.length]); (this.listeners[event] ??= []).push(listener); return this; }
  once(event, listener) { return this.on(event, listener); }
  emit(event, ...args) { const ls = this.listeners[event] ?? []; for (const l of ls) l(...args); return ls.length > 0; }
}
export const socket = new Emitter();
export const dialog = Object.assign(new Emitter(), { onClose: null });
export const store = { read(key, kind) { return kind === undefined ? `plain:${key}` : `${kind}:${key}`; } };
export const logger = { lastArgs: [], log(...args) { this.lastArgs = args; } };
