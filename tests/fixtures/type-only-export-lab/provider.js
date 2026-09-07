export class Client {
  constructor(payload) { this.payload = payload; }
  send(payload) { return { text: `${this.payload.text}:${payload.text}` }; }
  static create(payload) { return new Client(payload); }
}
export function hidden(payload) { throw new Error("hidden is not a value export of the entry"); }
export function visible(payload) { return new Client(payload); }
