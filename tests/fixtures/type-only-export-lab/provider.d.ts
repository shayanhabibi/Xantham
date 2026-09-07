export interface Payload { text: string; }
export type Message = Payload;
export class Client {
  constructor(payload: Payload);
  send(payload: Payload): Payload;
  static create(payload: Payload): Client;
}
export function hidden(payload: Payload): Payload;
export type HiddenFunction = typeof hidden;
export function visible(payload: Payload): Client;
