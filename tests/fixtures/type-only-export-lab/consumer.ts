import { visible, type Payload, type Message, type Client, type HiddenFunction, hidden } from './index.js';

const payload: Message = { text: 'message' };
const client: Client = visible(payload);
const returned: Payload = client.send(payload);
export function retain(callback: HiddenFunction): HiddenFunction { return callback; }

// @ts-expect-error type-only function exports have no value binding
hidden(returned);
// @ts-expect-error the class instance type does not export its constructor value
new Client(payload);
// @ts-expect-error static calls also require the class value export
Client.create(payload);
