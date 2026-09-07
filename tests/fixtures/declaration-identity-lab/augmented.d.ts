import "./adapter.js";
declare module "./shared.js" {
  interface Client { extra: string; }
}
export { Client, Box, Item } from "./shared.js";
