import type { Consumer } from "./consumer";
declare module "./common" { interface A { added: boolean; } }
export interface Augmented { consumer: Consumer; }
