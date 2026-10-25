import type { Consumer } from "./consumer";
declare module "./common" { interface Base { added: boolean; } }
export interface Augmented { consumer: Consumer; }
