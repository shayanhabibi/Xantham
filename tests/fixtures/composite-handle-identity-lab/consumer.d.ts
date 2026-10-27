import type { Base, A, C } from "./common";
import type { Producer } from "./producer";
export interface Consumer { producer: Producer; value: Omit<Base, "count"> & { right: number }; choice: A | C; }
