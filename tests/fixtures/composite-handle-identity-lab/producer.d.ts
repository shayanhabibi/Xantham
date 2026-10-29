import type { Base, A, B } from "./common";
export interface Producer { value: Omit<Base, "text"> & { left: string }; choice: A | B; }
