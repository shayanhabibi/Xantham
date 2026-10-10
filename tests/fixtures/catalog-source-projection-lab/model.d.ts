import { Explicit } from "./named";

export interface Scalar { content: string | number; }
export interface ArrayContent { content: string | string[]; }
export interface Nullable { content: string | undefined; }
export interface TupleContent { content: [string, number]; }
export interface ObjectContent { content: { value: string }; }
export interface Named { content: Explicit; }
