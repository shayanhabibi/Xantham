/// <reference path="./index.d.ts" />
import { Actor } from "class-lab:runtime";
export declare class Explicit<T> extends Actor<T> {
    constructor(seed: T);
    send(value: T): T;
}
export declare class Implicit<T> extends Actor<T> { send(value: T): T; }
export interface Extension<T> extends Actor<T> { label: string; }
export interface Plain<T> { value: T; }
export interface PlainDerived<T> extends Plain<T> { extra: string; }
