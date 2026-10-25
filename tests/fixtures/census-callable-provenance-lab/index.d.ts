export interface CloneWindow { clone<T>(value: T): T }
declare namespace Helpers { function clone<T>(value: T): T; }
export type CloneSelf = CloneWindow & typeof Helpers;
export declare const cloneSelf: CloneSelf;

export interface RenamedWindow { echo<T>(value: T): T }
declare namespace RenamedHelpers { function echo<T>(value: T): T; }
export type RenamedSelf = RenamedWindow & typeof RenamedHelpers;
export declare const renamedSelf: RenamedSelf;

import { aliased } from './helpers';
declare namespace AliasHelpers { export { aliased }; }
export interface AliasWindow { aliased<T>(value: T): T }
export type AliasSelf = AliasWindow & typeof AliasHelpers;
export declare const aliasSelf: AliasSelf;

export interface Properties {
    readonly ordinary: <T>(value: T) => T;
    writable: <T>(value: T) => T;
}
export type NonCallable = { value: string } & { count: number };

export interface OptionalProperties { optional?: <T>(value: T) => T }
export type MixedProperty = CloneWindow & { clone: <T>(value: T) => T };
export interface Hybrid { <T>(value: T): T; readonly tag: string }
export interface Indexed { <T>(value: T): T; [key: string]: unknown }
export interface Constructable { <T>(value: T): T; new(value: string): CloneWindow }
export interface HeldCallables { hybrid: Hybrid; indexed: Indexed; constructable: Constructable }

export type FunctionProperty = { clone: <T>(value: T) => T } & typeof Helpers;
