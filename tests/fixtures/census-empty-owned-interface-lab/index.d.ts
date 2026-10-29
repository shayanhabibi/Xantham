export interface Empty {}
export type Renamed = Empty;
export namespace Scope {
    interface Empty {}
    type Renamed = Empty;
}
export class Private { private brand; }
export type PrivateAlias = Private;
export interface Values { text: string; anything: any; }
