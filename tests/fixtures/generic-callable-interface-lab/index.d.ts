export interface Identity { <T>(value: T): T; }
export interface PairWith<T> { <U>(value: U): [T, U]; }
export interface InheritedIdentity extends Identity {}
export interface RedundantIdentity extends Identity, InheritedIdentity {}
export interface StringPair extends PairWith<string> {}
export interface RedundantPair extends PairWith<string>, StringPair {}
export interface RenamedPair<V> extends PairWith<V> {}
export interface RedundantGeneric<V> extends PairWith<V>, RenamedPair<V> {}
export interface Collision<U> extends PairWith<U> {}
export declare const identity: Identity;
export declare const anonymousIdentity: <T>(value: T) => T;
export declare function declaredIdentity<T>(value: T): T;
export declare const identityAlias: typeof declaredIdentity;
export declare function accept(callback: Identity): Identity;
export declare function pairWith<T>(value: T): PairWith<T>;
export declare function anonymous(callback: <T>(value: T) => T): <T>(value: T) => T;
export declare function inherited(): InheritedIdentity;
export declare function redundant(): RedundantIdentity;
