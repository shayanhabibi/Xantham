export interface Foo<T> {
    typeArgProp: T;
}

export interface ExtendedFoo extends Foo<string> {
    additionalProp: string
}

export interface GenericExtendedFoo<T = string> extends Foo<T> {
    additionalProp: T
}

export interface GenericExtendedFoo2 extends GenericExtendedFoo {
    numberProp: number
}

export interface ExtendedFoo2<T, U = number> extends Foo<U> {
    additionalProp: T
}

export interface GenericExtendedFoo3<T> extends ExtendedFoo2<T> {
    numberProp: number
}