export interface AccessedInterface {
    stringProperty: string;
    numberProperty: number;
    booleanProperty: boolean;
}

export interface TestedInterface {
    stringAccess: AccessedInterface['stringProperty'];
    numberAccess: AccessedInterface['numberProperty'];
    booleanAccess: AccessedInterface['booleanProperty'];
}

export interface GenericTest<T extends keyof AccessedInterface> {
    accessedProperty: AccessedInterface[T];
}