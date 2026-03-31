// TEST TARGET: indexed access type resolution
//
// Concrete accesses (AccessedInterface['prop']) must be resolved to the
// underlying primitive TypeKey.  Generic accesses (AccessedInterface[T]) must
// preserve TsAstNode.IndexAccessType semantics with the correct Index (type
// parameter) and Object (interface reference) type entries in the result.

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
