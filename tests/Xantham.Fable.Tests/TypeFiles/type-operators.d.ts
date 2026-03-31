export interface KeyedInterface {
    stringProperty: string;
    numberProperty: number;
    booleanProperty: boolean;
}

export type InterfaceKey = keyof KeyedInterface;

// TODO - test generic/intersection and keyof outcome