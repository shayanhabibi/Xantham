export interface IUtilityInterface {
    stringProperty: string;
    numberProperty: number;
    booleanProperty: boolean;
}

export type PartialInterface = Partial<IUtilityInterface>;
export type OmitInterface = Omit<IUtilityInterface, "stringProperty">;
export type PickInterface = Pick<IUtilityInterface, "stringProperty">;
export type DerivedInterface = Partial<OmitInterface>;
export type GenericUtility<T extends keyof IUtilityInterface> = Pick<PartialInterface, T>;