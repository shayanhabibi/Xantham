export interface BaseInterface {
    parentProp: string;
}

export interface ExtendedInterface extends BaseInterface {
    childProp: number;
}

export interface ExtendedInterface2 extends BaseInterface {
    childProp2: number;
}