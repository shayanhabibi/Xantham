export interface BaseInterface {
    parentProp: string;
}

export interface ExtendedInterface extends BaseInterface {
    childProp: number;
}