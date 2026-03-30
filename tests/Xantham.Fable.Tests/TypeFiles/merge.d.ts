export interface MergeProps {
    name: string;
}

export type Dummy = {}

export interface MergeProps {
    age: number;
}

export type D = {
    name: MergeProps['age'];
}