// TEST TARGET: duplicate interface declaration merging
//
// Two source declarations of MergeProps share the same TypeKey.  The pipeline
// must collapse them into a single node whose Members list contains all
// properties from both declarations.  An index-access type on the merged
// interface is also exercised to verify the merged members are reachable.

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
