import { Scalar, ArrayContent, Nullable, TupleContent, ObjectContent, Named } from "./model";

export type ScalarProjection = Scalar["content"];
export type ArrayProjection = ArrayContent["content"];
export type NullableProjection = Nullable["content"];
export type TupleProjection = TupleContent["content"];
export type ObjectProjection = ObjectContent["content"];

export declare function scalar(value: Scalar): Scalar;
export declare function array(value: ArrayContent): ArrayContent;
export declare function nullable(value: Nullable): Nullable;
export declare function tuple(value: TupleContent): TupleContent;
export declare function object(value: ObjectContent): ObjectContent;
export declare function named(value: Named): Named;
