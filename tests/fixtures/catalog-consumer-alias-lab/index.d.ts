import type { InstanceStatus } from "consumer-alias-owner-lab";
/** The consumer names the producer's anonymous union. */
export type WorkflowStatus = InstanceStatus["status"];
export declare function status(): InstanceStatus;
export declare function isDone(status: WorkflowStatus): boolean;
