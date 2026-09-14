import { Model } from "record-model-lab";
export type OutboundHandlerParams = Record<string, unknown>;
export type ModelParams = Record<string, Model>;
export type TextParams = Record<string, string>;
export declare function echo(value: OutboundHandlerParams): OutboundHandlerParams;
