// This must remain a global script: ambient modules and the global are harvested together.
interface LayoutPayload {
    value: string;
}

declare var sharedFlag: boolean;

declare module "layout-lab" {
    export function check(value: string): string;
    export const mode: string;
    export function echo(value: LayoutPayload): LayoutPayload;

    export function convert(value: string): string;
    export function convert(value: number): number;

    export function pick(value: string): string;
    export function pick(value: string): number;

    export function dispatch(kind: "left"): "left";
    export function dispatch(kind: "right"): "right";
}

declare module "layout-lab/strict" {
    export function check(value: string): string;
    export const mode: string;
    export function echo(value: LayoutPayload): LayoutPayload;
}

declare module "layout-lab/aliases" {
    export { check as renamedCheck } from "layout-lab";
    export type { check as typeOnlyCheck } from "layout-lab";
}
