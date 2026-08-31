export interface Point {
    x: number;
    y: number;
}

export function distance(a: Point, b: Point): number {
    return Math.hypot(a.x - b.x, a.y - b.y);
}

const wrong: number = "not a number";
