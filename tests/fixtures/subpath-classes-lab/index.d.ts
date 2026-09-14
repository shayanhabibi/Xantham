export declare class Base extends Error { constructor(code: string); readonly code: string }
export declare class Child extends Base { constructor(message: string); readonly detail: string }
