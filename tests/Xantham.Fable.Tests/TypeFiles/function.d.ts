// TEST TARGET: function parameter metadata
//
// Verifies TsParameter.Name, TsParameter.IsOptional, and TsParameter.IsSpread
// on top-level exported function declarations.
//
// Expected per function:
//   greet   → 2 params; "name" IsOptional=false IsSpread=false;
//                        "greeting" IsOptional=true IsSpread=false
//   collect → 1 param;  "items" IsOptional=false IsSpread=true
//   ping    → 0 params

// Two parameters; second is optional
export function greet(name: string, greeting?: string): string;

// Single rest/spread parameter
export function collect(...items: number[]): number[];

// Zero parameters
export function ping(): void;
