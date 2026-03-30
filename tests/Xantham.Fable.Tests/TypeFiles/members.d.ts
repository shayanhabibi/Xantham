// TEST TARGET: member kind classification
// Each interface isolates one or more TsMember cases to verify the correct
// TsMember discriminated union case is emitted.  Tests check member count,
// member names, and TsMember case — never TypeKey identity values.

// --- Properties ---
// WithProperties exercises TsMember.Property:
//   'name'   → required, mutable  (IsOptional = false, Accessor = ReadWrite)
//   'age'    → optional, mutable  (IsOptional = true,  Accessor = ReadWrite)
//   'active' → required, readonly (IsOptional = false, Accessor = ReadOnly)
export interface WithProperties {
    name: string;
    age?: number;
    readonly active: boolean;
}

// --- Methods ---
// WithMethods exercises TsMember.Method:
//   'greet' → 1 parameter
//   'add'   → 2 parameters
export interface WithMethods {
    greet(message: string): void;
    add(a: number, b: number): number;
}

// --- Signatures ---
// WithSignatures exercises TsMember.CallSignature, ConstructSignature,
// and IndexSignature.  All property-like members are typed 'unknown' so
// the index signature [key: string]: unknown is compatible with them.
export interface WithSignatures {
    // Call signature — the interface value itself is callable
    (x: number): boolean;
    // Construct signature — the interface value is newable
    new(value: string): object;
    // Index signature — arbitrary key-based access
    [key: string]: unknown;
}
