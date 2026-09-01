// TEST TARGET: TsProperty.IsOptional and TsProperty.Accessor modifiers
//
// Exercises all four combinations of required/optional × mutable/readonly so
// each modifier flag can be verified independently.
//
// Expected TsProperty values per member:
//   'required'     → IsOptional = false, Accessor = TsAccessor.ReadWrite
//   'optional'     → IsOptional = true,  Accessor = TsAccessor.ReadWrite
//   'readonlyProp' → IsOptional = false, Accessor = TsAccessor.ReadOnly
//   'optReadonly'  → IsOptional = true,  Accessor = TsAccessor.ReadOnly

export interface Modifiers {
    required: string;
    optional?: number;
    readonly readonlyProp: boolean;
    readonly optReadonly?: string;
}
