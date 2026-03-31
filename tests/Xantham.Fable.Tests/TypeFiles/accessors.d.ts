// TEST TARGET: get/set accessor member emission
//
// Verifies that TsMember.GetAccessor and TsMember.SetAccessor are emitted for
// accessor declarations.  Tests check member names, the resolved Type on a
// GetAccessor, and that a read-write pair produces both accessor cases under
// the same name.

export interface WithAccessors {
    get readable(): string;
    set writable(value: string);
    get readWrite(): number;
    set readWrite(value: number);
}
