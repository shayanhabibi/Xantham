# Opaque generic arguments

`index.d.ts` supplies the generic declaration used by the transport options. Its default type argument is concrete, so Connection, Manager and Agent remain nongeneric.

`missing-provider.d.ts` deliberately omits that declaration. The checker reports its use as an error type with retained alias arguments. That error must remain visible as a finding while its erased argument must not become a free parameter on surrounding declarations. This negative input is exercised separately; it is not a valid SDK input or an accepted coverage result.
