# Explicit ambient types

The entry requires the installed ambient-provider-lab package for its host:runtime import and HostBinding global. Its xantham.json selects that package through the TypeScript types option. The live test checks the compiler diagnostics, and the generated files are compiled with the other goldens.

missing.json requests a provider that is absent. Bootstrap must report the compiler diagnostic before generating bindings.
