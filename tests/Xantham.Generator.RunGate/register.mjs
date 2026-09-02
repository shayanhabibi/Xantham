// Node preload for the run gate: `node --import ./register.mjs fable-out/Program.js`.
//
// Two jobs. The `[<Global>]` bindings of the globals lab need their globals installed before
// the program's first line, so the fixture runtime is imported here. And the `[<Import>]`
// bindings name their packages (`phase-b-lab`) the way generated code always will - as bare
// specifiers - which node would look for under node_modules; the resolve hook points each
// fixture package name at its tracked runtime instead, so the gate needs no install step.
import { registerHooks } from "node:module";
import { pathToFileURL } from "node:url";
import path from "node:path";

const fixtures = path.resolve(import.meta.dirname, "..", "fixtures");
const packages = new Map([
    ["phase-b-lab", path.join(fixtures, "lab", "index.js")],
    ["statics-lab", path.join(fixtures, "statics-lab", "index.js")],
    ["flags-lab", path.join(fixtures, "flags-lab", "index.js")],
    ["ctor-lab", path.join(fixtures, "ctor-lab", "index.js")],
    ["inherit-lab", path.join(fixtures, "inherit-lab", "index.js")],
]);

registerHooks({
    resolve(specifier, context, nextResolve) {
        const runtime = packages.get(specifier);
        return runtime
            ? { url: pathToFileURL(runtime).href, shortCircuit: true }
            : nextResolve(specifier, context);
    },
});

await import(pathToFileURL(path.join(fixtures, "globals-lab", "index.js")).href);
