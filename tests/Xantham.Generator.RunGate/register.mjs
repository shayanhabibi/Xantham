// Node preload for the run gate: `node --import ./register.mjs fable-out/Program.js`.
//
// Two jobs. The `[<Global>]` bindings of the globals lab need their globals installed before
// the program's first line, so the fixture runtime is imported here. And the `[<Import>]`
// bindings name their packages (`phase-b-lab`) the way generated code always will - as bare
// specifiers - which node would look for under node_modules; the resolve hook points each
// fixture package name at its tracked runtime instead, so the gate needs no install step.
import { registerHooks } from "node:module";
import { pathToFileURL } from "node:url";
import fs from "node:fs";
import path from "node:path";

const fixtures = path.resolve(import.meta.dirname, "..", "fixtures");

// Discovered rather than listed. A lab fixture is its own npm package - a directory with a
// `package.json` and, where the gate runs it, a hand-written `index.js` - so the package name
// to resolve and the runtime to resolve it to are both already on disk, and reading them is
// exact where a second copy of the mapping can only drift from it. The literal map this
// replaces was one of the handful of append-only lists that every parallel branch edited and
// so every merge conflicted in; a lab added now is picked up with no edit here at all.
const packages = new Map(
    fs
        .readdirSync(fixtures, { withFileTypes: true })
        .filter((entry) => entry.isDirectory())
        .flatMap((entry) => {
            const dir = path.join(fixtures, entry.name);
            const runtime = path.join(dir, "index.js");
            const manifest = path.join(dir, "package.json");
            if (!fs.existsSync(runtime) || !fs.existsSync(manifest)) return [];
            const { name } = JSON.parse(fs.readFileSync(manifest, "utf8"));
            return name ? [[name, runtime]] : [];
        }),
);

registerHooks({
    resolve(specifier, context, nextResolve) {
        const runtime = packages.get(specifier);
        return runtime
            ? { url: pathToFileURL(runtime).href, shortCircuit: true }
            : nextResolve(specifier, context);
    },
});

// Imported for effect, not resolved on demand like the above: the globals lab's runtime assigns
// the ambient globals its `[<Global>]` bindings read, and nothing imports it to trigger that.
await import(pathToFileURL(path.join(fixtures, "globals-lab", "index.js")).href);
