// Probes every method the schema declares against the compiler that is actually installed.
//
// The point is drift: `APIMethodInfo` in `dist/api/proto.generated.d.ts` is generated from the Go
// source, but the `tsc` binary in the platform package is a separate artifact, and the two have
// disagreed before - see §7 of `docs/plans/tsgo-protocol.md`, where a preview build shipped 115 of
// the 137 methods its own schema declared. An unknown method answers with a distinctive
// `unknown API method "..."` error, so existence is cheap to test: send `{}` and read the failure.
//
// Run it from a directory with `typescript` and its platform package installed - the Wire's test
// project is one:
//
//     node ../../tools/probe-method-existence.mjs
//
// Every method is followed by a `ping`, because a bad payload can take the server down rather than
// produce an error frame, and a dead server would report every method after it as missing.

import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

const modules = path.resolve("node_modules");
const api = path.join(modules, "typescript", "dist", "api");

if (!fs.existsSync(api)) {
    console.error(`no typescript package under ${modules} - run \`npm install\` here first`);
    process.exit(1);
}

const platform = fs
    .readdirSync(path.join(modules, "@typescript"))
    .map(name => path.join(modules, "@typescript", name, "lib"))
    .flatMap(lib => ["tsc", "tsc.exe", "tsgo", "tsgo.exe"].map(stem => path.join(lib, stem)))
    .find(fs.existsSync);

if (!platform) {
    console.error("no platform package with an executable under node_modules/@typescript");
    process.exit(1);
}

// Not exported from the package, hence the file URL.
const { SyncRpcChannel } = await import(pathToFileURL(path.join(api, "syncChannel.js")));

const declarations = fs.readFileSync(path.join(api, "proto.generated.d.ts"), "utf8");
const block = declarations.slice(declarations.indexOf("export interface APIMethodInfo {"));
const methods = [...block.slice(0, block.indexOf("\n}")).matchAll(/^\s+(\w+): APIMethod</gm)].map(m => m[1]);

const spawn = () => new SyncRpcChannel(platform, ["--api", "--cwd", process.cwd()]);

let channel = spawn();
const unknown = [];
const fatal = [];

for (const method of methods) {
    try {
        channel.requestSync(method, "{}");
    } catch (error) {
        if (/unknown API method/.test(String(error.message))) unknown.push(method);
    }

    try {
        channel.requestSync("ping", "null");
    } catch {
        fatal.push(method);
        try {
            channel.close();
        } catch {}
        channel = spawn();
    }
}

console.log(`executable  ${platform}`);
console.log(`schema      ${methods.length} methods`);
console.log(`unknown     ${unknown.length}${unknown.length ? `: ${unknown.join(", ")}` : ""}`);
console.log(`fatal       ${fatal.length}${fatal.length ? `: ${fatal.join(", ")}` : ""}`);
console.log(`ping        ${channel.requestSync("ping", "null")}`);
channel.close();

process.exitCode = unknown.length === 0 ? 0 : 1;
