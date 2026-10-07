import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import assert from "node:assert/strict";

const gate = fileURLToPath(new URL(".", import.meta.url));
const root = path.resolve(gate, "../..");
const scratch = path.join(root, "tests/.scratch/partas-gate");
const digest = bytes => createHash("sha256").update(bytes).digest("hex");
const pin = JSON.parse(fs.readFileSync(path.join(gate, "partas-pin.json"), "utf8"));
assert.equal(digest(fs.readFileSync(path.join(gate, "partas-source.zip"))), pin.sha256, "Partas snapshot checksum");
if (fs.existsSync(scratch)) fs.rmSync(scratch, { recursive: true });
fs.mkdirSync(scratch, { recursive: true });
const env = { ...process.env, XanthamSupportProject: path.join(root, "src/Xantham.Fable.Core.TS/Xantham.Fable.Core.TS.fsproj") };
function run(command, args, cwd = root) {
  const result = spawnSync(command, args, { cwd, env, encoding: "utf8", shell: command === "npm" && process.platform === "win32", maxBuffer: 16 * 1024 * 1024 });
  if (result.error || result.status !== 0) throw new Error(`${command} ${args.join(" ")}\n${result.error ?? ""}\n${result.stdout}\n${result.stderr}`);
  return result.stdout;
}
function write(name, text) {
  const filename = path.join(scratch, name);
  fs.mkdirSync(path.dirname(filename), { recursive: true });
  fs.writeFileSync(filename, text);
}
const xml = value => value.replaceAll("&", "&amp;").replaceAll("'", "&apos;");
function project(directory, name, files, references = []) {
  write(`${directory}/${name}.fsproj`, `<Project Sdk='Microsoft.NET.Sdk'><PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup><ItemGroup>${files.map(f => `<Compile Include='${xml(f)}' />`).join("")}<PackageReference Include='Fable.Core' Version='5.2.0' />${references.map(r => `<ProjectReference Include='${xml(r)}' />`).join("")}</ItemGroup></Project>`);
}
fs.copyFileSync(path.join(root, "global.json"), path.join(scratch, "global.json"));
write(".config/dotnet-tools.json", fs.readFileSync(path.join(root, ".config/dotnet-tools.json"), "utf8"));
for (const name of ["Directory.Build.props", "Directory.Build.targets"]) write(name, "<Project />");
write("package.json", '{"type":"module"}');
run("dotnet", ["fsi", path.join(gate, "extract.fsx"), path.join(gate, "partas-source.zip"), path.join(scratch, "vendor")]);
for (const [name, hash] of Object.entries(pin.files)) assert.equal(digest(fs.readFileSync(path.join(scratch, "vendor", name))), hash, name);
console.log(`Partas ${pin.version} source snapshot verified (${pin.sha256}).`);
run("dotnet", ["run", "--project", "tools/customization-example", "--", "tests/fixtures/customization-dom-lab", path.join(scratch, "generated")]);
const companion = "Partas.Solid.CustomizationAcceptance.InputProperties.fs";
const generated = fs.readFileSync(path.join(scratch, "generated/customizations", companion), "utf8");
for (const member of ["value", "title", "tagName"]) assert.equal(generated.split(`member _.${member}`).length - 1, 1, `${member} exactly once`);
assert.match(generated, /member _\.tagName: string = jsNative/);
write("partas/Companions.fs", generated);
write("partas/Consumer.fs", fs.readFileSync(path.join(gate, "Consumer.fs"), "utf8"));
const partas = path.join(scratch, "vendor/src/Partas.Solid/Partas.Solid.fsproj");
const plugin = path.join(scratch, "vendor/src/Partas.Solid.FablePlugin/Partas.Solid.FablePlugin.fsproj");
project("partas", "PartasGate", ["Companions.fs", "Consumer.fs"], [partas, plugin]);
run("dotnet", ["fable", "PartasGate.fsproj", "-o", "js", "-e", ".fs.jsx", "--noCache", "--exclude", "Xantham.Fable.Core.TS", "--exclude", "Partas.Solid.FablePlugin"], path.join(scratch, "partas"));
const jsxPath = path.join(scratch, "partas/js/Consumer.fs.jsx");
assert.ok(fs.existsSync(jsxPath), "Fable must produce Consumer.fs.jsx");
const normalized = text => text.replace(/\s+/g, " ").trim();
assert.equal(normalized(fs.readFileSync(jsxPath, "utf8")), normalized(fs.readFileSync(path.join(gate, "fixtures/Consumer.expected.jsx"), "utf8")), "real plugin JSX acceptance");
console.log("Generated HTMLElement companion and real Partas plugin JSX passed.");
run("dotnet", ["run", "--project", "tools/customization-example", "--", "tests/fixtures/customization-lab", path.join(scratch, "direct-generated"), "--direct", "--lab"]);
const directFiles = fs.readdirSync(path.join(scratch, "direct-generated/customizations")).filter(name => name.endsWith(".fs"));
for (const file of directFiles) write(`bindings/${file}`, fs.readFileSync(path.join(scratch, "direct-generated/customizations", file), "utf8"));
write("bindings/CustomizationLab.fs", fs.readFileSync(path.join(scratch, "direct-generated/CustomizationLab.fs"), "utf8"));
project("bindings", "CustomizationBindings", ["CustomizationLab.fs", ...directFiles], [env.XanthamSupportProject]);
const consumer = fs.readFileSync(path.join(gate, "DirectConsumer.fs"), "utf8");
write("source/Consumer.fs", consumer);
write("dll/Consumer.fs", consumer);
const bindingProject = path.join(scratch, "bindings/CustomizationBindings.fsproj");
project("source", "SourceGate", ["Consumer.fs"], [bindingProject]);
project("dll", "DllGate", ["Consumer.fs"], [bindingProject]);
for (const [directory, name, extra] of [["source", "SourceGate", []], ["dll", "DllGate", ["--exclude", "CustomizationBindings"]]]) {
  run("dotnet", ["fable", `${name}.fsproj`, "-o", "js", "-e", ".js", "--noCache", "--exclude", "Xantham.Fable.Core.TS", ...extra], path.join(scratch, directory));
  run(process.execPath, ["js/Consumer.js"], path.join(scratch, directory));
}
console.log("Generated direct getter/setter access passed from source and binding DLL.");
write("runtime.test.jsx", fs.readFileSync(path.join(gate, "runtime.test.jsx"), "utf8"));
run("npm", ["ci", "--no-audit", "--no-fund"], gate);
fs.symlinkSync(path.join(gate, "node_modules"), path.join(scratch, "node_modules"), process.platform === "win32" ? "junction" : "dir");
console.log(run(process.execPath, [path.join(gate, "node_modules/vitest/vitest.mjs"), "run", "--config", path.join(gate, "vitest.config.mjs")], gate));
