import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { parseArgs } from "node:util";

const { values } = parseArgs({ options: {
  source: { type: "string", default: "tests/Xantham.Generator.Tests/golden" },
  output: { type: "string", default: "tests/.scratch/golden-artifact" },
  commit: { type: "string" },
} });

if (!/^[0-9a-f]{40}$/.test(values.commit ?? "")) throw new Error("--commit requires a full Git commit hash");
const source = fs.realpathSync(values.source);
const output = path.resolve(values.output);
const relative = path.relative(source, output);
if (!relative || (!relative.startsWith(`..${path.sep}`) && relative !== ".." && !path.isAbsolute(relative))) {
  throw new Error("The artifact output must be outside the golden corpus");
}
if (fs.existsSync(output)) throw new Error("The artifact output already exists; choose a fresh directory");

const files = {};
function collect(directory) {
  for (const entry of fs.readdirSync(directory, { withFileTypes: true }).sort((a, b) => a.name < b.name ? -1 : a.name > b.name ? 1 : 0)) {
    const filename = path.join(directory, entry.name);
    if (entry.isSymbolicLink()) throw new Error(`Golden symlinks are unsupported: ${filename}`);
    if (entry.isDirectory()) collect(filename);
    else if (entry.isFile()) {
      const name = path.relative(source, filename).split(path.sep).join("/");
      files[name] = fs.readFileSync(filename);
    }
  }
}
collect(source);
if (!Object.keys(files).length) throw new Error("The golden corpus is empty");
fs.mkdirSync(path.join(output, "golden"), { recursive: true });
const hashes = {};
for (const [name, bytes] of Object.entries(files)) {
  const destination = path.join(output, "golden", name);
  fs.mkdirSync(path.dirname(destination), { recursive: true });
  fs.writeFileSync(destination, bytes);
  hashes[name] = createHash("sha256").update(bytes).digest("hex");
}
fs.writeFileSync(path.join(output, "provenance.json"), JSON.stringify({
  schemaVersion: 1,
  sourceCommit: values.commit,
  files: hashes,
}, null, 2) + "\n");
console.log(`Golden artifact: ${Object.keys(files).length} files from ${values.commit}`);
