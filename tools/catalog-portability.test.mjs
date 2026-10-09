import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import test from "node:test";

const root = fileURLToPath(new URL("../", import.meta.url));

test("catalogue artifact generates a compiled consumer and rejects changed source bytes", t => {
  fs.mkdirSync(path.join(root, "tests/.scratch"), { recursive: true });
  const scratch = fs.mkdtempSync(path.join(root, "tests/.scratch/catalog-exchange-"));
  t.after(() => fs.rmSync(scratch, { recursive: true, force: true }));
  const artifact = path.join(scratch, "artifact");
  const run = (...args) => spawnSync("dotnet", ["fsi", "tools/catalog-portability.fsx", "--", ...args], {
    cwd: root, encoding: "utf8", timeout: 180_000,
  });
  const produce = run("produce", artifact);
  assert.equal(produce.status, 0, produce.stdout + produce.stderr);
  const catalog = JSON.parse(fs.readFileSync(path.join(artifact, "declarations.json")));
  assert.equal(catalog.schemaVersion, 2);
  assert.equal(catalog.compatibility.compiler.kind, "typescript-package");
  const consume = run("consume", artifact, path.join(scratch, "consumer"));
  assert.equal(consume.status, 0, consume.stdout + consume.stderr);
  assert.match(consume.stdout, /consumer compiled/);
  const manifestPath = path.join(artifact, "sources.json");
  const original = fs.readFileSync(manifestPath);
  const sources = JSON.parse(original);
  sources["index.d.ts"] = "changed";
  fs.writeFileSync(manifestPath, JSON.stringify(sources));
  const corrupt = run("consume", artifact, path.join(scratch, "corrupt"));
  assert.notEqual(corrupt.status, 0);
  assert.match(corrupt.stdout + corrupt.stderr, /fixture source hash mismatch/);
  assert.ok(!fs.existsSync(path.join(scratch, "corrupt")));
  fs.writeFileSync(manifestPath, original);
  const bindingPath = path.join(artifact, "Portability.Root.fs");
  const binding = fs.readFileSync(bindingPath);
  fs.appendFileSync(bindingPath, "\n// changed artifact\n");
  const changedPayload = run("consume", artifact, path.join(scratch, "changed-payload"));
  assert.notEqual(changedPayload.status, 0);
  assert.match(changedPayload.stdout + changedPayload.stderr, /artifact payload hash mismatch/);
  assert.ok(!fs.existsSync(path.join(scratch, "changed-payload")));
  fs.writeFileSync(bindingPath, binding);
  fs.unlinkSync(path.join(artifact, "declarations.json"));
  const missing = run("consume", artifact, path.join(scratch, "missing"));
  assert.notEqual(missing.status, 0);
  assert.match(missing.stdout + missing.stderr, /missing artifact file.*declarations.json/);
});
