import assert from "node:assert/strict";
import fs from "node:fs";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import test from "node:test";
import { brotliDecompressSync } from "node:zlib";
import { createHash } from "node:crypto";

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
  const plainBytes = fs.readFileSync(path.join(artifact, "declarations.json"));
  const compressedPath = path.join(artifact, "declarations.json.br");
  const compressedBytes = fs.readFileSync(compressedPath);
  assert.deepEqual(brotliDecompressSync(compressedBytes), plainBytes);
  const payload = JSON.parse(fs.readFileSync(path.join(artifact, "payload.json")));
  assert.equal(payload["declarations.json.br"], createHash("sha256").update(compressedBytes).digest("hex"));
  const consume = run("consume", artifact, path.join(scratch, "consumer"));
  assert.equal(consume.status, 0, consume.stdout + consume.stderr);
  assert.match(consume.stdout, /consumer compiled/);
  assert.equal((consume.stdout.match(/consumer compiled/g) ?? []).length, 2);
  for (const format of ["json", "brotli"]) {
    assert.ok(fs.existsSync(path.join(scratch, "consumer", format, "adapter", "Portability.Adapter.fs")));
    assert.ok(fs.existsSync(path.join(scratch, "consumer", format, "bin", "Release", "net10.0", "Consumer.dll")));
  }
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
  fs.appendFileSync(compressedPath, Buffer.from([0]));
  const changedCompressed = run("consume", artifact, path.join(scratch, "changed-compressed"));
  assert.notEqual(changedCompressed.status, 0);
  assert.match(changedCompressed.stdout + changedCompressed.stderr, /artifact payload hash mismatch/);
  assert.ok(!fs.existsSync(path.join(scratch, "changed-compressed")));
  fs.writeFileSync(compressedPath, compressedBytes);
  fs.unlinkSync(path.join(artifact, "declarations.json"));
  const missing = run("consume", artifact, path.join(scratch, "missing"));
  assert.notEqual(missing.status, 0);
  assert.match(missing.stdout + missing.stderr, /missing artifact file.*declarations.json/);
});
