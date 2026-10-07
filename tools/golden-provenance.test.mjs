import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import test from "node:test";

const root = fileURLToPath(new URL("../", import.meta.url));
const commit = "0123456789abcdef0123456789abcdef01234567";

test("golden artifact records its source commit and authenticates every copied byte", t => {
  fs.mkdirSync(path.join(root, "tests/.scratch"), { recursive: true });
  const scratch = fs.mkdtempSync(path.join(root, "tests/.scratch/golden-provenance-"));
  t.after(() => fs.rmSync(scratch, { recursive: true, force: true }));
  const source = path.join(scratch, "source");
  const output = path.join(scratch, "artifact");
  fs.mkdirSync(path.join(source, "lab"), { recursive: true });
  const bytes = Buffer.from("module Lab\r\nlet value = 1\r\n");
  fs.writeFileSync(path.join(source, "lab/Lab.fs"), bytes);
  fs.writeFileSync(path.join(source, "lab/manifest.json"), "{}\n");
  const run = (sha = commit, destination = output) => spawnSync(process.execPath, [
    "tools/golden-provenance.mjs", "--source", source, "--output", destination, "--commit", sha,
  ], { cwd: root, encoding: "utf8" });
  const result = run();
  assert.equal(result.status, 0, result.stderr);
  const manifest = JSON.parse(fs.readFileSync(path.join(output, "provenance.json")));
  assert.equal(manifest.sourceCommit, commit);
  assert.equal(manifest.schemaVersion, 1);
  assert.equal(manifest.files["lab/Lab.fs"], createHash("sha256").update(bytes).digest("hex"));
  assert.deepEqual(Object.keys(manifest.files), ["lab/Lab.fs", "lab/manifest.json"]);
  assert.deepEqual(fs.readFileSync(path.join(output, "golden/lab/Lab.fs")), bytes);
  const first = fs.readFileSync(path.join(output, "provenance.json"));
  assert.equal(run(commit, path.join(scratch, "artifact-again")).status, 0);
  assert.deepEqual(fs.readFileSync(path.join(scratch, "artifact-again/provenance.json")), first);
  assert.notEqual(run("bad-sha", path.join(scratch, "invalid")).status, 0);
  assert.ok(!fs.existsSync(path.join(scratch, "invalid")));
  assert.notEqual(run(commit, path.join(source, "nested")).status, 0);
  assert.ok(!fs.existsSync(path.join(source, "nested")));
  assert.notEqual(run().status, 0, "An existing artifact must not be overwritten");
});
