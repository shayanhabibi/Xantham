import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import fs from "node:fs";
import path from "node:path";

const root = fileURLToPath(new URL("../", import.meta.url));
const environment = { ...process.env };
delete environment.NUGET_API_KEY;

function build(...args) {
  const result = spawnSync("dotnet", ["fsi", "build.fsx", "--", ...args], {
    cwd: root,
    env: environment,
    encoding: "utf8",
    timeout: 120_000,
  });
  assert.ifError(result.error);
  return { status: result.status, output: result.stdout + result.stderr };
}

const publish = build("publish", "--explain");
assert.equal(publish.status, 0, publish.output);
assert.match(publish.output, /require nuget key/, "Publish must check its key before setup");
assert.ok(publish.output.indexOf("require nuget key") < publish.output.indexOf("restore"));

// Run the missing-key path only after confirming its guard precedes setup.
const missingKey = build("publish");
assert.notEqual(missingKey.status, 0, missingKey.output);
assert.match(missingKey.output, /require nuget key/);
assert.doesNotMatch(missingKey.output, /dotnet restore|dotnet build|dotnet pack|dotnet nuget push/);

const sentinel = "build-check-placeholder-key";
const masked = build("publish", "--nuget-key", sentinel, "--explain");
assert.equal(masked.status, 0, masked.output);
assert.doesNotMatch(masked.output, new RegExp(sentinel));
assert.match(masked.output, /dotnet nuget push .*bin\/\*\.nupkg.*-k \*\*\*/);

const local = build("test", "--ci", "false", "--no-format", "--explain");
assert.equal(local.status, 0, local.output);
assert.match(local.output, /format\s+\(skipped\)/);
assert.doesNotMatch(local.output, /restore\s+\(skipped\)|clean\s+\(skipped\)|initialise fixtures\s+\(skipped\)/);

const ci = build("test", "--ci", "--no-format", "--explain");
assert.equal(ci.status, 0, ci.output);
assert.match(ci.output, /dotnet fantomas --check/);
assert.doesNotMatch(ci.output, /format\s+\(skipped\)/);

const singleProject = build("build", "--project", "Xantham.Cli", "--quick", "--explain");
assert.equal(singleProject.status, 0, singleProject.output);
assert.match(singleProject.output, /build-src\/Xantham.Cli\/Xantham.Cli.fsproj/);
assert.doesNotMatch(singleProject.output, /build-src\/Xantham.Fable.Core/);

const artifacts = build("publish", "artifacts", "--nuget-key", sentinel, "--explain");
assert.equal(artifacts.status, 0, artifacts.output);
assert.match(artifacts.output, /validate packages/);
assert.doesNotMatch(artifacts.output, /dotnet restore|dotnet build|dotnet pack|npm install|npm ci/);
assert.doesNotMatch(artifacts.output, new RegExp(sentinel));
assert.ok(artifacts.output.indexOf("validate packages") < artifacts.output.indexOf("dotnet nuget push"));

const bin = path.join(root, "bin");
fs.mkdirSync(bin, { recursive: true });
const unexpected = path.join(bin, `build-check-${process.pid}.nupkg`);
fs.writeFileSync(unexpected, "Unexpected package", { flag: "wx" });
try {
  const refused = build("publish", "artifacts", "--nuget-key", sentinel);
  assert.notEqual(refused.status, 0, refused.output);
  assert.match(refused.output, /Package set differs/);
  assert.doesNotMatch(refused.output, /dotnet nuget push/);
} finally {
  fs.unlinkSync(unexpected);
}

const pack = build("pack", "--ci", "--explain");
assert.equal(pack.status, 0, pack.output);
assert.match(pack.output, /build without tests\s+\(skipped\)/);
assert.equal((pack.output.match(/dotnet build .*Xantham\.slnx/g) ?? []).length, 1, "Pack builds the solution once before testing");

const skipped = build("pack", "--skip-tests", "--explain");
assert.equal(skipped.status, 0, skipped.output);
assert.doesNotMatch(skipped.output, /build without tests\s+\(skipped\)/);
assert.match(skipped.output, /test\s+\(skipped\)/);

console.log("Build checks passed: early key rejection, masked explain, formatting, project selection, artifact publishing, single pack build.");
