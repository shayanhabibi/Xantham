import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

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

console.log("Build checks passed: early key rejection, masked explain, local formatting skip, CI formatting check, single-project build.");
