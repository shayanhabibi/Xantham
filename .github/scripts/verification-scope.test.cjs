const test = require('node:test');
const assert = require('node:assert/strict');
const scope = require('./verification-scope.cjs');

test('documentation and workflow changes are quick', () => {
  for (const file of ['README.md', '.github/workflows/test.yml', '.github/scripts/policy.cjs',
    '.github/dependabot.yml', '.github/branch-workflow.md', 'docs/example.md', 'static/banner.png'])
    assert.equal(scope.requiresFullVerification(file), false, file);
});
test('source, projects, scripts, build config and dependency pins require full checks', () => {
  for (const file of ['src/Library.fs', 'src/README.md', 'tests/golden/file.json', 'tools/check.mjs',
    '.github/example.fsx', 'sample.fsproj', 'build.fsx', 'Xantham.slnx', 'Directory.Build.targets',
    'global.json', 'package.json', 'package-lock.json', '.config/dotnet-tools.json', 'NuGet.Config',
    '.node-version', 'unknown.config'])
    assert.equal(scope.requiresFullVerification(file), true, file);
});
test('renaming source to a documentation filename still requires full checks', async () => {
  const output = {};
  await scope({ github: { rest: { pulls: { listFiles() {} } }, paginate: async () => [
    { filename: 'docs/deleted.md', previous_filename: 'src/Deleted.fs' }] },
    context: { eventName: 'pull_request', repo: { owner: 'o', repo: 'r' }, payload: { pull_request: { number: 1, changed_files: 1 } } },
    core: { setOutput: (k, v) => output[k] = v, info() {} } });
  assert.equal(output.full, 'true');
});
test('manual runs and truncated push diffs fail closed to full checks', async () => {
  for (const eventName of ['workflow_dispatch', 'push']) {
    const output = {};
    await scope({ github: { rest: { repos: { compareCommits: async () => ({ data: { files: Array(300).fill({ filename: 'README.md' }) } }) } } },
      context: { eventName, sha: 'head', repo: { owner: 'o', repo: 'r' }, payload: { before: 'base' } },
      core: { setOutput: (k, v) => output[k] = v, info() {} } });
    assert.equal(output.full, 'true');
  }
});
