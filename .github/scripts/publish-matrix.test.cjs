const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');

test('Publish waits for packages and every master test shard; PR packaging remains independent', () => {
  const workflow = fs.readFileSync('.github/workflows/push_master.yml', 'utf8');
  const packageJob = workflow.split('\n  package:')[1].split('\n  test:')[0];
  const testJob = workflow.split('\n  test:')[1].split('\n  publish:')[0];
  const publish = workflow.split('\n  publish:')[1];
  assert.match(packageJob, /pack --ci --skip-tests/);
  assert.doesNotMatch(packageJob, /needs:/);
  assert.match(testJob, /needs: package/);
  assert.match(testJob, /github.event_name != 'pull_request'.*refs\/heads\/master.*needs.package.outputs.full == 'true'/);
  assert.match(testJob, /fail-fast: false/);
  assert.match(publish, /needs: \[package, test\]/);
  assert.match(publish, /Download verified packages/);
  assert.match(publish, /publish artifacts/);
  const shards = /shard: \[([^\]]+)\]/.exec(testJob)[1].split(',').map(Number);
  const count = Number(/matrix.shard }}\/(\d+)/.exec(testJob)[1]);
  assert.deepEqual(shards, Array.from({ length: count }, (_, index) => index + 1));
  assert.match(testJob, /--run-gate \$\{\{ matrix.shard == 1 && 'true' \|\| 'false' }}/);
});

test('Test partition is exhaustive, disjoint, deterministic, and never silently empty', async () => {
  const { partition, testNames, batches, verifyExecution } = await import('../../tools/test-shards.mjs');
  const names = Array.from({ length: 103 }, (_, i) => `suite.test ${i}`);
  const shards = partition(names, 8);
  assert.deepEqual(shards.flat().sort(), [...names].sort());
  assert.equal(new Set(shards.flat()).size, names.length);
  assert.ok(shards.every(shard => shard.length > 0));
  assert.deepEqual(partition([...names].reverse(), 8), shards);
  assert.throws(() => partition(['same', 'same'], 2), /Duplicate/);
  assert.throws(() => partition(['one'], 2), /Every shard/);
  for (const count of [0, -1, 1.5]) assert.throws(() => partition(names, count));
  const parameterized = ['hello.world', 'case.{ field = "a"\n next = 1 }'];
  assert.deepEqual(testNames(JSON.stringify(parameterized)), parameterized);
  assert.throws(() => testNames('Unexpected compiler output'));
  for (const invalid of ['[]', '{}', '[1]', '[""]']) assert.throws(() => testNames(invalid));
  const long = Array.from({ length: 100 }, (_, i) => `test${i}${'a'.repeat(300)}`);
  assert.deepEqual(batches(long).flat(), long);
  assert.ok(batches(long).every(batch => batch.reduce((sum, name) => sum + name.length + 3, 0) <= 12000));
  verifyExecution('EXPECTO! 2 tests run in 00:01 for suite – 2 passed, 1 ignored, 0 failed, 0 errored.', 3);
  verifyExecution('EXPECTO! 0 tests run in 00:01 for suite – 0 passed, 1 ignored, 0 failed, 0 errored.', 1);
  verifyExecution('EXPECTO! 1 tests run in 00:01 for multiline\nparameter – 1 passed, 0 ignored, 0 failed, 0 errored.', 1);
  verifyExecution('EXPECTO! 1,343 tests run in 00:01 for suite – 1,343 passed, 1 ignored, 0 failed, 0 errored.', 1344);
  assert.throws(() => verifyExecution('EXPECTO! 0 tests run in 00:01 for suite – 0 passed, 0 ignored, 0 failed, 0 errored.', 1), /selection differs/);
  assert.throws(() => verifyExecution('no summary', 1), /did not report/);
});
