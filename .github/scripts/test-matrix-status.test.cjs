const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const verify = require('./test-matrix-status.cjs');

test('the required test check only accepts exhaustive success or explicit quick/reused checks', () => {
  const complete = { scope: 'success', full: 'true', reused: 'false', shards: 'success' };
  assert.equal(verify(complete, true), true);
  for (const result of ['failure', 'cancelled', 'skipped', '', undefined]) {
    assert.throws(() => verify({ ...complete, shards: result }), /Every test shard/);
    assert.throws(() => verify({ ...complete, scope: result }), /must succeed/);
  }
  for (const mode of [{ full: 'false', reused: 'false' }, { full: 'true', reused: 'true' }]) {
    const quick = { scope: 'success', ...mode, shards: 'skipped' };
    assert.equal(verify(quick), false);
    assert.throws(() => verify(quick, true), /Full-test evidence/);
    for (const result of ['success', 'failure', 'cancelled', '', undefined])
      assert.throws(() => verify({ ...quick, shards: result }), /must skip/);
  }
  for (const invalid of ['', undefined, 'maybe']) {
    assert.throws(() => verify({ ...complete, full: invalid }), /scope is missing/);
    assert.throws(() => verify({ ...complete, reused: invalid }), /decision is missing/);
  }
  assert.throws(() => verify({ scope: 'success', full: 'false', reused: 'true', shards: 'skipped' }), /Reuse requires/);
});

test('Test aggregate gates the release-reuse marker and golden artifacts on all shards', () => {
  const workflow = fs.readFileSync('.github/workflows/test.yml', 'utf8');
  const scope = workflow.split('\n  scope:')[1].split('\n  shards:')[0];
  const shards = workflow.split('\n  shards:')[1].split('\n  test:')[0];
  const aggregate = workflow.split('\n  test:')[1];
  assert.match(scope, /full: \$\{\{ steps.scope.outputs.full }}/);
  assert.match(scope, /reused: \$\{\{ steps.develop-tests.outputs.reused \|\| 'false' }}/);
  assert.match(scope, /node --test \.github\/scripts\/\*.test.cjs tools\/golden-provenance.test.mjs tools\/golden-tag.test.mjs/);
  assert.match(scope, /reuse-develop-tests.cjs/);
  assert.match(scope, /node tools\/verify-build.mjs/);
  const fullOnly = "if: needs.scope.outputs.full == 'true' && needs.scope.outputs.reused != 'true'";
  assert.match(shards, /needs: scope/);
  assert.ok(shards.includes(fullOnly));
  assert.match(shards, /uses: \.\/\.github\/workflows\/test-shards.yml/);
  assert.match(aggregate, /needs: \[scope, shards\]/);
  assert.match(aggregate, /if: always\(\)/);
  for (const binding of ['SCOPE_RESULT: ${{ needs.scope.result }}', 'FULL: ${{ needs.scope.outputs.full }}',
    'REUSED: ${{ needs.scope.outputs.reused }}', 'SHARDS_RESULT: ${{ needs.shards.result }}'])
    assert.ok(aggregate.includes(binding), binding);
  assert.match(aggregate, /Require successful verification\n\s+run: node \.github\/scripts\/test-matrix-status.cjs/);
  const verifiedOnly = "if: needs.scope.outputs.full == 'true' && (needs.scope.outputs.reused != 'true' || needs.scope.outputs.master-reused == 'true')";
  for (const name of ['Test', 'Record golden provenance', 'Upload verified goldens']) {
    const block = aggregate.split(`- name: ${name}\n`)[1].split('\n      - ')[0];
    assert.ok(block.trimStart().startsWith(verifiedOnly), name);
    assert.doesNotMatch(block, /always\(\)|continue-on-error:/);
  }
  assert.match(aggregate, /- name: Test\n[^\n]+\n\s+run: node \.github\/scripts\/test-matrix-status.cjs --require-full/);
  assert.ok(aggregate.indexOf('Require successful verification') < aggregate.indexOf('- name: Test\n'));
  assert.ok(aggregate.indexOf('- name: Test\n') < aggregate.indexOf('Record golden provenance'));
  assert.ok(aggregate.indexOf('Record golden provenance') < aggregate.indexOf('Upload verified goldens'));
  assert.match(aggregate, /SOURCE_COMMIT: \$\{\{ github.sha }}/);
  assert.match(aggregate, /name: goldens-\$\{\{ github.sha }}/);
  assert.match(aggregate, /if-no-files-found: error/);
  assert.doesNotMatch(aggregate, /continue-on-error:/);
});

test('only trusted master-tree reuse can issue full evidence without fresh shards', () => {
  for (const eventName of ['push', 'workflow_dispatch']) {
    const proof = { scope: 'success', full: 'true', reused: 'true', shards: 'skipped',
      masterReused: 'true', eventName, ref: 'refs/heads/master' };
    assert.equal(verify(proof, true), true);
    for (const invalid of [{ eventName: 'pull_request' }, { ref: 'refs/heads/develop' },
      { full: 'false' }, { reused: 'false' }, { shards: 'success' }, { shards: 'failure' }])
      assert.throws(() => verify({ ...proof, ...invalid }, true));
    for (const masterReused of ['', undefined, 'maybe'])
      assert.throws(() => verify({ ...proof, masterReused }, true));
  }
});
