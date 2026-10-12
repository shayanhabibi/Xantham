const assert = require('node:assert/strict');

function verify({ scope, full, reused, shards, masterReused = 'false', eventName, ref }, requireFull = false) {
  assert.equal(scope, 'success', 'Scope, CI policy, reuse and build command checks must succeed');
  assert.ok(['true', 'false'].includes(full), 'Verification scope is missing or invalid');
  assert.ok(['true', 'false'].includes(reused), 'Reuse decision is missing or invalid');
  assert.ok(reused !== 'true' || full === 'true', 'Reuse requires a full verification scope');
  assert.ok(['true', 'false'].includes(masterReused), 'Master reuse proof is missing or invalid');
  if (masterReused === 'true') {
    assert.equal(reused, 'true', 'Master proof requires reuse');
    assert.ok(['push', 'workflow_dispatch'].includes(eventName) && ref === 'refs/heads/master',
      'Master proof is only valid for master pushes and manual master runs');
  }
  const complete = full === 'true' && reused === 'false';
  assert.equal(shards, complete ? 'success' : 'skipped',
    complete ? 'Every test shard and both run gates must succeed' : 'Quick and reused runs must skip the shard workflow');
  const verified = complete || masterReused === 'true';
  if (requireFull) assert.ok(verified, 'Full-test evidence cannot come from quick or reused PR checks');
  return verified;
}

if (require.main === module) {
  verify({ scope: process.env.SCOPE_RESULT, full: process.env.FULL,
    reused: process.env.REUSED, shards: process.env.SHARDS_RESULT, masterReused: process.env.MASTER_REUSED,
    eventName: process.env.EVENT_NAME, ref: process.env.BRANCH_REF }, process.argv.includes('--require-full'));
}
module.exports = verify;
