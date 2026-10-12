const assert = require('node:assert/strict');

function verify({ scope, full, reused, shards }, requireFull = false) {
  assert.equal(scope, 'success', 'Scope, CI policy, reuse and build command checks must succeed');
  assert.ok(['true', 'false'].includes(full), 'Verification scope is missing or invalid');
  assert.ok(['true', 'false'].includes(reused), 'Reuse decision is missing or invalid');
  assert.ok(reused !== 'true' || full === 'true', 'Reuse requires a full verification scope');
  const complete = full === 'true' && reused === 'false';
  assert.equal(shards, complete ? 'success' : 'skipped',
    complete ? 'Every test shard and both run gates must succeed' : 'Quick and reused runs must skip the shard workflow');
  if (requireFull) assert.ok(complete, 'Full-test evidence cannot come from quick or reused checks');
  return complete;
}

if (require.main === module) {
  verify({ scope: process.env.SCOPE_RESULT, full: process.env.FULL,
    reused: process.env.REUSED, shards: process.env.SHARDS_RESULT }, process.argv.includes('--require-full'));
}
module.exports = verify;
