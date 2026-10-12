const assert = require('node:assert/strict');

function verify({ packageResult, full, masterReused, shards }) {
  assert.equal(packageResult, 'success', 'Verified packages must succeed before publication');
  assert.equal(full, 'true', 'Publication requires full verification');
  assert.ok(['true', 'false'].includes(masterReused), 'Master reuse proof is missing or invalid');
  assert.equal(shards, masterReused === 'true' ? 'skipped' : 'success',
    'Publication requires every shard or a verified identical-tree develop run');
}

if (require.main === module) {
  verify({ packageResult: process.env.PACKAGE_RESULT, full: process.env.FULL,
    masterReused: process.env.MASTER_REUSED, shards: process.env.SHARDS_RESULT });
}
module.exports = verify;
