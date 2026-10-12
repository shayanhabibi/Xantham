const test = require('node:test');
const assert = require('node:assert/strict');
const verify = require('./publish-matrix-status.cjs');

test('publication needs successful packages and fresh shards or verified master reuse', () => {
  const fresh = { packageResult: 'success', full: 'true', masterReused: 'false', shards: 'success' };
  verify(fresh);
  verify({ ...fresh, masterReused: 'true', shards: 'skipped' });
  for (const packageResult of ['failure', 'cancelled', 'skipped', '', undefined])
    assert.throws(() => verify({ ...fresh, packageResult }));
  for (const full of ['false', '', undefined])
    assert.throws(() => verify({ ...fresh, full }));
  for (const masterReused of ['', undefined, 'maybe'])
    assert.throws(() => verify({ ...fresh, masterReused }));
  for (const shards of ['failure', 'cancelled', 'skipped', '', undefined])
    assert.throws(() => verify({ ...fresh, shards }));
  for (const shards of ['success', 'failure', 'cancelled', '', undefined])
    assert.throws(() => verify({ ...fresh, masterReused: 'true', shards }));
});
