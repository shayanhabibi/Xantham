const test = require('node:test');
const assert = require('node:assert/strict');
const verify = require('./publish-matrix-status.cjs');

test('publication needs successful packages and authenticated master Test evidence', () => {
  const fresh = { packageResult: 'success', full: 'true', verificationResult: 'success', verified: 'true' };
  verify(fresh);
  for (const packageResult of ['failure', 'cancelled', 'skipped', '', undefined])
    assert.throws(() => verify({ ...fresh, packageResult }));
  for (const full of ['false', '', undefined])
    assert.throws(() => verify({ ...fresh, full }));
  for (const verified of ['false', '', undefined, 'maybe'])
    assert.throws(() => verify({ ...fresh, verified }));
  for (const verificationResult of ['failure', 'cancelled', 'skipped', '', undefined])
    assert.throws(() => verify({ ...fresh, verificationResult }));
});
